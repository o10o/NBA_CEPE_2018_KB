###############
#     MVI     #
###############


###########################
# Analyse des shots de KB #
###########################


######################################################
# 0. Import des données et chargement des librairies #
######################################################


# install.packages('dplyr')
# update.packages()

library(dplyr)


source_kb_shots <- read.csv("data/source_kb_shots.csv")
source_kb_shots_JSON <- read.csv("data/source_kbshots_JSON.csv")
source_kb_regseason <- read.csv("data/source_kbLog_regseason.csv", sep=';')
source_kb_playoffs <- read.csv("data/source_kbLog_playoffs.csv", sep=';')
source_Lakers_Saison1995_2015 <- read.csv("data/Source_Lakers_Saison1995_2015.csv", sep=';')
source_Lakers_Playoffs1995_2015 <- read.csv("data/Source_Lakers_Playoffs1995_2015.csv", sep=';')

# lag2 = function(x){
#   x<-lag(x)
#   x[1]=0
#   x
# }

#####################################################################
# 1. Analyse des données sources et création de nouvelles variables #
#####################################################################

###################################
# Analyse descrpitive des données #
###################################

# Analyse combined_shot_type et action_type 

source_kb_shots %>%
  group_by(combined_shot_type) %>%
  summarise(nb=n()) %>%
  arrange(desc(nb))

source_kb_shots %>%
  group_by(action_type) %>%
  summarise(nb=n()) %>%
  arrange(desc(nb))

analyse_shot_type <- source_kb_shots %>%
  group_by(combined_shot_type, action_type) %>%
  summarise(nb=n())
  
# --> action_type est une variable détaillée de combined_shot_type
# --> Faible volumétrie sur certains action_type
# --> Création d'une variable à partir des deux avec de la volumétrie dans chaque modalité

# Nombre de matchs distincts
source_kb_shots %>% summarise(nb_game_id=n_distinct(game_id))
# --> 1559

# Nombre de dates distinctes
source_kb_shots %>% summarise(nb_game_date=n_distinct(game_date))
# --> 1559 (cohérent, on a bien autant de matchs que de dates distinctes)

# Nombre de matchs distincts par saison
source_kb_shots %>% group_by(season) %>% summarise(nb_match_season=n_distinct(game_date))

# Nombre de matchs vs. adversaires
matchs_vs_adv <- source_kb_shots %>% group_by(opponent) %>% summarise(nb_match_vs_adv=n_distinct(game_date)) %>%
  arrange(desc(nb_match_vs_adv))

# Taux de réussite vs. adversaires
ratio_shot_vs_adv <- source_kb_shots %>%
  filter(!is.na(real_shot_made_flag)) %>%
  group_by(opponent) %>%
  summarise(nb_shot=n(),
            nb_shot_ok=sum(real_shot_made_flag),
            ratio_shot_ok=nb_shot_ok/nb_shot) %>%
  arrange(desc(ratio_shot_ok))



#########################################
# Création de variables supplémentaires #
#########################################


#################################################
# A. Uniquement à partir de source_kb_shots.csv #
#################################################
kb_shots <- source_kb_shots %>%
  mutate(
    
    # temps_period : Temps en seconde depuis le début de la période QT
    #                (max 720 pour un QT et 300 en prolongations)
    temps_period=if_else(period<=4,
                         60*(11-minutes_remaining)+(59-seconds_remaining)+1,
                         60*(4-minutes_remaining)+(59-seconds_remaining)+1),
    
    # temps_remaining_period : Temps restant avant la fin de la période en seconde 
    temps_remaining_period=if_else(period<=4,
                                   720-temps_period,
                                   300-temps_period
                          ),
    
    # temps_total : Temps en seconde depuis le début du match (approximation de la fatigue)
    temps_total=if_else(period<=4,
                  720*(period-1)+temps_period,
                  720*4+300*(period-5)+temps_period),
    
    # boo_dom : Booléen match à domicile
    boo_dom=if_else(substr(matchup,5,5)=='@',0,1),
    
    # Passage de game_date au format date
    game_date=as.Date(game_date),
    
    # game_year : Année du match
    game_year=as.integer(format(game_date,"%Y")),
    
    # game_month : Mois du match
    game_month=as.integer(format(game_date,"%m")),
    
    # game_day : Jour du match
    game_day=as.integer(format(game_date,"%d")),
    
    # boo_noel : Booléen match le jour de Noël
    boo_noel=if_else(game_day==25 & game_month==12,1,0),
    
    # num_season : Numéro de la saison
    num_season=as.numeric(substr(season,1,4))-1995,
    
    # phase_carriere : Phase de la carrière de KB
    phase_carriere=if_else(game_date<"2004-07-01",1, # Avec O'Neal
                    if_else(game_date>="2004-07-01" & game_date<"2008-02-05",2, # Seul
                      if_else(game_date>="2008-02-05" & game_date<"2013-11-01",3, # Avec Gasol
                        if_else(game_date>="2013-11-01",4,99)))), # Après sa blessure
    
    # age : Âge de KB au moment du match
    age=round((as.Date(game_date)-as.Date("1978-08-23"))/365.25,1)
    
  )



##############################################################
# On ajoute la données de shot du JSON qui est plus complète #
##############################################################
kb_shots_json <- source_kb_shots_JSON %>% 
  rename(real_shot_made_flag=SHOT_MADE_FLAG,
         game_year=year,
         game_month=month,
         game_day=day) %>%
  select(-X)

kb_shots <- inner_join(kb_shots,
                   kb_shots_json,
                   by=c("period","minutes_remaining","seconds_remaining",
                        "game_year","game_month","game_day","game_event_id"))

# --> On a bien real_shot_made_flag qui vaut shot_made_flag quand ce dernier ne vaut pas NA.


# Ajout du temps de repos (nombre de jours par rapport au dernier match, max 15 en version corrigé)
ajout_temps_repos <- kb_shots %>%
  distinct(game_date) %>%
  arrange(game_date) %>%
  mutate(last_game_date=lag(game_date),
         temps_repos=as.numeric(game_date-last_game_date),
         temps_repos_corr=if_else(is.na(temps_repos) | temps_repos>15,15,temps_repos)
        ) %>%
  select(-last_game_date)

kb_shots <- kb_shots %>% inner_join(ajout_temps_repos, by="game_date")


# Ajout des variables premier/dernier shoots de KB du QT/match
premier_dernier_kb_shots_qt <- kb_shots %>%
  group_by(game_date, period) %>%
  summarise(min_qt_game_event_id=min(game_event_id),
            max_qt_game_event_id=max(game_event_id))

premier_dernier_kb_shots_match <- kb_shots %>%
  group_by(game_date) %>%
  summarise(min_match_game_event_id=min(game_event_id),
            max_match_game_event_id=max(game_event_id))

kb_shots <- kb_shots %>%
  left_join(premier_dernier_kb_shots_qt, by=c("game_date","period")) %>%
  left_join(premier_dernier_kb_shots_match, by=c("game_date")) %>%
  mutate(boo_premier_shot_qt=if_else(game_event_id==min_qt_game_event_id,1,0),
         boo_dernier_shot_qt=if_else(game_event_id==max_qt_game_event_id,1,0),
         boo_premier_shot_match=if_else(game_event_id==min_match_game_event_id,1,0),
         boo_dernier_shot_match=if_else(game_event_id==max_match_game_event_id,1,0)) %>%
  select(-min_qt_game_event_id,-max_qt_game_event_id,-min_match_game_event_id,-max_match_game_event_id)


# Ajout du temps dernier/prochain shot
# si c'est le premier shot du QT on met 12*60=720 secondes pour le temps dernier shot
# si c'est le dernier shot du QT on met 12*60=720 secondes pour le temps prochain shot
kb_shots <- kb_shots %>%
  arrange(game_date, period, temps_period, game_event_id) %>%
  mutate(temps_dernier_shot=if_else(game_date==lag(game_date) & period==lag(period),
                                    temps_period-lag(temps_period),720),
         temps_dernier_shot=if_else(is.na(temps_dernier_shot),720,temps_dernier_shot),
         temps_prochain_shot=if_else(game_date==lead(game_date) & period==lead(period),
                                    lead(temps_period)-temps_period,720),
         temps_prochain_shot=if_else(is.na(temps_prochain_shot),720,temps_prochain_shot)
        )


# Ajout nombre total de shot dans le QT/match (notion d'intensité en divisant par le temps total du QT/match)
kb_shots <- kb_shots %>%
  group_by(game_date, period) %>%
  mutate(nb_shot_qt=n(),
         intensite_shot_qt=if_else(period<=4,nb_shot_qt/12,nb_shot_qt/5)) %>%
  group_by(game_date) %>%
  mutate(nb_qt_match=max(period,4),
         nb_shot_match=n(),
         intensite_shot_match=nb_shot_match/(48+5*(nb_qt_match-4))) %>%
  ungroup()


# Au préalable on calcule le pourcentage au shot de KB pour chaque match 
# pct_shot_kb_match <- kb_shots %>%
#   group_by(game_date) %>%
#   summarise(nb_shot_tot_match=n(),
#             nb_shot_ok_match=sum(real_shot_made_flag),
#             pct_shot_match=round(nb_shot_ok_match/nb_shot_tot_match,2)) %>%
#   ungroup() %>%
#   arrange(game_date) %>%
#   mutate(nb_shot_tot_match_lag=lag(nb_shot_tot_match),
#          nb_shot_tot_match_lag=if_else(is.na(nb_shot_tot_match_lag),0L,nb_shot_tot_match_lag),
#          nb_shot_ok_match_lag=lag(nb_shot_ok_match),
#          nb_shot_ok_match_lag=if_else(is.na(nb_shot_ok_match_lag),0L,nb_shot_ok_match_lag),
#          pct_shot_match_lag=lag(pct_shot_match),
#          pct_shot_match_lag=if_else(is.na(pct_shot_match_lag),0.45,pct_shot_match_lag)) %>%
#   select(game_date, nb_shot_tot_match_lag, nb_shot_ok_match_lag, pct_shot_match_lag)
# 
# kb_shots <- kb_shots %>% inner_join(pct_shot_kb_match, by="game_date")


# Ajout du nombre total, réussi et ratio de shots  au moment ou KB prend le shot depuis le début du QT
stats_shot_kb_debut_qt <- kb_shots %>%
  arrange(game_date, period, temps_period, game_event_id) %>%
  select(game_date, period, temps_period, game_event_id, real_shot_made_flag) %>%
  filter(!is.na(real_shot_made_flag)) %>%
  group_by(game_date, period) %>%
  mutate(
    constante=1,
    nb_shot_deb_qt=cumsum(constante), # Shot pris par KB depuis le début du QT
    nb_shot_ok_deb_qt=cumsum(real_shot_made_flag), # Shot réussi par KB depuis le début du QT
    pct_shot_ok_deb_qt=round(cummean(real_shot_made_flag),2) # Pourcentage shot réussi par KB depuis le début du QT
    # Note : équivalent de nb_shot_ok_qt/nb_shot_qt
        ) %>%
  ungroup() %>%
  select(-constante) %>%
  arrange(game_date, period, temps_period, game_event_id) %>%
  mutate(
    # On calcul le lag pour ne pas prendre en compte le résultat du shot courant
    nb_shot_deb_qt_lag=if_else(game_date==lag(game_date) & period==lag(period),lag(nb_shot_deb_qt),0),
    nb_shot_deb_qt_lag=if_else(is.na(nb_shot_deb_qt_lag),0,nb_shot_deb_qt_lag),
    nb_shot_ok_deb_qt_lag=if_else(game_date==lag(game_date) & period==lag(period),lag(nb_shot_ok_deb_qt),0L),
    nb_shot_ok_deb_qt_lag=if_else(is.na(nb_shot_ok_deb_qt_lag),0L,nb_shot_ok_deb_qt_lag),
    pct_shot_ok_deb_qt_lag=if_else(game_date==lag(game_date) & period==lag(period),
                               lag(pct_shot_ok_deb_qt), 0.45), # Note : quand on est au premier shot du QT 45%
    pct_shot_ok_deb_qt_lag=if_else(is.na(pct_shot_ok_deb_qt_lag),0.45,pct_shot_ok_deb_qt_lag)
   ) %>%
  select(-real_shot_made_flag)

kb_shots <- kb_shots %>% inner_join(stats_shot_kb_debut_qt,
                                    by=c("game_date", "period", "temps_period", "game_event_id"))


# Ajout du nombre total, réussi et ratio de shots  au moment ou KB prend le shot depuis le début du match
stats_shot_kb_debut_match <- kb_shots %>%
  arrange(game_date, temps_total, game_event_id) %>%
  select(game_date, temps_total, game_event_id, real_shot_made_flag) %>%
  filter(!is.na(real_shot_made_flag)) %>%
  group_by(game_date) %>%
  mutate(
    constante=1,
    nb_shot_deb_match=cumsum(constante), # Shot pris par KB depuis le début du match
    nb_shot_ok_deb_match=cumsum(real_shot_made_flag), # Shot réussi par KB depuis le début du match
    pct_shot_ok_deb_match=round(cummean(real_shot_made_flag),2) # Pourcentage shot réussi par KB depuis le début du match
    # Note : équivalent de nb_shot_ok_match/nb_shot_match
  ) %>%
  ungroup() %>%
  select(-constante) %>%
  arrange(game_date, temps_total, game_event_id) %>%
  mutate(
    # On calcul le lag pour ne pas prendre en compte le résultat du shot courant
    nb_shot_deb_match_lag=if_else(game_date==lag(game_date),lag(nb_shot_deb_match),0),
    nb_shot_deb_match_lag=if_else(is.na(nb_shot_deb_match_lag),0,nb_shot_deb_match_lag),
    nb_shot_ok_deb_match_lag=if_else(game_date==lag(game_date),lag(nb_shot_ok_deb_match),0L),
    nb_shot_ok_deb_match_lag=if_else(is.na(nb_shot_ok_deb_match_lag),0L,nb_shot_ok_deb_match_lag),
    pct_shot_ok_deb_match_lag=if_else(game_date==lag(game_date),
                               lag(pct_shot_ok_deb_match), 0.45), # Note : quand on est au premier shot du match 45%
    pct_shot_ok_deb_match_lag=if_else(is.na(pct_shot_ok_deb_match_lag),0.45,pct_shot_ok_deb_match_lag)
  ) %>%
  select(-real_shot_made_flag)

kb_shots <- kb_shots %>% inner_join(stats_shot_kb_debut_match,
                                    by=c("game_date", "temps_total", "game_event_id"))



# Ajout de la qualité des derniers shots (jusqu'à -5) et des prochains (jusqu'à +5)
kb_shots <- kb_shots %>%
  arrange(game_date, period, temps_period, game_event_id) %>%
  mutate(score_shot_made=if_else(is.na(real_shot_made_flag),0,if_else(real_shot_made_flag==1,1,-1)),
         
         score_shot_m1=if_else(lag(game_date)==game_date,
                               lag(score_shot_made),
                               0),
         score_shot_m1=if_else(is.na(score_shot_m1),0,score_shot_m1),
         
         score_shot_m2=if_else(lag(game_date, n=2)==game_date,
                               score_shot_m1+lag(score_shot_made, n=2),
                               score_shot_m1),
         score_shot_m2=if_else(is.na(score_shot_m2),score_shot_m1,score_shot_m2),
         
         score_shot_m3=if_else(lag(game_date, n=3)==game_date,
                               score_shot_m2+lag(score_shot_made, n=3),
                               score_shot_m2),
         score_shot_m3=if_else(is.na(score_shot_m3),score_shot_m2,score_shot_m3),
         
         score_shot_m4=if_else(lag(game_date, n=4)==game_date,
                               score_shot_m3+lag(score_shot_made, n=4),
                               score_shot_m3),
         score_shot_m4=if_else(is.na(score_shot_m4),score_shot_m3,score_shot_m4),
         
         score_shot_m5=if_else(lag(game_date, n=5)==game_date,
                               score_shot_m4+lag(score_shot_made, n=5),
                               score_shot_m4),
         score_shot_m5=if_else(is.na(score_shot_m5),score_shot_m4,score_shot_m5),
         
         score_shot_p1=if_else(lead(game_date)==game_date,
                               lead(score_shot_made),
                               0),
         score_shot_p1=if_else(is.na(score_shot_p1),0,score_shot_p1),
         
         score_shot_p2=if_else(lead(game_date, n=2)==game_date,
                               score_shot_p1+lead(score_shot_made, n=2),
                               score_shot_p1),
         score_shot_p2=if_else(is.na(score_shot_p2),score_shot_p1,score_shot_p2),
         
         score_shot_p3=if_else(lead(game_date, n=3)==game_date,
                               score_shot_p2+lead(score_shot_made, n=3),
                               score_shot_p2),
         score_shot_p3=if_else(is.na(score_shot_p3),score_shot_p2,score_shot_p3),
         
         score_shot_p4=if_else(lead(game_date, n=4)==game_date,
                               score_shot_p3+lead(score_shot_made, n=4),
                               score_shot_p3),
         score_shot_p4=if_else(is.na(score_shot_p4),score_shot_p3,score_shot_p4),
         
         score_shot_p5=if_else(lead(game_date, n=5)==game_date,
                               score_shot_p4+lead(score_shot_made, n=5),
                               score_shot_p4),
         score_shot_p5=if_else(is.na(score_shot_p5),score_shot_p4,score_shot_p5)
        
        ) %>%
  arrange(combined_shot_type, game_date, game_event_id) %>%
  
  # Ajout de la qualité des derniers shots (jusqu'à -5) et des prochains (jusqu'à +5)
  # par type de shots et sans réinitialiser par match comme on le fait juste au dessus
  mutate(score_shot_type_m1=if_else(lag(combined_shot_type)==combined_shot_type,
                               lag(score_shot_made),
                               0),
         score_shot_type_m1=if_else(is.na(score_shot_type_m1),0,score_shot_type_m1),
         
         score_shot_type_m2=if_else(lag(combined_shot_type, n=2)==combined_shot_type,
                                    score_shot_type_m1+lag(score_shot_made, n=2),
                                    score_shot_type_m1),
         score_shot_type_m2=if_else(is.na(score_shot_type_m2),score_shot_type_m1,score_shot_type_m2),
         
         score_shot_type_m3=if_else(lag(combined_shot_type, n=3)==combined_shot_type,
                                    score_shot_type_m2+lag(score_shot_made, n=3),
                                    score_shot_type_m2),
         score_shot_type_m3=if_else(is.na(score_shot_type_m3),score_shot_type_m2,score_shot_type_m3),
         
         score_shot_type_m4=if_else(lag(combined_shot_type, n=4)==combined_shot_type,
                                    score_shot_type_m3+lag(score_shot_made, n=4),
                                    score_shot_type_m3),
         score_shot_type_m4=if_else(is.na(score_shot_type_m4),score_shot_type_m3,score_shot_type_m4),
         
         score_shot_type_m5=if_else(lag(combined_shot_type, n=5)==combined_shot_type,
                                    score_shot_type_m4+lag(score_shot_made, n=5),
                                    score_shot_type_m4),
         score_shot_type_m5=if_else(is.na(score_shot_type_m5),score_shot_type_m4,score_shot_type_m5),
       
         
         
         score_shot_type_p1=if_else(lead(combined_shot_type)==combined_shot_type,
                               lead(score_shot_made),
                               0),
         score_shot_type_p1=if_else(is.na(score_shot_type_p1),0,score_shot_type_p1),
         
         score_shot_type_p2=if_else(lead(combined_shot_type, n=2)==combined_shot_type,
                               score_shot_type_p1+lead(score_shot_made, n=2),
                               score_shot_type_p1),
         score_shot_type_p2=if_else(is.na(score_shot_type_p2),score_shot_type_p1,score_shot_type_p2),
         
         score_shot_type_p3=if_else(lead(combined_shot_type, n=3)==combined_shot_type,
                               score_shot_type_p2+lead(score_shot_made, n=3),
                               score_shot_type_p2),
         score_shot_type_p3=if_else(is.na(score_shot_type_p3),score_shot_type_p2,score_shot_type_p3),
         
         score_shot_type_p4=if_else(lead(combined_shot_type, n=4)==combined_shot_type,
                               score_shot_type_p3+lead(score_shot_made, n=4),
                               score_shot_type_p3),
         score_shot_type_p4=if_else(is.na(score_shot_type_p4),score_shot_type_p3,score_shot_type_p4),
         
         score_shot_type_p5=if_else(lead(combined_shot_type, n=5)==combined_shot_type,
                               score_shot_type_p4+lead(score_shot_made, n=5),
                               score_shot_type_p4),
         score_shot_type_p5=if_else(is.na(score_shot_type_p5),score_shot_type_p4,score_shot_type_p5)
        ) %>%
  select(-score_shot_made)


# Sélection des variables et mise en forme de la table
kb_shots <- kb_shots %>%
  select(shot_id, game_date, game_day, game_month, game_year,
         season, num_season, playoffs, boo_noel, age,
         opponent, boo_dom,
         temps_repos, temps_repos_corr,
         game_event_id, temps_total, period, temps_period, temps_remaining_period,
         temps_dernier_shot, temps_prochain_shot,
         boo_premier_shot_qt,boo_dernier_shot_qt,boo_premier_shot_match,boo_dernier_shot_match,
         nb_shot_qt, intensite_shot_qt, nb_shot_match, intensite_shot_match,
         nb_shot_deb_qt, nb_shot_ok_deb_qt, pct_shot_ok_deb_qt,
         nb_shot_deb_qt_lag, nb_shot_ok_deb_qt_lag, pct_shot_ok_deb_qt_lag,
         nb_shot_deb_match, nb_shot_ok_deb_match, pct_shot_ok_deb_match,
         nb_shot_deb_match_lag, nb_shot_ok_deb_match_lag, pct_shot_ok_deb_match_lag,
         score_shot_m1,score_shot_m2,score_shot_m3,score_shot_m4,score_shot_m5,
         score_shot_p1,score_shot_p2,score_shot_p3,score_shot_p4,score_shot_p5,
         score_shot_type_m1,score_shot_type_m2,score_shot_type_m3,score_shot_type_m4,score_shot_type_m5,
         score_shot_type_p1,score_shot_type_p2,score_shot_type_p3,score_shot_type_p4,score_shot_type_p5,
         shot_type, combined_shot_type, action_type,
         loc_x, loc_y, shot_distance, shot_zone_range,
         shot_zone_area, shot_zone_basic,
         shot_made_flag, real_shot_made_flag
        ) %>%
  arrange(game_date, period, temps_period, game_event_id)




###########################################################################
# B1. Ajout de données supplémentaires (stats KB sur l'ensemble du match) #
###########################################################################

kb_regseason <- source_kb_regseason %>% mutate(source='kb_regseason')
kb_playoffs <- source_kb_playoffs %>% mutate(source='kb_playoffs')

kb_stats <- bind_rows(kb_regseason,kb_playoffs) %>%
  mutate(game_date=as.Date(Date, format="%d/%m/%Y")) %>%
  arrange(game_date) %>%
  mutate(
        # X2... Stats sur les shots à 2 points du match
        X2P=FG-X3P,
        X2PA=FGA-X3PA,
        X2Ppct=round(X2P/X2PA,3),
        
        # Quand le pourcentage au tir est manquant (car aucun tir de ce type) on remplace par la moyenne carrière de KB
        FGpct=if_else(is.na(FGpct),0.444,FGpct),
        X2Ppct=if_else(is.na(X2Ppct),0.476,X2Ppct),
        X3Ppct=if_else(is.na(X3Ppct),0.3,X3Ppct),
        FTpct=if_else(is.na(FTpct),0.829,FTpct),

        # Calcul de l'EFF (Efficiency)
        EFF=PTS+ORB+DRB+AST+STL+BLK-(FGA-FG)-(FTA-FT)-TOV,
        
        # second_played : Nombre de secondes joués par KB dans le match
        second_played=if_else(nchar(MP)==5,as.numeric(substr(MP,1,2)),as.numeric(substr(MP,1,2))*60+as.numeric(substr(MP,4,5))),
        
        # ratio_played : Pourcentage du match joué par KB (2880=match complet sans prolongation, le ratio peut dépasser 1 si prolongation)
        ratio_played=round(100*second_played/2880,2),
        
        # boo_win : booléen victoire sur le match
        boo_win=if_else(substr(Win_Loss,1,1)=='W',1,-1),
        
        # plus_moins_corr: Imputation manuelle à partir de stats desc sur la variable plus_moins
        plus_moins_corr=if_else(is.na(plus_moins) & boo_win==-1 & source=="kb_playoffs",-9,
                          if_else(is.na(plus_moins) & boo_win==-1 & source=="kb_regseason",1,
                            if_else(is.na(plus_moins) & boo_win==1 & source=="kb_regseason",4,
                              if_else(is.na(plus_moins) & boo_win==1 & source=="kb_playoffs",10,as.numeric(plus_moins))))),
        
        # Variable lag du match précédent
        boo_win_lag=lag(boo_win),
        boo_win_lag=if_else(is.na(boo_win_lag),1,boo_win_lag), # Le match avant les débuts de KB est une victoire
        second_played_lag=lag(second_played),
        second_played_lag=if_else(is.na(second_played_lag),0,second_played_lag),
        ratio_played_lag=lag(ratio_played),
        ratio_played_lag=if_else(is.na(ratio_played_lag),0,ratio_played_lag),
        PTS_lag=lag(PTS),
        PTS_lag=if_else(is.na(PTS_lag),0L,PTS_lag),
        GmSc_lag=lag(GmSc),
        GmSc_lag=if_else(is.na(GmSc_lag),0,GmSc_lag),
        EFF_lag=lag(EFF),
        EFF_lag=if_else(is.na(EFF_lag),0L,EFF_lag),
        FG_lag=lag(FG),
        FG_lag=if_else(is.na(FG_lag),0L,FG_lag),
        FGA_lag=lag(FGA),
        FGA_lag=if_else(is.na(FGA_lag),0L,FGA_lag),
        FGpct_lag=lag(FGpct),
        FGpct_lag=if_else(is.na(FGpct_lag),0.444,FGpct_lag),
        X2P_lag=lag(X2P),
        X2P_lag=if_else(is.na(X2P_lag),0L,X2P_lag),
        X2PA_lag=lag(X2PA),
        X2PA_lag=if_else(is.na(X2PA_lag),0L,X2PA_lag),
        X2Ppct_lag=lag(X2Ppct),
        X2Ppct_lag=if_else(is.na(X2Ppct_lag),0.476,X2Ppct_lag),
        X3P_lag=lag(X3P),
        X3P_lag=if_else(is.na(X3P_lag),0L,X3P_lag),
        X3PA_lag=lag(X3PA),
        X3PA_lag=if_else(is.na(X3PA_lag),0L,X3PA_lag),
        X3Ppct_lag=lag(X3Ppct),
        X3Ppct_lag=if_else(is.na(X3Ppct_lag),0.3,X3Ppct_lag),
        FT_lag=lag(FT),
        FT_lag=if_else(is.na(FT_lag),0L,FT_lag),
        FTA_lag=lag(FTA),
        FTA_lag=if_else(is.na(FTA_lag),0L,FTA_lag),
        FTpct_lag=lag(FTpct),
        FTpct_lag=if_else(is.na(FTpct_lag),0.829,FTpct_lag),
        ORB_lag=lag(ORB),
        ORB_lag=if_else(is.na(ORB_lag),0L,ORB_lag),
        DRB_lag=lag(DRB),
        DRB_lag=if_else(is.na(DRB_lag),0L,DRB_lag),
        TRB_lag=lag(TRB),
        TRB_lag=if_else(is.na(TRB_lag),0L,TRB_lag),
        AST_lag=lag(AST),
        AST_lag=if_else(is.na(AST_lag),0L,AST_lag),
        STL_lag=lag(STL),
        STL_lag=if_else(is.na(STL_lag),0L,STL_lag),
        BLK_lag=lag(BLK),
        BLK_lag=if_else(is.na(BLK_lag),0L,BLK_lag),
        TOV_lag=lag(TOV),
        TOV_lag=if_else(is.na(TOV_lag),0L,TOV_lag),
        PF_lag=lag(PF),
        PF_lag=if_else(is.na(PF_lag),0L,PF_lag)
        
  ) %>%
  select(game_date, GS, Win_Loss, boo_win, boo_win_lag,
         MP, second_played, second_played_lag, ratio_played, ratio_played_lag,
         FG, FGA, FGpct, FG_lag, FGA_lag, FGpct_lag,
         X2P, X2PA, X2Ppct, X2P_lag, X2PA_lag, X2Ppct_lag,
         X3P, X3PA, X3Ppct, X3P_lag, X3PA_lag, X3Ppct_lag,
         FT, FTA, FTpct, FT_lag, FTA_lag, FTpct_lag,
         ORB, DRB, TRB, ORB_lag, DRB_lag, TRB_lag,
         AST, AST_lag,
         STL, STL_lag,
         BLK, BLK_lag,
         TOV, TOV_lag,
         PF, PF_lag,
         PTS, PTS_lag,
         GmSc, GmSc_lag,
         EFF, EFF_lag,
         plus_moins, plus_moins_corr)

kb_analyse <- kb_shots %>% inner_join(kb_stats, by="game_date")
  

#########################################################################
# B2. Ajout de données suppkémentaires (résulats des matchs des Lakers) #
#########################################################################
lakers_1995_2015 <- bind_rows(source_Lakers_Saison1995_2015,source_Lakers_Playoffs1995_2015) %>%
  mutate(game_date=as.Date(Date, format="%d/%m/%Y"),
         streak_win_lose=if_else(substr(Streak,1,1)=='W',as.numeric(substr(Streak,3,4)),-as.numeric(substr(Streak,3,4))),
         ecart_pts=Tm-Opp) %>%
  arrange(game_date) %>%
  mutate(streak_win_lose_lag=lag(streak_win_lose),
         ecart_pts_lag=lag(ecart_pts)) %>%
  select(game_date, streak_win_lose, streak_win_lose_lag, ecart_pts, ecart_pts_lag)
  
kb_analyse <- kb_analyse %>% inner_join(lakers_1995_2015, by="game_date")


#######################################
# Sauvegarde de la table pour analyse #
#######################################
write.csv(kb_analyse, file='data/kb_analyse.csv')


################################
# Chargement du CSV kb_analyse #
################################
kb_analyse <- read.csv(file='data/kb_analyse.csv') %>%
  select(-X) %>%
  mutate(game_date=as.Date(game_date),
         # plus_moins_corr: Imputation manuelle à partir de stats desc sur la variable plus_moins
         plus_moins_corr=if_else(is.na(plus_moins) & boo_win==-1 & playoffs==1,-9,
                                 if_else(is.na(plus_moins) & boo_win==-1 & playoffs==0,1,
                                         if_else(is.na(plus_moins) & boo_win==1 & playoffs==0,4,
                                                 if_else(is.na(plus_moins) & boo_win==1 & playoffs==1,10,as.numeric(plus_moins)))))
        ) %>%
  arrange(game_date, period, temps_period, game_event_id)
  
# Export en CSV de la liste des variables de kb_analyse
as.data.frame(colnames(kb_analyse)) %>% write.csv(file='data/nom_var.csv')

###
# Idées autres variables à créer ? :
#
# * Variable Équipe adverse (fusion de franchise ?)
# * Variable qualité shoot au début du match (Q1/Q2 pour prédire Q3/Q4 par exemple)
# * Variable clunch moment
# * Variable qualité de la saison A et A-1 (score saison régulière et tour élimination playoffs)
###


####################################
# Tests et contructions de modèles #
####################################

# Chargement de librairies
library(caret)
library(Matrix)
library(DiagrammeR)
library(data.table)
library(mlr) # Attention, écrase le train de caret

# Initialisation de kb_model, passage de real_shot_made_flag en facteur et gestion du type de certaines variables explicatives
kb_model <- kb_analyse %>%
  mutate(real_shot_made_flag=as.factor(if_else(real_shot_made_flag==1,'Réussi','Raté')),
         period=as.factor(period),
         playoffs=as.factor(playoffs),
         boo_dom=as.factor(boo_dom),
         game_day=as.factor(game_day),
         game_month=as.factor(game_month),
         game_year=as.factor(game_year),
         boo_noel=as.factor(boo_noel),
         boo_premier_shot_qt=as.factor(boo_premier_shot_qt),
         boo_dernier_shot_qt=as.factor(boo_dernier_shot_qt),
         boo_premier_shot_match=as.factor(boo_premier_shot_match),
         boo_dernier_shot_match=as.factor(boo_dernier_shot_match),
         GS=as.factor(GS),
         boo_win=as.factor(boo_win),
         boo_win_lag=as.factor(boo_win_lag)
         #alea=runif(nrow(kb_analyse)) # Ajout d'une variable aléatoire)
  )


# On fixe la référence à 'Réussi'
kb_model$real_shot_made_flag <- relevel(kb_model$real_shot_made_flag, ref = 'Réussi')


# Échantillon train / test / validation
kb_model_train_test <- kb_model %>% filter(!(is.na(shot_made_flag))) %>% select(-shot_made_flag)
kb_model_validation <- kb_model %>% filter(is.na(shot_made_flag)) %>% select(-shot_made_flag)

set.seed(75)
splitIndex <- createDataPartition(y = kb_model_train_test$real_shot_made_flag, p=0.8, list=FALSE, times=1)
kb_model_train <- data.frame(kb_model_train_test[ splitIndex,])
kb_model_test  <- data.frame(kb_model_train_test[-splitIndex,])
  
# Proportion de Y=1 dans les différents échantillons
table(kb_model_train$real_shot_made_flag)/sum(table(kb_model_train$real_shot_made_flag))
table(kb_model_test$real_shot_made_flag)/sum(table(kb_model_test$real_shot_made_flag))
table(kb_model_validation$real_shot_made_flag)/sum(table(kb_model_validation$real_shot_made_flag))
# --> OK : les proportions sont très proches


# Passage en data table
setDT(kb_model_train) 
setDT(kb_model_test)
setDT(kb_model_validation)


# Gestion de la variable cible (extraction, passage en numérique et passage de 'Raté' à la valeur 0 ('Réussi' valeur 1))
train_labels <- kb_model_train$real_shot_made_flag %>% relevel(ref = 'Raté') %>% as.numeric()-1
test_labels <- kb_model_test$real_shot_made_flag %>% relevel(ref = 'Raté') %>% as.numeric()-1
validation_labels <- kb_model_validation$real_shot_made_flag %>% relevel(ref = 'Raté') %>% as.numeric()-1


###
# Sélection des variables explicatives
###

# Version "light" : variables de base uniquement
kb_model_train_light <- kb_model_train %>%
  select(real_shot_made_flag,
         action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
         shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
         boo_dom, opponent, temps_total, temps_period)

kb_model_test_light <- kb_model_test %>%
  select(real_shot_made_flag,
         action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
         shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
         boo_dom, opponent, temps_total, temps_period)

kb_model_validation_light <- kb_model_validation %>%
  select(real_shot_made_flag,
         action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
         shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
         boo_dom, opponent, temps_total, temps_period)

  
# Version "prévision" : variables de base + variables sur les matchs et dynamiques des shots dans le passé uniquement
kb_model_train_prev <- kb_model_train %>%
  select(real_shot_made_flag,
       action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
       shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
       boo_dom, opponent, temps_total, temps_period,
       # Variables supplémentaires par rapport à la version "light"
       game_day, game_month, game_year,
       boo_noel, age, temps_repos_corr, temps_remaining_period,
       temps_dernier_shot, # temps_prochain_shot,
       boo_premier_shot_qt, # boo_dernier_shot_qt,
       boo_premier_shot_match, # boo_dernier_shot_match,
       #nb_shot_qt, intensite_shot_qt,
       #nb_shot_match, intensite_shot_match,
       nb_shot_deb_qt, #nb_shot_ok_deb_qt, pct_shot_ok_deb_qt,
       nb_shot_deb_qt_lag, nb_shot_ok_deb_qt_lag, pct_shot_ok_deb_qt_lag,
       nb_shot_deb_match, #nb_shot_ok_deb_match, pct_shot_ok_deb_match,
       nb_shot_deb_match_lag, nb_shot_ok_deb_match_lag, pct_shot_ok_deb_match_lag,
       score_shot_m1, score_shot_m2, score_shot_m3, score_shot_m4, score_shot_m5,
       #score_shot_p1, score_shot_p2, score_shot_p3, score_shot_p4, score_shot_p5,
       score_shot_type_m1, score_shot_type_m2, score_shot_type_m3, score_shot_type_m4, score_shot_type_m5, 
       #score_shot_type_p1, score_shot_type_p2, score_shot_type_p3, score_shot_type_p4, score_shot_type_p5,
       GS,
       #boo_win,
       boo_win_lag,
       #second_played,
       second_played_lag,
       #ratio_played,
       ratio_played_lag,
       #FGA, FG, FGpct,
       FGA_lag, FG_lag, FGpct_lag,
       #X2PA, X2P, X2Ppct,
       X2P_lag, X2PA_lag, X2Ppct_lag,
       #X3PA, X3P, X3Ppct,
       X3P_lag, X3PA_lag, X3Ppct_lag,
       #FT, FTA, FTpct,
       FT_lag, FTA_lag, FTpct_lag,
       #ORB, DRB, TRB, AST, STL, BLK, TOV, PF
       ORB_lag, DRB_lag, TRB_lag, AST_lag, STL_lag, BLK_lag, TOV_lag, PF_lag,
       # PTS,
       PTS_lag,
       #GmSc, EFF
       GmSc_lag, EFF_lag,
       #plus_moins_corr,
       #streak_win_lose,
       streak_win_lose_lag,
       #ecart_pts,
       ecart_pts_lag)


kb_model_test_prev <- kb_model_test %>%
  select(real_shot_made_flag,
       action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
       shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
       boo_dom, opponent, temps_total, temps_period,
       # Variables supplémentaires par rapport à la version "light"
       game_day, game_month, game_year,
       boo_noel, age, temps_repos_corr, temps_remaining_period,
       temps_dernier_shot, # temps_prochain_shot,
       boo_premier_shot_qt, # boo_dernier_shot_qt,
       boo_premier_shot_match, # boo_dernier_shot_match,
       #nb_shot_qt, intensite_shot_qt,
       #nb_shot_match, intensite_shot_match,
       nb_shot_deb_qt, #nb_shot_ok_deb_qt, pct_shot_ok_deb_qt,
       nb_shot_deb_qt_lag, nb_shot_ok_deb_qt_lag, pct_shot_ok_deb_qt_lag,
       nb_shot_deb_match, #nb_shot_ok_deb_match, pct_shot_ok_deb_match,
       nb_shot_deb_match_lag, nb_shot_ok_deb_match_lag, pct_shot_ok_deb_match_lag,
       score_shot_m1, score_shot_m2, score_shot_m3, score_shot_m4, score_shot_m5,
       #score_shot_p1, score_shot_p2, score_shot_p3, score_shot_p4, score_shot_p5,
       score_shot_type_m1, score_shot_type_m2, score_shot_type_m3, score_shot_type_m4, score_shot_type_m5, 
       #score_shot_type_p1, score_shot_type_p2, score_shot_type_p3, score_shot_type_p4, score_shot_type_p5,
       GS,
       #boo_win,
       boo_win_lag,
       #second_played,
       second_played_lag,
       #ratio_played,
       ratio_played_lag,
       #FGA, FG, FGpct,
       FGA_lag, FG_lag, FGpct_lag,
       #X2PA, X2P, X2Ppct,
       X2P_lag, X2PA_lag, X2Ppct_lag,
       #X3PA, X3P, X3Ppct,
       X3P_lag, X3PA_lag, X3Ppct_lag,
       #FT, FTA, FTpct,
       FT_lag, FTA_lag, FTpct_lag,
       #ORB, DRB, TRB, AST, STL, BLK, TOV, PF
       ORB_lag, DRB_lag, TRB_lag, AST_lag, STL_lag, BLK_lag, TOV_lag, PF_lag,
       # PTS,
       PTS_lag,
       #GmSc, EFF
       GmSc_lag, EFF_lag,
       #plus_moins_corr,
       #streak_win_lose,
       streak_win_lose_lag,
       #ecart_pts,
       ecart_pts_lag)


kb_model_validation_prev <- kb_model_validation %>%
  select(real_shot_made_flag,
       action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
       shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
       boo_dom, opponent, temps_total, temps_period,
       # Variables supplémentaires par rapport à la version "light"
       game_day, game_month, game_year,
       boo_noel, age, temps_repos_corr, temps_remaining_period,
       temps_dernier_shot, # temps_prochain_shot,
       boo_premier_shot_qt, # boo_dernier_shot_qt,
       boo_premier_shot_match, # boo_dernier_shot_match,
       #nb_shot_qt, intensite_shot_qt,
       #nb_shot_match, intensite_shot_match,
       nb_shot_deb_qt, #nb_shot_ok_deb_qt, pct_shot_ok_deb_qt,
       nb_shot_deb_qt_lag, nb_shot_ok_deb_qt_lag, pct_shot_ok_deb_qt_lag,
       nb_shot_deb_match, #nb_shot_ok_deb_match, pct_shot_ok_deb_match,
       nb_shot_deb_match_lag, nb_shot_ok_deb_match_lag, pct_shot_ok_deb_match_lag,
       score_shot_m1, score_shot_m2, score_shot_m3, score_shot_m4, score_shot_m5,
       #score_shot_p1, score_shot_p2, score_shot_p3, score_shot_p4, score_shot_p5,
       score_shot_type_m1, score_shot_type_m2, score_shot_type_m3, score_shot_type_m4, score_shot_type_m5, 
       #score_shot_type_p1, score_shot_type_p2, score_shot_type_p3, score_shot_type_p4, score_shot_type_p5,
       GS,
       #boo_win,
       boo_win_lag,
       #second_played,
       second_played_lag,
       #ratio_played,
       ratio_played_lag,
       #FGA, FG, FGpct,
       FGA_lag, FG_lag, FGpct_lag,
       #X2PA, X2P, X2Ppct,
       X2P_lag, X2PA_lag, X2Ppct_lag,
       #X3PA, X3P, X3Ppct,
       X3P_lag, X3PA_lag, X3Ppct_lag,
       #FT, FTA, FTpct,
       FT_lag, FTA_lag, FTpct_lag,
       #ORB, DRB, TRB, AST, STL, BLK, TOV, PF
       ORB_lag, DRB_lag, TRB_lag, AST_lag, STL_lag, BLK_lag, TOV_lag, PF_lag,
       # PTS,
       PTS_lag,
       #GmSc, EFF
       GmSc_lag, EFF_lag,
       #plus_moins_corr,
       #streak_win_lose,
       streak_win_lose_lag,
       #ecart_pts,
       ecart_pts_lag)


# Version "full" : variables de base + variables sur les matchs et dynamiques des shots dans le passé et dans le futur
# On s'interdit d'utiliser les variables qui permettent d'avoir accès au nombre de points que KB a marqué dans le match courant
kb_model_train_full <- kb_model_train %>%
  select(real_shot_made_flag,
       action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
       shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
       boo_dom, opponent, temps_total, temps_period,
       # Variables supplémentaires par rapport à la version "light"
       game_day, game_month, game_year,
       boo_noel, age, temps_repos_corr, temps_remaining_period,
       temps_dernier_shot, temps_prochain_shot,
       boo_premier_shot_qt, boo_dernier_shot_qt,
       boo_premier_shot_match, boo_dernier_shot_match,
       nb_shot_qt, intensite_shot_qt,
       nb_shot_match, intensite_shot_match,
       nb_shot_deb_qt, #nb_shot_ok_deb_qt, pct_shot_ok_deb_qt,
       nb_shot_deb_qt_lag, nb_shot_ok_deb_qt_lag, pct_shot_ok_deb_qt_lag,
       nb_shot_deb_match, #nb_shot_ok_deb_match, pct_shot_ok_deb_match,
       nb_shot_deb_match_lag, nb_shot_ok_deb_match_lag, pct_shot_ok_deb_match_lag,
       score_shot_m1, score_shot_m2, score_shot_m3, score_shot_m4, score_shot_m5,
       score_shot_p1, score_shot_p2, score_shot_p3, score_shot_p4, score_shot_p5,
       score_shot_type_m1, score_shot_type_m2, score_shot_type_m3, score_shot_type_m4, score_shot_type_m5, 
       score_shot_type_p1, score_shot_type_p2, score_shot_type_p3, score_shot_type_p4, score_shot_type_p5,
       GS, boo_win, boo_win_lag,
       second_played, second_played_lag, ratio_played, ratio_played_lag,
       FGA, # FG, FGpct,
       FGA_lag, FG_lag, FGpct_lag,
       X2PA, #X2P, X2Ppct,
       X2P_lag, X2PA_lag, X2Ppct_lag,
       X3PA, #X3P, X3Ppct,
       X3P_lag, X3PA_lag, X3Ppct_lag,
       FT, FTA, FTpct, FT_lag, FTA_lag, FTpct_lag,
       ORB, DRB, TRB, AST, STL, BLK, TOV, PF,
       ORB_lag, DRB_lag, TRB_lag, AST_lag, STL_lag, BLK_lag, TOV_lag, PF_lag,
       # PTS,
       PTS_lag,
       # GmSc, EFF,
       GmSc_lag, EFF_lag,
       #plus_moins_corr,
       streak_win_lose, streak_win_lose_lag, ecart_pts, ecart_pts_lag
  )

kb_model_test_full <- kb_model_test %>%
  select(real_shot_made_flag,
       action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
       shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
       boo_dom, opponent, temps_total, temps_period,
       # Variables supplémentaires par rapport à la version "light"
       game_day, game_month, game_year,
       boo_noel, age, temps_repos_corr, temps_remaining_period,
       temps_dernier_shot, temps_prochain_shot,
       boo_premier_shot_qt, boo_dernier_shot_qt,
       boo_premier_shot_match, boo_dernier_shot_match,
       nb_shot_qt, intensite_shot_qt,
       nb_shot_match, intensite_shot_match,
       nb_shot_deb_qt, #nb_shot_ok_deb_qt, pct_shot_ok_deb_qt,
       nb_shot_deb_qt_lag, nb_shot_ok_deb_qt_lag, pct_shot_ok_deb_qt_lag,
       nb_shot_deb_match, #nb_shot_ok_deb_match, pct_shot_ok_deb_match,
       nb_shot_deb_match_lag, nb_shot_ok_deb_match_lag, pct_shot_ok_deb_match_lag,
       score_shot_m1, score_shot_m2, score_shot_m3, score_shot_m4, score_shot_m5,
       score_shot_p1, score_shot_p2, score_shot_p3, score_shot_p4, score_shot_p5,
       score_shot_type_m1, score_shot_type_m2, score_shot_type_m3, score_shot_type_m4, score_shot_type_m5, 
       score_shot_type_p1, score_shot_type_p2, score_shot_type_p3, score_shot_type_p4, score_shot_type_p5,
       GS, boo_win, boo_win_lag,
       second_played, second_played_lag, ratio_played, ratio_played_lag,
       FGA, # FG, FGpct,
       FGA_lag, FG_lag, FGpct_lag,
       X2PA, #X2P, X2Ppct,
       X2P_lag, X2PA_lag, X2Ppct_lag,
       X3PA, #X3P, X3Ppct,
       X3P_lag, X3PA_lag, X3Ppct_lag,
       FT, FTA, FTpct, FT_lag, FTA_lag, FTpct_lag,
       ORB, DRB, TRB, AST, STL, BLK, TOV, PF,
       ORB_lag, DRB_lag, TRB_lag, AST_lag, STL_lag, BLK_lag, TOV_lag, PF_lag,
       # PTS,
       PTS_lag,
       # GmSc, EFF,
       GmSc_lag, EFF_lag,
       #plus_moins_corr,
       streak_win_lose, streak_win_lose_lag, ecart_pts, ecart_pts_lag
  )

kb_model_validation_full <- kb_model_validation %>%
  select(real_shot_made_flag,
       action_type, combined_shot_type, loc_x, loc_y, period, playoffs, season,
       shot_distance, shot_type, shot_zone_area, shot_zone_basic, shot_zone_range,
       boo_dom, opponent, temps_total, temps_period,
       # Variables supplémentaires par rapport à la version "light"
       game_day, game_month, game_year,
       boo_noel, age, temps_repos_corr, temps_remaining_period,
       temps_dernier_shot, temps_prochain_shot,
       boo_premier_shot_qt, boo_dernier_shot_qt,
       boo_premier_shot_match, boo_dernier_shot_match,
       nb_shot_qt, intensite_shot_qt,
       nb_shot_match, intensite_shot_match,
       nb_shot_deb_qt, #nb_shot_ok_deb_qt, pct_shot_ok_deb_qt,
       nb_shot_deb_qt_lag, nb_shot_ok_deb_qt_lag, pct_shot_ok_deb_qt_lag,
       nb_shot_deb_match, #nb_shot_ok_deb_match, pct_shot_ok_deb_match,
       nb_shot_deb_match_lag, nb_shot_ok_deb_match_lag, pct_shot_ok_deb_match_lag,
       score_shot_m1, score_shot_m2, score_shot_m3, score_shot_m4, score_shot_m5,
       score_shot_p1, score_shot_p2, score_shot_p3, score_shot_p4, score_shot_p5,
       score_shot_type_m1, score_shot_type_m2, score_shot_type_m3, score_shot_type_m4, score_shot_type_m5, 
       score_shot_type_p1, score_shot_type_p2, score_shot_type_p3, score_shot_type_p4, score_shot_type_p5,
       GS, boo_win, boo_win_lag,
       second_played, second_played_lag, ratio_played, ratio_played_lag,
       FGA, # FG, FGpct,
       FGA_lag, FG_lag, FGpct_lag,
       X2PA, #X2P, X2Ppct,
       X2P_lag, X2PA_lag, X2Ppct_lag,
       X3PA, #X3P, X3Ppct,
       X3P_lag, X3PA_lag, X3Ppct_lag,
       FT, FTA, FTpct, FT_lag, FTA_lag, FTpct_lag,
       ORB, DRB, TRB, AST, STL, BLK, TOV, PF,
       ORB_lag, DRB_lag, TRB_lag, AST_lag, STL_lag, BLK_lag, TOV_lag, PF_lag,
       # PTS,
       PTS_lag,
       # GmSc, EFF,
       GmSc_lag, EFF_lag,
       #plus_moins_corr,
       streak_win_lose, streak_win_lose_lag, ecart_pts, ecart_pts_lag
  )


###########
# XGBoost #
###########

# Chargement de xgboost
library(xgboost)

# Vérification des valeurs manquantes

table(is.na(kb_model_train_light))
sapply(kb_model_train_light, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_test_light))
sapply(kb_model_test_light, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_validation_light))
sapply(kb_model_validation_light, function(x) sum(is.na(x))/length(x))*100


table(is.na(kb_model_train_prev))
sapply(kb_model_train_prev, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_test_prev))
sapply(kb_model_test_prev, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_validation_prev))
sapply(kb_model_validation_prev, function(x) sum(is.na(x))/length(x))*100


table(is.na(kb_model_train_full))
sapply(kb_model_train_full, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_test_full))
sapply(kb_model_test_full, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_validation_full))
sapply(kb_model_validation_full, function(x) sum(is.na(x))/length(x))*100



# Passage en en matrice one hot encoding (une modalité devient une variable binaire)

train_light_matrix <- model.matrix(~.+0, data=kb_model_train_light[,-c("real_shot_made_flag"), with=F])
test_light_matrix <- model.matrix(~.+0, data=kb_model_test_light[,-c("real_shot_made_flag"), with=F])
validation_light_matrix <- model.matrix(~.+0, data=kb_model_validation_light[,-c("real_shot_made_flag"), with=F])

train_prev_matrix <- model.matrix(~.+0, data=kb_model_train_prev[,-c("real_shot_made_flag"), with=F])
test_prev_matrix <- model.matrix(~.+0, data=kb_model_test_prev[,-c("real_shot_made_flag"), with=F])
validation_prev_matrix <- model.matrix(~.+0, data=kb_model_validation_prev[,-c("real_shot_made_flag"), with=F])

train_full_matrix <- model.matrix(~.+0, data=kb_model_train_full[,-c("real_shot_made_flag"), with=F])
test_full_matrix <- model.matrix(~.+0, data=kb_model_test_full[,-c("real_shot_made_flag"), with=F])
validation_full_matrix <- model.matrix(~.+0, data=kb_model_validation_full[,-c("real_shot_made_flag"), with=F])


# Passage en xgb.DMatrix

train_light_xgb_matrix <- xgb.DMatrix(data=train_light_matrix, label=train_labels)
test_light_xgb_matrix <- xgb.DMatrix(data=test_light_matrix, label=test_labels)
validation_light_xgb_matrix <- xgb.DMatrix(data=validation_light_matrix, label=validation_labels)

train_prev_xgb_matrix <- xgb.DMatrix(data=train_prev_matrix, label=train_labels)
test_prev_xgb_matrix <- xgb.DMatrix(data=test_prev_matrix, label=test_labels)
validation_prev_xgb_matrix <- xgb.DMatrix(data=validation_prev_matrix, label=validation_labels)

train_full_xgb_matrix <- xgb.DMatrix(data=train_full_matrix, label=train_labels)
test_full_xgb_matrix <- xgb.DMatrix(data=test_full_matrix, label=test_labels)
validation_full_xgb_matrix <- xgb.DMatrix(data=validation_full_matrix, label=validation_labels)



# Choix des paramètres (et choix de la logloss pour la métrique)
params <- list(booster="gbtree", objective="binary:logistic", eval_metric="logloss",
               eta=0.001, gamma=0, max_depth=9, min_child_weight=0.1, subsample=0.75, colsample_bytree=0.7, lambda=1, alpha=0)

# Modèles "Light"
set.seed(75)
xgb_light_optim_val<- xgb.train(params=params, data=train_light_xgb_matrix, print_every_n=1,
                                nrounds=20000, early_stopping_rounds=1000, maximize=F,
                                watchlist=list(train=train_light_xgb_matrix,
                                               test=test_light_xgb_matrix,
                                               validation=validation_light_xgb_matrix))

set.seed(75)
xgb_light_optim_test <- xgb.train(params=params, data=train_light_xgb_matrix, print_every_n=1,
                                  nrounds=20000, early_stopping_rounds=1000, maximize=F,
                                  watchlist=list(train=train_light_xgb_matrix,
                                                 validation=validation_light_xgb_matrix,
                                                 test=test_light_xgb_matrix))


# Modèles "Prev"
set.seed(75)
xgb_prev_optim_val<- xgb.train(params=params, data=train_prev_xgb_matrix, print_every_n=1,
                                nrounds=20000, early_stopping_rounds=1000, maximize=F,
                                watchlist=list(train=train_prev_xgb_matrix,
                                               test=test_prev_xgb_matrix,
                                               validation=validation_prev_xgb_matrix))

set.seed(75)
xgb_prev_optim_test <- xgb.train(params=params, data=train_prev_xgb_matrix, print_every_n=1,
                                  nrounds=20000, early_stopping_rounds=1000, maximize=F,
                                  watchlist=list(train=train_prev_xgb_matrix,
                                                 validation=validation_prev_xgb_matrix,
                                                 test=test_prev_xgb_matrix))

# Modèles "Full"
set.seed(75)
xgb_full_optim_val<- xgb.train(params=params, data=train_full_xgb_matrix, print_every_n=1,
                                nrounds=20000, early_stopping_rounds=1000, maximize=F,
                                watchlist=list(train=train_full_xgb_matrix,
                                               test=test_full_xgb_matrix,
                                               validation=validation_full_xgb_matrix))

set.seed(75)
xgb_full_optim_test <- xgb.train(params=params, data=train_full_xgb_matrix, print_every_n=1,
                                  nrounds=20000, early_stopping_rounds=1000, maximize=F,
                                  watchlist=list(train=train_full_xgb_matrix,
                                                 validation=validation_full_xgb_matrix,
                                                 test=test_full_xgb_matrix))
 




# Ajout de la Probabilité et calcul de la LogLoss
kb_train_light_optim_val <- kb_model_train_light %>%
  mutate(prediction=predict(xgb_light_optim_val, train_light_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))
kb_train_light_optim_test <- kb_model_train_light %>%
  mutate(prediction=predict(xgb_light_optim_test, train_light_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_test_light_optim_val <- kb_model_test_light %>%
  mutate(prediction=predict(xgb_light_optim_val, test_light_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_test_light_optim_test <- kb_model_test_light %>%
  mutate(prediction=predict(xgb_light_optim_test, test_light_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_validation_light_optim_val <- kb_model_validation_light %>%
  mutate(prediction=predict(xgb_light_optim_val, validation_light_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_validation_light_optim_test <- kb_model_validation_light %>%
  mutate(prediction=predict(xgb_light_optim_test, validation_light_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

mean(kb_train_light_optim_val$logLoss)
mean(kb_train_light_optim_test$logLoss)
mean(kb_test_light_optim_val$logLoss)
mean(kb_test_light_optim_test$logLoss) 
mean(kb_validation_light_optim_val$logLoss)
mean(kb_validation_light_optim_test$logLoss)



kb_train_prev_optim_val <- kb_model_train_prev %>%
  mutate(prediction=predict(xgb_prev_optim_val, train_prev_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))
kb_train_prev_optim_test <- kb_model_train_prev %>%
  mutate(prediction=predict(xgb_prev_optim_test, train_prev_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_test_prev_optim_val <- kb_model_test_prev %>%
  mutate(prediction=predict(xgb_prev_optim_val, test_prev_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_test_prev_optim_test <- kb_model_test_prev %>%
  mutate(prediction=predict(xgb_prev_optim_test, test_prev_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_validation_prev_optim_val <- kb_model_validation_prev %>%
  mutate(prediction=predict(xgb_prev_optim_val, validation_prev_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_validation_prev_optim_test <- kb_model_validation_prev %>%
  mutate(prediction=predict(xgb_prev_optim_test, validation_prev_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

mean(kb_train_prev_optim_val$logLoss)
mean(kb_train_prev_optim_test$logLoss)
mean(kb_test_prev_optim_val$logLoss)
mean(kb_test_prev_optim_test$logLoss) 
mean(kb_validation_prev_optim_val$logLoss)
mean(kb_validation_prev_optim_test$logLoss)



kb_train_full_optim_val <- kb_model_train_full %>%
  mutate(prediction=predict(xgb_full_optim_val, train_full_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))
kb_train_full_optim_test <- kb_model_train_full %>%
  mutate(prediction=predict(xgb_full_optim_test, train_full_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_test_full_optim_val <- kb_model_test_full %>%
  mutate(prediction=predict(xgb_full_optim_val, test_full_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_test_full_optim_test <- kb_model_test_full %>%
  mutate(prediction=predict(xgb_full_optim_test, test_full_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_validation_full_optim_val <- kb_model_validation_full %>%
  mutate(prediction=predict(xgb_full_optim_val, validation_full_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

kb_validation_full_optim_test <- kb_model_validation_full %>%
  mutate(prediction=predict(xgb_full_optim_test, validation_full_xgb_matrix),
         realite=if_else(real_shot_made_flag=='Réussi',1,0),
         logLoss=if_else(realite==1,-log(prediction),-log(1-prediction)))

mean(kb_train_full_optim_val$logLoss)
mean(kb_train_full_optim_test$logLoss)
mean(kb_test_full_optim_val$logLoss)
mean(kb_test_full_optim_test$logLoss) 
mean(kb_validation_full_optim_val$logLoss)
mean(kb_validation_full_optim_test$logLoss)



# Prédictions
train_light_preds_optim_val <-if_else(predict(xgb_light_optim_val, train_light_xgb_matrix)>0.5,1,0)
train_light_preds_optim_test <-if_else(predict(xgb_light_optim_test, train_light_xgb_matrix)>0.5,1,0)
train_prev_preds_optim_val <-if_else(predict(xgb_prev_optim_val, train_prev_xgb_matrix)>0.5,1,0)
train_prev_preds_optim_test <-if_else(predict(xgb_prev_optim_test, train_prev_xgb_matrix)>0.5,1,0)
train_full_preds_optim_val <-if_else(predict(xgb_full_optim_val, train_full_xgb_matrix)>0.5,1,0)
train_full_preds_optim_test <-if_else(predict(xgb_full_optim_test, train_full_xgb_matrix)>0.5,1,0)

test_light_preds_optim_val <-if_else(predict(xgb_light_optim_val, test_light_xgb_matrix)>0.5,1,0)
test_light_preds_optim_test <-if_else(predict(xgb_light_optim_test, test_light_xgb_matrix)>0.5,1,0)
test_prev_preds_optim_val <-if_else(predict(xgb_prev_optim_val, test_prev_xgb_matrix)>0.5,1,0)
test_prev_preds_optim_test <-if_else(predict(xgb_prev_optim_test, test_prev_xgb_matrix)>0.5,1,0)
test_full_preds_optim_val <-if_else(predict(xgb_full_optim_val, test_full_xgb_matrix)>0.5,1,0)
test_full_preds_optim_test <-if_else(predict(xgb_full_optim_test, test_full_xgb_matrix)>0.5,1,0)

validation_light_preds_optim_val <-if_else(predict(xgb_light_optim_val, validation_light_xgb_matrix)>0.5,1,0)
validation_light_preds_optim_test <-if_else(predict(xgb_light_optim_test, validation_light_xgb_matrix)>0.5,1,0)
validation_prev_preds_optim_val <-if_else(predict(xgb_prev_optim_val, validation_prev_xgb_matrix)>0.5,1,0)
validation_prev_preds_optim_test <-if_else(predict(xgb_prev_optim_test, validation_prev_xgb_matrix)>0.5,1,0)
validation_full_preds_optim_val <-if_else(predict(xgb_full_optim_val, validation_full_xgb_matrix)>0.5,1,0)
validation_full_preds_optim_test <-if_else(predict(xgb_full_optim_test, validation_full_xgb_matrix)>0.5,1,0)


# Matrices de confusion

confusionMatrix(train_light_preds_optim_val, train_labels)
confusionMatrix(train_light_preds_optim_test, train_labels)
confusionMatrix(train_prev_preds_optim_val, train_labels)
confusionMatrix(train_prev_preds_optim_test, train_labels)
confusionMatrix(train_full_preds_optim_val, train_labels)
confusionMatrix(train_full_preds_optim_test, train_labels)

confusionMatrix(test_light_preds_optim_val, test_labels)
confusionMatrix(test_light_preds_optim_test, test_labels)
confusionMatrix(test_prev_preds_optim_val, test_labels)
confusionMatrix(test_prev_preds_optim_test, test_labels)
confusionMatrix(test_full_preds_optim_val, test_labels)
confusionMatrix(test_full_preds_optim_test, test_labels)

confusionMatrix(validation_light_preds_optim_val, validation_labels)
confusionMatrix(validation_light_preds_optim_test, validation_labels)
confusionMatrix(validation_prev_preds_optim_val, validation_labels)
confusionMatrix(validation_prev_preds_optim_test, validation_labels)
confusionMatrix(validation_full_preds_optim_val, validation_labels)
confusionMatrix(validation_full_preds_optim_test, validation_labels)

confusionMatrix(train_preds, train_labels) # Accuracy : "Light" : 72,00 % / "Prev" : 75,15 % / "Full" : 86,58 %
confusionMatrix(test_preds, test_labels) # Accuracy : "Light" : 68,73 % / "Prev" : 68,87 % / "Full" : 70,05 %
confusionMatrix(validation_preds, validation_labels) # Accuracy : "Light" : 68,06 % / "Prev" : 68,40 % / "Full" : 69,54 %

# Importance des variables
imp_mat_light_optim_val <- xgb.importance(feature_names=colnames(train_light_matrix), model=xgb_light_optim_val)
xgb.plot.importance (importance_matrix=imp_mat_light_optim_val[1:20])
imp_mat_light_optim_test <- xgb.importance(feature_names=colnames(train_light_matrix), model=xgb_light_optim_test)
xgb.plot.importance (importance_matrix=imp_mat_light_optim_test[1:20])

imp_mat_prev_optim_val <- xgb.importance(feature_names=colnames(train_prev_matrix), model=xgb_prev_optim_val)
xgb.plot.importance (importance_matrix=imp_mat_prev_optim_val[1:20])
imp_mat_prev_optim_test <- xgb.importance(feature_names=colnames(train_prev_matrix), model=xgb_prev_optim_test)
xgb.plot.importance (importance_matrix=imp_mat_prev_optim_test[1:20])

imp_mat_full_optim_val <- xgb.importance(feature_names=colnames(train_full_matrix), model=xgb_full_optim_val)
xgb.plot.importance (importance_matrix=imp_mat_full_optim_val[1:20])
imp_mat_full_optim_test <- xgb.importance(feature_names=colnames(train_full_matrix), model=xgb_full_optim_test)
xgb.plot.importance (importance_matrix=imp_mat_full_optim_test[1:20])



###
# Test code
###

# Test de xgb.cv
# Paramètres par défauts (et choix de la logloss pour la métrique)
params <- list(booster="gbtree", objective="binary:logistic", eval_metric="logloss",
               eta=0.05, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1, lambda=1, alpha=0)

set.seed(75)
xgb_m1_cv <- xgb.cv(params=params, data=train_xgb_matrix, nrounds=100, nfold=10, showsd=T, stratified=T,
                 print_every_n=1, early_stopping_rounds=20, maximize=F)


# xgboost task parameters
nrounds <- 1000
folds <- 10
obj <- 'binary:logistic'
eval <- 'logloss'

# Parameter grid to search
params <- list(
  eval_metric = eval,
  objective = obj,
  eta = c(0.1,0.01),
  max_depth = c(4,6,8,10),
  max_delta_step = c(0,1),
  subsample = 1,
  scale_pos_weight = 1
)

# Table to track performance from each worker node
res <- data.frame()

kb_train <- kb_model_train %>% select(-real_shot_made_flag)


# Simple cross validated xgboost training function (returning minimum error for grid search)
xgbCV <- function (params) {
  fit <- xgb.cv(
    data = data.matrix(kb_train), 
    label = train_labels, 
    param =params, 
    missing = NA, 
    nfold = folds, 
    prediction = FALSE,
    early.stop.round = 50,
    maximize = FALSE,
    nrounds = nrounds
  )
  rounds <- nrow(fit)
  metric = paste('test.',eval,'.mean',sep='')
  idx <- which.min(fit[,fit[[metric]]]) 
  val <- fit[idx,][[metric]]
  res <<- rbind(res,c(idx,val,rounds))
  colnames(res) <<- c('idx','val','rounds')
  return(val)
}

# xgb.cv(params=params, data=train_xgb_matrix, nrounds=100, nfold=10, showsd=T, stratified=T,
#        print_every_n=1, early_stopping_rounds=20, maximize=F)

# Find minimal testing error in parallel
library(parallel)
library(NMOF)

stopCluster(cl)
cl <- makeCluster(detectCores()-1)

clusterExport(cl, c("xgb.cv",'kb_train','train_labels','nrounds','res','eval','folds'))
sol <- gridSearch(
  fun = xgbCV,
  levels = params,
  method = 'snow',
  cl = cl,
  keepNames = TRUE,
  asList = TRUE
)

# Combine all model results
comb=clusterEvalQ(cl,res)
results <- ldply(comb,data.frame)
stopCluster(cl)

# Train model given solution above
params <- c(sol$minlevels,objective = obj, eval_metric = eval)
xgbModel <- xgboost(
  data = xgb.DMatrix(data.matrix(train),missing=NaN, label = trainLabel),
  param = params,
  nrounds = results[which.min(results[,2]),1]
)

print(params)
print(results)








####
# Test d'une grille de validation des paramètres
####

# Passer toutes les variables charater en facteur si c'est utile
fact_col <- colnames(kb_model_train)[sapply(train,is.character)]
# --> Dans ce cas on a fact_col qui est vide

for(i in fact_col)
  set(kb_model_train, j=i, value=factor(kb_model_train[[i]]))

for(i in fact_col)
  set(kb_model_test, j=i, value=factor(kb_model_test[[i]]))

for(i in fact_col)
  set(kb_model_test, j=i, value=factor(kb_model_test[[i]]))

# Création d'une variable a qui vaut "a" pour débugger !!?
# kb_model_train <- kb_model_train %>% select(-a)
# kb_model_test <- kb_model_test %>% select(-a)
# kb_model_validation <- kb_model_validation %>% select(-a)

kb_model_train_task <- makeClassifTask(data=kb_model_train, target="real_shot_made_flag")

kb_model_test_task <- makeClassifTask(data=kb_model_test, target="real_shot_made_flag")

kb_model_validation_task <- makeClassifTask(data=kb_model_validation, target="real_shot_made_flag")


# One hot encoding (une modalité devient une variable binaire)
kb_model_train_task <- createDummyFeatures(obj=kb_model_train_task)
kb_model_test_task <- createDummyFeatures(obj=kb_model_test_task)
kb_model_validation_task <- createDummyFeatures(obj=kb_model_validation_task)


# Création d'un learner
lrn <- makeLearner("classif.xgboost", predict.type="prob")
lrn$par.vals <- list(
  objective="binary:logistic",
  eval_metric="logloss",
  nrounds=10L,
  eta=0.3
)

# Définition des paramètreq qu'on va tester
params <- makeParamSet(
  makeDiscreteParam("booster", values=c("gbtree","gblinear")),
  makeIntegerParam("max_depth", lower=3L, upper=10L),
  makeNumericParam("min_child_weight", lower=1L, upper=10L),
  makeNumericParam("subsample", lower=0.5, upper=1),
  makeNumericParam("colsample_bytree", lower=0.5, upper=1)
)

# Définition de la stratégie de pour l'échantillonage
rdesc <- makeResampleDesc("CV", stratify=T, iters=5L)

# Définition de la stratégie de recherche
ctrl <- makeTuneControlRandom(maxit=10L)


# Mise en place de la parralélisation
library(parallel)
library(parallelMap)

parallelStartSocket(cpus=detectCores()-1)

# Lancement de la recherche avec les paramètres définis ci-dessus
mytune <- tuneParams(learner=lrn
                     ,task=kb_model_train_task
                     ,resampling=rdesc
                     ,measures=logloss
                     ,par.set=params
                     ,control=ctrl
                     ,show.info=T)

# Valeur du logloss.test.mean 
mytune$y
# --> 0.6084876


#set hyperparameters
lrn_tune <- setHyperPars(lrn, par.vals=mytune$x)

#train model
xgmodel <- train(learner=lrn_tune, task=kb_model_train_task)

#predict model
kb_pred_train <- predict(xgmodel, kb_model_train_task)
kb_pred_test <- predict(xgmodel, kb_model_test_task)
kb_pred_validation <- predict(xgmodel, kb_model_validation_task)

# Matrice de confusion
confusionMatrix(kb_pred_train$data$response,kb_pred_train$data$truth) # 0.6904
confusionMatrix(kb_pred_test$data$response,kb_pred_test$data$truth) # 0.4655
confusionMatrix(kb_pred_validation$data$response,kb_pred_validation$data$truth) # 0.4742

test <- kb_pred_validation$data$truth



kb_model_train <- kb_model_train %>% select(-alea) # Est-ce qu'on garde l'aléa ?
kb_model_train_matrix <- sparse.model.matrix(real_shot_made_flag~.-1, data=kb_model_train)
kb_model_test_matrix <- sparse.model.matrix(real_shot_made_flag~.-1, data=kb_model_test)
kb_model_validation_matrix <- sparse.model.matrix(real_shot_made_flag~.-1, data=kb_model_validation)
output_vector_train = kb_model_train[,'real_shot_made_flag'] == "Réussi"


tune_XGB <- xgboost(data=kb_model_train_matrix, label=output_vector_train, nrounds=25, max_depth=5, min_child_weight=50,
                    objective="binary:logistic", eval_metric="logloss")

importance_matrix <- xgb.importance(kb_model_train_matrix@Dimnames[[2]], model=tune_XGB)
importance_matrix_var_imp <- importance_matrix %>% filter(Gain>=0.01) %>% as.data.table()
xgb.plot.importance(importance_matrix_var_imp)
xgb.ggplot.deepness(model=tune_XGB)


# Graphe pour voir des modèles avec nrounds petit et max_depth petit
# xgb.plot.tree(feature_names=kb_model_train_matrix@Dimnames[[2]], model=tune_XGB)


tune_XGB_cv1 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=1,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv2 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=2,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv3 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=3,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv4 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=4,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv5 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=5,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv6 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=6,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv7 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=7,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv8 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=8,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv9 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=9,
                       objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv10 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=10,
                        objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv11 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=11,
                        objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)
tune_XGB_cv12 <- xgb.cv(data=kb_model_train_matrix, label=output_vector_train, nfold=10, nrounds=50, early_stopping_rounds=10, max_depth=12,
                        objective = "binary:logistic", eval_metric="logloss", maximize=FALSE)

tune_XGB_cv1$evaluation_log[tune_XGB_cv1$best_iteration,]
tune_XGB_cv2$evaluation_log[tune_XGB_cv2$best_iteration,]
tune_XGB_cv3$evaluation_log[tune_XGB_cv3$best_iteration,]
tune_XGB_cv4$evaluation_log[tune_XGB_cv4$best_iteration,]
tune_XGB_cv5$evaluation_log[tune_XGB_cv5$best_iteration,]
tune_XGB_cv6$evaluation_log[tune_XGB_cv6$best_iteration,]
tune_XGB_cv7$evaluation_log[tune_XGB_cv7$best_iteration,]
tune_XGB_cv8$evaluation_log[tune_XGB_cv8$best_iteration,]
tune_XGB_cv9$evaluation_log[tune_XGB_cv9$best_iteration,]
tune_XGB_cv10$evaluation_log[tune_XGB_cv10$best_iteration,]
tune_XGB_cv11$evaluation_log[tune_XGB_cv11$best_iteration,]
tune_XGB_cv12$evaluation_log[tune_XGB_cv12$best_iteration,]


kb_train <- kb_model_train
kb_train$prediction <- predict(tune_XGB, kb_model_train_matrix)
kb_train <- kb_train %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))
kb_train <- kb_train %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))
mean(kb_train$logLoss)

kb_test <- kb_model_test
kb_test$prediction <- predict(tune_XGB, kb_model_test_matrix)
kb_test <- kb_test %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))
kb_test <- kb_test %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))
mean(kb_test$logLoss)

kb_validation <- kb_model_validation
kb_validation$prediction <- predict(tune_XGB, kb_model_validation_matrix)
kb_validation <- kb_validation %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))
kb_validation <- kb_validation %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))
mean(kb_validation$logLoss)



data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

a<-as.matrix(train$data)

test <- as.matrix(kb_model_train)[,-real_shot_made_flag]







# Calcul en parallèle
library(parallel)
library(doParallel)

detectCores()

cluster <- makeCluster(detectCores() - 1) # par convention on laisse un coeur pour l'OS
registerDoParallel(cluster)


# Sélection des variables en entrée du modèle
kb_train <- kb_model_train # %>% select(real_shot_made_flag, combined_shot_type, boo_dom, temps_total)
 
kb_test <- kb_model_test
kb_validation <- kb_model_validation

# Paramètres du train pour la LogLoss
objControl <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss, allowParallel=TRUE)

# Modèle Logistique avec choix de alpha et lambda
gridsearch <- expand.grid(alpha=c(seq(0,1,0.5)), lambda=c(0,0.1,1,10))

# Avec logLoss
tune_log <- train(real_shot_made_flag ~ .,
                  data=kb_train,
                  method="glmnet",
                  family="binomial",
                  metric="logLoss",
                  tuneGrid=gridsearch,
                  trControl=objControl)



tune_log

plot(tune_log)
tune_log$bestTune

summary(tune_log)
remove(tune_log)

tune_log$predictors


# Ajout de la variable prediction
kb_train$prediction <- predict(object=tune_log, kb_train, type="prob")$Réussi
kb_test$prediction <- predict(object=tune_log, kb_test, type="prob")$Réussi
kb_validation$prediction <- predict(object=tune_log, kb_validation, type="prob")$Réussi

# Ajout de la variables boo_shot_ok qui vaut 1 si le shot est OK, 0 sinon
kb_train <- kb_train %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))
kb_test <- kb_test %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))
kb_validation <- kb_validation %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))

# Calcul de la logLoss
kb_train <- kb_train %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))
kb_test <- kb_test %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))
kb_validation <- kb_validation %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))

# Affichage de la logLoss
mean(kb_train$logLoss)
mean(kb_test$logLoss)
mean(kb_validation$logLoss)


kb_train %>% filter(combined_shot_type=='Dunk') %>%
  summarise(min=min(prediction), moy=mean(prediction), med=median(prediction), max=max(prediction),
            avg_logLoss=mean(logLoss), avg_boo_shot_ok=mean(boo_shot_ok))


stats <- kb_model_train_test %>%
  mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0)) %>%
  group_by(combined_shot_type, action_type) %>%
  summarise(nb_shot=n(),
            nb_shot_ok=sum(boo_shot_ok),
            ratio_shot_ok=nb_shot_ok/nb_shot) %>%
  arrange(desc(ratio_shot_ok))


test <- kb_train %>% filter(combined_shot_type=='Dunk')



roc_imp <- filterVarImp(x=kb_train[,2:18], y=kb_train$real_shot_made_flag)
head(roc_imp)

imp <- abs(coef(tune_log$finalModel,tune_log$bestTune$lambda))
imp2 <- abs(coef(tune_log$finalModel))

impdf <- data.frame(names = row.names(imp), imp = imp[,1])
impdf2 <- data.frame(names = row.names(imp2), imp2 = imp2[,1])

impdf <- impdf[order(impdf$imp, decreasing = TRUE),]
impdf[1:30,]





library(e1071)
library(reshape2)

conf.mat <- confusionMatrix(pred, test$real_shot_made_flag)
cm.plot(conf.mat$table)

test$prediction <- pred

mat_conf <- test %>% group_by(real_shot_made_flag, prediction) %>% summarise(nb=n())

cm.plot <- function(table_cm){
  tablecm <- round(t(t(table_cm) / colSums(as.matrix(table_cm))*100))
  tablemelt <- melt(tablecm)
  ggplot(tablemelt, aes(Reference, Prediction)) +
    geom_point(aes(size = value, color=value), alpha=0.8, show_guide=FALSE) +
    geom_text(aes(label = value), color="white") +
    scale_size(range = c(5,25)) +
    scale_y_discrete(limits = rev(levels(tablemelt$Prediction)))+
    theme_bw()
}



# Avec Accuracy
tune_log <- train(as.matrix(app_X), app$real_shot_made_flag, method="glmnet",
                  tuneGrid=gridsearch, family='binomial', trControl=objControl, metric='Accuracy')

# Avec ROC
tune_log <- train(real_shot_made_flag ~ .,
                  data=app,
                  method="glmnet",
                  family="binomial",
                  metric="ROC",
                  tuneGrid=gridsearch,
                  trControl = objControl)


# Random forest
gridsearch <- expand.grid(ntree = seq(30,200,50))
tune_RF <- train(real_shot_made_flag ~ ., data=kb_train, method="rf", mtretrControl=objControl, metric='LogLoss')


set.seed(75)
library(randomForest)
kb_model_train <- kb_model_train %>% select(-alea)
tune_RF <- randomForest(real_shot_made_flag ~ ., data=kb_model_train, ntree=500, mtry=4, nodesize=50)
print(tune_RF)
varImpPlot(tune_RF)
tune_RF$importance
tune_RF$importance[order(tune_RF$importance[, 1], decreasing = TRUE), ]

plot(tune_RF$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")


# Ajout de la variable prediction
kb_train$prediction <- predict(object=tune_RF, kb_model_train, type="prob")[,1]
kb_test$prediction <- predict(object=tune_RF, kb_model_test, type="prob")[,1]
kb_validation$prediction <- predict(object=tune_RF, kb_model_validation, type="prob")[,1]

# Ajout de la variables boo_shot_ok qui vaut 1 si le shot est OK, 0 sinon
kb_train <- kb_train %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))
kb_test <- kb_test %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))
kb_validation <- kb_validation %>% mutate(boo_shot_ok=if_else(real_shot_made_flag=='Réussi',1,0))

# Calcul de la logLoss
kb_train <- kb_train %>% mutate(prediction=if_else(prediction==1,0.999,if_else(prediction==0,0.001,prediction)))
kb_test <- kb_test %>% mutate(prediction=if_else(prediction==1,0.999,if_else(prediction==0,0.001,prediction)))
kb_validation <- kb_validation %>% mutate(prediction=if_else(prediction==1,0.999,if_else(prediction==0,0.001,prediction)))

kb_train <- kb_train %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))
kb_test <- kb_test %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))
kb_validation <- kb_validation %>% mutate(logLoss=if_else(boo_shot_ok==1,-log(prediction),-log(1-prediction)))


a<- kb_validation %>%
  mutate(alea2=runif(nrow(kb_validation)),
         logLoss2=if_else(boo_shot_ok==1,-log(alea2),-log(1-alea2)))
                        

# Affichage de la logLoss
mean(kb_train$logLoss)
mean(kb_test$logLoss)
mean(kb_validation$logLoss)






# Modèle CART
gridsearch <- expand.grid(cp=seq(0, 0.1, 0.025))
tune_CART <- train(app_X, app$real_shot_made_flag, method="rpart", tuneGrid=gridsearch, trControl=objControl, metric='ROC')
tune_CART
createFolds




# Autres Paramètres du train
objControl <- trainControl(method='cv',
                           number=10,
                           returnResamp='none',
                           classProbs=TRUE,
                           summaryFunction=twoClassSummary,
                           allowParallel=TRUE,
                           seeds=NA)

LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}