###############
#     MVI     #
###############


###########################
# Analyse des shots de KB #
###########################


#############################################################################
# 0. Import des données, chargement des librairies et création de fonctions #
#############################################################################

library(dplyr)

source_kb_shots <- read.csv("data/source_kb_shots.csv")
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
  filter(!is.na(shot_made_flag)) %>%
  group_by(opponent) %>%
  summarise(nb_shot=n(),
            nb_shot_ok=sum(shot_made_flag),
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
    game_year=format(game_date,"%Y"),
    
    # game_month : Mois du match
    game_month=format(game_date,"%m"),
    
    # game_day : Jour du match
    game_day=format(game_date,"%d"),
    
    # boo_noel : Booléen match le jour de Noël
    boo_noel=if_else(game_day==25 & game_month==12,1,0),
    
    # num_season : Numéro de la saison
    num_season=as.numeric(substr(season,1,4))-1995,
    
    # phase : Phase de la carrière de KB
    phase=if_else(game_date<"2004-07-01",1, # Avec O'Neal
            if_else(game_date>="2004-07-01" & game_date<"2008-02-05",2, # Seul
              if_else(game_date>="2008-02-05" & game_date<"2013-11-01",3, # Avec Gasol
                if_else(game_date>="2013-11-01",4,99)))), # Après sa blessure
    
    # age : Âge de KB au moment du match
    age=round((as.Date(game_date)-as.Date("1978-08-23"))/365.25,1)
    
  )

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


write.csv()


# Ajout du nombre total, réussi et ratio de shots  au moment ou KB prend le shot depuis le début du QT
stats_shot_kb_debut_qt <- kb_shots %>%
  arrange(game_date, period, temps_period, game_event_id) %>%
  select(game_date, period, temps_period, game_event_id, shot_made_flag) %>%
  filter(!is.na(shot_made_flag)) %>%
  group_by(game_date, period) %>%
  mutate(
    constante=1,
    nb_shot_qt=cumsum(constante), # Shot pris par KB depuis le début du QT
    nb_shot_ok_qt=cumsum(shot_made_flag), # Shot réussi par KB depuis le début du QT
    pct_shot_ok_qt=cummean(shot_made_flag) # Pourcentage shot réussi par KB depuis le début du QT
    # Note : équivalent de nb_shot_ok_qt/nb_shot_qt
        ) %>%
  ungroup() %>%
  select(-constante) %>%
  arrange(game_date, period, temps_period, game_event_id) %>%
  mutate(
    # On calcul le lag pour ne pas prendre en compte le résultat du shot courant
    nb_shot_qt_lag=if_else(game_date==lag(game_date) & period==lag(period),lag(nb_shot_qt),0),
    nb_shot_ok_qt_lag=if_else(game_date==lag(game_date) & period==lag(period),lag(nb_shot_ok_qt),0L),
    pct_shot_ok_qt_lag=if_else(game_date==lag(game_date) & period==lag(period),lag(pct_shot_ok_qt),999)
        )


        
test2 <- test %>% filter(avg_shot_ok_qt==ratio_shot_ok_qt)



# Ajout de la qualité des derniers shots (jusqu'à -5) et des prochains (jusqu'à +5)
kb_shots <- kb_shots %>%
  arrange(game_date, period, temps_period, game_event_id) %>%
  mutate(score_shot_made=if_else(is.na(shot_made_flag),0,if_else(shot_made_flag==1,1,-1)),
         
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
         score_shot_m1,score_shot_m2,score_shot_m3,score_shot_m4,score_shot_m5,
         score_shot_p1,score_shot_p2,score_shot_p3,score_shot_p4,score_shot_p5,
         score_shot_type_m1,score_shot_type_m2,score_shot_type_m3,score_shot_type_m4,score_shot_type_m5,
         score_shot_type_p1,score_shot_type_p2,score_shot_type_p3,score_shot_type_p4,score_shot_type_p5,
         shot_type, combined_shot_type, action_type,
         loc_x, loc_y, shot_distance, shot_zone_range,
         shot_zone_area, shot_zone_basic,
         shot_made_flag
        ) %>%
  arrange(game_date, period, temps_period, game_event_id)




###########################################################################
# B1. Ajout de données suppkémentaires (stats KB sur l'ensemble du match) #
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
        plus_moins_corr=if_else(is.na(plus_moins) & boo_win==0 & source=="kb_playoffs",-9,
                          if_else(is.na(plus_moins) & boo_win==0 & source=="kb_regseason",1,
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
  select(-Date, -Rk, -G, -Age, -Tm, -source, -Opp, -Home_Away)



#########################################################################
# B2. Ajout de données suppkémentaires (résulats des matchs des Lakers) #
#########################################################################
lakers_1995_2015 <- bind_rows(source_Lakers_Saison1995_2015,source_Lakers_Playoffs1995_2015) %>%
  mutate(game_date=as.Date(Date, format="%d/%m/%Y"),
         streak=if_else(substr(Streak,1,1)=='W',as.numeric(substr(Streak,3,4)),-as.numeric(substr(Streak,3,4))),
         ecart_pts=Tm-Opp
  ) %>%
  arrange(game_date) %>%
  select(-Date, -G, -Notes)




##########################
# Assemblages des tables #
##########################

stats_kb_lakers <- inner_join(kb_stats, lakers_1995_2015, by='game_date')

# Autres variables à créer :

  # * Variable Équipe adverse (fusion de franchise ?)
  # * Variable qualité shoot au début du match (Q1/Q2 pour prédirer Q3/Q4 par exemple)
  # * Variable 1/2/3 derniers shoots
  # * Variable clunch moment
  # * Variable qualité de la saison A et A-1 (score saison régulière et tour élimination playoffs)






         


