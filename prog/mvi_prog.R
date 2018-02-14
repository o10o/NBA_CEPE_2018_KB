###############
#     MVI     #
###############


###########################
# Analyse des shots de KB #
###########################

#########################
# 0. Import des données #
#########################

source_kb_shots <- read.csv("data/source_kb_shots.csv")





#####################################################################
# 1. Analyse des données sources et création de nouvelles variables #
#####################################################################

library(dplyr)

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


# Création de variables supplémentaires


test <- source_kb_shots %>%
  select(period, minutes_remaining, seconds_remaining) %>%
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
                  720*4+300*(period-5)+temps_period)
                              )


# Autres variables à créer :
  # * Variable DOM/EXT
  # * Variable Nombre jours depuis le dernier match
  # * Variable phase la carrière (avec Oneal, seul, avec Gasol avant blessure, avec Gasol après blessure)
  # * Variable année/mois
  # * Variable saison
  # * Variable saison playoff (si oui, niveau du playoff) 
  # * Variable Age du joueur
  # * Variable Booléen jour anniversaire / Noël / MLK Day
  # * Variable Équipe adverse
  # * Variable total shoot du match
  # * Variable qualité shoot au début du match (Q1/Q2 pour prédirer Q3/Q4 par exemple)
  # * Variable 1/2/3 derniers shoots
  # * Variable clunch moment






         


