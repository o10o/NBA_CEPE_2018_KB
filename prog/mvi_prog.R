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
  
# --> Action type est une variable détaillée de combined_shot_type
# --> Faible volumétrie sur certains combined_shot_type
# --> Création d'une variable à partir des deux avec de la volumétrie dans chaque modalité

# Nombre de matchs distincts
source_kb_shots %>% summarise(nb_game_id=n_distinct(game_id))
# --> 1559

# Nombre de dates distinctes
source_kb_shots %>% summarise(nb_game_date=n_distinct(game_date))
# --> 1559 (cohérent, on a bien autant de matchs que de dates distinctes

# Nombre de matchs distincts par saison
source_kb_shots %>% group_by(season) %>% summarise(nb_match_season=n_distinct(game_date))

# Nombre de matchs vs. adversaires
test <- source_kb_shots %>% group_by(opponent) %>% summarise(nb_match_vs_adv=n_distinct(game_date)) %>%
  arrange(desc(nb_match_vs_adv))

# Taux de réussite vs. adversaires
test <- source_kb_shots %>%
  filter(!is.na(shot_made_flag)) %>%
  group_by(opponent) %>%
  summarise(nb_shot=n(),
            nb_shot_ok=sum(shot_made_flag),
            ratio_shot_ok=nb_shot_ok/nb_shot) %>%
  arrange(desc(ratio_shot_ok))

