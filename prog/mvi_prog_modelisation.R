###############
#     MVI     #
###############


###########################
# Analyse des shots de KB #
###########################


####################################
# Tests et contructions de modèles #
####################################

# Chargement de librairies
library(dplyr)
library(caret)
library(Matrix)
library(DiagrammeR)
library(data.table)
library(pROC)
library(mlr) # Attention, écrase le train de caret


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



########################################
# Sélection des variables explicatives #
########################################

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



# Vérification des valeurs manquantes (si pas de valeur manquantes on a que des FALSE)
# Code en commentaire permet d'identifier plus facilement où se situent les valeurs manquantes

table(is.na(kb_model_train_light))
table(is.na(kb_model_test_light))
table(is.na(kb_model_validation_light))
#sapply(kb_model_train_light, function(x) sum(is.na(x))/length(x))*100
#sapply(kb_model_test_light, function(x) sum(is.na(x))/length(x))*100
#sapply(kb_model_validation_light, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_train_prev))
table(is.na(kb_model_test_prev))
table(is.na(kb_model_validation_prev))
#sapply(kb_model_train_prev, function(x) sum(is.na(x))/length(x))*100
#sapply(kb_model_test_prev, function(x) sum(is.na(x))/length(x))*100
#sapply(kb_model_validation_prev, function(x) sum(is.na(x))/length(x))*100

table(is.na(kb_model_train_full))
table(is.na(kb_model_test_full))
table(is.na(kb_model_validation_full))
#sapply(kb_model_train_full, function(x) sum(is.na(x))/length(x))*100
#sapply(kb_model_test_full, function(x) sum(is.na(x))/length(x))*100
#sapply(kb_model_validation_full, function(x) sum(is.na(x))/length(x))*100



###################
# Modèles XGBoost #
###################

# Chargement de xgboost
library(xgboost)

# Passage en en matrice one hot encoding (une modalité devient une variable binaire)

# "Light"
train_light_matrix <- model.matrix(~.+0, data=kb_model_train_light[,-c("real_shot_made_flag"), with=F])
test_light_matrix <- model.matrix(~.+0, data=kb_model_test_light[,-c("real_shot_made_flag"), with=F])
validation_light_matrix <- model.matrix(~.+0, data=kb_model_validation_light[,-c("real_shot_made_flag"), with=F])

# "Prévision"
train_prev_matrix <- model.matrix(~.+0, data=kb_model_train_prev[,-c("real_shot_made_flag"), with=F])
test_prev_matrix <- model.matrix(~.+0, data=kb_model_test_prev[,-c("real_shot_made_flag"), with=F])
validation_prev_matrix <- model.matrix(~.+0, data=kb_model_validation_prev[,-c("real_shot_made_flag"), with=F])

# "Full"
train_full_matrix <- model.matrix(~.+0, data=kb_model_train_full[,-c("real_shot_made_flag"), with=F])
test_full_matrix <- model.matrix(~.+0, data=kb_model_test_full[,-c("real_shot_made_flag"), with=F])
validation_full_matrix <- model.matrix(~.+0, data=kb_model_validation_full[,-c("real_shot_made_flag"), with=F])


# Passage en xgb.DMatrix

# "Light"
train_light_xgb_matrix <- xgb.DMatrix(data=train_light_matrix, label=train_labels)
test_light_xgb_matrix <- xgb.DMatrix(data=test_light_matrix, label=test_labels)
validation_light_xgb_matrix <- xgb.DMatrix(data=validation_light_matrix, label=validation_labels)

# "Prévision"
train_prev_xgb_matrix <- xgb.DMatrix(data=train_prev_matrix, label=train_labels)
test_prev_xgb_matrix <- xgb.DMatrix(data=test_prev_matrix, label=test_labels)
validation_prev_xgb_matrix <- xgb.DMatrix(data=validation_prev_matrix, label=validation_labels)

# "Full"
train_full_xgb_matrix <- xgb.DMatrix(data=train_full_matrix, label=train_labels)
test_full_xgb_matrix <- xgb.DMatrix(data=test_full_matrix, label=test_labels)
validation_full_xgb_matrix <- xgb.DMatrix(data=validation_full_matrix, label=validation_labels)


# Choix des paramètres (et choix de la logloss pour la métrique)

# Paramètres par défaut
# params <- list(booster="gbtree", objective="binary:logistic", eval_metric="logloss",
#                eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1, lambda=1, alpha=0)

# Paramètres tunés
params <- list(booster="gbtree", objective="binary:logistic", eval_metric="logloss",
               eta=0.005, gamma=0, max_depth=9, min_child_weight=0.1, subsample=0.75, colsample_bytree=0.7, lambda=1, alpha=0)

# Note : Les modèles finaux sont avec eta=0.001 et early_stopping_rounds=1000
# mais les résultats avec eta=0.005 et early_stopping_rounds=50 sont proches et tounent beaucoup plus vite

# optim_val : On opitmise la LogLoss sur l'échantillon de validation Kaggle
# optim_test : On opitmise la LogLoss sur l'échantillon de test pris aléatoirement dans l'échantillon de base
# optim_val permet mécaniquement d'avoir une meilleure LogLoss pour l'échantilon de validation (mais effet très léger)

# Modèles "Light"
set.seed(75)
xgb_light_optim_val<- xgb.train(params=params, data=train_light_xgb_matrix, print_every_n=1,
                                nrounds=20000, early_stopping_rounds=50, maximize=F,
                                watchlist=list(train=train_light_xgb_matrix,
                                               test=test_light_xgb_matrix,
                                               validation=validation_light_xgb_matrix))

set.seed(75)
xgb_light_optim_test <- xgb.train(params=params, data=train_light_xgb_matrix, print_every_n=1,
                                  nrounds=20000, early_stopping_rounds=50, maximize=F,
                                  watchlist=list(train=train_light_xgb_matrix,
                                                 validation=validation_light_xgb_matrix,
                                                 test=test_light_xgb_matrix))


# Modèles "Prévision"
set.seed(75)
xgb_prev_optim_val<- xgb.train(params=params, data=train_prev_xgb_matrix, print_every_n=1,
                               nrounds=20000, early_stopping_rounds=50, maximize=F,
                               watchlist=list(train=train_prev_xgb_matrix,
                                              test=test_prev_xgb_matrix,
                                              validation=validation_prev_xgb_matrix))

set.seed(75)
xgb_prev_optim_test <- xgb.train(params=params, data=train_prev_xgb_matrix, print_every_n=1,
                                 nrounds=20000, early_stopping_rounds=50, maximize=F,
                                 watchlist=list(train=train_prev_xgb_matrix,
                                                validation=validation_prev_xgb_matrix,
                                                test=test_prev_xgb_matrix))

# Modèles "Full"
set.seed(75)
xgb_full_optim_val<- xgb.train(params=params, data=train_full_xgb_matrix, print_every_n=1,
                               nrounds=20000, early_stopping_rounds=50, maximize=F,
                               watchlist=list(train=train_full_xgb_matrix,
                                              test=test_full_xgb_matrix,
                                              validation=validation_full_xgb_matrix))

set.seed(75)
xgb_full_optim_test <- xgb.train(params=params, data=train_full_xgb_matrix, print_every_n=1,
                                 nrounds=20000, early_stopping_rounds=50, maximize=F,
                                 watchlist=list(train=train_full_xgb_matrix,
                                                validation=validation_full_xgb_matrix,
                                                test=test_full_xgb_matrix))





# Ajout de la Probabilité et calcul de la LogLoss

# "Light"
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


# "Prévision"
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


# "Full"
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



# Prédictions et matrices de confusion

# Train
train_light_preds_optim_val <-if_else(predict(xgb_light_optim_val, train_light_xgb_matrix)>0.5,1,0)
train_light_preds_optim_test <-if_else(predict(xgb_light_optim_test, train_light_xgb_matrix)>0.5,1,0)
train_prev_preds_optim_val <-if_else(predict(xgb_prev_optim_val, train_prev_xgb_matrix)>0.5,1,0)
train_prev_preds_optim_test <-if_else(predict(xgb_prev_optim_test, train_prev_xgb_matrix)>0.5,1,0)
train_full_preds_optim_val <-if_else(predict(xgb_full_optim_val, train_full_xgb_matrix)>0.5,1,0)
train_full_preds_optim_test <-if_else(predict(xgb_full_optim_test, train_full_xgb_matrix)>0.5,1,0)
confusionMatrix(train_light_preds_optim_val, train_labels)
confusionMatrix(train_light_preds_optim_test, train_labels)
confusionMatrix(train_prev_preds_optim_val, train_labels)
confusionMatrix(train_prev_preds_optim_test, train_labels)
confusionMatrix(train_full_preds_optim_val, train_labels)
confusionMatrix(train_full_preds_optim_test, train_labels)

# Test
test_light_preds_optim_val <-if_else(predict(xgb_light_optim_val, test_light_xgb_matrix)>0.5,1,0)
test_light_preds_optim_test <-if_else(predict(xgb_light_optim_test, test_light_xgb_matrix)>0.5,1,0)
test_prev_preds_optim_val <-if_else(predict(xgb_prev_optim_val, test_prev_xgb_matrix)>0.5,1,0)
test_prev_preds_optim_test <-if_else(predict(xgb_prev_optim_test, test_prev_xgb_matrix)>0.5,1,0)
test_full_preds_optim_val <-if_else(predict(xgb_full_optim_val, test_full_xgb_matrix)>0.5,1,0)
test_full_preds_optim_test <-if_else(predict(xgb_full_optim_test, test_full_xgb_matrix)>0.5,1,0)
confusionMatrix(test_light_preds_optim_val, test_labels)
confusionMatrix(test_light_preds_optim_test, test_labels)
confusionMatrix(test_prev_preds_optim_val, test_labels)
confusionMatrix(test_prev_preds_optim_test, test_labels)
confusionMatrix(test_full_preds_optim_val, test_labels)
confusionMatrix(test_full_preds_optim_test, test_labels)

# Validation
validation_light_preds_optim_val <-if_else(predict(xgb_light_optim_val, validation_light_xgb_matrix)>0.5,1,0)
validation_light_preds_optim_test <-if_else(predict(xgb_light_optim_test, validation_light_xgb_matrix)>0.5,1,0)
validation_prev_preds_optim_val <-if_else(predict(xgb_prev_optim_val, validation_prev_xgb_matrix)>0.5,1,0)
validation_prev_preds_optim_test <-if_else(predict(xgb_prev_optim_test, validation_prev_xgb_matrix)>0.5,1,0)
validation_full_preds_optim_val <-if_else(predict(xgb_full_optim_val, validation_full_xgb_matrix)>0.5,1,0)
validation_full_preds_optim_test <-if_else(predict(xgb_full_optim_test, validation_full_xgb_matrix)>0.5,1,0)
confusionMatrix(validation_light_preds_optim_val, validation_labels)
confusionMatrix(validation_light_preds_optim_test, validation_labels)
confusionMatrix(validation_prev_preds_optim_val, validation_labels)
confusionMatrix(validation_prev_preds_optim_test, validation_labels)
confusionMatrix(validation_full_preds_optim_val, validation_labels)
confusionMatrix(validation_full_preds_optim_test, validation_labels)

#confusionMatrix(train_preds, train_labels) # Accuracy : "Light" : 72,00 % / "Prev" : 75,15 % / "Full" : 86,58 %
#confusionMatrix(test_preds, test_labels) # Accuracy : "Light" : 68,73 % / "Prev" : 68,87 % / "Full" : 70,05 %
#confusionMatrix(validation_preds, validation_labels) # Accuracy : "Light" : 68,00 % / "Prev" : 68,44 % / "Full" : 69,54 %


# Calcul des AUC

# Sur échantillon de validation
roc(kb_validation_light_optim_test$realite, kb_validation_light_optim_test$prediction, algorithm=2) # 0.7127
roc(kb_validation_light_optim_val$realite, kb_validation_light_optim_val$prediction, algorithm=2) # 0.7129

roc(kb_validation_prev_optim_test$realite, kb_validation_prev_optim_test$prediction, algorithm=2) # 0.7089
roc(kb_validation_prev_optim_val$realite, kb_validation_prev_optim_val$prediction, algorithm=2) # 0.7093

roc(kb_validation_full_optim_test$realite, kb_validation_full_optim_test$prediction, algorithm=2) # 0.7469
roc(kb_validation_full_optim_val$realite, kb_validation_full_optim_val$prediction, algorithm=2) # 0.7471


# Importance des variables

# "Light"
imp_mat_light_optim_val <- xgb.importance(feature_names=colnames(train_light_matrix), model=xgb_light_optim_val)
xgb.plot.importance (importance_matrix=imp_mat_light_optim_val[1:20])
imp_mat_light_optim_test <- xgb.importance(feature_names=colnames(train_light_matrix), model=xgb_light_optim_test)
xgb.plot.importance (importance_matrix=imp_mat_light_optim_test[1:20])

# "Prévision"
imp_mat_prev_optim_val <- xgb.importance(feature_names=colnames(train_prev_matrix), model=xgb_prev_optim_val)
xgb.plot.importance (importance_matrix=imp_mat_prev_optim_val[1:20])
imp_mat_prev_optim_test <- xgb.importance(feature_names=colnames(train_prev_matrix), model=xgb_prev_optim_test)
xgb.plot.importance (importance_matrix=imp_mat_prev_optim_test[1:20])

# "Full"
imp_mat_full_optim_val <- xgb.importance(feature_names=colnames(train_full_matrix), model=xgb_full_optim_val)
xgb.plot.importance (importance_matrix=imp_mat_full_optim_val[1:20])
imp_mat_full_optim_test <- xgb.importance(feature_names=colnames(train_full_matrix), model=xgb_full_optim_test)
xgb.plot.importance (importance_matrix=imp_mat_full_optim_test[1:20])

############################################
# Stats desc sur les variables importantes #
############################################

kb_stats_desc <- bind_rows(kb_train_full_optim_test, kb_test_full_optim_test, kb_validation_full_optim_test) %>%
  select(action_type, temps_prochain_shot, temps_dernier_shot, temps_period, temps_total, loc_x, loc_y, shot_distance, age, ecart_pts,
         realite, prediction) %>%
  rename(prob=prediction) %>%
  mutate(prediction=if_else(prob>0.5,1,0))

# action_type
stats_desc_action_type <- kb_stats_desc %>% group_by(action_type) %>%
  summarise(nb=n(), pct_shot_real=mean(realite), pct_shot_pred=mean(prediction),
            moy_prob=mean(prob), min_prob=min(prob), max_prob=max(prob))

# temps_prochain_shot
stats_desc_temps_prochain_shot <- kb_stats_desc %>% group_by(temps_prochain_shot) %>%
  summarise(nb=n(), pct_shot_real=mean(realite), pct_shot_pred=mean(prediction),
            moy_prob=mean(prob), min_prob=min(prob), max_prob=max(prob))

# temps_dernier_shot
stats_desc_temps_dernier_shot <- kb_stats_desc %>% group_by(temps_dernier_shot) %>%
  summarise(nb=n(), pct_shot_real=mean(realite), pct_shot_pred=mean(prediction),
            moy_prob=mean(prob), min_prob=min(prob), max_prob=max(prob))

# temps_total
stats_desc_temps_period <- kb_stats_desc %>% group_by(temps_period) %>%
  summarise(nb=n(), pct_shot_real=mean(realite), pct_shot_pred=mean(prediction),
            moy_prob=mean(prob), min_prob=min(prob), max_prob=max(prob))


# ecart_pts
stats_desc_ecart_pts <- kb_stats_desc %>% group_by(ecart_pts) %>%
  summarise(nb=n(), pct_shot_real=mean(realite), pct_shot_pred=mean(prediction),
            moy_prob=mean(prob), min_prob=min(prob), max_prob=max(prob))

# age
stats_desc_age <- kb_stats_desc %>%
  mutate(age=round(age)) %>%
  group_by(age) %>%
  summarise(nb=n(), pct_shot_real=mean(realite), pct_shot_pred=mean(prediction),
            moy_prob=mean(prob), min_prob=min(prob), max_prob=max(prob))

# Export en CSv pour analyse sous Excel
write.csv(imp_mat_full_optim_test, file="test.csv")


# ----------------------------------------------------------------------------------------------------------------------------------- #

#############
# Test code #
#############

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