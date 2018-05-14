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






#Application de divers modeles sur le light

#Modele adaboost
library(ada)
#20 iter
results_ada <- ada(kb_model_train_light$real_shot_made_flag~. , data=kb_model_train_light,iter=20, loss="logistic")
                 
pred.ada<-predict(results_ada,kb_model_test_light)
mat_ada<-caret::confusionMatrix(pred.ada,kb_model_test_light$real_shot_made_flag)
accuracy.ada<-mat_ada$overall[1]
accuracy.ada


LogLoss<-function(actual, predicted)
{
  predicted<-(pmax(predicted, 1E-15 ))
  predicted<-(pmin(predicted, 1-1E-15))
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}
prob<-predict(results_ada,kb_model_test_light, type="prob")
ada_logloss1<-LogLoss(actual=as.numeric(kb_model_test_light$real_shot_made_flag)-1, predicted=prob[,1])



library(ada)
#1200 iter
results_ada <- ada(kb_model_train_light$real_shot_made_flag~. , data=kb_model_train_light,iter=120, loss="logistic")

pred.ada<-predict(results_ada,kb_model_test_light)
mat_ada<-caret::confusionMatrix(pred.ada,kb_model_test_light$real_shot_made_flag)
accuracy.ada<-mat_ada$overall[1]
accuracy.ada


LogLoss<-function(actual, predicted)
{
  predicted<-(pmax(predicted, 1E-15 ))
  predicted<-(pmin(predicted, 1-1E-15))
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}
prob<-predict(results_ada,kb_model_test_light, type="prob")
ada_logloss2<-LogLoss(actual=as.numeric(kb_model_test_light$real_shot_made_flag)-1, predicted=prob[,1])



#bagging
library(ipred)

modbag<-bagging(real_shot_made_flag ~ ., data=kb_model_train_light, coob=TRUE)
predbag<-predict(modbag,kb_model_test_light)
mat_bag<-caret::confusionMatrix(predbag,kb_model_test_light$real_shot_made_flag)
accuracy.bag<-mat_bag$overall[1]
accuracy.bag
probbag<-predict(modbag,kb_model_test_light, type="prob" )

bag_logloss1<-LogLoss(actual=as.numeric(kb_model_test_light$real_shot_made_flag)-1, predicted=prob[,1])

#rpart

  rp<-rpart(formula = kb_model_train_light$real_shot_made_flag~., data = kb_model_train_light[,-1], method = "class")
  plotcp(rp)
printcp(rp)
