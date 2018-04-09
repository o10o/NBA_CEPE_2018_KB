install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)

############# PRE TRAITEMENT

### import du jeu de données exhaustif
kbAnalyse <- fread("data/kb_analyse.csv")

str(kbAnalyse)

### Transformation des bool�ens et character en factor

# chr <- sapply(kbAnalyse, is.character)
# 
# kbAnalyseFactor <- kbAnalyse %>%
#   select(which(chr,TRUE))

# f <- function(x){
#   if(is.character(x)==TRUE) 
#   x <- as.factor(x)
#   }
# 
# chr <- sapply(kbAnalyse,f)
# chr <- data.table(chr)


kbAnalyse <- kbAnalyse[,':='(game_date=as.factor(game_date))]
kbAnalyse <- kbAnalyse[,':='(season=as.factor(season))]
kbAnalyse <- kbAnalyse[,':='(playoffs=as.factor(playoffs))]
kbAnalyse <- kbAnalyse[,':='(boo_noel=as.factor(boo_noel))]
kbAnalyse <- kbAnalyse[,':='(opponent=as.factor(opponent))]
kbAnalyse <- kbAnalyse[,':='(boo_dom=as.factor(boo_dom))]
kbAnalyse <- kbAnalyse[,':='(game_event_id=as.factor(game_event_id))]
kbAnalyse <- kbAnalyse[,':='(boo_premier_shot_qt=as.factor(boo_premier_shot_qt))]
kbAnalyse <- kbAnalyse[,':='(boo_dernier_shot_qt=as.factor(boo_dernier_shot_qt))]
kbAnalyse <- kbAnalyse[,':='(boo_premier_shot_match=as.factor(boo_premier_shot_match))]
kbAnalyse <- kbAnalyse[,':='(boo_dernier_shot_match=as.factor(boo_dernier_shot_match))]
kbAnalyse <- kbAnalyse[,':='(shot_type=as.factor(shot_type))]
kbAnalyse <- kbAnalyse[,':='(combined_shot_type=as.factor(combined_shot_type))]
kbAnalyse <- kbAnalyse[,':='(action_type=as.factor(action_type))]
kbAnalyse <- kbAnalyse[,':='(shot_zone_range=as.factor(shot_zone_range))]
kbAnalyse <- kbAnalyse[,':='(shot_zone_area=as.factor(shot_zone_area))]
kbAnalyse <- kbAnalyse[,':='(shot_zone_basic=as.factor(shot_zone_basic))]
kbAnalyse <- kbAnalyse[,':='(shot_made_flag=as.factor(shot_made_flag))]
kbAnalyse <- kbAnalyse[,':='(real_shot_made_flag=as.factor(real_shot_made_flag))]
kbAnalyse <- kbAnalyse[,':='(GS=as.factor(GS))]
kbAnalyse <- kbAnalyse[,':='(Win_Loss=as.factor(Win_Loss))]
kbAnalyse <- kbAnalyse[,':='(boo_win=as.factor(boo_win))]
kbAnalyse <- kbAnalyse[,':='(boo_win_lag=as.factor(boo_win_lag))]
kbAnalyse <- kbAnalyse[,':='(MP=as.factor(MP))]

### Mettre la modalité "1" ie tir réussi comme classe de positif avant de modéliser
kbAnalyse$shot_made_flag <- relevel(kbAnalyse$shot_made_flag,"1")


### Suppression des variables a posteriori (post-shot) et redondantes
kbPredict <- kbAnalyse %>%
  select(-c(V1,season,temps_repos,temps_prochain_shot,boo_dernier_shot_qt,
            boo_dernier_shot_match,nb_shot_qt,nb_shot_match,nb_shot_deb_qt,
            nb_shot_ok_deb_qt,nb_shot_deb_match,nb_shot_ok_deb_match,intensite_shot_qt,
            intensite_shot_match,boo_win,pct_shot_ok_deb_qt,pct_shot_ok_deb_match,
            score_shot_p1,score_shot_p2,score_shot_p3,score_shot_p4,score_shot_p5,
            score_shot_type_p1,score_shot_type_p2,score_shot_type_p3,score_shot_type_p4,
            score_shot_type_p5,Win_Loss,boo_win,MP,second_played,ratio_played,FG,FGA,
            FGpct,X2P,X2PA,X2Ppct,X3P,X3PA,X3Ppct,FT,FTA,FTpct,ORB,DRB,TRB,AST,STL,BLK,
            TOV,PF,PTS,GmSc,EFF,plus_moins,plus_moins_corr,streak_win_lose,ecart_pts,
            real_shot_made_flag))

### Suppression des variables ayant un nombre de modalité difficile ou impossible à assumer
### pour les modèles (+ risque de sur-apprentissage avec action_type??)

kbPredict <- kbPredict %>%
  select(-c(game_date,game_event_id,action_type))

### on ne conserve que les réponses connues pour la modélisation

kbPredictNoNA <- kbPredict %>%
  filter(!is.na(shot_made_flag))


### Important pour la modélisation : renommer les différentes modalités des facteurs
### pour qu'elles soient comprises par R (ex : "0" et "1" ne sont pas des noms compris par R)

feature.names=names(kbPredictNoNA)

for (f in feature.names) {
  if (class(kbPredictNoNA[[f]])=="factor") {
    levels <- unique(c(kbPredictNoNA[[f]]))
    kbPredictNoNA[[f]] <- factor(kbPredictNoNA[[f]],
                                 labels=make.names(levels))
  }
}





############### Classification supervisée sur kbPredictNoNA
install.packages("caret")
library(caret)

### pourcentage global de réussite au tir dans l'échantillons hors NA
table(kbPredictNoNA[,"shot_made_flag"])/sum(table(kbPredictNoNA[,"shot_made_flag"]))

### split données apprentissage/test
set.seed(1234)
splitIndex <- createDataPartition(kbPredictNoNA[,"shot_made_flag"],p=.80, list = FALSE, times=1)
kbPredictTrain <- data.table(kbPredictNoNA[splitIndex,])
kbPredictTest <- data.table(kbPredictNoNA[-splitIndex,])

yTrain <- factor(kbPredictTrain[,shot_made_flag],labels=c("made","missed")) #pour avoir des modalités plus claires
yTest <- factor(kbPredictTest[,shot_made_flag],labels=c("made","missed"))
table(yTrain)/sum(table(yTrain))
table(yTest)/sum(table(yTest))
#on a bien conservé le pourcentage global de réussite au tirs dans les 2 échantillons


### supprimer la variable réponse des données apprentissage et test (pour la fonction train)
kbPredictTrain <- kbPredictTrain %>%
  select(-shot_made_flag)

kbPredictTest <- kbPredictTest %>%
  select(-shot_made_flag)


### calcul parallélisé
library(parallel)
install.packages("doParallel")
library(doParallel)


#cluster <- makeCluster(detectCores() - 1)
cluster <- makeCluster(4)
registerDoParallel(cluster)



### choix de la méthode de validation croisée avec 3 blocs
objControl <- trainControl(method='cv', number=3, returnResamp='none', classProbs = TRUE, 
                           summaryFunction=twoClassSummary, allowParallel = TRUE, 
                           seeds = NA)




###### arbre de décision simple

gridsearchRPART <- expand.grid(cp=seq(0, 0.1, 0.025)) 
tuneRPART <- train(kbPredictTrain,yTrain,method = "rpart",tuneGrid=gridsearchRPART, 
              trControl = objControl, metric='ROC')

tuneRPART
plot(tuneRPART)

#Prédiction pour la méthode RPART
pred.RPART <- predict(object=tuneRPART$finalModel, kbPredictTest,type='class')

#matrice de confusion pour la méthode RPART
install.packages("e1071")
library(e1071)
conf.matRPART <- confusionMatrix(pred.RPART,yTest)

library(reshape2)
library(ggplot2)
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

cm.plot(conf.matRPART$table)

conf.matRPART$overall
conf.matRPART$byClass


impRPART <- varImp(tuneRPART$finalModel)
impdfRPART <- data.frame(names = row.names(impRPART), impRPART = impRPART[,1])
impdfRPART <- impdfRPART[order(impdf$imp, decreasing = TRUE),]
names(impdfRPART)[2]<-colnames(impRPART)[1]
impdfRPART[1:10,]


# représentation de l'arbre
# install.packages("rattle")
# library(rattle)
# fancyRpartPlot(tune$finalModel)

#courbe ROC
install.packages("pROC")
library(pROC)

pred.rpart <- predict(object=tuneRPART$finalModel, kbPredictTest,type='prob')
rocCurve.rpart   <- roc(response = yTest, predictor = pred.rpart[, "made"], 
                        levels = rev(levels(yTest)))
plot(rocCurve.rpart, print.thres = "best")
rocCurve.rpart$auc




########## FORET ALEATOIRE

gridsearchRF <- expand.grid(mtry = seq(30,200,50))
tuneRF <- train(kbPredictTrain,yTrain,method = "rf",tuneGrid=gridsearchRF, 
              trControl = objControl,metric='ROC')

tuneRF
plot(tuneRF)

predRF <- predict(object=tuneRF$finalModel, kbPredictTest,type='class')
conf.matRF <- confusionMatrix(predRF, yTest)
cm.plot(conf.matRF$table)

conf.matRF$overall
conf.matRF$byClass

impRF <- varImp(tuneRF$finalModel)
impdfRF <- data.frame(names = row.names(impRF), impRF = impRF[,1])
impdfRF <- impdfRF[order(impdfRF$impRF, decreasing = TRUE),]
names(impdfRF)[2]<-colnames(impRF)[1]
impdfRF[1:30,]

pred.rf <- predict(object=tuneRF$finalModel, kbPredictTest,type='prob')
rocCurve.rf <- roc(response = yTest, predictor = pred.rf[, "economie"])
rocCurve.rf$auc




########## SVM
gridsearchSVM <- expand.grid(C = c(0,0.1, 1, 10))
tuneSVM <- train(kbPredictTrain,yTrain,method = "svmLinear",tuneGrid=gridsearchSVM, 
              trControl = objControl,metric='ROC')

tuneSVM
plot(tuneSVM)

pred <- predict(object=tuneSVM$finalModel, kbPredictTest,type='response')

conf.matSVM <- confusionMatrix(pred.svm, yTest)
cm.plot(conf.matSVM$table)

conf.matSVM$overall
conf.matSVM$byClass

pred.svmlin <- predict(object=tuneSVM$finalModel, kbPredictTest,type='prob')
rocCurve.svmlin   <- roc(response = yTest, predictor = pred.svmlin[, "economie"])
rocCurve.svmlin$auc





########## REGRESSION LOGISTIQUE
gridsearchRL <- expand.grid(alpha=c(0, .5, 1), lambda=c(.1, 1, 10))
tuneRL <- train(as.matrix(kbPredictTrain),yTrain,method = "glmnet",tuneGrid=gridsearchRL, 
              family='binomial', trControl = objControl,metric='ROC')

tuneRL
plot(tuneRL)

pred.rl <- predict(object=tuneRL, as.matrix(kbPredictTest),type='raw')
conf.matRL <- confusionMatrix(pred.rl, yTest)
cm.plot(conf.matRL$table)

conf.matRL$overall
conf.matRL$byClass

impRL <- abs(coef(tuneRL$finalModel,tuneRL$bestTune$lambda))
impdfRL <- data.frame(names = row.names(impRL), impRL = impRL[,1])
impdfRL <- impdfRL[order(impdfRL$impRL�, decreasing = TRUE),]
impdfRL[1:30,]