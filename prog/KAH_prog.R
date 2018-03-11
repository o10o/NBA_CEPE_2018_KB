############################ Projet : modèle de prédiction sur les tirs de Kobe Bryant - KAH ####

################ Manipulation des données

install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)

kbShots <- fread("data/source_kb_shots.csv")

#ajout des variables année, mois, jour, domicile/extérieur
kbShots <- cbind(kbShots,kbShots[,.(year=substr(kbShots[,game_date],1,4))])
kbShots <- cbind(kbShots,kbShots[,.(month=substr(kbShots[,game_date],6,7))])
kbShots <- cbind(kbShots,kbShots[,.(day=substr(kbShots[,game_date],9,10))])

kbShots <- cbind(kbShots,kbShots[,.(where=substr(kbShots[,matchup],5,5))])


#suppression des variables inutiles ou difficiles à manipuler
kbShots <- kbShots[,matchup:=NULL
                   ][,game_date:=NULL
                     ][,team_id:=NULL
                       ][,team_name:=NULL
                         ][,game_id:=NULL
                             ][,lat:=NULL
                               ][,lon:=NULL]

#tri des tirs par ordre chronologique puis modification de l'index shot_id
kbShots <- kbShots[order(year,month,day,game_event_id),]
kbShots <- kbShots[,game_event_id:=NULL]
kbShots <- kbShots[,':='(shot_id=seq(1,nrow(kbShots),1))]



#ajout de la variable temps dans le match
kbShots <- cbind(kbShots,kbShots[,.(shot_time=sapply(period,function(x){
                                                     60*(12*min(4,x)+5*max(x-4,0))})
                                    -60*minutes_remaining
                                    -seconds_remaining)])

kbShots <- kbShots[,period:=NULL][,minutes_remaining:=NULL][,seconds_remaining:=NULL]


#connaître les modalités de certaines variables a priori intéressantes
#d'abord transformer les variables qualitatives en factor au lieu de chr
kbShots <- kbShots[,':='(action_type=as.factor(action_type))]
kbShots <- kbShots[,':='(combined_shot_type=as.factor(combined_shot_type))]
kbShots <- kbShots[,':='(playoffs=as.factor(playoffs))]
kbShots <- kbShots[,':='(season=as.factor(season))]
kbShots <- kbShots[,':='(shot_made_flag=as.factor(shot_made_flag))]
kbShots <- kbShots[,':='(shot_type=as.factor(shot_type))]
kbShots <- kbShots[,':='(shot_zone_area=as.factor(shot_zone_area))]
kbShots <- kbShots[,':='(shot_zone_basic=as.factor(shot_zone_basic))]
kbShots <- kbShots[,':='(shot_zone_range=as.factor(shot_zone_range))]
kbShots <- kbShots[,':='(opponent=as.factor(opponent))]
kbShots <- kbShots[,':='(where=as.factor(where))]
kbShots <- kbShots[,':='(year=as.integer(year))]
kbShots <- kbShots[,':='(month=as.integer(month))]
kbShots <- kbShots[,':='(day=as.integer(day))]

str(kbShots)

summary(kbShots)




############## Régression logistique simple avec toutes les données

kbShotsNoNA <- kbShots[shot_made_flag!="",]
rLogSimple <- glm(shot_made_flag~., data = kbShotsNoNA,family = "binomial")
summary(rLogSimple)
