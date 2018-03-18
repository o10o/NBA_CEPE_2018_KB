############################ Projet : modÃ¨le de prÃ©diction sur les tirs de Kobe Bryant - KAH ####

################ Manipulation des donnÃ©es

install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)

kbShots <- fread("data/source_kb_shots.csv")

#ajout des variables annÃ©e, mois, jour, domicile/extÃ©rieur
kbShots <- cbind(kbShots,kbShots[,.(year=substr(kbShots[,game_date],1,4))])
kbShots <- cbind(kbShots,kbShots[,.(month=substr(kbShots[,game_date],6,7))])
kbShots <- cbind(kbShots,kbShots[,.(day=substr(kbShots[,game_date],9,10))])

kbShots <- cbind(kbShots,kbShots[,.(home_game_flag=substr(kbShots[,matchup],5,5))])
kbShots <- kbShots[home_game_flag=="v",':='(home_game_flag="1")]
kbShots <- kbShots[home_game_flag=="@",':='(home_game_flag="0")]

#suppression de variables inutiles ou difficiles Ã  manipuler
kbShots <- kbShots[,matchup:=NULL
                     ][,team_id:=NULL
                       ][,team_name:=NULL
                          ][,lat:=NULL
                            ][,lon:=NULL]


#ajout de la variable temps dans le match
kbShots <- cbind(kbShots,kbShots[,.(shot_time=sapply(period,function(x){
                                                     60*(12*min(4,x)+5*max(x-4,0))})
                                    -60*minutes_remaining
                                    -seconds_remaining)])

kbShots <- kbShots[,minutes_remaining:=NULL][,seconds_remaining:=NULL] #[,period:=NULL]


#tri des tirs par ordre chronologique puis modification de l'index shot_id
kbShots <- kbShots[order(year,month,day,shot_time),]
kbShots <- kbShots[,game_event_id:=NULL]
kbShots <- kbShots[,':='(shot_id=seq(1,nrow(kbShots),1))]



#ajout de la variable nombre de tirs pris dans le match, dans une pÃ©riode (au moment du tir considÃ©rÃ©!!)
kbShots <- cbind(kbShots, kbShots[,.(FGA=rep(1,nrow(kbShots)))],
                 kbShots[,.(FGA_period=rep(1,nrow(kbShots)))])

kbShots <- kbShots[,':='(FGA=cumsum(FGA)),by=.(game_id)
                      ][,':='(FGA_period=cumsum(FGA_period)),by=.(game_id,period)]


#ajout de la variable temps depuis le dernier tir (intra-pÃ©riode)
kbShots <- cbind(kbShots, kbShots[,.(timeBtwShots=c(0,kbShots[2:nrow(kbShots),shot_time]-
                                       kbShots[(1:nrow(kbShots)-1),shot_time]))])

kbShots <- kbShots[FGA_period==1,':='(timeBtwShots=0)]


#ajout de la variable récupération (indicateur de fraÃ®cheur physique)
calendar <- kbShots[,.N,by=.(game_date)][,N:=NULL]
calendar <- cbind(calendar, calendar[,.(previous_game_date=c("",calendar[1:(nrow(calendar)-1),game_date]))])

calendar <- calendar[,':='(game_date=strptime(game_date, format = "%Y-%m-%d"))
                   ][,':='(previous_game_date=strptime(previous_game_date, format = "%Y-%m-%d"))]

calendar <- cbind(calendar, calendar[,.(recovery_days=c(0,as.integer(difftime(calendar[2:nrow(calendar),game_date],
                                                    calendar[(2:nrow(calendar)),previous_game_date],
                                                    units = "days"))
                                                  ))])

kbShots <- kbShots[,':='(game_date=strptime(game_date, format = "%Y-%m-%d"))
                     ]

kbShots <- merge(kbShots,calendar)
rm(calendar)

#informations tirs précédent

#suppression de variables inutiles ou difficiles Ã  manipuler
kbShots <- kbShots[,game_date:=NULL
                   ][,game_id:=NULL
                     ][,previous_game_date:=NULL]


#connaÃ®tre les modalitÃ©s de certaines variables a priori intÃ©ressantes
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
kbShots <- kbShots[,':='(home_game_flag=as.factor(home_game_flag))]
kbShots <- kbShots[,':='(year=as.integer(year))]
kbShots <- kbShots[,':='(month=as.integer(month))]
kbShots <- kbShots[,':='(day=as.integer(day))]

str(kbShots)

summary(kbShots)




############## Régression logistique

kbShotsNoNA <- kbShots[shot_made_flag!="",]
X <- model.matrix(shot_made_flag~.,data=kbShotsNoNA)[,-1]
Y <- kbShotsNoNA[,shot_made_flag]
rLogSimple <- glm(Y,X-opponent-action_type, data = kbShotsNoNA,family = "binomial")
summary(rLogSimple)

step(rLogSimple)


#Certaines variables ont trop de modalitÃ©s, crÃ©er des classes si l'on veut les utiliser
