library(jsonlite)
#kbAllShots <- fromJSON(txt = "http://stats.nba.com/stats/shotchartdetail?Period=0&VsConference=&LeagueID=00&LastNGames=0&TeamID=0&PlayerPosition=&Location=&Outcome=&ContextMeasure=FGA&DateFrom=&StartPeriod=&DateTo=&OpponentTeamID=0&ContextFilter=&RangeType=&Season=2015-16&AheadBehind=&PlayerID=977&EndRange=&VsDivision=&PointDiff=&RookieYear=&GameSegment=&Month=0&ClutchTime=&StartRange=&EndPeriod=&SeasonType=Regular+Season&SeasonSegment=&GameID=")

#création de la table à alimenter
kbAllShots <- data.table()

#vecteurs des fichiers à extraire
fi <- system("ls JSON/*",intern = T)

#récupération et concaténation des résultats de tous les tirs pris par Kobe
for (i in 1:length(fi)){
  JSON <- fromJSON(fi[i])
  #length(JSON[[1]]$rowSet[[1]])
  #length(JSON[[1]]$rowSet)
  
  ll <- lapply(JSON$resultSets[[1]]$rowSet,unlist)
  dataR <- do.call(rbind,ll)
  dt <- data.table(dataR)
  names(dt) <- JSON$resultSets[[1]]$headers
  
  dt <- rename(dt,period=PERIOD,minutes_remaining=MINUTES_REMAINING,seconds_remaining=SECONDS_REMAINING,game_date=GAME_DATE,game_event_id=GAME_EVENT_ID)
  dt$game_event_id <- as.integer(dt$game_event_id)
  dt$period <- as.integer(dt$period)
  dt$minutes_remaining <- as.integer(dt$minutes_remaining)
  dt$seconds_remaining <- as.integer(dt$seconds_remaining)
  
  dt <- cbind(dt,dt[,.(year=substr(dt[,game_date],1,4))])
  dt <- cbind(dt,dt[,.(month=substr(dt[,game_date],5,6))])
  dt <- cbind(dt,dt[,.(day=substr(dt[,game_date],7,8))])
  dt <- dt[,c("period","minutes_remaining","seconds_remaining","year","month","day","game_event_id","SHOT_MADE_FLAG")]
  
  kbAllShots <- rbind(kbAllShots,dt)
}


#jointure avec notre jeu de données

kbAllShotsMerge <- merge(kbAllShots,kbShots,by=c("period","minutes_remaining","seconds_remaining","year","month","day","game_event_id"))

#vérifier la correspondance sur les tirs communs aux 2 jeux de données (shot_made_flage vs SHOT_MADE_FLAG)
Check <- kbAllShotsMerge[shot_made_flag!="",]
Check <- Check[,':='(shot_made_flag=as.integer(shot_made_flag))]
Check <- Check[,':='(SHOT_MADE_FLAG=as.integer(SHOT_MADE_FLAG))]
Check <- cbind(Check,Check[,.(check=Check[,shot_made_flag]-Check[,SHOT_MADE_FLAG])])
sum(abs(Check[,check]))
