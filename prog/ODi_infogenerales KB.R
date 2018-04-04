library(rvest)
library(rjson)

library(httr)
library(dplyr)
library(data.table)
options(stringsAsFactors = FALSE)

#L'identifiant de Kobe est le 977

player_info <- read_html(paste0(
  "http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=",
  977,
  "&SeasonType=Regular+Season"))

player_json <- fromJSON(html_text(player_info))
cols <- player_json$resultSets[[1]]$headers # taking the column names from the initial API test call
player_df <- data.frame(matrix(NA, nrow=1, ncol=29)) # empty dataframe
#Lecture des noms de colonnes
names(player_df) <- tolower(cols)
player_stats <- data.frame(matrix(NA, nrow=1, ncol=7)) # empty dataframe
#Lecture des noms de colonnes
names(player_stats) <- tolower(player_json$resultSets[[2]]$headers)


  url <- paste0(
    "http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=",
    977,
    "&SeasonType=Regular+Season")
  
  player_info <- try(read_html(url), silent=TRUE)
  
  if (!("try-error" %in% class(player_info))) 
  {
    player_json <- fromJSON(html_text(player_info))
    
    for (x in 1:29) {
      if (!is.null(player_json$resultSets[[1]][[3]][[1]][[x]])) {
        player_df[1,x] <- player_json$resultSets[[1]][[3]][[1]][[x]] 
      }
    }
    for (y in 1:7){
       if(!is.null(player_json$resultSets[[2]][[3]][[1]][[y]])) {
         player_stats[1,y] <- player_json$resultSets[[2]][[3]][[1]][[y]] 
        }
    }
  }

player_df <- filter(player_df, !(is.na(first_name)))
player.table <- data.table(player_df)
save(player.table, file="./data/player.table.Rda")
write.csv(player.table, "./data/player.table.csv", row.names=FALSE)
write.csv(player_stats,"./data/kb.statsCarriere.csv", row.names=FALSE)





