######Stats descriptives

install.packages("ggplot2")
library(ggplot2)

##réussite au tir en fonction de la distance
FGpct_distance <- kb_analyse %>%
  filter(shot_distance<30) %>%
  group_by(shot_distance) %>%
  summarise(FGpct = mean(real_shot_made_flag))

plot_FG_distance <- ggplot(data=FGpct_distance, aes(x=shot_distance,y=FGpct)) +
  geom_line(colour="black")+
  ggtitle("Evolution du % au tir en fonction \n de la distance au panier")+
  xlab("distance (ft.)") + ylab("FG%") +
  theme(plot.background = element_rect(fill="purple"),
        panel.background = element_rect(fill = "white",colour = "black"),
        #panel.grid.major = element_line(colour="purple",linetype = "solid"),
        #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
        axis.text = element_text(color = "gold"),
        axis.title = element_text(color = "gold"),
        plot.title = element_text(face = "bold", color = "gold")
  )
plot_FG_distance


##est-il plus à l'aise à gauche, au centre ou à droite sur ses mid_range?
FGpct_side <- kb_analyse %>%
  filter(shot_zone_basic=="Mid-Range")%>%
  group_by(side = round((loc_x/10),0)) %>%
  summarise(FGpct = mean(as.integer(real_shot_made_flag), na.rm=TRUE))

plot_FG_side <- ggplot(data=FGpct_side, aes(x=side,y=FGpct)) +
  geom_line(colour="black") +
  ggtitle("Evolution du % au tir selon la position latérale")+
  xlab("position latérale du tir (ft.)") + ylab("FG%") +
  theme(plot.background = element_rect(fill="purple"),
        panel.background = element_rect(fill = "white",colour = "black"),
        #panel.grid.major = element_line(colour="purple",linetype = "solid"),
        #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
        axis.text = element_text(color = "gold"),
        axis.title = element_text(color = "gold"),
        plot.title = element_text(face = "bold", color = "gold")
  )
plot_FG_side


##réussite au tir en fonction du moment du tir
FGpct_time <- kb_analyse %>%
  filter(period<5) %>%
  group_by(shot_time=round((temps_total/60),0)) %>%
  summarise(FGpct = mean(as.integer(real_shot_made_flag), na.rm=TRUE))

plot_FG_time <- ggplot(data=FGpct_time, aes(x=shot_time,y=FGpct)) +
  geom_line(colour="black") +
  ggtitle("Evolution du % au tir dans le match")+
  xlab("moment du tir (min)") + ylab("FG%") +
  theme(plot.background = element_rect(fill="purple"),
        panel.background = element_rect(fill = "white",colour = "black"),
        #panel.grid.major = element_line(colour="purple",linetype = "solid"),
        #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
        axis.text = element_text(color = "gold"),
        axis.title = element_text(color = "gold"),
        plot.title = element_text(face = "bold", color = "gold")
  )
plot_FG_time


##réussite au tir par macro-zone, par saison
FGpct_area_season <- kb_analyse %>%
  mutate(zone_tir = ifelse(shot_zone_basic=="Above the Break 3", "3 Points",
                             ifelse(shot_zone_basic=="Left Corner 3", "3 Points",
                               ifelse(shot_zone_basic == "Right Corner 3", "3 Points",
                                 ifelse(shot_zone_basic == "Backcourt", "3 Points",
                                   ifelse(shot_zone_basic == "Mid-Range", "Mi-distance",
                                     ifelse(shot_zone_basic == "Restricted Area", "Peinture",
                                                                 "Peinture"))))))) %>%
  # mutate(comment = ifelse(num_season<4,"beginning",
  #                         ifelse(num_season<9, "with Shaq",
  #                                ifelse(num_season<12,"alone",
  #                                       ifelse(num_season<18,"with Gasol","injuries"))))) %>%
  # mutate(num_comment = ifelse(comment=="beginning",1,
  #                           ifelse(comment=="with Shaq",2,
  #                                  ifelse(comment=="alone",3,
  #                                         ifelse(comment=="with Gasol",4,5)))))%>%
  #filter(shot_zone_basic=="Mid-Range", shot_zone_area!="Back Court(BC)") %>%
  #group_by(comment,num_comment,shot_zone_basic) %>%
  group_by(season=as.integer(substr(season,1,4)),zone_tir) %>%
  summarise(FGpct = mean(as.integer(real_shot_made_flag)))

plot_FG_season <- ggplot(data=FGpct_area_season, aes(x=season,y=FGpct, colour = zone_tir)) +
  geom_line()+
  ggtitle("Evolution du % au fil des saisons")+
  xlab("saison") + ylab("FG%") +
  theme(plot.background = element_rect(fill="purple"),
        panel.background = element_rect(fill = "white",colour = "black"),
        #panel.grid.major = element_line(colour="purple",linetype = "solid"),
        #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
        axis.text = element_text(color = "gold"),
        axis.title = element_text(color = "gold"),
        plot.title = element_text(face = "bold", color = "gold"),
        legend.position = "bottom"
  )
plot_FG_season
