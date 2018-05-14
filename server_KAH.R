#Chargement des library
library(plotly)
library(shiny)
library(ggplot2)
library(hexbin)
library(png)
library(grid)
library(hexbin)
library(dplyr)
library(stringr)
library(radarchart)
library(devtools)

options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output) {
  
    #Lecture du fichier
    shr<-read.csv( "./data/kb_analyse.csv", header=T)  %>%
       select(-X) %>%
       mutate(game_date=as.Date(game_date)) %>%
       arrange(game_date, period, temps_period, game_event_id)
    #ne garde que les colonnes utiles pour chargement plus rapide
    shr<-shr[,c(1:12,15,62,63,64,65,66,68,69,70,71,72)]
    
    
    #chargement du terrain
    court<-readPNG("./www/Lakers2.PNG")
    gcourt<- rasterGrob(court, width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)

    #chargement des lignes    
    gcourt2<-rasterGrob(readPNG("./www/Lakers3.PNG"), width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
    
    #force la transparence pour les pixels non noirs
    gcourt2$raster[gcourt2$raster=="#FFFFFFFF"]="#FFFFFF00" 
      
    
    #Chargement photo de KB
     output$player_photo <- renderImage({
        #Photo de Kobe
        outfile <- normalizePath('./www/KobeBryant.jpg')
        # Return a list containing the filename
        list(src = outfile  , contentType = 'image/jpeg', width = 180,  height = 150,alt = "This is alternate text" )
    }, deleteFile = F)

#******************************************************************************************************************************************************     
#onglet de visu des shoots
observeEvent(input$actionB,{
     
    #recuperer les filtrages selectionnÃÂ©s dans shrr
     #shrr<-reactive({
       
    #geston de la variable dom_ext    
      if (input$dom_ext==0) dx<-c(0,1)
      else if(input$dom_ext==1) dx<-c(1,1)
      else dx<-c(0,0)
     
     #prepare l'affichage des shoots reussis ou pas ou les deux
     if (input$reussi==2) 
       reussi<-c(0,1)
     else
       reussi<-input$reussi
     
     #gestion variable playoffs
     if (input$playoffs==2)
        playoffs<-c(0,1)
     else
        playoffs<-input$playoffs
     #Chaine rÃÂ©cupÃÂ©rant les diffÃÂ©rents filtres
     sh_filtre<-shr[shr$combined_shot_type %in% input$type_shoot & shr$opponent %in% input$adversaire & shr$season %in% input$saison & shr$real_shot_made_flag %in% reussi & shr$boo_dom %in% dx & shr$shot_type %in% input$zone_shoot & shr$playoffs %in% playoffs,]
      
    # })#fin du reactive

     shrr<-sh_filtre

   ## traitement de donnÃ©es post-filtre avant affichage
     #crÃ©ation des modalitÃ©s tir rÃ©ussi et tir ratÃ©
     shot_result<-factor(shrr$real_shot_made_flag,labels=c("missed","made"))
     shrr<-cbind(shrr,shot_result)
     #calcul des nombres/pourcentages au tir selon shot-zone-area x shot-zone-range (pour hexagones)
     
     # FG_per_area <- shrr %>%
     #   group_by(shot_zone_area,shot_zone_range) %>%
     #   summarise(shot_volume = n()/nrow(shrr),
     #             FG_pct_area = mean(real_shot_made_flag)*100)
     # 
     # overall_FGpct <- sum(FG_per_area$FG_pct_area*FG_per_area$shot_volume)
     # 
     # shrr <- inner_join(shrr,FG_per_area)
       #filter(shot_zone_basic!="Backcourt")

    #Affichage du terrain des tirs et heatmap
    output$plot <- renderPlot({
            decoup<-c(0, 100, 250, 500, 1000, 1500, 2000, 2500, Inf)


        #affiche uniquement les tirs sous forme de points, couleur fonction de reussi ou non
        if (input$typ_affich==1)
           {
              p<-ggplot(data=shrr) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                
                geom_point(mapping = aes(x=loc_x, y=loc_y, colour=shot_result))+  
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+
                theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      plot.margin=margin(0,0,0,0),
                      legend.position="right")+
                annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) 
           }
        else  #affiche uniquement les hex
           {
             #calculate_hex_coords(sh_filtre, c(1,1))
             hex_data <- calculate_hexbins_from_shots(sh_filtre, binwidths = c(15, 15), 
                                                      min_radius_factor = 0.3, fg_pct_limits = c(0.2, 0.7))
             p<-generate_hex_chart(hex_data,alpha_range = c(0.85, 0.98))
           }

        p
        
    })
    
    #Affichage d'un tableau avec les Tirs rÃÂ©ussis et ratÃÂ©s, en nombre et en %
    res<-NULL
    FGpct<-NULL
    FG<-NULL
    FGA<-NULL
    res<-table(shrr$shot_zone_basic,shrr$real_shot_made_flag)
    FG<-res[,2]
    FGA<-res[,1]+res[,2]
    res<-cbind(FG,FGA)
    for (ii in 1:dim(res)[1])
    {
      
      if_else (FGA[ii] >0, FGpct[ii]<-FG[ii]/FGA[ii],NULL)
    }
    FGpct = scales::percent(FGpct)
    fin<-cbind(round(res),FGpct)
    output$tirs <-  DT::renderDataTable({
      DT::datatable(fin)
    })
    
     
  }) #fin du obserEvent    
    # Generate a summary of the data ----
    output$summary <- renderPrint({ summary(shr)})
#******************************************************************************************************************************************************    
    # Visu du fichier complet ----
    output$table <- renderDataTable({shr})

  
#******************************************************************************************************************************************************  
    
    #Genere l'onglet avec l'ensemble des statistiques du joueur sur sa carriÃÂ¨re
      #  Lecture du fichier infogenerale sur KB
      ig<-read.csv( "./data/player.table.csv", header=T)
      #ne garde que les colonnes pertinentes
      ig<-ig[,c(1:3,7:9,11:15,18,23,24,27:29)]
      
      #Lecture du fichier des stats en carrière
      stcarriere<-read.csv( "./data/kb.statsCarriere.csv", header=T)
      
      output$InfoGene<-renderUI({
        
       HTML(paste("<font color=\"blue\"><b>",h4("informations générales"),"</b></font>","ID joueur en NBA: ","<font color=\"purple\"><b>",ig[1,1],"</b></font>",br(),
              "Nom : ","<font color=\"purple\"><b>",ig[1,2],"</b></font>"," ","<font color=\"purple\"><b>", ig[1,3],"</b></font>","&emsp;", "né le : ","<font color=\"purple\"><b>",strftime(as.Date(substring(ig[1,4],1,10),"%Y-%m-%d"),"%d/%m/%Y"),"</b></font>","&emsp;","nationalité : ","<font color=\"purple\"><b>",ig[1,6],"</b></font>",br(),
              "taille: ","<font color=\"purple\"><b>",ig[1,7],"</b></font>"," pieds","&emsp;", "poids: ","<font color=\"purple\"><b>",ig[1,8]/2,"</b></font>"," kg",br(),
              "Numéro: ","<font color=\"purple\"><b>",ig[1,10],"</b></font>","&emsp;", "poste de jeu: ","<font color=\"purple\"><b>",ig[1,11],"</b></font>",br(),
              " Equipe: ","<font color=\"purple\"><b>",ig[1,12],"</b></font>","&emsp;","nombre de saisons NBA: ","<font color=\"purple\"><b>",20,"</b></font>","&emsp;","de ","<font color=\"purple\"><b>",1996,"</b></font>"," à ","<font color=\"purple\"><b>",2016,"</b></font>",br(),
              "Ecole: :","<font color=\"purple\"><b>",ig[1,5],"</b></font>","&emsp;","drafté en ","<font color=\"purple\"><b>",ig[1,13],"</b></font>"," en ","<font color=\"purple\"><b>",ig[1,17],"</b></font>"," ème position au ",ig[1,16],"</b></font>","er tour",br())
        )
      })
      
      output$InfoStats<-renderUI({
        
        HTML(paste(br(),"<font color=\"blue\"><b>",h4("Statistiques en carrière"),"</b></font>",
                   "<font color=\"purple\"><b>", stcarriere[1,7],"</b></font>"," fois all star (sélectionné pour le all star game)",br(),
                   "Moyenne de points marqués par matchs: ","<font color=\"purple\"><b>",stcarriere[1,4],"</b></font>" ,"&emsp;", "passes décisives: ","<font color=\"purple\"><b>",stcarriere[1,5],"</b></font>","&emsp;", "Rebonds: ","<font color=\"purple\"><b>",stcarriere[1,6],"</b></font>","</b></font>")
        )
      })
      
      output$TitreProfil<-renderUI({
        
        HTML(paste(br(),"<font color=\"blue\"><b>",h4("Profil de réussite aux tirs par type de tir"),"</b></font>"))
      })
      
      
      #affichage du profil en carrière de réussite par typ de shoot
      output$profil<-renderChartJSRadar({
        shr<-shr[shr$real_shot_made_flag %in% c(0,1),]
        vshotmq<-shr%>%select(combined_shot_type,real_shot_made_flag)%>%group_by(combined_shot_type)%>%filter(real_shot_made_flag==0)%>%dplyr::summarise(nb=n())
        vshotmis<-shr%>%select(combined_shot_type,real_shot_made_flag)%>%group_by(combined_shot_type)%>%filter(real_shot_made_flag==1)%>%dplyr::summarise(nb=n())
        
        vshotmis<-as.data.frame(vshotmis)
        vshotmq<-as.data.frame(vshotmq)
        
        titres<-as.vector(vshotmis[,1])
        reussi<-as.vector(vshotmis[,2])
        manque<-as.vector(vshotmq[,2])
        ratio<-reussi/(reussi+manque)
        lst<-list("% aux tirs"=ratio)
        c<-chartJSRadar(labs = titres,scores=lst, height = '120',width = '120', labelSize = 14, addDots = T,showLegend = T )
        c
        
      })
      
      
      
#******************************************************************************************************************************************************        
  #onglet stats desc
      kb_analyse <- read.csv( "./data/kb_analyse.csv", header=T)
      
      # % par saison 2 et 3 pts
      output$PctEvol<-renderPlot({
        
        FGDeuxTrois <- kb_analyse %>%
          group_by(season=as.integer(substr(season,1,4)), shot_type) %>%
          summarise(FGpct = mean(real_shot_made_flag))
        
        plot_FGDeuxTrois <- ggplot(data=FGDeuxTrois, aes(x=season,y=FGpct, colour=shot_type)) +
          geom_line() +
          ggtitle("Evolution du % au tir à 2 et 3 points")+
          xlab("saison") + ylab("FG%") +
          scale_x_continuous(breaks = seq(1996,2015,1)) + 
          scale_y_continuous(labels = scales::percent, breaks = seq(0,0.7,0.05)) + 
          theme(plot.background = element_rect(fill="purple"),
                panel.background = element_rect(fill = "white",colour = "black"),
                panel.grid.major = element_line(colour="purple",linetype = "solid"),
                #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
                axis.text = element_text(color = "gold",size = 15),
                axis.text.x = element_text(angle = 45),
                axis.title = element_text(color = "gold",size = 15),
                plot.title = element_text(face = "bold", color = "gold",size = 15)
          )
        plot_FGDeuxTrois
      }
      )
      
      output$PctRegPlayoffs<-renderPlot({
        
        FGRegPlayoffs <- kb_analyse %>%
          group_by(season=as.integer(substr(season,1,4)),playoffs) %>%
          summarise(FGpct = mean(real_shot_made_flag))%>%
          mutate(playoffs=ifelse(playoffs==0,"Saison Régulière","Playoffs"))
        
        plot_FGRegPlayoffs <- ggplot(data=FGRegPlayoffs, aes(x=season,y=FGpct, colour=playoffs)) +
          geom_line() +
          ggtitle("Evolution du % au tir")+
          xlab("saison") + ylab("FG%") +
          scale_x_continuous(breaks = seq(1996,2015,1)) +
          scale_y_continuous(labels = scales::percent, breaks = seq(0,0.7,0.05)) + 
          theme(plot.background = element_rect(fill="purple"),
                panel.background = element_rect(fill = "white",colour = "black"),
                panel.grid.major = element_line(colour="purple",linetype = "solid"),
                #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
                axis.text = element_text(color = "gold",size = 15),
                axis.text.x = element_text(angle = 45),
                axis.title = element_text(color = "gold",size = 15),
                plot.title = element_text(face = "bold", color = "gold",size = 15)
          )
        plot_FGRegPlayoffs
      }
      )
      
      
      #affichage de l'Ã©volution de son pourcentage aux tirs global, Ã  2 pts, Ã  3 pts
      output$PctDistance<-renderPlot({

        FGpct_distance <- kb_analyse %>%
          filter(shot_distance<30) %>%
          group_by(shot_distance) %>%
          summarise(FGpct = mean(real_shot_made_flag))

        plot_FG_distance <- ggplot(data=FGpct_distance, aes(x=shot_distance,y=FGpct)) +
          geom_line(colour="black")+
          ggtitle("Evolution du % au tir en fonction \n de la distance au panier")+
          xlab("distance (ft.)") + ylab("FG%") +
          scale_x_continuous(breaks = seq(0,30,2)) +
          scale_y_continuous(labels = scales::percent, breaks = seq(0,0.7,0.05)) + 
          theme(plot.background = element_rect(fill="purple"),
                panel.background = element_rect(fill = "white",colour = "black"),
                panel.grid.major = element_line(colour="purple",linetype = "solid"),
                #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
                axis.text = element_text(color = "gold",size = 15),
                axis.title = element_text(color = "gold",size = 15),
                plot.title = element_text(face = "bold", color = "gold",size = 15)
          )
        plot_FG_distance
      }
      )
      
      output$PctMoment<-renderPlot({
        
        FGpct_time <- kb_analyse %>%
          filter(period<5) %>%
          group_by(shot_time=round((temps_total/60),0)) %>%
          summarise(FGpct = mean(as.integer(real_shot_made_flag), na.rm=TRUE))
        
        plot_FG_time <- ggplot(data=FGpct_time, aes(x=shot_time,y=FGpct)) +
          geom_line(colour="black") +
          ggtitle("Evolution du % au tir dans le match")+
          xlab("moment du tir (min)") + ylab("FG%") +
          scale_x_continuous(breaks = seq(0,48,2)) +
          scale_y_continuous(labels = scales::percent, breaks = seq(0,0.7,0.05)) + 
          theme(plot.background = element_rect(fill="purple"),
                panel.background = element_rect(fill = "white",colour = "black"),
                panel.grid.major = element_line(colour="purple",linetype = "solid"),
                #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
                axis.text = element_text(color = "gold",size = 15),
                axis.title = element_text(color = "gold",size = 15),
                plot.title = element_text(face = "bold", color = "gold",size = 15)
          )
        plot_FG_time
      }
      )
      
      output$PctMidRangeSide<-renderPlot({
        
        FGpct_side <- kb_analyse %>%
          filter(shot_zone_basic=="Mid-Range")%>%
          group_by(side = round((loc_x/10),0)) %>%
          summarise(FGpct = mean(as.integer(real_shot_made_flag), na.rm=TRUE))
        
        plot_FG_side <- ggplot(data=FGpct_side, aes(x=side,y=FGpct)) +
          geom_line(colour="black") +
          ggtitle("Evolution du % au tir selon la position latérale")+
          xlab("position latérale du tir (ft.)") + ylab("FG%") +
          scale_x_continuous(breaks = seq(-25,25,5)) +
          scale_y_continuous(labels = scales::percent, breaks = seq(0,0.7,0.05)) + 
          theme(plot.background = element_rect(fill="purple"),
                panel.background = element_rect(fill = "white",colour = "black"),
                panel.grid.major = element_line(colour="purple",linetype = "solid"),
                #panel.grid.minor = element_line(colour="purple",linetype = "solid"),
                axis.text = element_text(color = "gold",size = 15),
                axis.title = element_text(color = "gold",size = 15),
                plot.title = element_text(face = "bold", color = "gold",size = 15)
          )
        plot_FG_side
      }
      )

}




# Run the application 
#shiny::runApp()
