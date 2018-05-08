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
library(dplyr)

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
     
    #recuperer les filtrages selectionnÃ©s dans shrr
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
     #Chaine rÃ©cupÃ©rant les diffÃ©rents filtres
     sh_filtre<-shr[shr$combined_shot_type %in% input$type_shoot & shr$opponent %in% input$adversaire & shr$season %in% input$saison & shr$real_shot_made_flag %in% reussi & shr$boo_dom %in% dx & shr$shot_type %in% input$zone_shoot & shr$playoffs %in% playoffs,]
      
    # })#fin du reactive

     shrr<-sh_filtre

   ## traitement de données post-filtre avant affichage
     #création des modalités tir réussi et tir raté
     shot_result<-factor(shrr$real_shot_made_flag,labels=c("missed","made"))
     shrr<-cbind(shrr,shot_result)
     #calcul des nombres/pourcentages au tir selon shot-zone-area x shot-zone-range (pour hexagones)
     
     FG_per_area <- shrr %>%
       group_by(shot_zone_area,shot_zone_range) %>%
       summarise(shot_volume = n()/nrow(shrr),
                 FG_pct_area = mean(real_shot_made_flag)*100)

     shrr <- inner_join(shrr,FG_per_area)

    #Affichage du terrain des tirs et heatmap
    output$plot <- renderPlot({
            decoup<-c(0, 100, 250, 500, 1000, 1500, 2000, 2500, Inf)


        #affiche uniquement les tirs sous forme de points, couleur fonction de reussi ou non
        if (input$typ_affich==1)
           {
              p<-ggplot(data=shrr) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                
                geom_point(mapping = aes(x=loc_x, y=loc_y, colour=shot_result,shape=combined_shot_type))+  
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="right")+
                annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) 
           }
        else  #affiche uniquement les hex
           {
              p<-ggplot(data=shrr) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                geom_hex(aes(x=loc_x,y=loc_y,colour=FG_pct_area),
                         binwidth=8)+
                #scale_fill_gradient2(low="red",high = "green",mid = "blue",midpoint = 25)+
                #stat_binhex(aes(x=loc_x,y=loc_y, alpha=..count..),binwidth = 25)+
                scale_fill_gradientn(colours=c("blue","light green","green", "dark green"),name = "Frequency",na.value=NA)+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="right")+
                annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) 
              
           }

        p
        
    })
    
    #Affichage d'un tableau avec les Tirs rÃ©ussis et ratÃ©s, en nombre et en %
    res<-NULL
    colsum<-NULL
    FGpct<-NULL
    FG<-NULL
    FGA<-NULL
    res<-table(shrr$shot_zone_basic,shrr$real_shot_made_flag)
    FG<-res[,2]
    FGA<-res[,1]+res[,2]
    res<-cbind(FG,FGA)
    for (ii in 1:dim(res)[1])
    {
      
      if_else (FGA[ii] >0, FGpct[ii]<-round((FG[ii]/FGA[ii])*100,1),NULL)
    }
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
    
    #Genere l'onglet avec l'ensemble des statistiques du joueur sur sa carriÃ¨re
      #  Lecture du fichier infogenerale sur KB
      ig<-read.csv( "./data/player.table.csv", header=T)
      #ne garde que les colonnes pertinentes
      ig<-ig[,c(1:3,7:9,11:15,18,23,24,27:29)]
      
      #Lecture du fichier des stats en carriÃ¨re
      stcarriere<-read.csv( "./data/kb.statsCarriere.csv", header=T)
      
      output$InfoGene<-renderUI({
        
       HTML(paste("<font color=\"blue\"><b>",h4("informations gÃ©nÃ©rales"),"</b></font>","ID joueur en NBA: ","<font color=\"purple\"><b>",ig[1,1],"</b></font>",br(),
              "Nom : ","<font color=\"purple\"><b>",ig[1,2],"</b></font>"," ","<font color=\"purple\"><b>", ig[1,3],"</b></font>","&emsp;", "nÃ© le : ","<font color=\"purple\"><b>",strftime(as.Date(substring(ig[1,4],1,10),"%Y-%m-%d"),"%d/%m/%Y"),"</b></font>","&emsp;","nationalitÃ© : ","<font color=\"purple\"><b>",ig[1,6],"</b></font>",br(),
              "taille: ","<font color=\"purple\"><b>",ig[1,7],"</b></font>"," pieds","&emsp;", "poids: ","<font color=\"purple\"><b>",ig[1,8]/2,"</b></font>"," kg",br(),
              "NumÃ©ro: ","<font color=\"purple\"><b>",ig[1,10],"</b></font>","&emsp;", "poste de jeu: ","<font color=\"purple\"><b>",ig[1,11],"</b></font>",br(),
              " Equipe: ","<font color=\"purple\"><b>",ig[1,12],"</b></font>","&emsp;","nombre de saisons NBA: ","<font color=\"purple\"><b>",ig[1,9],"</b></font>","&emsp;","de ","<font color=\"purple\"><b>",ig[1,13],"</b></font>"," Ã  ","<font color=\"purple\"><b>",ig[1,14],"</b></font>",br(),
              "Ecole: :","<font color=\"purple\"><b>",ig[1,5],"</b></font>","&emsp;","draftÃ© en ","<font color=\"purple\"><b>",ig[1,13],"</b></font>"," en ","<font color=\"purple\"><b>",ig[1,17],"</b></font>"," Ã¨me position au ",ig[1,16],"</b></font>","er tour",br())
        )
      })
      
      output$InfoStats<-renderUI({
        
        HTML(paste(br(),"<font color=\"blue\"><b>",h4("Statistiques en carriÃ¨re"),"</b></font>",
                   "<font color=\"purple\"><b>", stcarriere[1,7],"</b></font>"," fois all star (sÃ©lectionnÃ© pour le all star game)",br(),
                   "Moyenne de points marquÃ©s par matchs: ","<font color=\"purple\"><b>",stcarriere[1,4],"</b></font>" ,"&emsp;", "passes dÃ©cisives: ","<font color=\"purple\"><b>",stcarriere[1,5],"</b></font>","&emsp;", "Rebonds: ","<font color=\"purple\"><b>",stcarriere[1,6],"</b></font>","</b></font>")
        )
      })
      
      output$TitreProfil<-renderUI({
        
        HTML(paste(br(),"<font color=\"blue\"><b>",h4("Profil de rÃ©ussite aux tirs par type de tir"),"</b></font>"))
      })
      
      
      #affichage du profil en carriÃ¨re de rÃ©ussite par typ de shoot
      output$profil<-renderChartJSRadar({
        shr<-shr[shr$real_shot_made_flag %in% c(0,1),]
        vshotmq<-shr%>%select(combined_shot_type,real_shot_made_flag)%>%group_by(combined_shot_type)%>%filter(real_shot_made_flag==0)%>%summarise(nb=n())
        vshotmis<-shr%>%select(combined_shot_type,real_shot_made_flag)%>%group_by(combined_shot_type)%>%filter(real_shot_made_flag==1)%>%summarise(nb=n())
        
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
      
      
      
      
      #Prepare les donnÃ©es statisitiques
      #Moyenne de points en carriÃ¨re, min et max
      #% de rÃ©ussite au tir en carriÃ¨re
      #% de rÃ©ussite par type de shoot
      #% de rÃ©ussite Ã  3pts
      #% de rÃ©ussite Ã  2pts
#      
      #% de victoire en carriÃ¨re
      #Moyenne de points par saison, min et max
      #% de victoire par saison
      #% de rÃ©ussite par saison
      #% de rÃ©ussite par saison et par type de tir
      #% de rÃ©ussite par saison Ã  3pts
      
      resum_tir_carriere<-shr%>% select (season,shot_type,real_shot_made_flag,combined_shot_type )%>%
        group_by(season, real_shot_made_flag,combined_shot_type, shot_type)%>% summarise(nb=n())%>%
        arrange(season,combined_shot_type,real_shot_made_flag)
        tir_mq<-sum(resum_tir_carriere[resum_tir_carriere$real_shot_made_flag==0,]$nb)
        tir_reussis<-sum(resum_tir_carriere[resum_tir_carriere$real_shot_made_flag==1,]$nb)
      #% reussite carriere
          ratio_carriere<-tir_reussis/(tir_mq+tir_reussis)*100
      
      pts_match_saison<-shr%>%select(season, game_date,opponent,shot_made_flag)%>%
        group_by(season,opponent,game_date,shot_made_flag) %>% filter(shot_made_flag==1) %>%summarise(nbpts=n())
      
      


}

# Run the application 
#shiny::runApp()
