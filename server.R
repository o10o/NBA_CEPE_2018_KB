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
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    
   # shr <- reactive({
    #    infile <- input$datafile
    #    if (is.null(infile)) {
           
    #        return(NULL)
    #    }
    #   else  #read.csv(infile$datapath)
     #shr<-read.csv( "./data/source_kb_shots.csv", header=T)
     shr<-read.csv( "./data/kb_analyse.csv", header=T)  %>%
       select(-X) %>%
       mutate(game_date=as.Date(game_date)) %>%
       arrange(game_date, period, temps_period, game_event_id)
     #ne garde que les colonnes utiles pour chargement plus rapide
     shr<-shr[,c(1:8,10,11,12,15,62,63,64,65,66,71,72)]
    #lecture à partir du RDS 
    #shr <- readRDS("./data/kb.rds")
    #chargement du terrain
    court<-readPNG("./www/Lakers2.PNG")
    gcourt<- rasterGrob(court, width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
    #chargement des lignes    
    
    
    gcourt2<-rasterGrob(readPNG("./www/Lakers3.PNG"), width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
    #force la transparence pour les pixels non noirs
    gcourt2$raster[gcourt2$raster=="#FFFFFFFF"]="#FFFFFF00" 
      
    # })
  
     output$player_photo <- renderImage({
        #Photo de Kobe
        outfile <- normalizePath('./www/KobeBryant.jpg')
        # Return a list containing the filename
        list(src = outfile  , contentType = 'image/jpeg', width = 180,  height = 150,alt = "This is alternate text" )
    }, deleteFile = F)

#******************************************************************************************************************************************************     
#onglet de visu des shoots
  observeEvent(input$action,{
    
    #recuperer les filtrages selectionnés dans shrr
     shrr<-reactive({
      if (input$dom_ext==0) dx<-c(0,1)
     else if(input$dom_ext==1) dx<-c(1,1)
     else dx<-c(0,0)
     
     #prepare l'affichae des shoots reussis ou pas ou les deux
     if (input$reussi==2) 
       reussi<-c(0,1)
     else
       reussi<-input$reussi
     #Chaine récupérant les différents filtres
     #sh_filtre<-shr()[shr()$combined_shot_type %in% input$type_shoot & shr()$opponent %in% input$adversaire & shr()$season %in% input$saison & shr()$shot_made_flag %in% reussi & str_locate(shr()$matchup,dx),]
     sh_filtre<-shr[shr$combined_shot_type %in% input$type_shoot & shr$opponent %in% input$adversaire & shr$season %in% input$saison & shr$real_shot_made_flag %in% reussi & shr$boo_dom %in% dx & shr$shot_type %in% input$zone_shoot,]
    })
     
     
    #Affichage du terrain des tirs et heatmap
    output$plot <- renderPlot({
            decoup<-c(0, 100, 250, 500, 1000, 1500, 2000, 2500, Inf)


        #affiche tir et hexbin  #affiche uniquement les tirs sous forme de points, couleur fonction de reussi ou non
        if (input$typ_affich==0)
        {
          
            p<-ggplot(data=shrr()) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                stat_binhex(aes(x=loc_x,y=loc_y, alpha=..count..),binwidth = 25)+
                scale_fill_gradientn(colours=c("blue","light green","green", "dark green"),name = "Frequency",na.value=NA)+
                geom_point(mapping = aes(x=loc_x,y=loc_y,color=shrr()$real_shot_made_flag,shape=shrr()$combined_shot_type))+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
              #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="none")+
              annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) 
          }      
          else
          {
              #affiche uniquement les tirs sous forme de points, couleur fonction de reussi ou non
            if (input$typ_affich==1)
            {
              p<-ggplot(data=shrr()) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                  
                geom_point(mapping = aes(x=loc_x,y=loc_y,color=shrr()$real_shot_made_flag,shape=shrr()$combined_shot_type))+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="none")+
                annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) 
            }
            else  #affiche uniquement les hex
            {
              p<-ggplot(data=shrr()) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                stat_binhex(aes(x=loc_x,y=loc_y, alpha=..count..),binwidth = 25)+
                scale_fill_gradientn(colours=c("blue","light green","green", "dark green"),name = "Frequency",na.value=NA)+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="none")+
                annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) 
              
            }
          }

        p
        
    })
    
    #Affichage des ratio aux tirs par saisons
    output$graph <- renderPlot({
  #calcul du nombre de tirs et du ratio pour alim du graphique
      gg<-shrr() %>% select(season,real_shot_made_flag) %>% group_by(season,real_shot_made_flag) %>% summarise( nbs=n())
      gg_manq<-shrr() %>% select(season,real_shot_made_flag) %>% group_by(season,real_shot_made_flag) %>% summarise( nbs_mq=n())%>%filter(real_shot_made_flag==0)
      gg_mis<-shrr() %>% select(season,real_shot_made_flag) %>% group_by(season,real_shot_made_flag) %>% summarise( nbs_mis=n())%>%filter(real_shot_made_flag==1)
      gg_rat<-gg_mis%>% select(season, nbs_mis)%>%inner_join(gg_manq, by="season")%>%
                  select(season,nbs_mis, nbs_mq)%>% summarise(ratio=nbs_mis/(nbs_mis+nbs_mq))
      na.omit(gg_rat)
     
      gg_rat<-as.data.frame(gg_rat)
      #tirs<-factor(x=gg$shot_made_flag,levels=c("1","0"), labels = c("tirs réussis","tirs manqués"))
           g<-ggplot(data=gg_rat)+
            #geom_point(aes(x=gg$season,y=gg$nbs,color=tirs))+
             geom_bar(mapping=aes(x=season,y=ratio,fill=ratio),stat="identity")+
            ylim (0,0.6)+ 
           #expand_limits(y = c(0, gg$nbs+20))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            labs(title=" Pourcentage de réussite de Kobe au tir sur les périodes sélectionnées", col="orange")
         #theme x &axis rotate)=90
        # ajouter des labels pour les 0 1
           
      g
    }, height = 300) 
    
    
    
  }) #fin du obserEvent    
    # Generate a summary of the data ----
    output$summary <- renderPrint({ summary(shr)})
#******************************************************************************************************************************************************    
    # Visu du fichier complet ----
    output$table <- renderDataTable({shr})

  
  #******************************************************************************************************************************************************  
    
    #Genere l'onglet avec l'ensemble des statistiques du joueur sur sa carrière
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
              " Equipe: ","<font color=\"purple\"><b>",ig[1,12],"</b></font>","&emsp;","nombre de saisons NBA: ","<font color=\"purple\"><b>",ig[1,9],"</b></font>","&emsp;","de ","<font color=\"purple\"><b>",ig[1,13],"</b></font>"," à ","<font color=\"purple\"><b>",ig[1,14],"</b></font>",br(),
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
      
      
      
      
      #Prepare les données statisitiques
      #Moyenne de points en carrière, min et max
      #% de réussite au tir en carrière
      #% de réussite par type de shoot
      #% de réussite à 3pts
      #% de réussite à 2pts
#      
      #% de victoire en carrière
      #Moyenne de points par saison, min et max
      #% de victoire par saison
      #% de réussite par saison
      #% de réussite par saison et par type de tir
      #% de réussite par saison à 3pts
      
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
