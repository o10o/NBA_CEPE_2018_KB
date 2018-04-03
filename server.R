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
    #lecture à partir du RDS 
    shr <- readRDS("./data/kb.rds")
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
     sh_filtre<-shr[shr$combined_shot_type %in% input$type_shoot & shr$opponent %in% input$adversaire & shr$season %in% input$saison & shr$real_shot_made_flag %in% reussi & shr$boo_dom %in% dx,]
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
       c<-chartJSRadar(labs = titres,scores=lst, height = '120',width = '120', labelSize = 14, addDots = T,showLegend = T,main = "Profil de réussite aux tirs par type de tir")
       c
       
    }
    )
  }) #fin du obserEvent    
    # Generate a summary of the data ----
    output$summary <- renderPrint({ summary(shr())})
    
    # Generate an HTML table view of the data ----
    output$table <- renderDataTable({shr()})
    


}

# Run the application 
#shiny::runApp()
