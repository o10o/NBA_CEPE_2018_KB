library(plotly)
library(shiny)
library(ggplot2)
library(hexbin)
library(png)
library(grid)
library(hexbin)
library(dplyr)
library(stringr)


#devtools::install_github('hadley/ggplot2')

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
     shr<-read.csv( "./data/source_kb_shots.csv", header=T)
      
      
    # })
  
     output$player_photo <- renderImage({
        #Photo de Kobe
        outfile <- normalizePath('./www/KobeBryant.jpg')
        # Return a list containing the filename
        list(src = outfile  , contentType = 'image/jpeg', width = 180,  height = 150,alt = "This is alternate text" )
    }, deleteFile = F)
    
    #recuperer les filtrages selectionnés dans shrr
     shrr<-reactive({
      if (input$dom_ext==0) dx<-c("@","vs")
     else if(input$dom_ext==1) dx<-c("@","@")
     else dx<-c("vs","vs")
     
     #prepare l'affichae des shoots reussis ou pas ou les deux
     if (input$reussi==2) 
       reussi<-c(0,1)
     else
       reussi<-input$reussi
     #Chaine récupérant les différents filtres
     #sh_filtre<-shr()[shr()$combined_shot_type %in% input$type_shoot & shr()$opponent %in% input$adversaire & shr()$season %in% input$saison & shr()$shot_made_flag %in% reussi & str_locate(shr()$matchup,dx),]
     sh_filtre<-shr[shr$combined_shot_type %in% input$type_shoot & shr$opponent %in% input$adversaire & shr$season %in% input$saison & shr$shot_made_flag %in% reussi & str_locate(shr$matchup,dx),]
    })
     
     
    #Affichage du terrain des tirs et heatmap
    output$plot <- renderPlot({
            decoup<-c(0, 100, 250, 500, 1000, 1500, 2000, 2500, Inf)
            #chargement du terrain
            court<-readPNG("./www/Lakers2.PNG")
            gcourt<- rasterGrob(court, width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
            #chargement des lignes    
            bblines<-readPNG("./www/BBCourtLines.PNG")
            gbblines<- rasterGrob(bblines, width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
    
            #force la transparence pour les pixels blancs
            gbblines$raster[gbblines$raster=="#FFFFFFFF"]="#FFFFFF00"
            

        #affiche tir et hexbin  #affiche uniquement les tirs sous forme de points, couleur fonction de reussi ou non
        if (input$typ_affich==0)
        {
            p<-ggplot(data=shrr()) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                stat_binhex(aes(x=loc_x,y=loc_y, alpha=..count..),binwidth = 25)+
                scale_fill_gradientn(colours=c("yellow","red"),name = "Frequency",na.value=NA)+
                geom_point(mapping = aes(x=loc_x,y=loc_y,color=shrr()$shot_made_flag,shape=shrr()$combined_shot_type))+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
              #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="none")+
              annotation_custom(gbblines, -Inf, Inf, -Inf, Inf) 
          }      
          else
          {
              #affiche uniquement les tirs sous forme de points, couleur fonction de reussi ou non
            if (input$typ_affich==1)
            {
              p<-ggplot(data=shrr()) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                
                geom_point(mapping = aes(x=loc_x,y=loc_y,color=shrr()$shot_made_flag,shape=shrr()$combined_shot_type))+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="none")+
                annotation_custom(gbblines, -Inf, Inf, -Inf, Inf) 
            }
            else  #affiche uniquement les hex
            {
              p<-ggplot(data=shrr()) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                stat_binhex(aes(x=loc_x,y=loc_y, alpha=..count..),binwidth = 25)+
                scale_fill_gradientn(colours=c("yellow","red"),name = "Frequency",na.value=NA)+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="none")+
                annotation_custom(gbblines, -Inf, Inf, -Inf, Inf) 
              
            }
          }

        p
        
    })
    
    #Affichage du terrain des tirs et heatmap
    output$graph <- renderPlot({
      gg<-shrr() %>% select(season,shot_made_flag) %>% group_by(season,shot_made_flag) %>% summarise( nbs=n())
         g<-ggplot(data=gg)+
        geom_point(aes(x=gg$season,y=gg$nbs,color=factor(gg$shot_made_flag)))+
           ylim (0,2000)
         #theme x &axis rotate)=90
        # ajouter des labels pour les 0 1
           
      g
    }) 
    # Generate a summary of the data ----
    output$summary <- renderPrint({ summary(shr())})
    
    # Generate an HTML table view of the data ----
    output$table <- renderDataTable({shr()})
    


}

# Run the application 
#shiny::runApp()
