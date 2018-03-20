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
    
    shr <- reactive({
    #    infile <- input$datafile
    #    if (is.null(infile)) {
           
    #        return(NULL)
    #    }
    #   else  #read.csv(infile$datapath)
        read.csv( "./data/source_kb_shots.csv", header=T)
     })
  
    
    
    #rempli la liste avec les saisons
    output$saison<-renderUI({
        sh=shr()
        if(is.null(sh)) return (NULL)
        sais<-sh%>%distinct(season)
        selectInput(inputId = "saison",label="Sel. Saison",choices = sais)
    })
    
    
    #rempli la liste avec les types de shoot possibles
    output$type_shoot<-renderUI({
        sh=shr()
        if(is.null(sh)) return (NULL)
        dist_typsh<-sh%>%distinct(combined_shot_type)
        selectInput(inputId = "typ_shoot",label="Type de shoot",choices = dist_typsh)
    })
    #rempli la liste avec les adversaires
    output$adversaire<-renderUI({
        sh=shr()
        if(is.null(sh)) return (NULL)
        advers<-sh%>%distinct(opponent)
        selectInput(inputId = "adversaire",label="Sel.Adversaire",choices = c("toto ",advers),)
    })
    
  
    
    output$type_shoot <- renderUI({
        #convertit en entree le type de shoot selectionne pour quil puise etre passe au plot    
        selectInput("ts", "type shoot:", unique(as.character(shr()$combined_shot_type)),multiple=T)
    })
    
    output$adversaire <- renderUI({
        #convertit en entree l'adversaire selectionne pour quil puisse etre passe au plot    
        selectInput("opp", "adversaire:", unique(as.character(shr()$opponent)),multiple=T)
    })
    
    output$saison <- renderUI({
        #convertit en entree l'adversaire selectionne pour quil puisse etre passe au plot    
        selectInput("saiz", "saison:", unique(as.character(shr()$season)),multiple = T)
    })
    output$player_photo <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- normalizePath('./www/KobeBryant.jpg')
     
        # Return a list containing the filename
        list(src = outfile  , contentType = 'image/jpeg', width = 180,  height = 150,alt = "This is alternate text" )
    }, deleteFile = F)
    
    output$plot <- renderPlotly({
        decoup<-c(0, 100, 250, 500, 1000, 1500, 2000, 2500, Inf)
          #court<-readPNG("./www/Lakers2.PNG")
          #  gcourt<- rasterGrob(court, width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
        #selectInput("type_shoot", "type_shoot", type_shoot_y)
       
        
        #sh=shr()
        #analyse code domicile exterieur pour requetage
        if (input$dom_ext==0) dx<-c("@","vs")
        else if(input$dom_ext==1) dx<-c("@","@")
        else dx<-c("vs","vs")
        
        #prepare l'affichae des shoots reussis ou pas ou les deux
        if (input$reussi==2) 
            reussi<-c(0,1)
        else
            reussi<-input$reussi
        
        
        #sh_filtre<-shr()[shr()$combined_shot_type==input$ts & shr()$opponent==input$opp & shr()$season==input$saiz & shr()$shot_made_flag %in% reussi & str_locate(shr()$matchup,dx),]
        sh_filtre<-shr()[shr()$combined_shot_type %in% input$ts & shr()$opponent %in% input$opp & shr()$season %in% input$saiz & shr()$shot_made_flag %in% reussi & str_locate(shr()$matchup,dx),]
        p<-ggplot(data=sh_filtre) + 
            #theme_void()+
           # annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
            #geom_hex(aes(x=loc_x, y=loc_y ,fill =cut(sqrt(loc_x^2+loc_y^2),decoup )), 
            #         data=shr()[shr()$combined_shot_type==input$ts & shr()$opponent==input$opp & shr()$season==input$saiz & shr()$shot_made_flag %in% reussi & str_locate(shr()$matchup,dx),], stat="binhex", bins=500)+
            #scale_fill_brewer(palette = "Set1",
            #                  labels = c("<100", "100-250", "250-500",
            #                             "500-1000", "1000-1500",
            #                             "1500-2000", "2000-2500",
            #                             ">2500"))+
            geom_point(mapping = aes(x=loc_x,y=loc_y,color=sh_filtre$shot_made_flag,shape=sh_filtre$combined_shot_type))+
            xlim(-250,250)+
            ylim(-44,800)+
            theme(legend.position="none") 
         ggplotly(p,tooltip="text")
        #hbin <- (sh$loc_x, sh$loc_y, xbins = 600)
        #gplot.hexbin(hbin, style=c("colorscale", "centroids", "lattice",
        #                           "nested.lattice", "nested.centroids", "constant.col"),colramp = BTY)
    })
    
    
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({ summary(shr())})
    
    # Generate an HTML table view of the data ----
    output$table <- renderDataTable({shr()})
    
    output$court <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- normalizePath('./www/Lakers2.png')
        
        # Return a list containing the filename
        list(src = outfile  , contentType = 'image/png', width = 400,  height = 300,alt = "This is alternate text" )
    }, deleteFile = F)

}

# Run the application 
#shiny::runApp()
