#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
library(grid)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram

sh <- read.csv( "./data/source_kb_shots.csv", header=T)
valsais<-unique(as.character(sh$season))
valadv<-unique(as.character(sh$opponent))
if(is.null(sh)) return (NULL)
dist_typsh<-sh%>%distinct(combined_shot_type)


#theme="bootstrap.css"
ui <- fluidPage(
     tags$head(tags$style(
            HTML('
                     body, label, input, button, select { 
                     font-family: "Arial";
                     }'))),
    
    # App title ----
    titlePanel("Kobe Bryant, performance aux tirs"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(id="sidebar",
            imageOutput("player_photo", width="100%", height="20%"),
            
            #fileInput('datafile', label='Sel. Fichier',buttonLabel = 'Sel. Fichier',
            #          accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
            
            
            selectInput(inputId = "type_shoot",label="Type de shoot",choices = dist_typsh,multiple=T),
            #radio bouton pour shoots reussis ou pas
            radioButtons("reussi", label = h4("Tirs réussis"),
                         choices = list("Oui" = 1, "Non" = 0,"Les deux" = 2), 
                         selected = 2),
  
            
            #selection de la saison
            
            selectInput("saison", "saison:", valsais,multiple = T,selected = valsais),
            #selection de l'adversaire
            selectInput("adversaire", "adversaire:", valadv,multiple=T,selected=valadv),
            
        
            #radio bouton pour choix domicile exterieur les deux
            radioButtons("dom_ext", label = h4("Domicile/Extérieur"),
                         choices = list("Les deux"=0,"Domicile" = 1, "Extérieur" = 2), 
                         selected = 1),
            
            #radio bouton pour affichage tir heat les deux
            radioButtons("typ_affich", label = h4("Affichage"),
                         choices = list("Les deux"=0,"tirs" = 1, " hexagones " = 2), 
                         selected = 0)
 
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(id="main",
            
            # Output: Tabset w/ plot, summary, et table ----
            tabsetPanel(type = "tabs",
                        #Premier onbglet plot les tirs
                        tabPanel(id="tab","Visualisation des tirs", plotOutput("plot",height = "800"),plotOutput("graph")),
                        tabPanel("Stats", verbatimTextOutput("summary")),
                        tabPanel("données", dataTableOutput("table"))
            )
            
        )
    )
)
