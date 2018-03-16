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
ui <- fluidPage(
     tags$head(tags$style(
            HTML('
         #sidebar {
                     background-color: black;
                     }
         #main {
                     background-color: black;
                     }
                     
                     body, label, input, button, select { 
                     font-family: "Arial";
                     }'))),
    
    # App title ----
    titlePanel("Kobe Bryant, performance aux tirs"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(id="sidebar",
            imageOutput("player_photo", width="100%"),
            
            #fileInput('datafile', label='Sel. Fichier',buttonLabel = 'Sel. Fichier',
            #          accept=c('text/csv', 'text/comma-separated-values,text/plain')),
            
            #radio bouton pour shoots reussis ou pas
            radioButtons("reussi", label = h4("Tirs réussis"),
                         choices = list("Oui" = 1, "Non" = 0,"Les deux" = 2), 
                         selected = 2),
            #radio bouton pour choix domicile exterieur les deux
            radioButtons("dom_ext", label = h4("Domicile/Extérieur"),
                         choices = list("Les deux"=0,"Domicile" = 1, "Extérieur" = 2), 
                         selected = 1),
        
            
            
            #selection de la saison
            uiOutput(outputId="saison"),
            
            uiOutput("adversaire"),
        
            
            
            #selection de  ou des adversaires
            uiOutput(outputId="type_shoot")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(id="main",
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Visualisation des tirs", plotlyOutput("plot"),imageOutput("court")),
                        tabPanel("Stats", verbatimTextOutput("summary")),
                        tabPanel("données", dataTableOutput("table"))
            )
            
        )
    )
)
