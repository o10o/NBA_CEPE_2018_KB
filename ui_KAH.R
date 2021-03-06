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
library(radarchart)

# Define UI for application that draws a histogram
#sh <- read.csv( "./data/source_kb_shots.csv", header=T)
shr<-read.csv( "./data/kb_analyse.csv", header=T)  %>%
  select(-X) %>%
  mutate(game_date=as.Date(game_date)) %>%
  arrange(game_date, period, temps_period, game_event_id)
#ne garde que les colonnes utiles pour chargement plus rapide
sh<-shr[,c(1:12,15,62,63,64,65,66,71,72)]
#lectutre Ã  partir du RDs
#sh <- readRDS("./data/kb.rds")

valsais<-unique(as.character(sh$season))
valadv<-unique(as.character(sh$opponent))
if(is.null(sh)) return (NULL)
dist_typsh<-sh%>%distinct(combined_shot_type)
z_sh<-sh%>%distinct(shot_type)
action<-unique(as.character(sh$action_type))

#theme="bootstrap.css"
ui <- fluidPage(
     #tags$head(tags$style(
            #HTML('
                     #body, label, input, button, select { 
                     #font-family: "Arial";
                     #}'))),
     tags$head(
       tags$style(HTML('.my_style_1{ 
                        background-color: #F4F4F7;
                       background-image: url(logoLakers4.jpg); position: absolute; left: 0;
                      width:100%;height:100%
                      opacity: 0.8;
                      filter:alpha(opacity=80);
                      background-size: 100%;
                      background-repeat: no-repeat;
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
    
            actionButton("actionB", label = "Actualisation"),
            
            selectInput(inputId = "type_shoot",label="Type de shoot",choices = dist_typsh,multiple=T,selectize = TRUE),
            selectInput(inputId = "zone_shoot",label="Zone de shoot",choices = z_sh,multiple=T,selectize = TRUE),
            #radio bouton pour shoots reussis ou pas
            radioButtons("reussi", label = h4("Tirs r�ussis"),
                         choices = list("Oui" = 1, "Non" = 0,"Les deux" = 2), 
                         selected = 2),
  
            
            #selection de la saison
            
            #selectInput("saison", "saison:", valsais,multiple = T,selected = valsais),
            selectInput("saison", "Saison(s):", valsais,multiple=TRUE, selectize=TRUE),
            #radio bouton pour choix domicile exterieur les deux
            radioButtons("playoffs", label = h4("Saison r�guli�re/playoff"),
                         choices = list("Saison R�guli�rere"=0,"Playoffs" = 1, "Saison r�g.+playoff" = 2), 
                         selected = 0),
            
            #selection de l'adversaire
            #selectInput("adversaire", "adversaire:", valadv,multiple=T,selected=valadv),
            selectInput("adversaire", "Adversaire(s):", valadv,multiple=TRUE, selectize=TRUE),
        
            #radio bouton pour choix domicile exterieur les deux
            radioButtons("dom_ext", label = h4("Domicile/Extérieur"),
                         choices = list("Les deux"=0,"Domicile" = 1, "Ext�rieur" = 2), 
                         selected = 0),
            
            #radio bouton pour affichage tir heat les deux
            radioButtons("typ_affich", label = h4("Affichage"),
                         choices = list("tirs" = 1, " hexagones " = 2), 
                         selected = 2)
 
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(id="main",
            
            # Output: Tabset w/ plot, summary, et table ----
            tabsetPanel(type = "tabs",
                        tabPanel( class="my_style_1",id="IG","Le joueur en carrière",htmlOutput("InfoGene"),
                                  htmlOutput("InfoStats"), htmlOutput("TitreProfil"),
                                  chartJSRadarOutput (outputId = "profil", height="140%")),
                        tabPanel(id="tab","Stats Descriptives",
                                 tabsetPanel(type = "tabs",
                                             tabPanel(id="tab","FG% / 2pts vs 3 pts",
                                                      plotOutput("PctEvol")),
                                             tabPanel(id="tab","FG% / Saison Régulière vs Playoffs",
                                                      plotOutput("PctRegPlayoffs")),
                                             tabPanel(id="tab","FG% / distance au panier",
                                                      plotOutput("PctDistance")),
                                             tabPanel(id="tab","FG% / moment du tir",
                                                      plotOutput("PctMoment")),
                                             tabPanel(id="tab","mid_range FG% / position latérale",
                                                      plotOutput("PctMidRangeSide"))
                                 )
                        ),         
                        tabPanel(id="tab","Visualisation des tirs", plotOutput("plot",height = "800"), 
                                 DT::dataTableOutput("tirs"),plotOutput("graph")),
                        # tabPanel("Stats", verbatimTextOutput("summary")),
                        tabPanel("données", dataTableOutput("table"))
                        
            )
            
        )
    )
)
