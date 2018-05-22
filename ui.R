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
shr <- shr%>%mutate(dom_ext = ifelse(shr$boo_dom==1,"DOM","EXT"))%>%
  mutate(periode = ifelse(shr$playoffs==1,"Playoffs","Saison Reguliere"))%>%
  mutate(reussite = ifelse(shr$real_shot_made_flag==1,"reussi","rate"))

#ne garde que les colonnes utiles pour chargement plus rapide
sh<-shr[,c(1:12,15,62,63,64,65,66,71,72,134,135,136)]
#lectutre à partir du RDs
#sh <- readRDS("./data/kb.rds")
print(names(sh))
valperiod<-unique(as.character(sh$periode))
valdom<-unique(as.character(sh$dom_ext))
valsais<-unique(as.character(sh$season))
valreussite<-unique(as.character(sh$reussite))
valadv<-unique(as.character(sh$opponent))

if(is.null(sh)) return (NULL)
dist_typsh<-unique(as.character(sh$combined_shot_type))
z_sh<-unique(as.character(sh$shot_type))
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
            
            selectInput(inputId = "type_shoot",label="Type de shoot",choices = dist_typsh,multiple=TRUE, selectize=FALSE),
            selectInput(inputId = "zone_shoot",label="Zone de shoot",choices = z_sh,multiple=TRUE, selectize=FALSE,selected=z_sh),
           
            #radio bouton pour shoots reussis ou pas
            selectInput("reussite", "Tirs", valreussite,multiple=TRUE, selectize=FALSE,selected=valreussite),
            
            
            #selection de la saison
            
            #selectInput("saison", "saison:", valsais,multiple = T,selected = valsais),
            selectInput("saison", "Saison(s):", valsais,multiple=TRUE, selectize=FALSE),
            #radio bouton pour choix domicile exterieur les deux
            selectInput("playoffs", "Periode", valperiod,multiple=TRUE, selectize=FALSE, selected = valperiod),
            
            #selection de l'adversaire
            #selectInput("adversaire", "adversaire:", valadv,multiple=T,selected=valadv),
            selectInput("adversaire", "Adversaire(s):", valadv,multiple=TRUE, selectize=FALSE),
        
            #radio bouton pour choix domicile exterieur les deux
            selectInput("dom_ext", "DOM/EXT", valdom,multiple=TRUE, selectize=FALSE,selected=valdom),
            
            #radio bouton pour affichage tir heat les deux
            radioButtons("typ_affich", label = h4("Affichage"),
                         choices = list("tirs" = 1, " hexagones " = 2), 
                         selected = 2)
 
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(id="main",
            
            # Output: Tabset w/ plot, summary, et table ----
            tabsetPanel(type = "tabs",
                        tabPanel( class="my_style_1",id="IG","Le joueur en carrière",htmlOutput("InfoGene"),htmlOutput("InfoStats"), htmlOutput("TitreProfil"),
                                                                                    chartJSRadarOutput (outputId = "profil", height="140%"),
                                                                                    HTML(paste(br(),"<font color=\"blue\"><b>",h4("Adresse par types de shoots détaillés"),"</b></font>")),
                                                                                    plotOutput("shoot_adresse"),
                                                                                    fluidRow(column(width=6,
                                                                                                    HTML(paste(br(),"<font color=\"blue\"><b>",h4("Répartitions en vol. de shoots sur la largeur"),"</b></font>")),
                                                                                                    plotOutput("repartX")
                                                                                                    ),
                                                                                             column(width=6,  
                                                                                                    HTML(paste(br(),"<font color=\"blue\"><b>",h4("Répartitions en vol. de shoots sur la longueur"),"</b></font>")),
                                                                                                    plotOutput("repartY")
                                                                                                    )
                                                                                            ),
                                                                                    
                                                                                    fluidRow(column(width=12,
                                                                                                    HTML(paste("<font color=\"blue\"><b>",h4("Mais de où Kobe prend il ses shoots?"),"</b></font>"))
                                                                                                    ),
                                                                                             fluidRow(
                                                                                                      column(width=4,
                                                                                                              plotOutput("shoot_zone_area")
                                                                                                              ),
                                                                                                      column(width=4,
                                                                                                             plotOutput("shoot_zone_basic")
                                                                                                              ),
                                                                                                      column(width=4,
                                                                                                             plotOutput("shoot_zone_range")
                                                                                                              )
                                                                                                       )
                                                                                             )
                                  ),
                        tabPanel(id="tab","Visualisation des tirs", plotOutput("plot",height = "800"), DT::dataTableOutput("tirs"),plotOutput("graph")),
                        #tabPanel(id="tab2","Visualisation des tirs 2", plotOutput("plot2", height="800") ),
                        tabPanel("données", dataTableOutput("table"))
                        
            )
            
        )
    )
)
