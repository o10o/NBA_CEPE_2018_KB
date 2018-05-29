#######################################################################################

#PRogramme de visualisation des statisitiques descriptives de  Kobe Bryant sur l'ensemble de sa carrière

#Partie ui.R
#######################################################################################
#3 onglets sont fournis
#onglet "Le joueur en carrière"
#       Fournit 1-les informations générales sur le joeurs récupérée sur le site de la NBA via un fichier JSON et stockées danssplayer.table.rda
#               2-Radar de performance au tir par macro typs de shoots
#               3-performance par type de shoot détaillés ordonnées du meilleur % au moins bon
#               4- profil de volume de shoots reussis et manqués en fonction de l'éloignement en largeur par rappor au cercle
#               5-profil de volume de shoots reussis et manqués en fonction de l'éloignement en profondeur par rappor au cercle
#               6-Affcihage selon les trois catégorisation de zones de shoots des volumes de shoots pris et des pourcentages sur l'ensemble de la carrière
#                        Shot_zone_area (geographique)
#                        shot zone basic (par rapport aux règles du jeu)
#                        shot_zone_range (par rapport à l'éloignement au cercle)
#***************
#Onglet "Visualisation des tirs"
#             Representation dynamique des shoots, type de shoots, adresse et volume combiné par rapport à la position sur le terrain
#             sélection des ceritères en amont (types de shoot, adversaires , saisons, dom/ext , en sasion, en playoff...)
#             Tableau récapitulatif par type de shoot des % de réussite
#**************
#onglet "données, qui ne fait qu'aficher le contenu du fichier conteant ls informations sur les shoots
#**************
#Olivier Dissaux, Karim Hammour, Matthias Villaverde

#Projet de datascience pour la session Datascientist Ensae CEPE 2017-2018 (exam le 5 juin)

#######################################################################################
library(shiny)
library(png)
library(grid)
library(ggplot2)
library(plotly)
library(radarchart)


shr<-read.csv( "./data/kb_analyse.csv", header=T)  %>%
  select(-X) %>%
  mutate(game_date=as.Date(game_date)) %>%
  arrange(game_date, period, temps_period, game_event_id)
shr <- shr%>%mutate(dom_ext = ifelse(shr$boo_dom==1,"DOM","EXT"))%>%
  mutate(periode = ifelse(shr$playoffs==1,"Playoffs","Saison Reguliere"))%>%
  mutate(reussite = ifelse(shr$real_shot_made_flag==1,"reussi","rate"))

#ne garde que les colonnes utiles pour chargement plus rapide
sh<-shr[,c(1:12,15,62,63,64,65,66,71,72,134,135,136)]


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
        
        #Panneau principal ----
        mainPanel(id="main",
            
            
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
