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
   
     shr <- shr%>%mutate(dom_ext = ifelse(shr$boo_dom==1,"DOM","EXT"))%>%
      mutate(periode = ifelse(shr$playoffs==1,"Playoffs","Saison Reguliere"))%>%
      mutate(reussite = ifelse(shr$real_shot_made_flag==1,"reussi","rate"))
    
    #ne garde que les colonnes utiles pour chargement plus rapide
    shr<-shr[,c(1:12,15,62,63,64,65,66,68:72,134,135,136)]
    
    
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
     
      
     
     #Chaine récupérant les différents filtres
     sh_filtre<-shr[shr$combined_shot_type %in% input$type_shoot & shr$opponent %in% input$adversaire &
                      shr$season %in% input$saison & shr$reussite %in% input$reussite &
                      shr$dom_ext %in% input$dom_ext & shr$shot_type %in% input$zone_shoot & shr$periode %in% input$playoffs,]
      
     shrr<-sh_filtre
   
    #Affichage du terrain des tirs et heatmap
    output$plot <- renderPlot({
            decoup<-c(0, 100, 250, 500, 1000, 1500, 2000, 2500, Inf)


        #affiche tir et hexbin  #affiche uniquement les tirs sous forme de points, couleur fonction de reussi ou non
        if (input$typ_affich==0)
        {
          
            p<-ggplot(data=shrr) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                stat_binhex(aes(x=loc_x,y=loc_y, alpha=..count..),binwidth = 25)+
                scale_fill_gradientn(colours=c("blue","light green","green", "dark green"),name = "Frequency",na.value=NA)+
                geom_point(mapping = aes(x=loc_x,y=loc_y,color=shrr$real_shot_made_flag,shape=shrr$combined_shot_type))+
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
              p<-ggplot(data=shrr) + 
                #theme_void()+
                annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
                  
                geom_point(mapping = aes(x=loc_x,y=loc_y,color=shrr$real_shot_made_flag,shape=shrr$combined_shot_type))+
                scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
                scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
                #plot.background = element_rect(fill = "black"),
                coord_equal()+theme(plot.margin=margin(0,0,0,0),legend.position="none")+
                annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) 
            }
            else  #affiche uniquement les hex
            {
              p<-ggplot(data=shrr) + 
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
    
    #Affichage d'un tableau avec les Tirs réussis et ratés, en nombre et en %
    
    res<-NULL
    Pourcentage<-NULL
    Tirs_reussis<-NULL
    Tirs_tentes<-NULL
    res<-table(shrr$shot_zone_basic,shrr$real_shot_made_flag)
    
    if (!is.null(res)){
      if (dim(res)[2]==2){
        Tirs_reussis<-res[,2]
        Tirs_tentes<-res[,1]+res[,2]
      }
      else{
        if (input$reussite=="reussi")
          Tirs_reussis<-res[,1]
        
      
      Tirs_tentes<-res[,1]
        
      }
       res<-cbind(Tirs_reussis,Tirs_tentes)
       print(Tirs_reussis)
       for (ii in 1:dim(res)[1])
       {
         
         if (!is.null(Tirs_reussis[ii])) Pourcentage[ii]<-Tirs_reussis[ii]/Tirs_tentes[ii]
         else Pourcentage[ii]<-0
       }
       Pourcentage = scales::percent(Pourcentage)
       fin<-cbind(round(res),Pourcentage)
       output$tirs <-  DT::renderDataTable({
         DT::datatable(fin)
       })
    }
    #Affichage des ratio aux tirs par saisons
    output$graph <- renderPlot({
    #calcul du nombre de tirs et du ratio pour alim du graphique
      gg<-shrr %>% select(season,real_shot_made_flag) %>% group_by(season,real_shot_made_flag) %>% summarise( nbs=n())
      gg_manq<-shrr %>% select(season,real_shot_made_flag) %>% group_by(season,real_shot_made_flag) %>% summarise( nbs_mq=n())%>%filter(real_shot_made_flag==0)
      gg_mis<-shrr %>% select(season,real_shot_made_flag) %>% group_by(season,real_shot_made_flag) %>% summarise( nbs_mis=n())%>%filter(real_shot_made_flag==1)
      gg_rat<-gg_mis%>% select(season, nbs_mis)%>%inner_join(gg_manq, by="season")%>%
                  select(season,nbs_mis, nbs_mq)%>% summarise(ratio=nbs_mis/(nbs_mis+nbs_mq))
      na.omit(gg_rat)
     
      gg_rat<-as.data.frame(gg_rat)
      g<-ggplot(data=gg_rat)+
            #geom_point(aes(x=gg$season,y=gg$nbs,color=tirs))+
             geom_bar(mapping=aes(x=season,y=ratio,fill=ratio),stat="identity")+
            ylim (0,0.6)+ 
           #expand_limits(y = c(0, gg$nbs+20))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            labs(title=" Pourcentage de réussite de Kobe au tir sur les périodes sélectionnées", col="orange")
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
      
      
      
      
        resum_tir_carriere<-shr%>% select (season,shot_type,real_shot_made_flag,combined_shot_type )%>%
        group_by(season, real_shot_made_flag,combined_shot_type, shot_type)%>% summarise(nb=n())%>%
        arrange(season,combined_shot_type,real_shot_made_flag)
        tir_mq<-sum(resum_tir_carriere[resum_tir_carriere$real_shot_made_flag==0,]$nb)
        tir_reussis<-sum(resum_tir_carriere[resum_tir_carriere$real_shot_made_flag==1,]$nb)
      #% reussite carriere
          ratio_carriere<-tir_reussis/(tir_mq+tir_reussis)*100
      
      pts_match_saison<-shr%>%select(season, game_date,opponent,shot_made_flag)%>%
        group_by(season,opponent,game_date,shot_made_flag) %>% filter(shot_made_flag==1) %>%summarise(nbpts=n())
      
      
     
      #Graphique adresse par type de shoots détaillés
      #regroupe les shoots peu nombreux dans la catégorie autres
      f<-shr
      f %>% count(action_type) %>%
        arrange(desc(n)) %>% filter(n < 20) -> actions
      f$action_type[f$action_type %in% actions$action_type] <- "Autres shoots"
     
      output$shoot_adresse<-renderPlot({
       prop.table(table(f$action_type, f$real_shot_made_flag),1) -> temp
        temp<-na.omit(temp)
      as.data.frame.matrix(temp) -> temp

      
      temp$shot <- rownames(temp)
      
      ggplot(temp, aes(x = reorder(shot, `1`), y = 1)) +
        geom_bar(aes(y = `1`,width=0.2), size = 1, color = " orange", stat = "identity") +
        coord_flip() +
        labs(y = "Adresse en %", x = "")
      }) 
      
      
      #Répartition des shoots selon abscisses et ordonnés
        f$x_abs <- cut(f$loc_x, breaks = 25)
        f$y_ord <- cut(f$loc_y, breaks = 25)
        
        output$repartX<-renderPlot({
          ggplot(f ,aes(x=f$x_abs,fill=as.factor(f$real_shot_made_flag)))+ 
          geom_bar(colour="blue" ) +
            scale_fill_discrete(name ="Réussite") +
            theme(axis.text.x=element_text(angle=-90,hjust=1),legend.position = c(0.8, 0.8),legend.title = element_text(colour="purple"))
          
          
      }, height = 400) 
      
      
      output$repartY<-renderPlot({
          ggplot(f ,aes(x=f$y_ord,fill=as.factor(f$real_shot_made_flag)))+ 
          geom_bar(colour="blue") +
          scale_fill_discrete(name ="Réussite") +
          theme(axis.text.x=element_text(angle=-90,hjust=1),legend.position = c(0.8, 0.8),legend.title = element_text(colour="purple"))
        
          
      }, height = 400) 
      
      #répartition des shoots selon la zone area
      #agrege les shoot par zone pour les afficher
      res<-f%>%select(loc_x,loc_y,shot_zone_area, real_shot_made_flag)%>%
          group_by(shot_zone_area)%>%summarise(nb=n(), x_moy=mean(loc_x),y_moy=mean(loc_y),reussite=100*sum(real_shot_made_flag)/nb)
      output$shoot_zone_area<-renderPlot({
          ggplot(f,aes(x = f$loc_x, y = f$loc_y)) +
          annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
          geom_point(aes_q(color = f$shot_zone_area), alpha = 0.7, size = 1) +
          scale_color_brewer("zones de shoots", palette="Set2")+
          scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
          scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
          coord_equal()+
          annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) +
          theme(legend.title = element_text(colour="purple")) +
          ggtitle(ggtitle("critère shot zone area"))+
          geom_label(data =res, size=3, col="blue",fontface="bold",aes(label=paste(res$nb,"\n à ",round(res$reussite,1),"%")),x=round(res$x_moy),y=round(res$y_moy),alpha=0.2)
      })
      
      #répartition des shoots selon la zone 
      #agrege les shoot par zone pour les afficher
      #si zone à 3 pts on remonte ordonne pour affichage correct
      res2<-f%>%select(loc_x,loc_y,shot_zone_basic, real_shot_made_flag) %>%
        group_by(shot_zone_basic) %>%summarise(nb=n(), x_moy=mean(loc_x),y_moy=mean(loc_y),reussite=100*sum(real_shot_made_flag)/nb)
      #init colonne de libelle
      res2$lib<-""
      for (ii in  1: nrow(res2))
      { 
        res2[ii,"lib"]<-paste(res2[ii,"nb"]," à ",round(res2[ii,"reussite"],1),"%")
        if(res2[ii,1]=="Above the Break 3") res2[ii,]$y_moy<-res2[ii,]$y_moy+50
        if (res2[ii,1]=="Mid-Range") res2[ii,]$y_moy<-res2[ii,]$y_moy+80
        if (res2[ii,1]=="Restricted Area" ) res2[ii,]$y_moy<-res2[ii,]$y_moy+10
        if (res2[ii,1]=="Left Corner 3" ) {                res2[ii,]$x_moy<-res2[ii,]$x_moy+15
                res2[ii,"lib"]<-paste(res2[ii,"nb"],"\nà\n",round(res2[ii,"reussite"],1),"%")
        }
        if (res2[ii,1]=="Right Corner 3" ) {  res2[ii,]$x_moy<-res2[ii,]$x_moy-30
            res2[ii,"lib"]<-paste(res2[ii,"nb"],"\nà\n",round(res2[ii,"reussite"],1),"%")
        }
      }
      output$shoot_zone_basic<-renderPlot({
        ggplot(f,aes(x = f$loc_x, y = f$loc_y)) +
          annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
          geom_point(aes_q(color = f$shot_zone_basic), alpha = 0.7, size = 1) +
          scale_color_brewer("zones de shoots", palette="Set2")+
          scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
          scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
          coord_equal()+
          annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) +
          theme(legend.title = element_text(colour="purple")) +
          ggtitle("critère :shot zone basic")+
          geom_label(data =res2, size=3,col="blue",fontface="bold", aes(label=res2$lib),x=round(res2$x_moy),y=round(res2$y_moy),alpha=0.2)
      })
      
      #répartition des shoots selon la zone 
      #agrege les shoot par zone pour les afficher
      res3<-f%>%select(loc_x,loc_y,shot_zone_range, real_shot_made_flag)%>%group_by(shot_zone_range)%>%
        summarise(nb=n(), x_moy=mean(loc_x),y_moy=mean(loc_y)+45,reussite=100*sum(real_shot_made_flag)/nb)
      for (ii in  1: nrow(res3))
      {
        if(res3[ii,1]=="24+ ft.") res3[ii,]$y_moy<-res3[ii,]$y_moy+65
       
      }
      output$shoot_zone_range<-renderPlot({
        ggplot(f,aes(x = f$loc_x, y = f$loc_y)) +
          annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
          geom_point(aes_q(color = f$shot_zone_range), alpha = 0.7, size = 1) +
          scale_color_brewer("zones de shoots", palette="Set2")+
          scale_x_continuous(limits=c(-250,250),expand = c(0,0))+
          scale_y_continuous(limits=c(-47.5,-47.5+940),expand = c(0,0))+
          coord_equal()+
          annotation_custom(gcourt2, -Inf, Inf, -Inf, Inf) +
          theme(legend.title = element_text(colour="purple")) +
          ggtitle("critère :shot zone range")+
          geom_label(data =res3, size=3,col="blue",fontface="bold",aes(label=paste(res3$nb,"à ",round(res3$reussite,1),"%") ),x=round(res3$x_moy),y=round(res3$y_moy),alpha=0.2)
      })
}

# Run the application 
#shiny::runApp()
