#OD

#Essai de dessin du terrain en ggplot hexbin

#chargement des library
library(ggplot2)
library(hexbin)
library(png)
library(grid)
library(ggmap)

#lecture des coordonnées de shoot
sh<-read.table("./data/source_kb_shots.csv", header=T,sep=",")
#lecture du fichier image de fond
court<-readPNG("./img/Lakers2.PNG")
gcourt<- rasterGrob(court, width=unit(1,"npc"), height=unit(1,"npc"),interpolate=TRUE)
rcourt<-as.raster(court, max=255L)
sh_mis<-sh[sh$shot_made_flag==1,]
sh_manq<-sh[sh$shot_made_flag==0,]

ggimage(rcourt, fullpage = T)+
  geom_hex(aes(x=loc_x, y=loc_y),data=sh_mis)
decoup<-c(0, 100, 250, 500, 1000, 1500, 2000, 2500, Inf)
),

ggplot(sh_mis) + 
    annotation_custom(gcourt, -Inf, Inf, -Inf, Inf) +
  geom_hex(aes(x=loc_x, y=loc_y,fill = cut(sqrt(loc_x^2+loc_y^2),decoup )), data=sh_mis, stat=StatIdentity, bins=500)+
  scale_fill_brewer(palette = "GnBu",
                    labels = c("<100", "100-250", "250-500",
                               "500-1000", "1000-1500",
                               "1500-2000", "2000-2500",
                               ">2500"))+
  theme(legend.position="none") +
  xlim(-150, 150) +
    ylim(-5, 790)
  #guides(alpha = FALSE, size = FALSE) +



  
  
  
  
   hbin <- hexbin(sh$loc_x, sh$loc_y, xbins = 600)
 gplot.hexbin(hbin, style=c("colorscale", "centroids", "lattice",
                            "nested.lattice", "nested.centroids", "constant.col"),colramp = BTY)
