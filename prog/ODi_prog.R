#OD

#Essai de dessin du terrain en ggplot hexbin

#chargement des library
library(ggplot2)
library(hexbin)
library(png)
library(grid)

#lecture des coordonnées de shoot
sh<-read.table("./data/source_kb_shots.csv", header=T,sep=",")
#lecture du fichier image de fond
court<-readPNG("./img/Lakers.PNG")
gcourt<- rasterGrob(court, interpolate=TRUE)
ggplot(sh, aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -3, 100, -102, 3) +
  geom_hex(aes(alpha= .3, fill = class, bins = 25))+
  guides(alpha = FALSE, size = FALSE) +
  xlim(-1, 102) +
  ylim(102, -3) +
  coord_fixed()

 hbin <- hexbin(sh$loc_x, sh$loc_y, xbins = 600)
 gplot.hexbin(hbin, style=c("colorscale", "centroids", "lattice",
                            "nested.lattice", "nested.centroids", "constant.col"),colramp = BTY)
