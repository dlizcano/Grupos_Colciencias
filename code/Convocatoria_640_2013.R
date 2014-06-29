
library(ggplot2)
library(ggmap)
library (reshape2)
library (plyr)
library (dplyr)
library (googleVis)

# read table 
grupos<-read.csv(file="C:\\Users\\Diego\\Documents\\CodigoR\\blog\\Colciencias\\data\\fulltable3.csv",header=TRUE,encoding="Latin-1")
Areas<-colsplit(grupos$Área1, pattern = "\\ -- ", names = c('Area1', 'Area2'))
grupos<-cbind(grupos, Areas)

ggplot(grupos, aes(Clasificacion, fill=Area1)) + geom_bar() + coord_flip() 
ggplot(grupos, aes(Clasificacion, fill=Area1)) + geom_bar() + facet_wrap(~ Genero)

### Takes 5 minutes and require Internet conexion
### make a query in google maps add details for some city
ciudades<-as.character(unique(grupos$Cuidad))
ciudades[ciudades == "Florencia"] <- "Florencia, Caqueta"
ciudades[ciudades == "Armenia"] <- "Armenia, Quindio"
ciudades[ciudades == "Pamplona"] <- "Pamplona, Norte de Santander"
ciudades[ciudades == "Madrid"] <- "Madrid, Cundinamarca"
ciudades[ciudades == "Socorro"] <- "Socorro, Colombia"

latlongit<-geocode(as.character(ciudades), output = "latlona")
geocodeQueryCheck() #see queries remaining
# back to original
ciudades[ciudades == "Florencia, Caqueta"] <- "Florencia"
ciudades[ciudades == "Armenia, Quindio"] <- "Armenia"
ciudades[ciudades == "Pamplona, Norte de Santander"] <- "Pamplona"
ciudades[ciudades == "Madrid, Cundinamarca"] <- "Madrid"
ciudades[ciudades == "Socorro, Colombia"] <- "Socorro"

# paste coord
ciudades<-cbind(ciudades,latlongit)
ciudades<-ciudades[,-4]
colnames(ciudades)<- c( "Cuidad",	"lon",	"lat")
grupos2<-left_join(grupos, ciudades)


#get polygon of Colombia
co<-getData("GADM",country="CO",level=1,download=TRUE)
col_depto <- fortify(co,region="NAME_1") # make compatible to ggplot2

mapbase<- ggplot(col_depto, aes(long,lat,group=group)) + geom_polygon(fill="grey60") + coord_equal() +
  geom_path(color="grey") 


# change Distrito Capital by Cundiamarca
for(i in 1:length(grupos$Codigo)){
  if(grupos$Departamento[i]=="Distrito Capital") {grupos$Departamento[i]<-"Cundinamarca"}
  # nombres_apellidos$generoscore2[i]<-xpathSApply(htmlParse(nombres_apellidos$generoscore[i]), "//p",xmlValue)
}

# grupos per ciudad 
map3<-mapbase + geom_point(aes(x = lon, y = lat, group = FALSE), size=1, 
                             data = grupos2,alpha=I(0.25),colour="red") + 
  stat_binhex(aes(x = lon, y = lat, group = FALSE),
              size = 1, binwidth = c(1,1), alpha = 1/3,data = grupos2) +
  scale_fill_continuous(low = "green", high = "red") + facet_wrap(~ Clasificacion) + theme_bw() 



#  investigadores per Depto
s1<-summarise(group_by(grupos, Departamento),sum(integrantes))
colnames(s1)<-c("Departamento", "investigadores")
# investigadores per ciudad
s2<-summarise(group_by(grupos, Cuidad),sum(integrantes))
invest<-left_join(s2, ciudades)
invest$loc=paste(invest$lat, invest$lon, sep=":")

# map total integrantes per Ciudad 
G2 <- gvisGeoChart(invest, "loc", "sum(integrantes)"  ,
                  options=list(displayMode="Markers", region="CO", resolution="provinces",
                               colorAxis="{colors:['blue','purple', 'red']}",
                               backgroundColor="lightblue", width=500, height=400), chartid="Integrantes_Grupo")

# Barchart integrantes per Ciudad 
B <- gvisBarChart(s1[,1:2], yvar="investigadores", xvar="Departamento",                  
                  options=list(width=400, height=600, legend='none'))

GB <- gvisMerge(G2,B,horizontal=TRUE) 
                 
plot(GB)
# save file
print(GB, "chart", file = "Colciencias/map_chart.html")







