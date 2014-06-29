

library(stringr)
codtable<-read.csv(file="C:\\Users\\Diego\\Documents\\CodigoR\\blog\\Colciencias\\data\\grupos_cod.txt",header=FALSE)
colnames(codtable)<-c("codigo1")#, "Código",  "Nombre de Grupo",	"Líder",	"Avalado",	"Estado",	"Clasificado en")
a<-"http://scienti.colciencias.gov.co:8083/ciencia-war/busquedaAvanzadaGrupos.do?depInst=&nmeGrupo=&progNacionalSec=&integrantes=&codIdGrupo="
b<-"&productos=&nmeLider=&status=&genLider=&nmeInstitucion=&areaConocimiento=&progNacional=&ciuInst=&buscar=buscar&proyectos=&annoCreacion="
codtable$codigo1<-str_replace_all(string=codtable$codigo1, pattern=" ", repl="")
codtable$web1<-paste(a,codtable$codigo1,b,sep="")

#fulltable<-data.frame(t(rep(NA,20)))

fulllist<-list()
library(XML)
# URL of interest: 
# Very slow.... because loop... next time I will make a function!
for(i in 574:length(codtable$web1)){
   mps <- codtable$web1[i]
  # parse the document for R representation: 
  mps.doc <- htmlParse(mps) # info basica grupo en html
  r.page<-getHTMLLinks(codtable$web1[i]) # get all links for the group
  infodetalle <- r.page[7] # extrae link con detalle del grupo
  r.page2<-readHTMLTable(infodetalle) # make a list of all tables
  datbasic<-as.data.frame(t(r.page2[[1]])) # get 1st table: dato basico
  institucion<-as.data.frame(t(r.page2[[2]]))  # get 2nst table: instutucion
  integrantes<-length(as.data.frame(t(r.page2[[5]]))) # get number of persons. Add more tables if you want !!!!
  # get all the tables in mps.doc as data frames
  mps.tabs <- readHTMLTable(mps.doc) 
  table = mps.tabs[[5]]
  
  fulllist[[i]]<-cbind(table,datbasic[2,],institucion,integrantes) # feed the data frame
}


library (plyr)
library(RCurl)
library (reshape)
fulltable <- ldply (fulllist, data.frame) # convert the list to data frame
# some edits and cleaning
fulltable2<-fulltable[,-c(1,5)]

names(fulltable2)<-c("Codigo", "NombreGrupo",
                    "Lider", "Avalado", "Estado",  "Clasificado_en",
                    "Anio_mes_creado",
                    "Departamento_Ciudad",
                    "Lider",	"infocertificada",	"web",
                    "Email",
                    "Clasificacion",
                    "Área1",	"Programa1", "Programa2", 
                    "institucion","integrantes")
# split depto and city
dept_city<-colsplit(fulltable2$Departamento_Ciudad, split = "\\ - ", names = c('Departamento', 'Ciudad'))
# delete Departamento_Ciudad
fulltable2<-fulltable2[,-8]
# paste dept_city
fulltable2<-cbind(fulltable2,dept_city)
# split anio and mes
Anio_mes<-colsplit(fulltable2$Anio_mes_creado, split = "\\ - ", names = c('Anio', 'Mes'))
# delete anio-mes
fulltable2<-fulltable2[,-7]
fulltable2<-cbind(fulltable2,Anio_mes)


##############################
### Gender
##########################

# write.csv(fulltable2, file="C:\\Users\\Diego\\Documents\\CodigoR\\blog\\Colciencias\\data\\fulltable2.csv")
# fulltable2<-read.csv(file="C:\\Users\\Diego\\Documents\\CodigoR\\blog\\Colciencias\\data\\fulltable2.csv",header=TRUE)


nombres_apellidos<-colsplit(fulltable2$Lider, split = "\\ ", names = c('Nombre1', 'Nombre2'))

# chk consistancy and fill blanks <- NA
for(i in 1:length(nombres_apellidos$Nombre1)){
  if(nombres_apellidos$Nombre2[i]=="") {nombres_apellidos$Nombre2[i]<-"NA"}
  # nombres_apellidos$generoscore2[i]<-xpathSApply(htmlParse(nombres_apellidos$generoscore[i]), "//p",xmlValue)
}


# make url
nombres_apellidos$generoscore<-as.character(paste("http://api.onomatic.com/onomastics/api/gendre", nombres_apellidos$Nombre1, nombres_apellidos$Nombre2, sep="/"))

# get the scores. SLOW !!!!!!!!!!!!!!
for(i in 1:length(nombres_apellidos$Nombre1)){
  # if(nombres_apellidos$Nombre2[i]=="") {nombres_apellidos$Nombre2[i]<-"NA"}
  nombres_apellidos$generoscore2[i]<-xpathSApply(htmlParse(nombres_apellidos$generoscore[i]), "//p",xmlValue)
}

# put  gender
for(i in 1:length(nombres_apellidos$Nombre1)){
   if(nombres_apellidos$generoscore2[i] <=0) {nombres_apellidos$genero[i]<-"Hombre"} else if (nombres_apellidos$generoscore2[i] >0) {nombres_apellidos$genero[i]<-"Mujer"}
  # nombres_apellidos$generoscore2[i]<-xpathSApply(htmlParse(nombres_apellidos$generoscore[i]), "//p",xmlValue)
}
##### paste genero in fulltable2
Genero<-nombres_apellidos$genero
fulltable2<-cbind(fulltable2,Genero)
# some cleaning 
fulltable2<-fulltable2[,-c(25,24,23,22,8,1)]



# chk consistancy and fill blanks <- NA
for(i in 1:length(nombres_apellidos$Nombre1)){
  if(fulltable2$Área1[i]=="") {fulltable2$Área1[i]<-"NA"}
  # nombres_apellidos$generoscore2[i]<-xpathSApply(htmlParse(nombres_apellidos$generoscore[i]), "//p",xmlValue)
}


#### save
write.csv(fulltable2, file="C:\\Users\\Diego\\Documents\\CodigoR\\blog\\Colciencias\\data\\fulltable3.csv")






