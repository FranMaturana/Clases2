###Class 03 - Data Management & Visualization###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl)
library(data.table)

rm(list=ls())

casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)

names(casos) #Me da el nombre de las columnas en la base de datos
casos<-casos[Región=="Metropolitana",]

saveRDS(casos,"Class_03/casosRM.rds")

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8')

writexl::write_xlsx(casos,path = "Class_03/CasosenExcel.xlsx")

library(foreign)

write.dta



casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

casosRM[,table(Sexo)]
casosRM[Sexo=="Fememino",Sexo:="Femenino"] #Nose que hace esto

casosRM[`Centro de salud`=="Clínica Alemana",`Centro de salud`:="Clinica Alemana"] #Tampoco se que hace esto
casosRM[,.N,by=.(`Centro de salud`)] #Da el numero de casos de la Región Metropolitana por centro de salud

# Creating (factor) variables

class(casosRM$Sexo) #Que clase es la columna sexo

casosRM[,Sexo:=factor(Sexo)]

head(casosRM$Sexo) #Un resumen de los primeros 6 datos de la columna sexo
head(as.numeric(casosRM$Sexo)) #1 da femenino, 2 masculino, igual los primeros 6

table(casosRM$Sexo) #Me da una tabla con los valores totales de mujeres y hombres en los datos
casosRM[,.N,by=.(Sexo)] #Una mini tabla con el numero de casos, ordenados por sexo.
casosRM[,.N,by=.(Sexo,`Centro de salud`)] #Numero de casos por centro de salud y sexo.

#Collapsing by Centro de Salud 

names(casosRM)
obj1<-casosRM[,.N,by=.(`Centro de salud`)] #Se creo una mini data solo con los numeros de casos por centro de salud


obj1[,sum(N,na.rm = T)] #El total de casos en la RM, eliminando los que no tienen datos.

obj1[,porc:=N/sum(N,na.rm = T)]

# collapsing (colapsar) by average age


A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)] #Se creo una mini data con el promedio de la edad de los contagiados por centro de salud, la columna se llama Avdata.

B<-casosRM[,.(Total_centro=.N),by=.(`Centro de salud`)] #Crea una mini data con el numero de casos por centro de salud

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)] #Crea una mini data con el total de contagiadas mujeres por centro de salud

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)] #Lo mismo de arriba pero con hombres

dim(A) #Me arroja primero la cantidad de datos y luego la cantidad de variables
dim(B)
dim(C)
dim(D)


#merging data sets
#Mezcla las otras mini datas filtradas que habíamos hecho arriba


AB<-merge(A,B,by = "Centro de salud",all = T,sort = F) #Junto las datas de A y B, por centro de salud


ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F)
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F)

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro] #Agregó una columna con el porcentaje de mujeres contagiadas por centro con respecto a los contagiados totales


# reshaping (Reorganización)

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=.N),by=.(`Centro de salud`,Sexo)] #Me da los casos con el promedio de edad de los contagiados y separados por sexo

G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud') #Me separo las variables anteriores de AvAge y Casos confirmados por sexo y lo escribio en la columna arriba

#---- Part 2: Visualization  -------------------

#Scatter plot
  #Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`) #Me imprime el G, sacado anterior
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5) #Le agrega el nombre del centro de salud a los contagiados del gráfico

#ggplot2
p1<-ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T)
p1

#plotly
library(plotly)
ggplotly(p1)

# other useful ways to show data

#high charter
# http://jkunst.com/highcharter/index.html


#---- Part 3: Intro to Mapping  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F) #Me creó un data frame con los datos del censo, sale la población por rangos de edad.

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]
#Se ordenan los adultos mayores por zona geográfica
zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",]

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F) #Le agregó el nombre de la comuna a zonas valparaiso

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),] #Solo me arroja los datos de las comunas con esos dos códigos. en este caso son Valparaiso y Viña del Mar

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F)
#Me da cuantos adultos mayores hay por comuna en la zonas de valparaiso, ordenado por geocódigo

#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds"))


ggplot(zonas_valparaiso) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 11)
#Me muestra el gráfico de las zonas de Valparaiso y Viña del Mar, por densidad de población adulto mayor, mientras más oscuro es más mayor la población. (Con la paleta de colores que descargó recién)

# creating a fake spatial distribution of adult population in space

zonas_valparaiso2<-cbind(zonas_valparaiso[,c("geocodigo","codigo_comuna","codigo_provincia","codigo_region","geometry")],"AdultosMayores"=sample(zonas_valparaiso$AdultosMayores,size = length(zonas_valparaiso$AdultosMayores)))


ggplot(zonas_valparaiso2) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 13)

#comparing histograms of the same variable

hist(zonas_valparaiso$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo") #Hace histogramas, gráficos de barra.

hist(zonas_valparaiso2$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")
