library(sp)
library(sf)
library(raster)
library(ggplot2)
library(ggspatial)
library(grid)
library(gridExtra)

setwd("D:/DOCS/ASIGNATURAS IMPARTIDAS/GESTIÓN DE LA INFORMACIÓN GEOGRÁFICA/Gestion-De-La-Informacion-Geografica/")# Establecer ruta de trabajo

Datos = read.csv("csvImgs/UNIFICADO.csv")
  
head(Datos,5)

#Vamos a extraer los dias, meses y años de dataframe

Datos$DD <- format(as.Date(Datos$Fecha), "%d")
Datos$MM <- format(as.Date(Datos$Fecha), "%m")
Datos$YYYY <- format(as.Date(Datos$Fecha), "%Y")


#==============================================================
#Vamos hacer una prueba con los datos subidos

Prueba1 = Datos[Datos$YYYY=="2010" & Datos$MM=="07",]

Prueba2 = sp::SpatialPointsDataFrame(coords = Prueba1[,2:3], 
                                     data= data.frame(Prueba1$VALOR), 
                                     proj4string = CRS('+init=epsg:4326'))
Prueba3= st_as_sf(Prueba2)

ggplot(data=Prueba3) +
  geom_sf(aes(color=Prueba1.VALOR)) +
  scale_color_viridis_c(option = "C")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  guides(colour=guide_legend(title="VHI"))+
  annotation_north_arrow(pad_x = unit(0.15,"cm"),pad_y = unit(5,"cm"), 
                         width = unit(1,"cm"), height = unit(2,"cm"), 
                         style = north_arrow_nautical())+
  annotation_scale(location = "br")


#==============================================================
#Vamos a crear múltiples mapas empleando un bucle
#MAPAS TEMPORALES

Anios=  unique(Datos$YYYY) #Para iterar por años
Anios

Meses = unique(Datos$MM) #Para iterar por meses
Meses


for(i in 1:length(Anios)){
  for(j in 1:length(Meses)){
    
    Datos_filtrados = Datos[Datos$MM==Meses[j] & Datos$YYYY==Anios[i],]
    Datos_filtrados= sp::SpatialPointsDataFrame(coords = Datos_filtrados[,2:3], 
                                                data= data.frame(Datos_filtrados$VALOR), 
                                                proj4string = CRS('+init=epsg:4326'))
    Datos_filtrados = st_as_sf(Datos_filtrados)
    
    ggplot(Datos_filtrados) +
      geom_sf(aes(color = Datos_filtrados.VALOR)) +
      scale_fill_viridis_c(option = "C")+ 
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      guides(colour=guide_legend(title="VHI"))+
      annotation_north_arrow(pad_x = unit(0.15,"cm"),pad_y = unit(5,"cm"), 
                             width = unit(1,"cm"), height = unit(2,"cm"), 
                             style = north_arrow_nautical())+
      annotation_scale(location = "br")+ labs(title=paste("VHI de ", Anios[i], "-", Meses[j], sep=""))
    ggsave(paste("Graficos/","VHI-",Anios[i],"-",Meses[j] ,".png",sep=""))
  }
}


#==============================================================
#Visualización de datos

tail(Datos,5)

hist(Datos$VALOR, xlab="VHI", ylab="Frecuencia absoluta")
boxplot(VALOR~MM, data= Datos)
boxplot(VALOR~YYYY, data= Datos)
plot(as.Date(Datos$Fecha),Datos$VALOR, type="l")

#==============================================================
#Alteración de datos 

ValoresNuevos = matrix(NA, nrow = 3290738, ncol=1)

for(i in 1:3290738){
  if(Datos[i,7]=="01"){
    ValoresNuevos[i,1] = Datos[i,4]*0.1
  }else if(Datos[i,7]=="02"){
    ValoresNuevos[i,1] = Datos[i,4]*0.2
  }else if(Datos[i,7]=="03"){
    ValoresNuevos[i,1] = Datos[i,4]*0.35
  }else if(Datos[i,7]=="04"){
    ValoresNuevos[i,1] = Datos[i,4]*0.65
  }else if(Datos[i,7]=="05"){
    ValoresNuevos[i,1] = Datos[i,4]*0.60
  }else if(Datos[i,7]=="06"){
    ValoresNuevos[i,1] = Datos[i,4]*0.25
  }else if(Datos[i,7]=="07"){
    ValoresNuevos[i,1] = Datos[i,4]*0.15
  }else if(Datos[i,7]=="08"){
    ValoresNuevos[i,1] = Datos[i,4]*0.08
  }else if(Datos[i,7]=="09"){
    ValoresNuevos[i,1] = Datos[i,4]*0.21
  }else if(Datos[i,7]=="10"){
    ValoresNuevos[i,1] = Datos[i,4]*0.69
  }else if(Datos[i,7]=="11"){
    ValoresNuevos[i,1] = Datos[i,4]*0.75
  }else{
    ValoresNuevos[i,1] = Datos[i,4]*0.29
  }
}

Datos$VALOR2 = ValoresNuevos

colnames(Datos)[9]= c("Valor2")

hist(Datos$Valor2, xlab="VHI", ylab="Frecuencia absoluta")
boxplot(Valor2~MM, data= Datos)
boxplot(Valor2~YYYY, data= Datos)
plot(as.Date(Datos$Fecha),Datos$Valor2, type="l")

Datos$VALOR3 = rnorm(3290738,0.5,0.1)*Datos$Valor2
hist(Datos$VALOR3, xlab="VHI", ylab="Frecuencia absoluta")
boxplot(VALOR3~MM, data= Datos)
boxplot(VALOR3~YYYY, data= Datos)
plot(as.Date(Datos$Fecha),Datos$VALOR3, type="l")

#==============================================================
#Vamos hacer OTRA  prueba con los datos creados

PruebaA = Datos[Datos$YYYY=="2010" & Datos$MM=="07",]

PruebaB = sp::SpatialPointsDataFrame(coords = PruebaA[,2:3], 
                                     data= data.frame(PruebaA$VALOR3), 
                                     proj4string = CRS('+init=epsg:4326'))
PruebaC= st_as_sf(PruebaB)

ggplot(data=PruebaC) +
  geom_sf(aes(color=PruebaA.VALOR3)) +
  scale_color_viridis_c(option = "C")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  guides(colour=guide_legend(title="VHI"))+
  annotation_north_arrow(pad_x = unit(0.15,"cm"),pad_y = unit(5,"cm"), 
                         width = unit(1,"cm"), height = unit(2,"cm"), 
                         style = north_arrow_nautical())+
  annotation_scale(location = "br")

#==============================================================
#CREA MULTIPLES MAPAS CON LOS NUEVOS VALORES