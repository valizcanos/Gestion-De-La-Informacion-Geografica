library(raster)
library(dplyr)

print(getwd()) #Mostrar directorio de trabajo

setwd("D:/DOCS/ASIGNATURAS IMPARTIDAS/GESTIÓN DE LA INFORMACIÓN GEOGRÁFICA/Gestion-De-La-Informacion-Geografica/")

DirTrababajo = getwd()

PathImgCol = paste(DirTrababajo,"imgs", sep="/")

ListaImagenes = list.files(PathImgCol, pattern = "\\.tif$", full.names=TRUE)

PathFileCol = paste(DirTrababajo,"csvImgs", sep="/")

#Definir rango de fechas de las imagenes

Fechas = seq.Date(as.Date("2001-01-01"),as.Date("2021-12-31"),by = "month")

# Subir mascara

Mask = rgdal::readOGR("Sph.AltoPatia/AltoPatia.shp")
plot(Mask)

#Función de conversión de datos a texto

Raster2DF = function(PathiCDCol){
  for(i in 1:length(PathiCDCol)){
    ArchivoRaster = raster(PathiCDCol[i])
    ArchivoRaster = raster::crop(ArchivoRaster,Mask)
    df = as.data.frame(ArchivoRaster, xy=TRUE)
    df = na.omit(df) 
    write.csv(df, paste(PathFileCol, '/' ,Fechas[i],'.csv',sep=''), row.names = FALSE)
  }
}

Raster2DF(ListaImagenes) #Exportar datos de imagenes a .csv

#Unificar lista de archivos

DF_CD = list()

JoinDF2List = function(PathListaCSV){
  for(i in 1:length(PathListaCSV)){
    Archivos = read.csv(PathListaCSV[i])
    colnames(Archivos)[3]= 'VALOR'
    Archivos = cbind(Archivos,rep(Fechas[i],nrow(Archivos)))
    colnames(Archivos)[4]= 'Fecha'
    DF_CD[[i]] = list(Archivos)
  }
  return(DF_CD) 
}

ListaArchivos = list.files(PathFileCol, pattern = "\\.csv$", full.names=TRUE)

ListaFiles = JoinDF2List(ListaArchivos)

ArchivosUnificados = bind_rows(ListaFiles)

write.csv(ArchivosUnificados,paste(PathFileCol,"/UNIFICADO.csv",sep=""))
