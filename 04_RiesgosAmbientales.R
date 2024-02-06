#-------------LIBRERÍAS-----------------------------

library(sp)
library(sf)
library(raster)
library(ggplot2)

#-------------CONCEPTOS-----------------------------

#Riesgo:  Probabilidad de ocurrencia (o frecuencia) x Gravedad de las consecuencias.
#         El riesgo ambiental es la función de la probabilidad de ocurrencia de un 
#         suceso y de la cuantia del daño ambiental que puede provocar.

#Peligro:  Es la probabilidad de ocurrencia de un evento y su capacidad de generar daño.
#          Fuente o situación o acto con POTENCIAL de daño en términos de enfermedad 
#          o lesión a las personas, o una combinación de estos.

#Exposición: Es el número de personas y bienes que pueden ser afectados por el evento.

#Vulnerabilidad: Es el grado suceptibilidad de la sociedad y los bienes a sufrir el daño

#Capacidad de respuesta: Es el conjunto de recursos y acciones que se pueden movilizar 
#                        para prevenir, mitigar o recuperarse del impacto de un evento.
#                        para medir este parámetro se pueden usar diferentes 
#                        indicadores, como, el grado de preparación, la eficiencia de 
#                        la intervención, la satisfacción de las necesidades básicas, 
#                        la restauración de los servicios esenciales, la reducción de
#                        la vulnerabilidad y la mejora de la resiliencia

#-------------ECUACIONES-----------------------------

# (1)--> Probabilidad de ocurrencia = n° casos favorables / n° casos posibles
# (2)--> Riesgo = Peligro x Exposición x Vulnerabilidad / Capacidad de respuesta


#-------------ESTABLECER DIRECTORIO DE TRABAJO-----------------------------

setwd("D:/DOCS/ASIGNATURAS IMPARTIDAS/GESTIÓN DE LA INFORMACIÓN GEOGRÁFICA/Gestion-De-La-Informacion-Geografica")

#-------------DATOS-----------------------------
Parametro = c("Color aparente",	"Turbiedad", "pH",	"COT",	"Nitritos",	"Nitratos",	"Fluoruros",	"Calcio",	"Alcalinidad total",	"Cloruros",	"Aluminio 3",	"Dureza total",	"Hierro total",	"Magnesio",	"Manganeso",	"Molibdeno",	"Sulfatos",	"Zinc",	"Fosfatos",	"Cloro residual libre", "Coliformes T",	"E. Coli")
Unidad=c("UPC",	"UNT","Sin unidad",	"mg/L",	"mg/L",	"mg/L",	"mg/L",	"mg/L",	"mg/L de CaCO3",	"mg/L",	"mg/L",	"mg/L de CaCO3",	"mg/L",	"mg/L",	"mg/L", 	"mg/L",	"mg/L",	"mg/L", 	"mg/L","mg/L"	, "UFC",	"UFC")
ValorMax = c(15,	2, 9	,5,	0.1,	10,	1,	60,	200,	250,	0.2,	300,	0.3,	36,	0.1,	0.07,	250,	3,	0.5,	2, 0,	0)
ValorMin = c(NA, NA, 6.5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.3,NA,NA)
pesos = c(6,	15,	1.5,	3,	3,	1,	1,	1,	1,	1,	3,	1,	1.5,	1,	1,	1,	1,	1,	1,	15,	15,	25)

ValoresMinMax = data.frame(Parametro,Unidad,ValorMax,ValorMin)
PuntajeRiesgo = data.frame(Parametro,pesos)
DatosPorCiudad = read.csv("datosParaIRCA.csv", dec=".", sep=",", header=T)

#-------------DATOS CUMPLEN LA NORMA-----------------------------
n = 22 #Cantidad de parámetros evaluados

DatosPorCiudad[1,4:22] <= t(ValoresMinMax$ValorMax)

MatrizCumplimiento = matrix(NA, nrow=5, ncol=25)

for(i in 1:5){
  for(j in 1:n){
      if(!(j %in% c(3))){
        MatrizCumplimiento[i,j+3]= DatosPorCiudad[i,j+3]<= t(ValoresMinMax$ValorMax)[j]
        #print(c(i,j))
      }else{
        MatrizCumplimiento[i,j+3]= (DatosPorCiudad[i,j+3]<= t(ValoresMinMax$ValorMax)[j]) & (DatosPorCiudad[i,j+3]>= t(ValoresMinMax$ValorMin)[j])
        #print(c(i,j))
      }
  }
}

colnames(MatrizCumplimiento)=  colnames(DatosPorCiudad)

cMatrizCumplimiento = data.frame(MatrizCumplimiento)
cMatrizCumplimiento[,1:3] = DatosPorCiudad[,1:3]

#-------------MEDICIÓN DEL RIESGO-----------------------------

MatrizDeRiesgo = data.frame(DatosPorCiudad[,1:3],SumNA=NA, SumAnalizado=NA, Riesgo=NA)

for(i in 1:5){
  MatrizDeRiesgo[i,4] = sum(PuntajeRiesgo[which(cMatrizCumplimiento[i,4:25]==FALSE),2])
  MatrizDeRiesgo[i,5] = sum(PuntajeRiesgo$pesos)
}

MatrizDeRiesgo$Riesgo = (MatrizDeRiesgo$SumNA/MatrizDeRiesgo$SumAnalizado)*100

#-------------ESPACIALIZACIÓN DE DATOS-----------------------------

MRiesgoSP =SpatialPointsDataFrame(coords = MatrizDeRiesgo[,2:3], 
                       data= MatrizDeRiesgo, 
                       proj4string = CRS('+init=epsg:4326'))

Colombia = shapefile("Col/gadm41_COL_2.shp")


plot(Colombia,axes=TRUE, las=2)
plot(MRiesgoSP,cex=MatrizDeRiesgo$Riesgo/10, pch=20, col=MatrizDeRiesgo$Riesgo, add=T)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("bottomleft", legend = paste(MatrizDeRiesgo$Ciudad, ", " ,MatrizDeRiesgo$Riesgo, "%"), col = MatrizDeRiesgo$Riesgo, pch = 20, cex=1.5,  bty = "n")

#https://r-coder.com/save-plot-r/

MRiesgoSP_objet = st_as_sf(MRiesgoSP) #Crear ibjeto sf para exportal los puntos

st_write(MRiesgoSP_objet, "IRCA.shp", driver="ESRI Shapefile")
