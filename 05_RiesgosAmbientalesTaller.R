#-------------LIBRERÍAS-----------------------------

library(sp)
library(sf)
library(raster)
library(ggplot2)

#-------------ESTABLECER DIRECTORIO DE TRABAJO-----------------------------

setwd("D:/DOCS/ASIGNATURAS IMPARTIDAS/GESTIÓN DE LA INFORMACIÓN GEOGRÁFICA/Gestion-De-La-Informacion-Geografica")

#-------------DATOS-----------------------------

DatosRio = read.csv("parametrosRioCauca.csv",  header = T)

Parametros = c("DBO5", "OD", "Coliformes F", "Coliformes T", "pH", "DT", "SD", "SS",
               "Cl", "Conductividad", "AT", "Grasas y Aceites", "N-NO3", "N-NH3",
               "PO4", "SAAM", "Color", "Turbiedad")
Unidad = c("mg/L", "mg/L", "NMP/100mL", "NMP/100mL", "SU", "mg/L CaCO3", "mg/L",
           "mg/L", "mg/L", "umhos/cm", "mg/L CaCO3", "mg/L", "mg/L", "mg/L",
           "mg/L", "mg/L", "UPC", "UNT")

Clasificacion = c("M. Orgánica","M. Orgánica","Bacteriológico", "Bacteriológico",
                  "M. Iónico","M. Iónico","M. Suspendido","M. Suspendido",
                  "M. Iónico","M. Iónico","M. Iónico","M. Suspendido",
                  "Nutrientes", "Nutrientes", "Nutrientes",
                  "Nutrientes", "M. Suspendido",
                  "M. Suspendido")
Pesos = c(5,5,4,3,1,1,0.5,1,0.5,2,1,2,2,2,2,3,1,0.5)

Wi = data.frame(Parametros,Unidad,Clasificacion,Pesos)

#-------------Indices Cálculos-----------------------------

EcuacionIndex_pH = function(N_observaciones, loc_pH, datos){
  pH_indice = matrix(NA, nrow = N_observaciones, ncol = 1)
  for(i in 1:N_observaciones){
    if(datos[i,loc_pH]<6.7){
      pH_indice[i,1]= 10^(0.2335*datos[i,loc_pH]+0.44)
    }else if(datos[i,loc_pH]>7.3){
      pH_indice[i,1]= 10^(4.22-0.293*datos[i,loc_pH])
    }else{
      pH_indice[i,1] = 100
    }
  }
  return(pH_indice)
}

DatosRio$pHIndex = EcuacionIndex_pH(7, 9, DatosRio) #Creamos una columna con los indices para pH

DatosRio$ColorIndex = 123*DatosRio$Color^(-0.295)
