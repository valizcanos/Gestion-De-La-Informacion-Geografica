library(sp)
library(sf)
library(raster)
library(ggplot2)
library(ggspatial)
library(grid)
library(gridExtra)

setwd("D:/DOCS/ASIGNATURAS IMPARTIDAS/GESTIÓN DE LA INFORMACIÓN GEOGRÁFICA/Gestion-De-La-Informacion-Geografica/examen")

riesgoSalud = read.csv("riesgos_salud_.csv")
valleDelCauca = shapefile("VCauca.shp")

Riesgos = sp::SpatialPointsDataFrame(coords = riesgoSalud[,3:4], 
                                     data= data.frame(riesgoSalud$Impacto), 
                                     proj4string = CRS('+init=epsg:4326'))

valleDelCauca= st_as_sf(valleDelCauca)
Riesgos= st_as_sf(Riesgos)

colnames(valleDelCauca)[2] = "Municipios"
colnames(Riesgos)[1] = "Impacto"

ggplot() +
  geom_sf(data=valleDelCauca, aes(fill=Municipios)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #guides(colour=guide_legend(title="Dpto."))+
  annotation_north_arrow(pad_x = unit(0.15,"cm"),pad_y = unit(5,"cm"), 
                         width = unit(1,"cm"), height = unit(2,"cm"), 
                         style = north_arrow_nautical())+
  annotation_scale(location = "br")+
  geom_sf(data=Riesgos,aes(color=Impacto))+scale_color_viridis_c(option = "C")
ggsave("mapa.png", width = 25, height = 16, units = "cm")
