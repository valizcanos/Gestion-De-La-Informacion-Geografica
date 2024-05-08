#Librerías
#================================================================================

library(raster)
library(sf)
library(sp)
library(ggplot2)
library(ggspatial)
library(grid)
library(gridExtra)

setwd("D:/DOCS/ASIGNATURAS IMPARTIDAS/GESTIÓN DE LA INFORMACIÓN GEOGRÁFICA/Gestion-De-La-Informacion-Geografica")


#Links de consulta
#================================================================================

# ggspatial: https://cran.r-project.org/web/packages/ggspatial/ggspatial.pdf
# theme de ggplot: https://ggplot2.tidyverse.org/reference/theme.html
# geom_sf de ggplot: https://ggplot2.tidyverse.org/reference/ggsf.html
# datos espaciales con ggplot: https://jsimkins2.github.io/geog473-673/spatial-plots-with-ggplot2.html
# visor mapview: https://www.paulamoraga.com/book-spatial/making-maps-with-r.html
# barra escala ggplot: https://ggplot2.tidyverse.org/reference/scale_viridis.html


#Datos
#================================================================================

USA2005 = shapefile("CrimenesUSA/TasasCrimen2005.shp")
#USA2005 = read_sf("CrimenesUSA/TasasCrimen2005.shp")
USA2010 = shapefile("CrimenesUSA/TasasCrimen2010.shp")
USA2015 = shapefile("CrimenesUSA/TasasCrimen2015.shp")
USA2020 = shapefile("CrimenesUSA/TasasCrimen2020.shp")

USA2005 = st_as_sf(USA2005)
USA2010 = st_as_sf(USA2010)
USA2015 = st_as_sf(USA2015)
USA2020 = st_as_sf('USA2020')

#Mapas sencillos
#================================================================================
ggplot(USA2005) +
  geom_sf(color = "blue", aes(fill = Murder)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"))+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 50), label_graticule = "NW",
           label_axes= list(top = "E", left = "N"), datum = sf::st_crs(4326))+
  scale_fill_viridis_c(option = "C")+ 
  annotation_north_arrow(pad_x = unit(0.15,"cm"),pad_y = unit(5,"cm"), 
                         width = unit(1,"cm"), height = unit(1,"cm"), 
                         style = north_arrow_nautical())+
  annotation_scale(location = "br")


ggplot(USA2005) +
  geom_sf(color = "blue", aes(fill = Assault)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"))+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 50), label_graticule = "NW",
           label_axes= list(top = "E", left = "N"), datum = sf::st_crs(4326))+
  scale_fill_viridis_c("Agresiones",option = "C")+ 
  annotation_north_arrow(location="tl", style = north_arrow_nautical())+
  annotation_scale(location = "br")
  
#Mapas multiples
#================================================================================

asesinatos = ggplot(USA2005) +
  geom_sf(color = "blue", aes(fill = Murder)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"))+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 50), label_graticule = "NW",
           label_axes= list(top = "E", left = "N"), datum = sf::st_crs(4326))+
  scale_fill_viridis_c("Asesinatos",option = "C")+ 
  annotation_north_arrow(location="tl", 
                         style = north_arrow_nautical())+
  annotation_scale(location = "br")

asaltos = ggplot(USA2005) +
  geom_sf(color = "blue", aes(fill = Assault)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"))+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 50), label_graticule = "NW",
           label_axes= list(top = "E", left = "N"), datum = sf::st_crs(4326))+
  scale_fill_viridis_c("Agresiones",option = "C")+ 
  annotation_north_arrow(location="tl", style = north_arrow_nautical())+
  annotation_scale(location = "br")

raponeo = ggplot(USA2005) +
  geom_sf(color = "blue", aes(fill = Rape)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"))+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 50), label_graticule = "NW",
           label_axes= list(top = "E", left = "N"), datum = sf::st_crs(4326))+
  scale_fill_viridis_c("Violaciones",option = "C")+ 
  annotation_north_arrow(location="tl", style = north_arrow_nautical())+
  annotation_scale(location = "br")

poblacion= ggplot(USA2005) +
  geom_sf(color = "blue", aes(fill = UrbanPop)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"))+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 50), label_graticule = "NW",
           label_axes= list(top = "E", left = "N"), datum = sf::st_crs(4326))+
  scale_fill_viridis_c("Población\nurbana",option = "C")+ 
  annotation_north_arrow(location="tl", style = north_arrow_nautical())+
  annotation_scale(location = "br")


grid.arrange(asesinatos,asaltos,raponeo,poblacion, ncol=2, nrow=2)

#Mapas con dos tipos de datos vectoriales
#================================================================================

Centroides2005 = sf::st_centroid(USA2005)

ggplot() + geom_sf(data = Centroides2005, aes(size = Assault))

ggplot() +
  geom_sf(data =USA2005 ,color = "blue", aes(fill = Murder)) +
  geom_sf(data = Centroides2005, aes(size = Assault))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "gray",linewidth = 1, linetype = "dashed"))+
  coord_sf(xlim = c(-130, -60), ylim = c(25, 50), label_graticule = "NW",
           label_axes= list(top = "E", left = "N"), datum = sf::st_crs(4326))+
  scale_fill_viridis_c(name="Asesinatos",option = "C")+ 
  annotation_north_arrow(location="tl", style = north_arrow_nautical())+
  annotation_scale(location = "br")

