

library(dplyr)
library(leaflet)
library(KernSmooth)
library(sp)
library(raster)
library(rgdal)
library(hablar)
library(leaflet.extras)
################ Mapas Intractivos con Leaflet:
#Cargamos las paqueterias Necesarias:


#Empezamos con las nociones basicas de Leaflet:
primermapa<- leaflet() %>% #uso de la libreria
  addTiles() %>% # API para el mapa, usa por defecto: OpenStreetMap
  addMarkers(lng=-5.664112384, lat=40.96500844,# Poner como parametros las coordenadas del lugar a graficar
             popup ="Plaza Mayor de Salamanca" ) 
primermapa


# Podemos ocupar diferentes clases de mapas:
m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()
m %>% addProviderTiles(providers$Stamen.Toner)
m %>% addProviderTiles(providers$CartoDB.Positron)
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)

# Podemos agrupar puntos de acuerdo a clusters entre los puntos y tambien cambiar
# los iconos usados:
data(quakes)

leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

leaflet(quakes) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

leaflet(data = quakes[1:20,]) %>% addTiles() %>% addCircleMarkers()

# Podemos cambiar colores y caracteristicas de los iconos:
# Some fake data
df <- sp::SpatialPointsDataFrame(
  cbind(
    (runif(20) - .5) * 10 - 90.620130,  # lng
    (runif(20) - .5) * 3.8 + 25.638077  # lat
  ),
  data.frame(type = factor(
    ifelse(runif(20) > 0.75, "pirate", "ship"),
    c("ship", "pirate")
  ))
)

pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))

leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(type == "ship", 6, 10),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.5
  )


# Podemos agregar etiquetas:
df <- read.csv(textConnection(
  "Name,Lat,Long
Samurai Noodle,47.597131,-122.327298
Kukai Ramen,47.6154,-122.327157
Tsukushinbo,47.59987,-122.326726"
))

leaflet(df) %>% addTiles() %>%
  addMarkers(~Long, ~Lat, popup = ~Name)

# Podemos agregar objetos espaciales a los mapas:
cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

#Circulos:
leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, popup = ~City
  )

#Rectangulos:
leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-118.456554, lat1=34.078039,
    lng2=-118.436383, lat2=34.062717,
    fillColor = "transparent"
  )
#722515
######################## Generamos un proyecto usando la DENUE y otros elementos:
patterns<-c("CAFETERIA","STARBUCKS","LA BORRA")
Datos<- read.csv("/Users/luisgerardoramosnajera/Casa de te/denue_inegi_22_.csv",header = TRUE,sep = ",") %>%
  mutate(Año=substr(fecha_alta, start = 0, stop = 4)) %>%
  mutate(Año=as.numeric(Año)) %>%
  dplyr::filter(cve_mun %in% as.numeric(14) & as.numeric(11)) %>%
  dplyr::filter(codigo_act %in% as.numeric(722515))%>%
  #nombre_act %in% as.character("Comercio al por menor de gasolina y diesel")) %>%
  #filter(A√±o %in% as_reliable_num(2019)) %>%
  #filter(per_ocu %in% as.character("0 a 5 personas"))	 %>%
  #mutate(cve_mun=as.character(cve_mun),
  #largoMun=nchar(cve_mun),
  #cve_mun=case_when(largoMun==1~paste("00",cve_mun,sep = ""),
  #largoMun==2~paste("0",cve_mun,sep = ""),
  #largoMun==3~paste("",cve_mun,sep = ""))) %>%
  #mutate(cve_ent=as.character(cve_ent),
#largoent=nchar(cve_ent),
#cve_ent=case_when(largoent==1~paste("0",cve_ent,sep = ""),
#largoent==2~paste("",cve_ent,sep = ""))) %>%
#select(-largoMun, -largoent) %>%
dplyr::filter(grepl("CAFETERIA|STARBUCKS|LA BORRA", nom_estab))

#| "STARBUCKS"|"BORRA"

########################################## Mapa de Calor
DatosCompletos <- Datos
DatosCompletos<-data.table::as.data.table(DatosCompletos)
kde <- bkde2D(DatosCompletos[ , list(longitud, latitud)],
              bandwidth=c(.0045, .0068))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)



###################################### Agregar otras modificaciones:
DatosCoordenadas<- data.frame(lat=Datos$latitud, lng=Datos$longitud)
DatosNombres= popup=Datos$nom_estab
DatosTamaño<- Datos$per_ocu
GirosMasComunes1<- Datos %>%
  group_by(nombre_act) %>%
  summarise(Numero=n()) %>%
  arrange(desc(Numero)) 
GirosMasComunes<-head(GirosMasComunes1$nombre_act,10)
GirosMasComunes2<-head(GirosMasComunes1$Numero,10)
GirosMasComunes3<-data.frame(Giros=as.character(paste(GirosMasComunes,":",formatC(GirosMasComunes2, big.mark=','),"Establecimientos")))

pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange','gold', 'darkviolet','lightpink','indianred4','wheat4'),
  domain =  GirosMasComunes
)

DatosGiro<- data.frame(Actividad=Datos$nombre_act)
DatosGiro$Color<-ifelse(DatosGiro$Actividad %in% GirosMasComunes,pal(DatosGiro$Actividad),"black")

############################## Datos CONAPO:
fw <- readOGR(dsn = "/Users/luisgerardoramosnajera/Análisis de Mercados con R/Mapa_de_grado_de_marginacion_urbana_por_AGEB_2005",
              layer = "IMU_2005",
              verbose = FALSE)

fw <- fw[fw@data[["CVE_ENT"]] %in% Datos$cve_ent, ]
fw <- fw[fw@data[["CVE_MUN_1"]] %in% Datos$cve_mun, ]
fw_latlon <- spTransform(fw, CRS("+proj=longlat +datum=WGS84"))
pal1 <- colorBin("PuBu", domain = as_reliable_num(fw_latlon@data[["IMU"]]))
pal2 <- colorBin("Purples", domain = as_reliable_num(fw_latlon@data[["POB_TOT_1"]]))



labels2 <- sprintf(
  "<strong>%s</strong><br/>%g personas",
  paste("AGEB:",fw_latlon@data[["AGEB_1"]]), round(as_reliable_num(fw_latlon@data[["POB_TOT_1"]]),0)
) %>% lapply(htmltools::HTML)


########################### Generamos el mapa completo:
DatosCoordenadas %>% 
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons(data=spgons, color = heat.colors(NLEV, NULL)[LEVS], group="Mapa de Calor") %>%
  addPolygons(data=fw_latlon, fillColor = ~pal1(as_reliable_num(IMU)),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              group="Indice de Marginalidad") %>%
  addPolygons(data=fw_latlon, fillColor = ~pal2(as_reliable_num(fw_latlon@data[["POB_TOT_1"]])),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels2,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group="Poblacion") %>%
  addCircleMarkers(popup = DatosNombres, 
                   clusterOptions = markerClusterOptions()
                   ,radius=~case_when(DatosTamaño=="0 a 5 personas"~4,
                                      DatosTamaño=="6 a 10 personas"~5,
                                      DatosTamaño=="11 a 30 personas"~6,
                                      DatosTamaño=="31 a 50 personas"~7,
                                      DatosTamaño=="51 a 100 personas"~8,
                                      DatosTamaño=="101 a 250 personas"~9),
                   color=~DatosGiro$Color,
                   fillColor=DatosGiro$Color,
                   fillOpacity = 1,
                   opacity = 1
                   ,group="Clusters") %>%
  addCircleMarkers(popup = DatosNombres
                   ,radius=~case_when(DatosTamaño=="0 a 5 personas"~4,
                                      DatosTamaño=="6 a 10 personas"~5,
                                      DatosTamaño=="11 a 30 personas"~6,
                                      DatosTamaño=="31 a 50 personas"~7,
                                      DatosTamaño=="51 a 100 personas"~8,
                                      DatosTamaño=="101 a 250 personas"~9),
                   color=~DatosGiro$Color,
                   fillColor=DatosGiro$Color,
                   fillOpacity = 1,
                   opacity = 1
                   ,group="Establecimientos") %>%
  addCircles(radius=500,group="Areas Comerciales (500 Mts)") %>%
  addMiniMap(
    tiles = "OpenStreetMap",
    toggleDisplay = TRUE)%>% 
  leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = TRUE)) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "red",
    completedColor = "red")%>%
  addLegend(pal = pal, values = ~ GirosMasComunes, opacity = 1, title="Principales Actividades Economicas (maximo 10):") %>%
  addLayersControl(overlayGroups = c("Clusters","Establecimientos","Poblacion","Indice de Marginalidad","Mapa de Calor",
                                     "Areas Comerciales (500 Mts)"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Poblacion","Indice de Marginalidad","Mapa de Calor","Clusters",
              "Areas Comerciales (500 Mts)")) %>%
  addFullscreenControl()%>%
  addDrawToolbar(
    targetGroup='datos',
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
  addStyleEditor()

