# load CB.lat, CB.lng
source("loadDataMap.R")

## sunprairie map
m.base <- leaflet() %>% 
  addTiles(options=tileOptions(minZoom=11)) %>%
  setMaxBounds(lng1=-92.88793,lat1=42.49192,
               lng2=-86.80587,lat2=47.05468) %>%
  addMarkers(lat=43.183603,lng=-89.232336,
             popup="Sun Prairie Town",
             group="Landmarks") %>%
  addCircleMarkers(lat=43.195710,lng=-89.227400,
                   label="CH Bird Elementary",
                   labelOptions=labelOptions(permanent=F),
                   radius=10,fillOpacity=0.5,
                   color="black",fillColor="red",
                   group="CH Bird") %>%
  addCircleMarkers(lat=43.181510,lng=-89.235820,
                   label="Westside Elementary",
                   labelOptions=labelOptions(permanent=F),
                   radius=10,fillOpacity=0.5,
                   color="black",fillColor="purple",
                   group="Westside") %>%
  addCircleMarkers(lat=43.193890,lng=-89.215970,
                   label="Northside Elementary",
                   labelOptions=labelOptions(permanent=F),
                   radius=10,fillOpacity=0.5,
                   color="black",fillColor="green",
                   group="Northside") %>%
  addCircleMarkers(lat=43.198850,lng=-89.194370,
                   label="Patrick Marsh Middle",
                   labelOptions=labelOptions(permanent=F),
                   radius=10,fillOpacity=0.5,
                   color="black",fillColor="blue",
                   group="Patrick Marsh") %>%
  addPolygons(lat=round(CB.lat[1:10],3),lng=round(CB.lng[1:10],3),
              color="purple",group="Westside",fill=T,
              weight=3,opacity=0.3,
              highlightOptions=highlightOptions(color="gold",
                                                fillColor="gold",
                                                weight=2,opacity=0.5,
                                                sendToBack=T)) %>%
  addPolygons(lat=round(CB.lat[11:25],3),lng=round(CB.lng[11:25],3),
              color="green",group="Northside",fill=T,
              weight=3,opacity=0.3,
              highlightOptions=highlightOptions(color="gold",
                                                fillColor="gold",
                                                weight=2,opacity=0.5,
                                                sendToBack=T)) %>%
  addPolygons(lat=round(CB.lat[26:30],3),lng=round(CB.lng[26:30],3),
              color="red",group="CH Bird",fill=T,
              weight=3,opacity=0.3,
              highlightOptions=highlightOptions(color="gold",
                                                fillColor="gold",
                                                weight=2,opacity=0.5,
                                                sendToBack=T)) %>% 
  addPolygons(lat=round(CB.lat[11:45],3),lng=round(CB.lng[11:45],3),
              color="blue",group="Patrick Marsh",fill=T,
              weight=3,opacity=0.3,
              labelOptions=labelOptions(permanent=T),
              highlightOptions=highlightOptions(color="gold",
                                                fillColor="gold",
                                                weight=2,opacity=0.5,
                                                sendToBack=T)) %>% 
  addLayersControl(
    overlayGroups=c("Landmarks","Westside","Northside",
                    "CH Bird","Patrick Marsh"),
    options=layersControlOptions(collapsed=F))

m.ws <- m.base %>% 
  hideGroup(c("CH Bird","Northside","Patrick Marsh"))
m.ns <- m.base %>% 
  hideGroup(c("CH Bird","Westside","Patrick Marsh"))
m.ch <- m.base %>% 
  hideGroup(c("Westside","Northside","Patrick Marsh"))
m.pm <- m.base %>% 
  hideGroup(c("CH Bird","Westside","Northside"))
m <- m.base %>%
  hideGroup(c("CH Bird","Westside","Northside","Patrick Marsh"))

## remove unneeded variables
rm(CB.lat); rm(CB.lng); rm(m.base)