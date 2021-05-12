# load CB.lat, CB.lng
source("loadDataMap.R")

## highlight options (for census block group polygons)
hl.options <- highlightOptions(color="gold",
                               fillColor="gold",
                               weight=2,opacity=0.5,
                               sendToBack=T)

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
  addPolygons(lat=round(CB.lat[1:4],3),lng=round(CB.lng[1:4],3),
              color="purple",group="Westside",
              opacity=0.3,label="011506-2",
              highlightOptions=hl.options) %>%
  addPolygons(lat=round(CB.lat[6:9],3),lng=round(CB.lng[6:9],3),
              color="purple",group="Westside",
              opacity=0.3,label="011505-1",
              highlightOptions=hl.options) %>%
  addPolygons(lat=round(CB.lat[11:14],3),lng=round(CB.lng[11:14],3),
              color="green",group="Northside",
              opacity=0.3,label="011505-1",
              highlightOptions=hl.options) %>%
  addPolygons(lat=round(CB.lat[16:19],3),lng=round(CB.lng[16:19],3),
              color="green",group="Northside",
              opacity=0.3,label="011505-3",
              highlightOptions=hl.options) %>%
  addPolygons(lat=round(CB.lat[21:24],3),lng=round(CB.lng[21:24],3),
              color="green",group="Northside",
              opacity=0.3,label="011506-3",
              highlightOptions=hl.options) %>%
  addPolygons(lat=round(CB.lat[26:29],3),lng=round(CB.lng[26:29],3),
              color="red",group="CH Bird",
              opacity=0.3,label="011504-1",
              highlightOptions=hl.options) %>% 
  addPolygons(lat=round(CB.lat[31:34],3),lng=round(CB.lng[31:34],3),
              color="blue",group="Patrick Marsh",
              opacity=0.3,label="011600-2",
              highlightOptions=hl.options) %>% 
  addPolygons(lat=round(CB.lat[36:39],3),lng=round(CB.lng[36:39],3),
              color="blue",group="Patrick Marsh",
              opacity=0.3,label="011700-2",
              highlightOptions=hl.options) %>%
  addPolygons(lat=round(CB.lat[41:44],3),lng=round(CB.lng[41:44],3),
              color="blue",group="Patrick Marsh",
              opacity=0.3,label="011506-1",
              highlightOptions=hl.options) %>%
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