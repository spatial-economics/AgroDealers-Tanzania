library(sf)
library(tidygraph)
library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(units)
library(tmap)
library(osmdata)
library(rgrass7)
library(link2GI)
library(nabor)
library(raster)
library(rgdal)
library(RQGIS3)


source('D:/Jordan/AgroDealearsTZ/code/AgroDealearsTZ_functions.R')

# data
agroids.with.district <- read.csv("../output/csv/agro_clstr_disthq.csv")
# tza.osm.roads_50m <- shapefile("../qgis/hotosm_tza_roads_lines_50m_TPST/hotosm_tza_roads_lines_50m_TPST.shp")

wgs.prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
tza.regions <- getData("GADM", country="TZA", level = 1, path = "../data/shp")
tza.districts <- getData("GADM", country="TZA", level = 2, path = "../data/shp")

# Agrodealers shapefile
all.agrodealers.shp <-
  SpatialPointsDataFrame(as.data.frame(agroids.with.district[, c("longitude", "latitude")]), 
                         agroids.with.district,
                         proj4string = wgs.prj)
all.districtHQs.shp <-
  SpatialPointsDataFrame(as.data.frame(agroids.with.district[, c("HQ_longitude", "HQ_latitude")]), 
                         agroids.with.district,
                         proj4string = wgs.prj)
# Merge "Mafinga Township Authority" into "Mufindi" and remove "Mkalama"
tza.study.districts_ <- intersect(tza.districts, all.agrodealers.shp)
mufindi.index <- match("Mufindi", tza.study.districts_$NAME_2 )
mkalama.index <- match("Mkalama", tza.study.districts_$NAME_2 )
aggregateList <- list(unlist(lapply(tza.study.districts_$NAME_2[-mkalama.index],
                                    function(x)
                                      ifelse(x %in% c("Mafinga Township Authority", 
                                                      "Mufindi" ), 1, x) )))
tza.study.districts <- aggregate(tza.study.districts_[-mkalama.index,], 
                                 by = aggregateList, 
                                 FUN=function(x) ifelse(length(x) == 2, 
                                                        x[mufindi.index], x[1])
)


tza.primarySecondary.road <- 
  tza.osm.roads_50m[which(tza.osm.roads_50m$highway %in% c("trunk", "primary", 
                                                     "secondary", "tertiary") ),]
tza.primary.road <- 
  tza.osm.roads_50m[which(tza.osm.roads_50m$highway %in% c("trunk", "primary") ),]
tza.secondary.road <- 
  tza.osm.roads_50m[which(tza.osm.roads_50m$highway %in% c("secondary", "tertiary") ),]



roads.all <- tza.primarySecondary.road

for (distrct.id in 1:dim(tza.study.districts)[1]) {
  print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
  print(tza.study.districts@data[distrct.id,"NAME_2"])
  print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
  # Get district roads
  district.bbox.poly <- as(extent(tza.study.districts[distrct.id,]@bbox), "SpatialPolygons")
  proj4string(district.bbox.poly) <- wgs.prj
  distrct.primary.road <- raster::intersect(roads.all, district.bbox.poly) 
  
  # Get district agrodealers
 
  distrct.agrodealers <- raster::intersect(all.agrodealers.shp,
                                           tza.study.districts[1,])
  print(as.character(distrct.agrodealers$Region))
  print(as.character(distrct.agrodealers$District))
  print(as.character(distrct.agrodealers$District.HQ))
  
  

  # Give each edge a unique index -------------------------------------------
  
  # Segment the roads
  roads.all.sf <- sf::st_as_sf(roads.all) 
  # roads.segments <-
  #   segmentSpatialLines(distrct.primary.road.sf, max.road.segment = 50)
  # st_coordinates(tza.primary.road.sf)
  # sf::st_as_sf(roads.segments )
  # names(roads.segments)
  
  edges.nodes.graph <-
    sf_to_tidygraph(roads.all.sf, directed = FALSE)
  
  edges <- edges.nodes.graph[["edges"]]
  nodes <- edges.nodes.graph[["nodes"]]
  graph_ <- edges.nodes.graph[["graph"]]
  
  # With the activate() verb, we specify if we want to manipulate the edges
  graph <- graph_  %>%
    activate(edges) %>%
    mutate(length = st_length(geometry))
  
  distances.graph <- graph_to_distance(graph,)
  distances <- distances.graph[[1]]
  graph <- distances.graph[[2]]
  
  roads.all.distances <-
    distance.btw.A.and.B(distrct.agrodealers@data[, c("longitude", "latitude")],
                         distrct.agrodealers@data[, c("HQ_longitude", "HQ_latitude")],
                         distances,
                         graph,
                         edges,
                         nodes,
    )
  
  if (distrct.id == 1) {
    euc_dist <- cbind(distrct.agrodealers@data[, c("agroid", "Region", "District", "cluster.id", "clstr.size" )],
                roads.all.distances[[1]])
    net_dist <- cbind(distrct.agrodealers@data[, c("agroid", "Region", "District", "cluster.id", "clstr.size" )],
                roads.all.distances[[2]])
  }
  
  if (!distrct.id == 1) {
    euc_dist <- rbind(euc_dist , cbind(distrct.agrodealers@data[, c("agroid", "Region", "District", "cluster.id", "clstr.size" )],
                                       roads.all.distances[[1]]))
    net_dist <- rbind(net_dist, cbind(distrct.agrodealers@data[, c("agroid", "Region", "District", "cluster.id", "clstr.size" )],
                      roads.all.distances[[2]]))
  }
  
  
  
  
  # if (distrct.id == 3) break()
 } 
  
  
  
euc_dist 
net_dist 
data.frame(euc_dist[,4:6], 
           net_dist[,6]  )
  
all.agrodealers.shp@data$clstr.size
  






# ggplot() +
#   geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) +
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), size = 0.5)


# ggplot() +
#   geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
#   geom_sf(data = agrolocation, size = 2, col = 'firebrick') +
#   geom_sf(data = distrctHQlocation, size = 2, col = 'firebrick') +
#   geom_sf_label(data = agrolocation, aes(label = 's'), nudge_x = 0.004) +
#   geom_sf_label(data = distrctHQlocation, aes(label = 'c'), nudge_x = 0.005)


# tmap_mode('view')
# tm_shape(graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) +
#   tm_lines() +
#   tm_shape(graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()) +
#   tm_dots() +
#   tmap_options(basemaps = 'OpenStreetMap')

# ggplot() +
#   geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
#   scale_colour_viridis_c(option = 'inferno') +
#   scale_size_continuous(range = c(0,4))
# 
# ggplot() +
#   geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
#   scale_colour_viridis_c(option = 'inferno') +
#   scale_size_continuous(range = c(0,4))

# ggplot() +
#   geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
#   geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
#   geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
#   geom_sf(data = muenster_station, size = 2) +
#   geom_sf(data = muenster_cathedral, size = 2)  +
#   geom_sf_label(data = muenster_station, aes(label = 'station'), nudge_x = 0.004) +
#   geom_sf_label(data = muenster_cathedral, aes(label = 'cathedral'), nudge_x = 0.005)

