# Map Agrodealers per district
library(sf)
library(raster)
library(ggplot2)
library(ggsn)


library(rgdal)
library(geosphere)
library(dismo)
library(rgeos)

tza.osm.roads_ <- shapefile("D:/Jordan/AgroDealearsTZ/data/Tanzania/Roads/OpenStreetMap/hotosm_tza_roads_lines.shp")
wgs.prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
map.agrodealers.csv <- read.csv("D:/Jordan/AgroDealearsTZ/output/tzaagrodealersoutput.csv")
map.agrodealers.shp <- SpatialPointsDataFrame(data.frame( 
                                                 map.agrodealers.csv$longitude, 
                                                 map.agrodealers.csv$latitude),
                                              data = map.agrodealers.csv,
                                              proj4string = wgs.prj
  )

dir.create( path =  "data/shp", showWarnings = FALSE, recursive = TRUE)
tza.international <- getData("GADM", country="TZA", level = 0, path = "data/shp")
tza.regions <- getData("GADM", country="TZA", level = 1, path = "data/shp")
tza.districts <- getData("GADM", country="TZA", level = 2, path = "data/shp")
tza.wards <- getData("GADM", country="TZA", level = 3, path = "data/shp")

srvy.districts <- intersect(tza.districts, map.agrodealers.shp)
srvy.wards <- intersect(tza.wards, map.agrodealers.shp)

dim(srvy.districts)
dim(srvy.wards)


rd.trunk <- tza.osm.roads_[which(tza.osm.roads_$highway %in% c("trunk")),]
rd.primary <- tza.osm.roads_[which(tza.osm.roads_$highway %in% c("primary")),]
rd.secondary <- tza.osm.roads_[which(tza.osm.roads_$highway %in% c("secondary")),]
rd.tertiary <- tza.osm.roads_[which(tza.osm.roads_$highway %in% c("tertiary")),]
district.names <- data.frame(District=tza.districts$NAME_2, 
                            Longitude=coordinates(tza.districts)[,1],
                            Latitude=coordinates(tza.districts)[,2])


# FUNCTION TO CREATE CLUSTERS BASED ON DISTANCE ---------------------------

radiusDist <- function(distance.threshold, color_){
  # # define the distance threshold, in this case 40 m
  # distance.threshold <- 100
  
  # use the distm function to generate a geodesic distance matrix in meters
  dist.matrix <- distm(map.agrodealers.shp)
  # Cluster all points using a hierarchical clustering approach
  horizontal.clust <- hclust(as.dist(dist.matrix), method="complete")
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  map.agrodealers.shp$clust <- cutree(horizontal.clust, h=distance.threshold)
  # expand the extent of plotting frame
  map.agrodealers.shp@bbox[] <- as.matrix(extend(extent(map.agrodealers.shp),0.001))
  # get the centroid coords for each cluster
  cent <- matrix(ncol=2, nrow=max(map.agrodealers.shp$clust))
  for (i in 1:max(map.agrodealers.shp$clust)){
    # gCentroid from the rgeos package
    cent[i,] <- gCentroid(subset(map.agrodealers.shp, clust == i))@coords
  }
  # compute circles around the centroid coords using "distance.threshold" radius
  # from the dismo package
  ci <- circles(cent, d=distance.threshold, lonlat=T)
  write.csv(ci@presence, "output/ClusterCoords.csv")
  geom_polygon(data = ci@polygons, mapping = aes(long,lat,group=group), 
               colour=color_, fill=NA, size = 0.4)
}


# MAP AGRODEALERS PER DISTRICT; GGPLOT2 -----------------------------------

map.all <- ggplot() +
  geom_polygon(data = tza.wards, mapping = aes(long,lat,group=group), 
               colour=" light grey", fill=NA,  size = 0.4, linetype = "dashed") +
  geom_polygon(data = tza.districts, mapping = aes(long,lat,group=group), 
               colour=" dark grey", fill=NA, size = 1, linetype = "dashed") +
  geom_polygon(data = tza.international, mapping = aes(long,lat,group=group), 
               colour=" black", fill=NA, size = 1.2) +
  geom_path(data = rd.tertiary, aes(long,lat,group=group), colour="brown1", size = 0.1)+
  geom_path(data = rd.secondary, aes(long,lat,group=group), colour="brown2", size = 0.2) +
  geom_path(data = rd.primary, aes(long,lat,group=group), colour="brown3", size = 0.3) +
  geom_path(data = rd.trunk, aes(long,lat,group=group), colour="brown4", size = 0.4) +
  geom_point(data = map.agrodealers.shp@data, aes(x=longitude, y=latitude), colour="green", shape=19) +
  geom_text(data = district.names, aes(x=Longitude, y=Latitude, label=District),
            colour = "dark grey", size = 4 )  +
  # geom_text(data = map.agrodealers.csv, aes(x=longitude, y=latitude, label=agroid),
  #           colour = "dark grey", size = 4 )  +
  coord_equal()+theme_bw()+labs(x="Longitude",y="Latitude") 

dir.create("plot/agromaps/pdf", showWarnings = FALSE, recursive = TRUE)
dir.create("plot/agromaps/png", showWarnings = FALSE, recursive = TRUE)


cluster.id <- read.csv("data/ClusterCoords.csv")
pdf(file = paste0("plot/agromaps/pdf/", "StudyDistricts11", ".pdf"),
    paper = "a4",  )
for (area_ in 1:nrow(srvy.districts)) {
  # print(bbox(srvy.districts[area_,]))
  area.lim <- bbox(srvy.districts[area_,])
  min.x <- area.lim["x","min"]
  max.x <- area.lim["x","max"]
  min.y <- area.lim["y","min"]
  max.y <- area.lim["y","max"]
  area.map <- map.all + labs(title = srvy.districts[area_,]$NAME_2) +
    coord_cartesian(xlim=c(min.x, max.x), ylim=c(min.y, max.y), default = TRUE) +
    geom_polygon(data = srvy.districts[area_,], mapping = aes(long,lat,group=group), 
                 colour=" blue", fill=NA, size = 1, linetype = "dashed") +
    radiusDist(1000, "blue") +
    geom_text(data = cluster.id, aes(x=Longitude, y=Latitude, label=ClusterID),
              colour = "black", size = 4 ) +
    scalebar(x.min=min.x, x.max=max.x, y.min=min.y, y.max=max.y, 
             dist = ceiling((max.x - min.x)*10) + 1,
             dist_unit = "km", transform = TRUE, model = "WGS84")
  
  grid::grid.newpage()
  v1 <- grid::viewport(width = 1, height = 1, x = 0.5, y = 0.5)
  print(area.map, vp=v1) 

}
  
dev.off()




