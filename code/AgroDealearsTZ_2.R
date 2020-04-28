
library(raster)
library(riverdist)
library(gmapsdistance)
# source('./code/AgroDealearsTZ.R')
source('D:/Jordan/AgroDealearsTZ/code/AgroDealearsTZ_functions.R')


agrodealers.clustered <- read.csv("data/tzaagrodealersclustered.csv")
clusters.of.agrodealers <- read.csv("data/ClusterCoords.csv")
study.dist.HQs <- read.csv("data/tzastudydistrictsHQs.csv")


tza.districts <- getData("GADM", country="TZA", level = 2, path = "data/shp")
wgs.prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea.prj <- CRS("+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +a=6370997 +b=6370997 
                +units=m +no_defs")

clstr.cmptn <- list( "No competition",
                    "1-2 competitors",
                    "3-5 competitors",
                    "6-10 competitors",
                    "> 10 competitors"
                    )

agrodealers.clustered_2 <- agrodealers.clustered[,c("agroid", "longitude", "latitude", "cluster")]

tza.study.districts_ <- intersect(tza.districts, tza.agrodealers.shp_)
mufindi.index <- match("Mufindi", tza.study.districts_$NAME_2 )
# mkalama.index <- match("Mkalama", tza.study.districts_$NAME_2 )
aggregateList <- list(unlist(lapply(tza.study.districts_$NAME_2, #[-mkalama.index]
                                    function(x)
                                      ifelse(x %in% c("Mafinga Township Authority", 
                                                      "Mufindi" ), 1, x) )))
tza.study.districts <- aggregate(tza.study.districts_, # [-mkalama.index,]
                                 by = aggregateList, 
                                 FUN=function(x) ifelse(length(x) == 2, 
                                                        x[mufindi.index], x[1])
)


agrodealers.clustered_2.dist <- intersect(SpatialPointsDataFrame(data.frame(agrodealers.clustered_2$longitude,
                                                                            agrodealers.clustered_2$latitude),
                                                                 agrodealers.clustered_2,
                                                                 proj4string = wgs.prj),  
                                          tza.study.districts)


names(agrodealers.clustered_2.dist)
head(agrodealers.clustered_2.dist)
agro.clstr.dist <- agrodealers.clustered_2.dist[,c("agroid", "longitude", "latitude", "NAME_1", "NAME_2", "cluster")]
# names(agro.clstr.dist) <- c("agroid", "longitude", "latitude", "region", "district", "cluster")

agro.clstr.dist.disthq <- merge(agro.clstr.dist, 
                                study.dist.HQs[, c("NAME_2", "District.HQ", 
                                                   "Longitude", "Latitude")], 
                                by = "NAME_2")


agro.clstr.dist.disthq@data["clstr.size"] <- 
  vector(mode = "integer", length = length(agro.clstr.dist$agroid))
agro.clstr.dist.disthq@data["competition"] <-
  vector(mode = "character", length = length(agro.clstr.dist$agroid))

for (clster.no in clusters.of.agrodealers$ClusterID) {
  clstr.agros.id <- which(agro.clstr.dist.disthq$cluster == clster.no)
  clstr.agros <- agro.clstr.dist.disthq@data[clstr.agros.id,]
  clstr.agros.no <- dim(agro.clstr.dist.disthq@data[clstr.agros.id,])[1]
  
  if (clstr.agros.no == 1) {
    agro.clstr.dist.disthq@data[clstr.agros.id, "clstr.size"] <- clstr.agros.no
    agro.clstr.dist.disthq@data[clstr.agros.id, "competition"] <- clstr.cmptn[[1]]
  }
  if (clstr.agros.no >= 2 & clstr.agros.no <= 3 ) {
    agro.clstr.dist.disthq@data[clstr.agros.id, "clstr.size"] <- clstr.agros.no
    agro.clstr.dist.disthq@data[clstr.agros.id, "competition"] <- clstr.cmptn[[2]]
  }
  if (clstr.agros.no >= 4 & clstr.agros.no <= 6 ) {
    agro.clstr.dist.disthq@data[clstr.agros.id, "clstr.size"] <- clstr.agros.no
    agro.clstr.dist.disthq@data[clstr.agros.id, "competition"] <- clstr.cmptn[[3]]
  }
  if (clstr.agros.no >= 7 & clstr.agros.no <= 11 ) {
    agro.clstr.dist.disthq@data[clstr.agros.id, "clstr.size"] <- clstr.agros.no
    agro.clstr.dist.disthq@data[clstr.agros.id, "competition"] <- clstr.cmptn[[4]]
  }
  if (clstr.agros.no >= 12) {
    agro.clstr.dist.disthq@data[clstr.agros.id, "clstr.size"] <- clstr.agros.no
    agro.clstr.dist.disthq@data[clstr.agros.id, "competition"] <- clstr.cmptn[[5]]
  }
}

for (dist.name in tza.study.districts$NAME_2) {
  distr.agros.id <- which(agro.clstr.dist.disthq$NAME_2 == dist.name)
  distr.agros <- agro.clstr.dist.disthq@data[distr.agros.id,]
  
  distr.agros.no <- dim(agro.clstr.dist.disthq@data[distr.agros.id,])[1]
  distr.hq <- distr.agros[1, c("District.HQ", "Longitude", "Latitude")]
  
  distr.agros.shp <- spTransform(SpatialPointsDataFrame(data.frame(distr.agros$longitude, 
                                                                   distr.agros$latitude), 
                                                        distr.agros,
                                                        proj4string = wgs.prj),
                                 laea.prj)
  
  distr.hq.shp <- spTransform(SpatialPointsDataFrame(data.frame(distr.hq$Longitude,
                                                            distr.hq$Latitude), 
                                                 distr.hq,
                                                 proj4string = wgs.prj),
                          laea.prj)
  
  time2distr_hq <- gdistance::accCost(tza.transition.adj, # from source('./code/AgroDealearsTZ.R')
                                      distr.hq.shp
                                      )
  
  agro.clstr.dist.disthq@data[distr.agros.id, "time2distr_hq" ] <- 
    extract(time2distr_hq , distr.agros.shp, method='simple')
  
}

output.df <- agro.clstr.dist.disthq@data[, c("agroid", "longitude", "latitude", 
                                             "NAME_1", "NAME_2", "District.HQ", 
                                             "Longitude", "Latitude", "time2distr_hq", "cluster", "clstr.size",
                                             "competition")]
names(output.df)<- c("agroid", "longitude", "latitude", 
                     "Region", "District", "District.HQ", 
                     "HQ_longitude", "HQ_latitude", "time2distrHQ_min", "cluster.id", "clstr.size",
                     "competition")
write.csv(output.df, "output/csv/agro_clstr_disthq.csv", row.names = FALSE)


