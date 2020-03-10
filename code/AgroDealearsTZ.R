# Calculate Several Measures for Each Agrodealer Location

library(osmdata)
library(raster)
library(rgeos)
library(sf)
# library(gdistance) # Library called inline to avoid namespace collisions with raster library

library(RColorBrewer)

# INPUT DATA --------------------------------------------------------------------------------------
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

agrodealers.csv <- read.csv("./data/Esoko safari/spatial/agroid_coordinates_prices.csv")
tza.osm.towns_ <- shapefile(
    "data/hotosm_tza_populated_places_points_shp/hotosm_tza_populated_places_points.shp")

# tza.roads <- shapefile("./data/Tanzania/Roads/TZA_Roads.shp")
tza.osm.roads_ <- shapefile("./data/Tanzania/Roads/OpenStreetMap/hotosm_tza_roads_lines.shp")

tza.population_ <- raster("data/TZA_popmap15adj_v2b.tif")
tza.landcover <- raster("spatial/glc2000/glc2000_dd.tif")
tza.slope <- raster("spatial/terrain/slope500m_laz.tif")

wgs.prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea.prj <- CRS("+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +a=6370997 +b=6370997 
                +units=m +no_defs")

tza.regions <- getData("GADM", country="TZA", level = 1, path = "data/shp")
tza.districts <- getData("GADM", country="TZA", level = 2, path = "data/shp")
#-------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



# DATA PREPARATION --------------------------------------------------------------------------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
# Create required folders
dir.create(path = "./output", showWarnings = FALSE)
dir.create(path = "./tmp", showWarnings = FALSE)
# 
tza.osm.roads <- spTransform(tza.osm.roads_, laea.prj)

# Tanzania Cities and Towns ----------------------------------------------------------------------#
head(tza.osm.towns_@data)
tza.osm.towns_$population <- as.numeric(tza.osm.towns_$population)
tza.osm.towns <- spTransform(tza.osm.towns_, laea.prj)

# Agrodealears locations -------------------------------------------------------------------------#
tza.agrodealers.shp_ <- SpatialPointsDataFrame(data.frame(agrodealers.csv$longitude, 
                                                     agrodealers.csv$latitude), 
                                          agrodealers.csv, proj4string = wgs.prj )
tza.agrodealers.shp <- spTransform(tza.agrodealers.shp_, laea.prj) 

# tza.regions <- getData("GADM", country="TZA", level = 1)
# tza.districts <- getData("GADM", country="TZA", level = 2)
# plot(tza.districts, border="light grey")
# plot(tza.regions, add=TRUE)
# plot(agrodealers.shp_, add=TRUE, col="red")
# Reproject and reclassify landcover raster to traveltimes (based on land cover)------------------#
# see road_traveltimes_worksheet.xlsx for reclass values
tza.landcover.laea <- projectRaster(tza.landcover, res = 1000, crs = laea.prj, method = "ngb")
df <- data.frame(id=0:24, v=c(0.11, 0.04, 0.02, 0.04, 0.05, 0.05, 0.02, 0.02, 0.04, 0.02, 
                              0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.04, 0.01, 0.02, 0.01, 
                              0.02, 0.02, 0.04, 0.11, 0.01) ) 
tza.lc.traveltimes <- subs(tza.landcover.laea, df) # Reclassify

# Tanzania Population ----------------------------------------------------------------------------#
tza.population.wgs <- aggregate(tza.population_, fact=10, fun=sum, na.rm=TRUE, 
                                filename="tmp/tzapopulationwgs.grd", overwrite=TRUE)
tza.population <- projectRaster(tza.population.wgs, tza.landcover.laea, method = "ngb", 
                                filename="tmp/tzapopulation.grd", overwrite=TRUE )

# Find district hqs ------------------------------------------------------------------------------#
plot(tza.districts)
plot(tza.agrodealers.shp_, add=TRUE)

# Merge "Mafinga Township Authority" into "Mufindi" and remove "Mkalama"
tza.study.districts_ <- intersect(tza.districts, tza.agrodealers.shp_)
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


tza.study.population <- mask(tza.population, tza.study.districts, 
                             filename="tmp/tzastudypopulation.grd", 
                             overwrite=TRUE)

              # Continue Here to extract dist HQS from towns #
                        # Meanwhile use all towns #
#-------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



# CREATE TRASITION OBJECT ------------------------------------------------------------------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# The Transition Object is later Used to make accumulated cost surfaces
    # Travel Distance to all towns (Along roads)

# Rasterize roads layer -------------------------------------------------------------------------#
createSurface <- function(road.shp, toRaster, field_, fun_, travel.rate, surface.col, surface_) {
    # Rasterise roads (lines shapefile) 
    tmp1 <- road.shp[which(road.shp@data[,surface.col] %in% surface_),]
    tmp1[["min_meter"]] <- travel.rate
    rasterize(tmp1, toRaster, field=field_, fun=fun_)
}

tza.trunk.n.pri.rds <- createSurface(road.shp = tza.osm.roads, toRaster = tza.lc.traveltimes, 
                                  fun_ = 'last', field_ = "min_meter", travel.rate = 0.0010, 
                                  surface.col = "highway", surface_ = c("trunk", "primary"))
tza.sec.n.ter.rds <- createSurface(road.shp = tza.osm.roads, toRaster = tza.lc.traveltimes,
                                   fun_ = 'last', field_ = "min_meter", travel.rate = 0.0015, 
                                   surface.col = "highway", surface_ = c("secondary", "tertiary"))


# Overlay Road surfaces based on priority ---------------------------------------------------------#
overlaySurfaces <- function(raster.stack) {
    # Merge roads layers into one - in case of overlap
    for (count in 1:dim(raster.stack)[3]) {
        if (count == 1) tza.roads.raster <- raster.stack[[count]]
        if (count > 1)
        tza.roads.raster <- overlay(tza.roads.raster, raster.stack[[count]], 
                                    fun=function(x) ifelse(is.na(x[1]), x[2], x[1]))
    }
    return(tza.roads.raster)
}
tza.rds.surface <- overlaySurfaces(stack(tza.trunk.n.pri.rds, tza.sec.n.ter.rds) )
cuts=c(-0.0015, 0.001, 0.0015) #set breaks
pal <- colorRampPalette(c("red","green"))
plot(tza.rds.surface, breaks=cuts, col = pal(3))

# Overlay Land cover over Road surface ------------------------------------------------------------#
tza.traveltimes <- overlaySurfaces(stack(tza.rds.surface, tza.lc.traveltimes))

cuts=c(0.001, 0.0015, c(0.01, 0.02, 0.04, 0.05, 0.11)) #set breaks
pal <- colorRampPalette(c("red", "yellow", "green", "blue"))
plot(tza.traveltimes, breaks=cuts, col = pal(7))

# Adjust travel times for slope - Reproject Slope to to align it to traveltimes raster ------------#
tza.slope.laea <- projectRaster(tza.slope, tza.traveltimes, filename="tmp/tzaslopelaea.grd", 
                                overwrite=TRUE )
decay <- 1.5 ## this is the decay coefficient; governs how much the slope impacts the speed
radians <- function(x) { x * pi / 180 }  
tza.traveltimes.adj <- tza.traveltimes * exp(decay*tan(radians(tza.slope.laea))) 

# Create transition layer -------------------------------------------------------------------------#
tza.transition <- gdistance::transition(tza.traveltimes.adj, function(x) 1/mean(x), 8)
# Correction for diagonal distances
tza.transition.adj <- gdistance::geoCorrection(tza.transition, type="r", multpl=FALSE)

#--------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



# CALCULATE TRAVEL TIMES --------------------------------------------------------------------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
tza.agrodealers.output <- tza.agrodealers.shp
# tza.agrodealers.output1 <- tza.agrodealers.output
# 1. Travel time to nearest district HQ

# 2. Travel time to nearest town of 50,000+ -------------------------------------------------------#
town.50k.plus.pop <- tza.osm.towns[which(tza.osm.towns$population >= 50000),]
tza.time2.50k.towns <- gdistance::accCost(tza.transition.adj,
                                          town.50k.plus.pop)
names(tza.time2.50k.towns) <- "min2_50Ktowns"
tza.agrodealers.output <- extract(tza.time2.50k.towns, tza.agrodealers.output, 
                                  method='simple', sp=TRUE)



# 2.2 Euclidean Distance to Nearest town (from agrodealer) of 50,000+
dist2town <- pointDistance(tza.agrodealers.shp, town.50k.plus.pop, lonlat = TRUE)
town.near.agro_id  <- apply(dist2town, 1, which.min)
tza.agrodealers.output@data["nearest50Ktwn"] <- town.50k.plus.pop@data$name[town.near.agro_id]


# 3. Travel time to nearest all-weather road ------------------------------------------------------#
tza.all.weather.road <- tza.osm.roads[which(tza.osm.roads$surface %in% c("asphalt", "gravel", 
                                                                         "concrete", "compacted",
                                                                         "sand") ),]
tza.time2.weather.road <- gdistance::accCost(tza.transition.adj, 
                                             st_coordinates(st_as_sf(tza.all.weather.road))[,1:2])

names(tza.time2.weather.road) <- "min2_AW_road"
tza.agrodealers.output <- extract(tza.time2.weather.road, tza.agrodealers.output, 
                                  method='simple', sp=TRUE)

# 3.2 Eucledian distance to road
# agro.dist.to.rd <- geosphere::dist2Line(tza.agrodealers.shp_, 
#                                         spTransform(tza.all.weather.road, wgs.prj) )
# tza.agrodealers.output@data["EucDist2road"] <- agro.dist.to.rd[  ]

# 4, 7-8. Travel time to nearest (other) agrodealer and ------------------------------------------#
#  Number of agrodealers within a given travel time 
time2otheragro <- nearestotheragro <- agroswithin30min <- agroswithin60min <- c()
rmv.fun <- function(a, rmv) a [! a %in% rmv]
time2agro.1hr.sum <- time2agro.30min.sum <- 0
for (agrodealer in 1:length(tza.agrodealers.shp)) {
    print(paste0("Agrodealer: ", agrodealer))
    time2agro  <- gdistance::accCost(tza.transition.adj, tza.agrodealers.shp[agrodealer, ])
    dist2agros <- extract(time2agro,
                         tza.agrodealers.output, 
                         method = 'simple')
    
    time2otheragro   <- c(time2otheragro, min(dist2agros[-agrodealer]))
    agroswithin30min <- c(agroswithin30min, toString(rmv.fun(which(dist2agros <= 30), agrodealer)))
    agroswithin60min <- c(agroswithin60min, toString(rmv.fun(which(dist2agros <= 60), agrodealer)))
    nearestotheragro <- c(nearestotheragro, 
                          toString(rmv.fun( which(dist2agros == min(dist2agros[-agrodealer])),
                                            agrodealer )))
   
    # time2agro.1hr.sum used later to calculate population with [0,1,2,3+] agrodealers within 1hr 
    reclassify.matrix.1hr <- matrix(c(-Inf, 1*60, 1, 1*60, Inf, 0), byrow = TRUE, ncol = 3)
    time2agro.1hr <- reclassify(time2agro, reclassify.matrix.1hr, filename="tmp/time2agro1hr.grd",
                                overwrite=TRUE)
    time2agro.1hr.sum <- time2agro.1hr.sum + time2agro.1hr
    
    # time2agro.1hr.sum used later to calculate population with [0,1,2,3+] agrodealers within 1hr 
    reclassify.matrix.30min <- matrix(c(-Inf, 1*30, 1, 1*30, Inf, 0), byrow = TRUE, ncol = 3)
    time2agro.30min <- reclassify(time2agro, reclassify.matrix.30min, filename="tmp/time2agro1hr.grd",
                                overwrite=TRUE)
    time2agro.30min.sum <- time2agro.30min.sum + time2agro.30min
    
    # plot(time2agro.1hr.sum)
    # break()
    
    }


tza.agrodealers.output@data["Time2NxtAgro"] <- time2otheragro
tza.agrodealers.output@data["NearestAgros"] <- nearestotheragro
tza.agrodealers.output@data["AgroIn30min"] <- agroswithin30min
tza.agrodealers.output@data["AgroIn60min"] <- agroswithin60min


# 5-6, Number of agrodealers within a radius  -----------------------------------------------------#
tza.dist2agro_ <- pointDistance(tza.agrodealers.shp, lonlat=FALSE)
# tza.dist2agro <- apply(abind::abind(tza.dist2agro_, t(tza.dist2agro_), along = 3), 1:2, 
#                    function(x) sum(x, na.rm = TRUE))
tza.dist2agro <- tza.dist2agro_

# # Agrodealers within 2km radius
# tza.agrodealers.output@data["AgroIn50M"] <- apply(tza.dist2agro, 1, 
#                                                    function(x) toString(which(x <= 50 & x > 0)))
# # Number of agrodealers within 2km radius
# tza.agrodealers.output@data["No_Agro50M"] <- apply(tza.dist2agro, 1, 
#                                                     function(x) length(which(x <= 50 & x > 0)))
# 
# tza.agrodealers.output@data["AgroIn100M"] <- apply(tza.dist2agro, 1, 
#                                                    function(x) toString(which(x <= 100 & x > 0)))
# # Number of agrodealers within 2km radius
# tza.agrodealers.output@data["No_Agro100M"] <- apply(tza.dist2agro, 1, 
#                                                      function(x) length(which(x <= 100 & x > 0)))
# 
# tza.agrodealers.output@data["AgroIn200M"] <- apply(tza.dist2agro, 1, 
#                                                    function(x) toString(which(x <= 200 & x > 0)))
# # Number of agrodealers within 2km radius
# tza.agrodealers.output@data["No_Agro200M"] <- apply(tza.dist2agro, 1, 
#                                                      function(x) length(which(x <= 200 & x > 0)))
# 
# tza.agrodealers.output@data["AgroIn500M"] <- apply(tza.dist2agro, 1, 
#                                                   function(x) toString(which(x <= 500 & x > 0)))
# # Number of agrodealers within 2km radius
# tza.agrodealers.output@data["No_Agro500M"] <- apply(tza.dist2agro, 1, 
#                                                    function(x) length(which(x <= 500 & x > 0)))

tza.agrodealers.output@data["AgroIn1KM"] <- apply(tza.dist2agro, 1, 
                                                  function(x) toString(which(x <= 1000 & x > 0)))
# Number of agrodealers within 2km radius
tza.agrodealers.output@data["No_Agro1KM"] <- apply(tza.dist2agro, 1, 
                                                   function(x) length(which(x <= 1000 & x > 0)))

tza.agrodealers.output@data["AgroIn2KM"] <- apply(tza.dist2agro, 1, 
                                                  function(x) toString(which(x <= 2000 & x > 0)))
# Number of agrodealers within 2km radius
tza.agrodealers.output@data["No_Agro2KM"] <- apply(tza.dist2agro, 1, 
                                                  function(x) length(which(x <= 2000 & x > 0)))

# # Agrodealers within 5km radius
# tza.agrodealers.output@data["AgroIn5KM"] <- apply(tza.dist2agro, 1, 
#                                                   function(x) toString(which(x <= 5000 & x > 0)))
# # Number of agrodealers within 5km radius
# tza.agrodealers.output@data["No_Agro5KM"] <- apply(tza.dist2agro, 1, 
#                                                   function(x) length(which(x <= 5000 & x > 0)))
# 
# # Agrodealers within 10km radius
# tza.agrodealers.output@data["AgroIn10KM"] <- apply(tza.dist2agro, 1, 
#                                                    function(x) toString(which(x <= 10000 & x > 0)))
# # Number of agrodealers within 10km radius
# tza.agrodealers.output@data["No_Agro10KM"] <- apply(tza.dist2agro, 1, 
#                                                   function(x) length(which(x <= 10000 & x > 0)))




# 11 Cluster Agrodalers ---------------------------------------------------





# 9-10 Calculate for each district in study: ------------------------------

# Travel Time to any agrodealear-Within District 
tza.traveltime.2agro <- gdistance::accCost(tza.transition.adj, 
                                           tza.agrodealers.shp)
# Classify travel time in 1hr bands
reclassify.matrix <- matrix(c(-Inf, 0.5*60, 0.5,  # <1hour
                              0.5*60, 1*60, 1,  # <1hour
                              1*60, 2*60, 2,  # <2hour
                              2*60, 3*60, 3,  # <3hour
                              3*60, 4*60, 4,  # <4hour
                              4*60, Inf, 5 ), byrow = TRUE, ncol = 3)
tza.travelzones.2agro <- reclassify(tza.traveltime.2agro, 
                                    reclassify.matrix)

# NB - PROJECTION ISSUE: Coverted travel zones to wgs to get realistic population values
# Use time2agro.1hr.sum calculated in earlier loop
time2agro.1hr.sum.wgs <- projectRaster(time2agro.1hr.sum, tza.population.wgs, method = 'ngb')
tza.travelzones.2agro.wgs <- projectRaster(tza.travelzones.2agro, tza.population.wgs, 
                                           method = 'ngb')

traveltimes.less30min <- traveltimes.less1hour <- traveltimes.less2hour <- 
    traveltimes.less3hour <- traveltimes.less4hour <- 
    agros.in.1hr.is0 <- agros.in.1hr.is1 <- agros.in.1hr.is2 <- 
    agros.in.1hr.is3 <- agros.in.1hr.is4 <- agros.in.1hr.is5 <- 
    agros.in.1hr.is2plus <- agros.in.1hr.is3plus <- agros.in.1hr.is4plus <- 
    agros.in.1hr.is5plus <- numeric(length(tza.study.districts) )


tza.rural.areas <- reclassify(tza.population.wgs, matrix(c(-Inf, 1000, 1, 1000, Inf, 0), 
                                                  byrow = TRUE, 
                                                  ncol = 3))
tza.rural.population <- tza.population.wgs*tza.rural.areas
                        
for (district.id in 1:length(tza.study.districts)) {
    print(district.id)
    district.rural.pop <- mask(tza.rural.population, tza.study.districts[district.id,], 
                               filename="tmp/districtruralpop.grd", overwrite=TRUE)
    
    traveltimes.zonal <- zonal( district.rural.pop, tza.travelzones.2agro.wgs, 
                                    fun = "sum", digits = 2, na.rm = TRUE
                                    )
    district.rural.pop.sum <- sum(values(district.rural.pop), na.rm = TRUE)
    print(traveltimes.zonal)
    
    # Share of rural population further than [1,2,3,4] hour of nearest agrodealer
    # < 30min
    traveltimes.less30min[district.id] <- traveltimes.zonal[1, 2] / district.rural.pop.sum
    # <1hour
    traveltimes.less1hour[district.id] <- traveltimes.less30min[district.id] +
        traveltimes.zonal[2, 2] / district.rural.pop.sum
    # <2hour
    traveltimes.less2hour[district.id] <- traveltimes.less1hour[district.id] + 
        traveltimes.zonal[3, 2] / district.rural.pop.sum
    # <3hour
    traveltimes.less3hour[district.id] <- traveltimes.less2hour[district.id] +
        traveltimes.zonal[4, 2] / district.rural.pop.sum
    # <4hour
    traveltimes.less4hour[district.id] <- traveltimes.less3hour[district.id] +
        traveltimes.zonal[5, 2] / district.rural.pop.sum
    
    
    
    # Share of rural population with [0,1,2,3+] agrodealers within 1hr travel time
    traveltime.1hr.zonal_ <- zonal( district.rural.pop, time2agro.1hr.sum.wgs, 
                                fun = "sum", digits = 2, na.rm = TRUE )
    traveltime.1hr.zonal <- traveltime.1hr.zonal_[which(traveltime.1hr.zonal_[,2] != 0), ]
    print(traveltime.1hr.zonal_)
    print(traveltime.1hr.zonal)
    
    zones.found <- nrow(traveltime.1hr.zonal)
    if (!is.null(zones.found)) {
        traveltime.1hr.zonal_1 <- traveltime.1hr.zonal[,1]
        traveltime.1hr.zonal_2 <- traveltime.1hr.zonal[,2]
        }
    if (is.null(zones.found)) {
        zones.found <- 1
        traveltime.1hr.zonal_1 <- traveltime.1hr.zonal[1]
        traveltime.1hr.zonal_2 <- traveltime.1hr.zonal[2]
        }
    for (zone_ in 1:zones.found) {
        if (length(traveltime.1hr.zonal) == 0) break()
        # 0 agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] == 0) {
            agros.in.1hr.is0[district.id] <- agros.in.1hr.is0[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
            }
        
        # 1 agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] == 1) {
            agros.in.1hr.is1[district.id] <- agros.in.1hr.is1[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
            }
        
        # 2 agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] == 2) {
            agros.in.1hr.is2[district.id] <- agros.in.1hr.is2[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        # 3 agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] == 3) {
            agros.in.1hr.is3[district.id] <- agros.in.1hr.is3[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        # 4 agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] == 4) {
            agros.in.1hr.is4[district.id] <- agros.in.1hr.is4[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        # 5 agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] == 5) {
            agros.in.1hr.is5[district.id] <- agros.in.1hr.is5[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        # 2+  agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] >= 2) {
            agros.in.1hr.is2plus[district.id] <- agros.in.1hr.is2plus[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        # 3+  agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] >= 3) {
            agros.in.1hr.is3plus[district.id] <- agros.in.1hr.is3plus[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        # 4+  agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] >= 4) {
            agros.in.1hr.is4plus[district.id] <- agros.in.1hr.is4plus[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        # 5+  agros in 1hr
        if (traveltime.1hr.zonal_1[zone_] >= 5) {
            agros.in.1hr.is5plus[district.id] <- agros.in.1hr.is5plus[district.id] +
                (traveltime.1hr.zonal_2[zone_] / district.rural.pop.sum)
        }
        
        }
    }
tza.study.districts1 <- tza.study.districts
tza.study.districts1@data["travelin30min"] <- traveltimes.less30min
tza.study.districts1@data["travelin1hr"] <- traveltimes.less1hour
tza.study.districts1@data["travelin2hr"] <- traveltimes.less2hour
tza.study.districts1@data["travelin3hr"] <- traveltimes.less3hour
tza.study.districts1@data["travelin4hr"] <- traveltimes.less4hour
tza.study.districts1@data["0agrosin1hr"] <- agros.in.1hr.is0
tza.study.districts1@data["1agrosin1hr"] <- agros.in.1hr.is1
tza.study.districts1@data["2agrosin1hr"] <- agros.in.1hr.is2
tza.study.districts1@data["3agrosin1hr"] <- agros.in.1hr.is3
tza.study.districts1@data["4agrosin1hr"] <- agros.in.1hr.is4
tza.study.districts1@data["5agrosin1hr"] <- agros.in.1hr.is5
tza.study.districts1@data["ovr2AGin1hr"] <- agros.in.1hr.is2plus
tza.study.districts1@data["ovr3AGin1hr"] <- agros.in.1hr.is3plus
tza.study.districts1@data["ovr4AGin1hr"] <- agros.in.1hr.is4plus
tza.study.districts1@data["ovr5AGin1hr"] <- agros.in.1hr.is5plus


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Save output -------------------------------------------------------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

shapefile(tza.study.districts1, "output/tzastudydistricts.shp", overwrite=TRUE)
shapefile(tza.agrodealers.output, "output/tzaagrodealersoutput.shp", overwrite=TRUE)

write.csv(tza.study.districts1@data, "output/tzastudydistricts.csv")
write.csv(tza.agrodealers.output@data, "output/tzaagrodealersoutput.csv")

# write.csv(tza.study.districts1@data, "output/tzastudydistricts1.csv")
# write.csv(tza.agrodealers.output@data, "output/tzaagrodealersoutput.csv")
#-------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

