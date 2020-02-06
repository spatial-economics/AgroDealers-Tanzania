# Calculate Several Measures for Each Agrodealer Location

library(osmdata)
library(raster)
library(sf)
# library(gdistance) # Library called inline to avoid namespace collisions with raster library

# INPUT DATA --------------------------------------------------------------------------------------
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

agrodealers.csv <- read.csv("./data/Esoko safari/spatial/agroid_coordinates_prices.csv")
tza.osm.towns <- shapefile(
    "data/hotosm_tza_populated_places_points_shp/hotosm_tza_populated_places_points.shp")

tza.roads <- shapefile("./data/Tanzania/Roads/TZA_Roads.shp")
tza.osm.roads_ <- shapefile("./data/Tanzania/Roads/OpenStreetMap/hotosm_tza_roads_lines.shp")

tza.population <- raster("data/TZA_popmap15adj_v2b.tif")
tza.landcover <- raster("spatial/glc2000/glc2000_dd.tif")
tza.slope <- raster("spatial/terrain/slope500m_laz.tif")

wgs.prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
laea.prj <- CRS("+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +a=6370997 +b=6370997 
                +units=m +no_defs")
#-------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



# DATA PREPARATION --------------------------------------------------------------------------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
# 
tza.osm.roads <- spTransform(tza.osm.roads_, laea.prj)

# Tanzania Cities and Towns ----------------------------------------------------------------------#
head(tza.osm.towns@data)
tza.osm.towns$population <- as.numeric(tza.osm.towns$population) 

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

# Find district hqs ------------------------------------------------------------------------------#
              # Continue Here to extract dist HQS from towns #
                        # Mean while use all towns #
#-------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



# CREATE TRASITION OBJECT ------------------------------------------------------------------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# The Transition Object is later Used to make accumulated cost surfaces
    # Travel Distance to all towns (Along roads)

# Rasterize roads layer -------------------------------------------------------------------------#
createSurface <- function(road.shp, toRaster, field_, fun_, travel.rate, surface.col, surface_) {
    # Rasterise roads (lines shapefile) 
    tmp1 <- road.shp[which(road.shp@data[,surface.col] == surface_),]
    tmp1[["min_meter"]] <- travel.rate
    rasterize(tmp1, toRaster, field=field_, fun=fun_)
}

tza.rds.asphalt <- createSurface(road.shp = tza.osm.roads, toRaster = tza.lc.traveltimes, 
                                 fun_ = 'last', field_ = "min_meter", travel.rate = 0.0010, 
                                 surface.col = "surface", surface_ = "asphalt")
tza.rds.gravel <- createSurface(road.shp = tza.osm.roads, toRaster = tza.lc.traveltimes, 
                                fun_ = 'last', field_ = "min_meter", travel.rate = 0.0010, 
                                 surface.col = "surface", surface_ = "gravel")
tza.rds.concrete <- createSurface(road.shp = tza.osm.roads, toRaster = tza.lc.traveltimes, 
                                  fun_ = 'last', field_ = "min_meter", travel.rate = 0.0010, 
                                 surface.col = "surface", surface_ = "concrete")
tza.rds.compacted <- createSurface(road.shp = tza.osm.roads, toRaster = tza.lc.traveltimes,
                                   fun_ = 'last', field_ = "min_meter", travel.rate = 0.0015, 
                                 surface.col = "surface", surface_ = "compacted")
tza.rds.sand <- createSurface(road.shp = tza.osm.roads, toRaster = tza.lc.traveltimes, 
                                 fun_ = 'last', field_ = "min_meter", travel.rate = 0.0015, 
                                 surface.col = "surface", surface_ = "sand")

# Overlay Road surfaces based on priority ---------------------------------------------------------#
overlaySurfaces <- function(raster.stack) {
    # Merge roads layers into one - in case of overlap
    for (count in 1:length(raster.stack)) {
        if (count == 1) tza.roads.raster <- raster.stack[count]
        if (count < 1)
        tza.roads.raster <- overlay(stack(tza.roads.raster, raster.stack[count]), 
                                    fun=function(x) ifelse(is.na(x[1]), x[2], x[1]))
    }
    return(tza.roads.raster)
}
tza.rds.surface <- overlaySurfaces(stack(tza.rds.asphalt, tza.rds.gravel, tza.rds.concrete,
                                         tza.rds.compacted, tza.rds.sand) )

# Overlay Land cover over Road surface -----------------------------------------------------------#
tza.traveltimes <- overlaySurfaces(stack(tza.rds.surface, tza.lc.traveltimes))

# Adjust travel times for slope - Reproject Slope to to align it to traveltimes raster
tza.slope.laea <- projectRaster(tza.slope, tza.traveltimes)
decay <- 1.5 ## this is the decay coefficient; governs how much the slope impacts the speed
radians <- function(x) { x * pi / 180 }  
tza.traveltimes.adj <- tza.traveltimes * exp(decay*tan(radians(tza.slope.laea))) 

# Create transition layer ------------------------------------------------------------------------#
tza.transition <- gdistance::transition(tza.traveltimes.adj, function(x) 1/mean(x), 8)
# Correction for diagonal distances
tza.transition.adj <- geoCorrection(tza.transition, type="r", multpl=FALSE)

#-------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



# CALCULATE TRAVEL TIMES --------------------------------------------------------------------------
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 1. Travel time to nearest district HQ

# 2. Travel time to nearest town of 50,000+ -------------------------------------------------------#
tza.osm.towns.50kplus <- tza.osm.towns[which(tza.osm.towns$population >= 50000),]
tza.time2.50kplus <- accCost(tza.transition.adj, tza.osm.towns.50kplus)

# Euclidean Distance to Nearest town (from agrodealer) of 50,000+
dist2town <- pointDistance(agrodealers.shp, tza.towns, lonlat = TRUE)
town.near.agro_id  <- apply(dist2town, 1, which.min)
town.near.agro <- data.frame(agrodealer=agrodealers.shp@data$agroid, 
                             town=tza.towns[town.near.agro_id,]
)


# 3. Travel time to nearest all-weather road ------------------------------------------------------#
tza.all.weather.road <- tza.osm.roads[which(tza.osm.roads$surface %in% c("asphalt", "gravel", 
                                                                         "concrete", "compacted",
                                                                         "sand") )]
tza.time2.weather.road <- accCost(tza.transition.adj, tza.all.weather.road)
# Eucledian distance to road
agro.dist.to.rd <- geosphere::dist2Line(p = st_coordinates(sf::st_as_sf(agrodealers.shp)), 
                                        line = st_coordinates(sf::st_as_sf(tza.roads))[,1:2])


# 4. Travel time to nearest (other) agrodealer ----------------------------------------------------#
time2otheragro.stack <- stack()
for (agrodealer in 1:length(tza.agrodealers.shp)) {
    time2otheragro.stack[agrodealer] <- accCost(tza.transition.adj, 
                                                tza.agrodealers.shp[agrodealer,] )
}
tza.time2.other.agro_ <- extract(dist2otheragro, tza.agrodealers.shp, method='simple', df=TRUE)
tza.time2.other.agro <- apply(tza.time2.other.agro_, 1, FUN=min)

# 7-8, Number of agrodealers within a given travel time -------------------------------------------#

# Number of (other) agrodealers within 30 minutes travel time - -----------------------------------#
tza.30min.2other.agro <- apply(tza.time2.other.agro_, 1, FUN=function(x) length(x[which(x <= 30)]))
# Number of (other) agrodealers within 60 minutes travel time  ------------------------------------#
tza.60min.2other.agro <- apply(tza.time2.other.agro_, 1, FUN=function(x) length(x[which(x <= 60)]))


# 5-6, Number of agrodealers within a radius  -----------------------------------------------------#
tza.dist2agro1 <- pointDistance(agrodealers.shp, lonlat = TRUE)
tza.dist2agro <- apply(abind::abind(dist2agro1, t(dist2agro1), along = 3), 1:2, 
                   function(x) sum(x, na.rm = TRUE))

# Number of agrodealers within 5km radius
tza.agro.within5km <- apply(tza.dist2agro, 1, function(x) which(x < 5000))

# Number of agrodealers within 10km radius
tza.agro.within10km <- apply(tza.dist2agro, dist2agro, 1, function(x) which(x < 10000))


# 9-10 Calculate for each district in study: Travel Time to agrodealear-Within District -----------#
tza.traveltime.2agro <- accCost(tza.transition.adj, tza.agrodealers.shp)
# Share of rural population further than [1,2,3,4] hour of nearest agrodealer
furthestpop <- function(time_) {
    time.mask <- calc(tza.traveltime.2agro, fun=function(x) ifelse(x > 60*time_, 1, NA))
    overlay( stack(tza.population, time.mask), 
             fun=function(x) ifelse(is.na(x[2]), x[2], x[1]))
}

tza.trvltime.2agro.1hr <- furthestpop(1) # <1hour
tza.trvltime.2agro.2hr <- furthestpop(2) # <2hour
tza.trvltime.2agro.3hr <- furthestpop(3) # <3hour
tza.trvltime.2agro.4hr <- furthestpop(4) # <4hour


# Share of rural population with [0,1,2,3+] agrodealers within 1hr travel time
nearestPop <- function(time_) {
    time.mask <- calc(tza.traveltime.2agro, fun=function(x) ifelse(x <= 60*time_, 1, NA))
    overlay( stack(tza.population, time.mask), 
             fun=function(x) ifelse(is.na(x[2]), x[2], x[1]))
}

tza.trvltime.2agro.1hr <- nearestPop(1) # >1hour
tza.trvltime.2agro.2hr <- nearestPop(2) # >2hour
tza.trvltime.2agro.3hr <- nearestPop(3) # >3hour
tza.trvltime.2agro.4hr <- nearestPop(4) # >4hour


#-------------------------------------------------------------------------------------------------#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
