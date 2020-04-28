library(raster)
library(rgdal)
library(RQGIS3)
library(geosphere)


# Data filepaths, QGIS path and processing tool ---------------------------

agroids.with.district.path <- "./doc/intermediate.csv"
tza.osm.roads_TrPrSeTe.path <- 
  './qgis/hotosm_tza_roads_lines_1km_TPST/hotosm_tza_roads_lines_1km_TPST.shp'

qgis.path <- "C:/Program Files/QGIS 3.10"
qgis.processingtool.search.term <- "shortestpathpointtopoint"

deg2meters <- 111319.5

# Prepare QGIS environment
set_env(root = qgis.path , dev = FALSE)
open_app()
# get_usage(alg = shortestpath.tool) # View tool INPUTS


# PREPARE DATA ------------------------------------------------------------

# Find qgis processing tool
shortestpath.tool <- find_algorithms(search_term = qgis.processingtool.search.term, 
                                     name_only = TRUE)

# Agrodealers shapefile
agroids.with.district <- read.csv(agroids.with.district.path)
agroids.with.district.shp <- 
  SpatialPointsDataFrame(as.data.frame(
    agroids.with.district[, c("longitude", "latitude")]),
    agroids.with.district,
    proj4string = wgs.prj)

# District HQs shapefile
all.districtHQs.shp <-
  SpatialPointsDataFrame(as.data.frame(agroids.with.district[, c("HQ_longitude", "HQ_latitude")]), 
                         agroids.with.district,
                         proj4string = wgs.prj)

# Trunk, Primary Secondary and Tertiary roads 
tza.osm.roads_TrPrSeTe <- shapefile(tza.osm.roads_TrPrSeTe.path)


# Get Study districts
tza.regions <- getData("GADM", country="TZA", level = 1, path = "./data/shp")
tza.districts <- getData("GADM", country="TZA", level = 2, path = "./data/shp")
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

# agroids.with.district.shp@data <-
# agroids.with.district.shp@data[which(!agroids.with.district.shp$Euc_dist > 0),] 

# c("Euc_dist", "agro2road",  "Hq2road", "Ntwk_dist", "Total_ntwk"),]
# data.frame(vector(mode = "integer", length = dim(agroids.with.district.shp@data)[1]),
#            vector(mode = "integer", length = dim(agroids.with.district.shp@data)[1]),
#            vector(mode = "integer", length = dim(agroids.with.district.shp@data)[1]),
#            vector(mode = "integer", length = dim(agroids.with.district.shp@data)[1]),
#            vector(mode = "integer", length = dim(agroids.with.district.shp@data)[1])
#            )


# CALCULATE DISTANCES -----------------------------------------------------
Sys.time()
i <- 0
failers <- c()
for (distrct.id in 2:dim(tza.study.districts)[1]) {
  # distrct.id <- 1
  # Get district agrodealers
  distrct.agrodealers <- 
    raster::intersect(agroids.with.district.shp[
      which(is.na(agroids.with.district.shp$Ntwk_dist) | agroids.with.district.shp$Ntwk_dist == 0), ],
      tza.study.districts[distrct.id, ])
  
  print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
  print(tza.study.districts@data[distrct.id, "NAME_2"])
  print("")
  print(as.character(distrct.agrodealers$Region))
  print(as.character(distrct.agrodealers$District))
  print(as.character(distrct.agrodealers$District.HQ))
  print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
  
  for (agro.count in distrct.agrodealers$agroid) {
    if (!agro.count %in% c(96, 108) ) next()
    # agro.count <- 138
    # Set tool parameters
    shortestpath.tool.params <- get_args_man(alg = shortestpath.tool)
    shortestpath.tool.params$INPUT <- tza.osm.roads_TrPrSeTe
    shortestpath.tool.params$OUTPUT <- "tmp/shortestpath.csv"
    shortestpath.tool.params$START_POINT <-
      paste0(
        agroids.with.district.shp$longitude[agro.count],
        ",",
        agroids.with.district.shp$latitude[agro.count],
        " ",
        "[EPSG:4326]"
      )
    shortestpath.tool.params$END_POINT <-
      paste0(
        agroids.with.district.shp$HQ_longitude[agro.count],
        ",",
        agroids.with.district.shp$HQ_latitude[agro.count],
        " ",
        "[EPSG:4326]"
      )
    shortestpath.tool.output <- tryCatch({ shortestpath.tool.output <- run_qgis(alg = shortestpath.tool,
                                                                                params = shortestpath.tool.params,
                                                                                load_output = TRUE
    )
    }, error = function(cond){failers <- c(failers, agro.count); return(list(cost=NA))},
    finally = function(cond){print("moving on")})
    
    # shortestpath.tool.output <- run_qgis(alg = shortestpath.tool,
    #                                     params = shortestpath.tool.params,
    #                                     load_output = TRUE)
    network_distance <- as.numeric(shortestpath.tool.output$cost) #* deg2meters
    
    if(agroids.with.district.shp$Euc_dist[agro.count] == 0){
      # agro.count <- 108 96 
      agroids.with.district.shp$Euc_dist[agro.count]  <- pointDistance(
        c(
          agroids.with.district.shp$longitude[agro.count],
          agroids.with.district.shp$latitude[agro.count]
        ),
        c(
          agroids.with.district.shp$HQ_longitude[agro.count],
          agroids.with.district.shp$HQ_latitude[agro.count]
        ),
        lonlat = TRUE
      )
      
      agroids.with.district.shp$agro2road[agro.count] <- 
        dist2Line(c(agroids.with.district.shp$longitude[agro.count],
                    agroids.with.district.shp$latitude[agro.count]),
                  tza.osm.roads_TrPrSeTe
        )[1]
      agroids.with.district.shp$Hq2road[agro.count] <-  
        dist2Line(c(agroids.with.district.shp$HQ_longitude[agro.count],
                    agroids.with.district.shp$HQ_latitude[agro.count]),
                  tza.osm.roads_TrPrSeTe)[1]
    }
    
    agroids.with.district.shp@data[agro.count, c("Ntwk_dist", "Total_ntwk")] <-
      c(network_distance, 
        agroids.with.district.shp$agro2road[agro.count] + agroids.with.district.shp$Hq2road[agro.count] + network_distance)
    write.csv(agroids.with.district.shp@data, "tmp/intermediate.csv")
    i <- i + 1
    print(paste(i, " : ", agro.count, " : ",  Sys.time()))
    
    
  }
  
  
  
}




# input.vector <- list.files("D:/Jordan/AgroDealearsTZ/tmp/Hai", pattern = "*.csv", full.names = TRUE)
# i <- 0
# distances_ <- vector(mode = "integer", length = length(input.vector))
# for (pathh in list.files("D:/Jordan/AgroDealearsTZ/tmp/Hai", pattern = "*.csv", full.names = TRUE)) {
#   
#   pp <- gsub(".csv", "", gsub("D:/Jordan/AgroDealearsTZ/tmp/Hai/Hai", "", pathh))
#   fille <- read.csv(pathh)
#   distances_[as.integer(pp)] <- fille$cost
#   
# }
# 
# agrodealears.ntwk$Total_ntwk
# 
# write.csv(distances_, "tmp/distvalues/outdists.csv")
# Data to merge
Ntwk.data <- read.csv("qgis/outdists_all.csv")
agrodealears.ntwk <- read.csv("qgis/intermediate.csv")

# Mkalama to Mbulu
agrodealears.ntwk[96, c("Euc_dist", "agro2road", "Hq2road", "Ntwk_dist", "Total_ntwk")] <-
  c(30192.88, 14.58509, 197.4586 , 0.7721124558222678 * 111319.5, 0)
agrodealears.ntwk[108, c("Euc_dist", "agro2road", "Hq2road", "Ntwk_dist", "Total_ntwk")] <-
  c(27600.38, 33.82899, 197.4586 , 0.7964303683764781 * 111319.5, 0)

agrodealears.ntwk[c(96, 108), c("Euc_dist", "agro2road", "Hq2road", "Ntwk_dist", "Total_ntwk")]

# Replace NA's in Ntwk_dist and Re compute Total_ntwk 
agrodealears.ntwk[which(is.na(agrodealears.ntwk$Ntwk_dist)),]

# Replace NA's in Ntwk_dist 
Ntwk.data$Id %in% which(is.na(agrodealears.ntwk$Ntwk_dist))
agrodealears.ntwk[Ntwk.data$Id, "Ntwk_dist"] <- Ntwk.data$dist_deg * 111319.5

# Re compute Total_ntwk 
total_ntwk <- agrodealears.ntwk$Total_ntwk
agrodealears.ntwk[, "Total_ntwk"] <- agrodealears.ntwk$agro2road + agrodealears.ntwk$Hq2road + agrodealears.ntwk$Ntwk_dist

# Save
agrodealears.ntwk
write.csv(agrodealears.ntwk[,-1:-3], "output/csv/agro_clstr_hq_dist.csv")


