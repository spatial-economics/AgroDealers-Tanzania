# Get the following
library(raster)
library(foreign)
library(ggplot2)
library(data.table)

source("code/AgroDealearsTZ_functions.R")
tza.time2.weather.road #Sourced from agrodealearsTZ
stockedfert.dta <- read.dta("data/Esoko safari/data/long_stockedfert_tz.dta")
stockedmaize.dta <- read.dta("data/Esoko safari/data/long_stockedmaize_tz.dta")
agrodealers.clustered <- read.csv("data/tzaagrodealersclustered.csv")
clusters.of.agrodealers <- read.csv("data/ClusterCoords.csv")
seed.cmpny.ctgry <- read.csv("data/SeedCompanyCategories.csv")
fertilizer.cmpny.ctgry <- read.csv("data/FertilizerCompanyCategories.csv")

# tza.osm.roads_ <- shapefile("./data/Tanzania/Roads/OpenStreetMap/hotosm_tza_roads_lines.shp")
# tza.osm.roads <- spTransform(tza.osm.roads_, laea.prj)

tza.primarySecondary.road <- 
  tza.osm.roads_[which(tza.osm.roads_$highway %in% c("trunk", "primary", 
                                                   "secondary", "tertiary") ),]
tza.primary.road <- 
  tza.osm.roads_[which(tza.osm.roads_$highway %in% c("trunk", "primary") ),]
tza.secondary.road <- 
  tza.osm.roads_[which(tza.osm.roads_$highway %in% c("secondary", "tertiary") ),]

clstr.mean.maize <- getClstrMean(stockedmaize.dta, 
             agrodealers.clustered,
             clusters.of.agrodealers$ClusterID,
             seed.cmpny.ctgry,
             "Maize_variety",
             "Cmpny_ctgry",
             "maizevar_stock",
             c("seed_selprice", "seed_pprice"))

clstr.mean.fert <- getClstrMean(stockedfert.dta, 
                           agrodealers.clustered,
                           clusters.of.agrodealers$ClusterID,
                           fertilizer.cmpny.ctgry,
                           "Fertilizer_type",
                           "Cmpny_ctgry",
                           "fert_name",
                           c("fertprice", "fert_cost"))


clusters.of.agrodealers.output_2 <- getTzaDistrict(clusters.of.agrodealers,
                                                 agrodealers.clustered)

clusters.of.agrodealers.output_2.shp <- 
  SpatialPointsDataFrame(clusters.of.agrodealers.output_2[,c("Longitude", "Latitude")], 
                         clusters.of.agrodealers.output_2, proj4string = wgs.prj)

# Time to road
plot(tza.time2.weather.road )
clusters.of.agrodealers.output_2["Time2Road"] <- 
  extract(tza.time2.weather.road, spTransform(clusters.of.agrodealers.output_2.shp, laea.prj) )

# Eucledian distance to road
ptm <- list()
ptm[["ptm1"]] <- proc.time()

clusters.of.agrodealers.output_2["Dist2AllRoad"] <- 
  geosphere::dist2Line(clusters.of.agrodealers.output_2[,c("Longitude", 
                                                           "Latitude")], 
                       tza.primarySecondary.road )[,"distance"]

ptm[["ptm2"]] <- proc.time()
print(ptm[["ptm2"]] - ptm[["ptm1"]])
clusters.of.agrodealers.output_2["Dist2PriRoad"] <- 
  geosphere::dist2Line(clusters.of.agrodealers.output_2[,c("Longitude", 
                                                           "Latitude")], 
                       tza.primary.road )[,"distance"]

ptm[["ptm3"]] <- proc.time()
print(ptm[["ptm3"]] - ptm[["ptm2"]])
clusters.of.agrodealers.output_2["Dist2SecRoad"] <- 
  geosphere::dist2Line(clusters.of.agrodealers.output_2[,c("Longitude", 
                                                           "Latitude")], 
                       tza.secondary.road )[,"distance"]

ptm[["ptm4"]] <- proc.time()
print(ptm[["ptm4"]] - ptm[["ptm3"]])





for (price_1 in c("seed_pprice", "seed_selprice")) {
  for (ctgry_ in c("Goverment", "Domestic", "International")) {
    clusters.of.agrodealers.output_2[paste(ctgry_, price_1, sep = ".")] <- 
      getCtgryData(clstr.mean.maize, "Category", ctgry_, price_1)
    
  }
}

for (price_1 in c("fert_cost", "fertprice")) {
  for (ctgry_ in c("Goverment", "Domestic", "International")) {
    clusters.of.agrodealers.output_2[paste(ctgry_, price_1, sep = ".")] <- 
      getCtgryData(clstr.mean.fert, "Category", ctgry_, price_1)
    
  }
}

# clusters.of.agrodealers.output_2a <- clusters.of.agrodealers.output_2 

clusters.of.agrodealers.output_2 <- 
  data.frame(clusters.of.agrodealers.output_2, 
             data.frame(getReliability(stockedmaize.dta, 
                                       agrodealers.clustered,
                                       clusters.of.agrodealers$ClusterID,
                                       "mzvar_rely", 
                                       c("Very reliable", 
                                         "Not reliable", 
                                         "Averagely reliable")
                            )
             ))

clusters.of.agrodealers.output_2 <- 
  data.frame(clusters.of.agrodealers.output_2, 
             data.frame(getReliability(stockedfert.dta, 
                                       agrodealers.clustered,
                                       clusters.of.agrodealers$ClusterID,
                                       "fert_reliablty", 
                                       c("Very reliable", 
                                         "Not reliable", 
                                         "Averagely reliable")
             )
             ))



head(clusters.of.agrodealers.output_2)




write.csv(clusters.of.agrodealers.output_2,
          "output/clstrsAgrosAnalysis_2.csv",
          row.names = FALSE)


# Plots -------------------------------------------------------------------
source("code/ClusterAnalysis2_plots.R")

