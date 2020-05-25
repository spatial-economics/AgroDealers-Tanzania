# Create Table 1 in agrodealers paper

library(foreign)
library(raster)
library(sf)
# library(ggplot2)
# library(data.table)


# Table 1 -----------------------------------------------------------------
table_1 <- list()


# Inputs ------------------------------------------------------------------

files.dta_folder <- list.files("data/Esoko safari/data", pattern = "*.dta", full.names = TRUE)

stockedfert.dta <- read.dta("data/Esoko safari/data/long_stockedfert_tz.dta")
stockedmaize.dta <- read.dta("data/Esoko safari/data/long_stockedmaize_tz.dta")
agrodealers.clustered <- read.csv("data/tzaagrodealersclustered.csv")
clusters.of.agrodealers <- read.csv("data/ClusterCoords.csv")

wgs.prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
agro_clstr_hq_dist.csv <- read.csv("D:/Jordan/AgroDealearsTZ/output/csv/agro_clstr_hq_dist.csv")


# Prepare Data ------------------------------------------------------------

# Prepare Agrodealears data to form Agrodealears Sf
agro_clstr_hq_dist.shp <- SpatialPointsDataFrame(data.frame(
  agro_clstr_hq_dist.csv$longitude,
  agro_clstr_hq_dist.csv$latitude),
  data = agro_clstr_hq_dist.csv,
  proj4string = wgs.prj
)
agro_clstr_hq_dist.sf <- merge(st_as_sf(agro_clstr_hq_dist.shp), 
                               agro_clstr_hq_dist.shp@data, 
                               names(agro_clstr_hq_dist.shp))


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

# Districs to sf
tza.study.districts.sf <- merge(st_as_sf(tza.study.districts), 
                                tza.study.districts@data, 
                                names(tza.study.districts))


files.dta <- sapply(basename(files.dta_folder), 
                    function(x){read.dta(file.path(dirname(files.dta_folder)[1], x))})

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Fill Table 1 ------------------------------------------------------------

# Create entries for each district
for (dist.name1 in tza.study.districts.sf$NAME_2) {
  if (dist.name1 == "Mbeya Urban") next
  if (dist.name1 == "Mbeya Rural") {
    table_1[["Mbeya"]] <- list()
    next
  }
  table_1[[dist.name1]] <- list()
  
}






for (dist.name2 in tza.study.districts.sf$NAME_2) {
  if (dist.name2 == "Mbeya Urban") next # Combined with Mbeya rural
  
  # Subset District data
  dist.name3 <- dist.name2
  district.data <- agro_clstr_hq_dist.sf[which(agro_clstr_hq_dist.sf$District %in% dist.name2),]
  # district.data.shp <- agro_clstr_hq_dist.shp[which(agro_clstr_hq_dist.shp$District %in% dist.name2),]
  
  if (dist.name2 == "Mbeya Rural") {
    district.data <- agro_clstr_hq_dist.sf[which(agro_clstr_hq_dist.sf$District %in% c("Mbeya Rural", "Mbeya Urban")),]
    district.data.shp <- agro_clstr_hq_dist.shp[which(agro_clstr_hq_dist.shp$District %in% c("Mbeya Rural", "Mbeya Urban")),]
    dist.name3 <- "Mbeya"
  }
  distr.agrosid <- district.data$agroid
  # 1
  # Total	Hai	Hanang	Mbulu	Siha	Mbeya (urban and rural together)	Mufindi	Mbozi
  # N	299
  # Number of agrodealears
  table_1[[dist.name3 ]][["Number"]] <- dim(district.data)[1]
  #2
  # Years in business  (2019 = 0, 2018 = 1,...)	
  table_1[[dist.name3 ]][["Years_in_business"]] <- mean(2019 - files.dta$wide_textfile_tz.dta$busyear[distr.agrosid])
  # 3
# Standard deviation
  table_1[[dist.name3 ]][["Years_in_business_sd"]] <- sd(2019 - files.dta$wide_textfile_tz.dta$busyear[distr.agrosid],
                                                         na.rm = TRUE)
  # 4
  # Member of agrodealer association (%)(
  table_1[[dist.name3 ]][["Member_of_agrodealer_association_(%)"]] <- 
    sum(files.dta$wide_textfile_tz.dta$assoc[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$assoc[distr.agrosid]) *100
  # 5
  # Products available (%)
  # 5 (i)
  # Maize seeds
  table_1[[dist.name3 ]][["Maize seeds_(%)"]] <- 
    sum(files.dta$wide_textfile_tz.dta$maize_seeds[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$maize_seeds[distr.agrosid]) *100
  # 5 (ii)
  # Other seeds	
  table_1[[dist.name3 ]][["Other_seeds"]] <- 
    sum(files.dta$wide_textfile_tz.dta$otherseeds[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$otherseeds[distr.agrosid]) *100
  # 5 (iii)
  # Fertilizer
  table_1[[dist.name3 ]][["Fertilizer"]] <- 
    sum(files.dta$wide_textfile_tz.dta$fertilizer[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$fertilizer[distr.agrosid]) *100
  # 5 (iv)
  # Herbicide
  table_1[[dist.name3 ]][["Herbicide"]] <- 
    sum(files.dta$wide_textfile_tz.dta$herbicides[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$herbicides[distr.agrosid]) *100	
  # 5 (v)			
  # Pesticide (for crop application)	
  table_1[[dist.name3 ]][["Pesticides"]] <- 
    sum(files.dta$wide_textfile_tz.dta$pesticides[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$pesticides[distr.agrosid]) *100	
  # 5 (vi)							
  # Lime								
  table_1[[dist.name3 ]][["Lime"]] <- 
    sum(files.dta$wide_textfile_tz.dta$lime[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$lime[distr.agrosid]) *100
  # 5 (vii)
  # Foliar feeds	
  table_1[[dist.name3 ]][["Foliar feeds"]] <- 
    sum(files.dta$wide_textfile_tz.dta$foliar_feeds[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$foliar_feeds[distr.agrosid]) *100
  # 5 (viii)
  # Other crop chemicals	
  table_1[[dist.name3 ]][["Other crop chemicals"]] <- 
    sum(files.dta$wide_textfile_tz.dta$other_crop_chem[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$other_crop_chem[distr.agrosid]) *100	
  # 5 (ix)						
  # Farm tools				
  table_1[[dist.name3 ]][["Farm tools"]] <- 
    sum(files.dta$wide_textfile_tz.dta$fam_tools[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$fam_tools[distr.agrosid]) *100	
  # 5 (x)			
  # Veterinary chemicals	
  table_1[[dist.name3 ]][["Veterinary chemicals"]] <- 
    sum(files.dta$wide_textfile_tz.dta$vet_chem[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$vet_chem[distr.agrosid]) *100
  # 5 (xi)						
  # Livestock feeds			
  table_1[[dist.name3 ]][["Livestock feeds"]] <- 
    sum(files.dta$wide_textfile_tz.dta$livestock_feeds[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$livestock_feeds[distr.agrosid]) *100		
  # 5 (xii)			
  # Other products like pharmaceutical, cosmetics, food, hardware,… 
  table_1[[dist.name3 ]][["Other products like pharmaceutical, cosmetics, food, hardware,…"]] <- 
    sum(files.dta$wide_textfile_tz.dta$fdcos_phamware[distr.agrosid]== "yes") / 
    length(files.dta$wide_textfile_tz.dta$fdcos_phamware[distr.agrosid]) *100
  # 5 (xiii)						
  # Other (Specify……)	
  table_1[[dist.name3 ]][["Other (Specify……)"]] <-
    sum(files.dta$wide_textfile_tz.dta$other_prodct[distr.agrosid] == "yes") /
    length(files.dta$wide_textfile_tz.dta$other_prodct[distr.agrosid]) *100
  
  # 6 
  # Share of revenue (%)
  # 6 (i)
  # Maize seed
  table_1[[dist.name3 ]][["revenue_Maize seeds_(%)"]] <- median(files.dta$wide_textfile_tz.dta$maize_share[distr.agrosid])
  # 6 (ii)
  # Fertilizer
  table_1[[dist.name3 ]][["revenue_Fertilizer_(%)"]] <- median(files.dta$wide_textfile_tz.dta$ferts_share[distr.agrosid])
  # 6 (iii)
  # Other crop inputs
  table_1[[dist.name3 ]][["revenue_othcrp_input_(%)"]] <- median(files.dta$wide_textfile_tz.dta$othcrp_input[distr.agrosid])
  # 6 (iv)
  # Livestock input
  table_1[[dist.name3 ]][["revenue_livestock_input_(%)"]] <- median(files.dta$wide_textfile_tz.dta$livestock_input[distr.agrosid])
  # 6 (v)
  # Other seeds
  table_1[[dist.name3 ]][["revenue_Other_seeds_(%)"]] <- median(files.dta$wide_textfile_tz.dta$othseed_share[distr.agrosid])
  # 6 (vi)
  # Other products 
  table_1[[dist.name3 ]][["revenue_Other_products_(%)"]] <- median(files.dta$wide_textfile_tz.dta$othprodct_share[distr.agrosid])
  
  # 7 (i)
  # Total available maize varieties in all shops (sum)
  dstrct.vrty.entrys <-
    files.dta$long_stockedmaize_tz.dta[which(files.dta$long_stockedmaize_tz.dta$agroid %in% distr.agrosid),]
  table_1[[dist.name3 ]][["Total_unique_maize_varieties"]] <- length(unique(dstrct.vrty.entrys$maizevar_stock))
  # 7 (ii)
  # Available varieties per shop (average)
  table_1[[dist.name3 ]][["Average_maize_varty_per_shop"]] <- 
    mean(sapply(unique(dstrct.vrty.entrys$agroid), 
                function(x) {
                  length(dstrct.vrty.entrys$maizevar_stock[which(dstrct.vrty.entrys$agroid == x)])
                }), na.rm = TRUE)
  
  # Standard deviation	
  table_1[[dist.name3 ]][["Standard_deviation_maize_varty_per_shop"]] <-
    sd(sapply(unique(dstrct.vrty.entrys$agroid),
              function(x) {
                length(dstrct.vrty.entrys$maizevar_stock[which(dstrct.vrty.entrys$agroid == x)])
              }),
       na.rm = TRUE)
  							
  # 7 (i)				
  # Total available fertilizer types in all shops (sum)
  dstrct.fert.entrys <-
    files.dta$long_stockedfert_tz.dta[which(files.dta$long_stockedfert_tz.dta$agroid %in% distr.agrosid),]
  
  table_1[[dist.name3 ]][["Total_unique_fertilizer_types"]] <- length(unique(dstrct.fert.entrys$fert_name))
  # 7 (ii)
  # Available fertilizers per shop (average)
  table_1[[dist.name3 ]][["Average_maize_varty_per_shop"]] <-
    mean(sapply(unique(dstrct.fert.entrys$agroid),
                function(x){
                  length(dstrct.fert.entrys$fert_name[which(dstrct.fert.entrys$agroid == x)])}), 
         na.rm = TRUE)
  # 7 (iii)
  # Standard deviation	
  table_1[[dist.name3 ]][["Standard_deviation_maize_varty_per_shop"]] <-
    sd(sapply(unique(dstrct.fert.entrys$agroid),
              function(x) {
                length(dstrct.fert.entrys$fert_name[which(dstrct.fert.entrys$agroid == x)])
              }),
       na.rm = TRUE)
  
  
  # 8
  # Agro-dealer / farmer ratio

  tza.rural.population # From AgrodealersTZ.R
  tza.travelzones.2agro.wgs # From AgrodealersTZ.R
  
  district.rural.pop <- 
    mask(tza.rural.population, 
         tza.study.districts[which(grepl(dist.name3, tza.study.districts$NAME_2)),],
         filename="tmp/districtruralpop2.grd", overwrite=TRUE)
    
    traveltimes.zonal <- zonal( district.rural.pop, tza.travelzones.2agro.wgs, 
                                fun = "sum", digits = 2, na.rm = TRUE)
    district.rural.pop.sum <- sum(values(district.rural.pop), na.rm = TRUE)
    
     # 8 (i)
  # % of farmers within 30 minutes travel time
    # Share of rural population further than [1,2,3,4] hour of nearest agrodealer
    # < 30min
    table_1[[dist.name3 ]][["Rural_population_<30min_traveltime"]] <- 
      traveltimes.zonal[1, 2] / district.rural.pop.sum
  # 8 (ii)
  # % of farmers within 1 hour travel time
    # <1hour
    table_1[[dist.name3 ]][["Rural_population_<1hr_traveltime"]] <-
      traveltimes.less30min[district.id] +
      traveltimes.zonal[2, 2] / district.rural.pop.sum
  # 8 (iii)
  # % of farmers within 2 hours travel time	
    # <2hour
    table_1[[dist.name3 ]][["Rural_population_<2hr_traveltime"]] <-
      traveltimes.less1hour[district.id] +
      traveltimes.zonal[3, 2] / district.rural.pop.sum
    
    
}


