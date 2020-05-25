# Create Table 1 in agrodealers paper

library(foreign)
library(raster)
library(sf)
# library(ggplot2)
# library(data.table)


# Table 1 -----------------------------------------------------------------
table_3 <- list()


# Inputs ------------------------------------------------------------------

files.dta_folder <- list.files("data/Esoko safari/data", pattern = "*.dta", full.names = TRUE)

clusters.of.agrodealers <- read.csv("data/ClusterCoords.csv")

agro_clstr_hq_dist.csv <- read.csv("D:/Jordan/AgroDealearsTZ/output/csv/agro_clstr_hq_dist.csv")
maize_vrty_categories.csv <- read.csv("data/MaizeVarietyCategories.csv")


# Prepare data ------------------------------------------------------------

wgs.prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
agro_clstr_hq_dist.sf <- merge(st_as_sf(agro_clstr_hq_dist.shp), 
                               agro_clstr_hq_dist.shp@data, 
                               names(agro_clstr_hq_dist.shp))
agro_clstr_hq_dist.sf$compLevel <- apply(agro_clstr_hq_dist.sf, MARGIN = 1,
                                         FUN=function(x){
                                           if (x["competition"] == "No competition")return(1)
                                           if (x["competition"] == "1-2 competitors")return(2)
                                           if (x["competition"] == "3-5 competitors")return(3)
                                           if (x["competition"] == "6-10 competitors")return(4)
                                           if (x["competition"] == "> 10 competitors")return(5)
                                         })
agro_clstr_hq_dist.sf[, "Level of Competition"] <- agro_clstr_hq_dist.sf$compLevel

files.dta <- sapply(basename(files.dta_folder), 
                    function(x){
                      dta.df <- read.dta(file.path(dirname(files.dta_folder)[1], x))
                      i <- sapply(dta.df, is.factor)
                      dta.df[i] <- sapply(dta.df[i], as.character)
                      dta.df
                      })



for (compLevel in unique(sort(agro_clstr_hq_dist.sf$compLevel))) {
  table_3[[compLevel]] <- list()
  # Total	No competition	1-2 competitors	3-5 competitors	6-10 competitors	>10 competitors	Comments
  compLevel.data <- agro_clstr_hq_dist.sf[which(
    agro_clstr_hq_dist.sf$compLevel %in% compLevel),]
  
  # Maize seed
  # M 1 (i)
  # # varieties per agro-dealer							take into account all varieties
  table_3[[compLevel]][["varieties per agro-dealer"]] <-
    mean(sapply(compLevel.data$agroid,
                function(x) {
                  length(files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                    files.dta$long_stockedmaize_tz.dta$agroid %in% x)])
                }), na.rm = TRUE)
  
  # A 1 (ii)
  # standard deviation
  table_3[[compLevel]][["standard deviation varieties per agro-dealer"]] <-
    sd(sapply(compLevel.data$agroid,
              function(x) {
                length(files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                  files.dta$long_stockedmaize_tz.dta$agroid %in% x)])
              }), na.rm = TRUE)
  # M 2
  # At least one variety available of (%)
  # M 2 (i)
  # Government varieties							all varieties
  
  gvmnt.vrty <- 
    maize_vrty_categories.csv$Maize_variety[which(
      maize_vrty_categories.csv$Category %in% "Government")]
  table_3[[compLevel]][["Government varieties > 1"]] <-
    sum(sapply(compLevel.data$agroid,
              function(x) {
                x.vrtys <- files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                  files.dta$long_stockedmaize_tz.dta$agroid %in% x)]
                length(x.vrtys[which(x.vrtys %in% gvmnt.vrty)])
                
              }) >= 1)

  
  # M 2 (ii)
  # Local companies							all varieties
  ntnl.rgnl.vrty <-
    maize_vrty_categories.csv$Maize_variety[which(
      maize_vrty_categories.csv$Category %in% "National_Regional")]
  table_3[[compLevel]][["National_Regional varieties > 1"]] <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 x.vrtys <-
                   files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                     files.dta$long_stockedmaize_tz.dta$agroid %in% x)]
                 length(x.vrtys[which(x.vrtys %in% ntnl.rgnl.vrty)])
                 
               }) >= 1)
  
  # M 2 (iii)
  # International companies							all varieties
  intnl.vrty <-
    maize_vrty_categories.csv$Maize_variety[which(
      maize_vrty_categories.csv$Category %in% "International")]
  table_3[[compLevel]][["International varieties > 1"]] <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 x.vrtys <-
                   files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                     files.dta$long_stockedmaize_tz.dta$agroid %in% x)]
                 length(x.vrtys[which(x.vrtys %in% intnl.vrty)])
                 
               }) >= 1)
  
  
  
  
  # M 3
  # Seed price per 2kg (in TSH)
  # M 3 (i) a
  # Government varieties							varieties available in 10% of stores, see word file for the list
  gvmnt.vrty_30 <-
    maize_vrty_categories.csv$Maize_variety[which(
      maize_vrty_categories.csv$Category %in% "Government" &
        maize_vrty_categories.csv$N30 %in% 1
    )]
  table_3[[compLevel]][["MEAN_Seed price per 2kg (in TSH)"]] <-
    mean(sapply(compLevel.data$agroid,
                function(x) {
                  x.vrtys <-
                    files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                      files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                        files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack"
                    )]
                  length(x.vrtys[which(x.vrtys %in% gvmnt.vrty_30)])
                  
                }))
  
  # M 3 (i) b
  # standard deviation
  table_3[[compLevel]][["SD_Seed price per 2kg (in TSH)"]] <-
    mean(sapply(compLevel.data$agroid,
                function(x) {
                  x.vrtys <-
                    files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                      files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                        files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack"
                    )]
                  length(x.vrtys[which(x.vrtys %in% gvmnt.vrty_30)])
                  
                }))
  
  # M 3 (ii) a
  # Local companies							varieties available in 10% of stores, see word file for the list
  ntnl.rgnl.vrty_30 <-
    maize_vrty_categories.csv$Maize_variety[which(
      maize_vrty_categories.csv$Category %in% "National_Regional" &
        maize_vrty_categories.csv$N30 %in% 1
    )]
  table_3[[compLevel]][["MEAN_Seed price per 2kg (in TSH)"]] <-
    mean(sapply(compLevel.data$agroid,
                function(x) {
                  x.vrtys <-
                    files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                      files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                        files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack"
                    )]
                  length(x.vrtys[which(x.vrtys %in% ntnl.rgnl.vrty_30)])
                  
                }))
  
  # M 3 (ii) b
  # standard deviation
  table_3[[compLevel]][["SD_Seed price per 2kg (in TSH)"]] <-
    mean(sapply(compLevel.data$agroid,
                function(x) {
                  x.vrtys <-
                    files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                      files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                        files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack"
                    )]
                  length(x.vrtys[which(x.vrtys %in% ntnl.rgnl.vrty_30)])
                  
                }))
  
  # M 3 (iii) a
  # International companies							varieties available in 10% of stores, see word file for the list
  intnl.vrty_30 <-
    maize_vrty_categories.csv$Maize_variety[which(
      maize_vrty_categories.csv$Category %in% "International" &
        maize_vrty_categories.csv$N30 %in% 1
    )]
  table_3[[compLevel]][["MEAN_Seed price per 2kg (in TSH)"]] <-
    mean(sapply(compLevel.data$agroid,
                function(x) {
                  x.vrtys <-
                    files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                      files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                        files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack"
                    )]
                  length(x.vrtys[which(x.vrtys %in% intnl.vrty_30)])
                  
                }))
  
  # M 3 (iii) b
  # standard deviation
  table_3[[compLevel]][["SD_Seed price per 2kg (in TSH)"]] <-
    sd(sapply(compLevel.data$agroid,
              function(x) {
                x.vrtys <-
                  files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                    files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                      files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack"
                  )]
                length(x.vrtys[which(x.vrtys %in% intnl.vrty_30)])
              }))
  # M 4
  # Reliable supply (%) (% very reliable)
  # M 4 (i)
  # Government varieties							varieties available in 10% of stores, see word file for the list
  gvmnt.reliable.supply_total <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 x.vrtys <-
                   files.dta$long_stockedmaize_tz.dta[which(
                     files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                       files.dta$long_stockedmaize_tz.dta$mzvar_rely %in% "Very reliable" &
                       files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% gvmnt.vrty_30
                   ),]
                 dim(x.vrtys)[1]
               }))
  table_3[[compLevel]][["Reliable supply (% very reliable)Government varieties >30%"]] <-
    gvmnt.reliable.supply_total * 100 / dim(files.dta$long_stockedmaize_tz.dta[which(
      files.dta$long_stockedmaize_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% gvmnt.vrty_30
    ), ])[1]
  
  
  # M 4 (ii)
  # Local companies							varieties available in 10% of stores, see word file for the list
  ntnl.rgnl.reliable.supply_total <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 x.vrtys <-
                   files.dta$long_stockedmaize_tz.dta[which(
                     files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                       files.dta$long_stockedmaize_tz.dta$mzvar_rely %in% "Very reliable" &
                       files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% ntnl.rgnl.vrty_30
                   ),]
                 dim(x.vrtys)[1]
               }))
  table_3[[compLevel]][["Reliable supply (% very reliable)Government varieties >30%"]] <-
    ntnl.rgnl.reliable.supply_total * 100 / dim(files.dta$long_stockedmaize_tz.dta[which(
      files.dta$long_stockedmaize_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% ntnl.rgnl.vrty_30
    ), ])[1]
  
  # M 4 (iii)
  # International companies							varieties available in 10% of stores, see word file for the list
  intnl.reliable.supply_total <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 x.vrtys <-
                   files.dta$long_stockedmaize_tz.dta[which(
                     files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                       files.dta$long_stockedmaize_tz.dta$mzvar_rely %in% "Very reliable" &
                       files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% intnl.vrty_30
                   ),]
                 dim(x.vrtys)[1]
               }))
  table_3[[compLevel]][["Reliable supply (% very reliable)Government varieties >30%"]] <-
    intnl.reliable.supply_total * 100 / dim(files.dta$long_stockedmaize_tz.dta[which(
      files.dta$long_stockedmaize_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% intnl.vrty_30
    ), ])[1]
  
  
  # Fertilizer Analysis -----------------------------------------------------
  # Fertilizer
  # F 1 (i)
  # # products per agro-dealer							all products
  table_3[[compLevel]][["Fertilizer types per agro-dealer"]] <-
    mean(sapply(compLevel.data$agroid,
                function(x) {
                  length(files.dta$long_stockedfert_tz.dta$fert_name[which(
                    files.dta$long_stockedfert_tz.dta$agroid %in% x)])
                }), na.rm = TRUE)
  
  
  # F 1 (ii)
  # standard deviation
  table_3[[compLevel]][["standard deviation varieties per agro-dealer"]] <-
    sd(sapply(compLevel.data$agroid,
              function(x) {
                length(files.dta$long_stockedfert_tz.dta$fert_name[which(
                  files.dta$long_stockedfert_tz.dta$agroid %in% x)])
              }), na.rm = TRUE)
  
  # F 2
  # Product available (%)
  # F 2 (i)
  # DAP							only DAP
  table_3[[compLevel]][["DAP_available"]] <-
    sum(sapply(compLevel.data$agroid,
              function(x) {
                if ("DAP" %in% files.dta$long_stockedfert_tz.dta$fert_name[which(
                  files.dta$long_stockedfert_tz.dta$agroid %in% x )]){
                  return(1)
                } else return(0)
              }))/dim(compLevel.data)[1]*100
  
  
  # F 2 (ii)
  # Urea							only urea
  table_3[[compLevel]][["Urea_available"]] <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 if ("Urea" %in% files.dta$long_stockedfert_tz.dta$fert_name[which(
                   files.dta$long_stockedfert_tz.dta$agroid %in% x )]){
                   return(1)
                 } else return(0)
               }))/dim(compLevel.data)[1]*100
  
  # F 2 (iii)
  # Can							only Can
  table_3[[compLevel]][["CAN (26:0:0)_available"]] <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 if ("CAN (26:0:0)" %in% files.dta$long_stockedfert_tz.dta$fert_name[which(
                   files.dta$long_stockedfert_tz.dta$agroid %in% x )]){
                   return(1)
                 } else return(0)
               }))/dim(compLevel.data)[1]*100
  # F 2 (iv)
  # SA							only SA
  table_3[[compLevel]][["SA (21:0:0)_available"]] <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 if ("SA (21:0:0)" %in% files.dta$long_stockedfert_tz.dta$fert_name[which(
                   files.dta$long_stockedfert_tz.dta$agroid %in% x )]){
                   return(1)
                 } else return(0)
               }))/dim(compLevel.data)[1]*100
 
  # F 3 (i) a
  # Fertilizer price per 50 kg (in TSH)
  # DAP							only DAP
  table_3[[compLevel]][["DAP_Fertilizer price per 50 kg"]] <-
    mean(unlist(sapply(compLevel.data$agroid,
                function(x) {
                  fert.pack.select <- 
                    files.dta$long_stockedfert_tz.dta$fertprice[which(
                      files.dta$long_stockedfert_tz.dta$agroid %in% x &
                        files.dta$long_stockedfert_tz.dta$fert_name %in% "DAP" &
                        files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                    )]
                })), na.rm = TRUE)
  
  # F 3 (i) b
  # standard deviation
  table_3[[compLevel]][["DAP_Fertilizer price per 50 kg_sd"]] <-
    sd(unlist(sapply(compLevel.data$agroid,
                     function(x) {
                       fert.pack.select <- 
                         files.dta$long_stockedfert_tz.dta$fertprice[which(
                           files.dta$long_stockedfert_tz.dta$agroid %in% x &
                             files.dta$long_stockedfert_tz.dta$fert_name %in% "DAP" &
                             files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                         )]
                     })), na.rm = TRUE)
  
  
  # F 3 (ii) a
  # Urea							only urea						
  table_3[[compLevel]][["Urea_Fertilizer price per 50 kg"]] <-
    mean(unlist(sapply(compLevel.data$agroid,
                       function(x) {
                         fert.pack.select <- 
                           files.dta$long_stockedfert_tz.dta$fertprice[which(
                             files.dta$long_stockedfert_tz.dta$agroid %in% x &
                               files.dta$long_stockedfert_tz.dta$fert_name %in% "Urea" &
                               files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                           )]
                       })), na.rm = TRUE)
  
  # F 3 (ii) b
  # standard deviation
  table_3[[compLevel]][["Urea_Fertilizer price per 50 kg_sd"]] <-
    sd(unlist(sapply(compLevel.data$agroid,
                     function(x) {
                       fert.pack.select <- 
                         files.dta$long_stockedfert_tz.dta$fertprice[which(
                           files.dta$long_stockedfert_tz.dta$agroid %in% x &
                             files.dta$long_stockedfert_tz.dta$fert_name %in% "Urea" &
                             files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                         )]
                     })), na.rm = TRUE)
  
  # F 3 (iii) a
  # Can							only Can						
  table_3[[compLevel]][["CAN (26:0:0)_Fertilizer price per 50 kg"]] <-
    mean(unlist(sapply(compLevel.data$agroid,
                       function(x) {
                         fert.pack.select <- 
                           files.dta$long_stockedfert_tz.dta$fertprice[which(
                             files.dta$long_stockedfert_tz.dta$agroid %in% x &
                               files.dta$long_stockedfert_tz.dta$fert_name %in% "CAN (26:0:0)" &
                               files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                           )]
                       })), na.rm = TRUE)
  
  # F 3 (iii) b
  # standard deviation
  table_3[[compLevel]][["CAN (26:0:0)_Fertilizer price per 50 kg_sd"]] <-
    sd(unlist(sapply(compLevel.data$agroid,
                     function(x) {
                       fert.pack.select <- 
                         files.dta$long_stockedfert_tz.dta$fertprice[which(
                           files.dta$long_stockedfert_tz.dta$agroid %in% x &
                             files.dta$long_stockedfert_tz.dta$fert_name %in% "CAN (26:0:0)" &
                             files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                         )]
                     })), na.rm = TRUE)
  
  # F 3 (iv) a
  # SA							only SA						
  table_3[[compLevel]][["SA (21:0:0)_Fertilizer price per 50 kg"]] <-
    mean(unlist(sapply(compLevel.data$agroid,
                       function(x) {
                         fert.pack.select <- 
                           files.dta$long_stockedfert_tz.dta$fertprice[which(
                             files.dta$long_stockedfert_tz.dta$agroid %in% x &
                               files.dta$long_stockedfert_tz.dta$fert_name %in% "SA (21:0:0)" &
                               files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                           )]
                       })), na.rm = TRUE)
  
  # F 3 (iv) b
  # standard deviation						
  table_3[[compLevel]][["SA (21:0:0)_Fertilizer price per 50 kg_sd"]] <-
    sd(unlist(sapply(compLevel.data$agroid,
                       function(x) {
                         fert.pack.select <- 
                           files.dta$long_stockedfert_tz.dta$fertprice[which(
                             files.dta$long_stockedfert_tz.dta$agroid %in% x &
                               files.dta$long_stockedfert_tz.dta$fert_name %in% "SA (21:0:0)" &
                               files.dta$long_stockedfert_tz.dta$fert_pack %in% "50kg bag"
                           )]
                       })), na.rm = TRUE)
  
  
  # F 4 
  # Reliable supply (%)  (% very reliable)
  # F 4 (i)
  # DAP							only DAP
  table_3[[compLevel]][["DAP_Reliable supply (% very reliable)"]] <-
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "DAP" &
        files.dta$long_stockedfert_tz.dta$fert_reliablty %in% "Very reliable"
    )]) *
    100 /
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "DAP"
    )])
  
  # F 4 (ii)
  # Urea							only urea
  table_3[[compLevel]][["Urea_Reliable supply (% very reliable)"]] <-
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "Urea" &
        files.dta$long_stockedfert_tz.dta$fert_reliablty %in% "Very reliable"
    )]) *
    100 /
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "Urea"
    )])
  
  
  # F 4 (iii)
  # Can							only Can
  table_3[[compLevel]][["CAN (26:0:0)_Reliable supply (% very reliable)"]] <-
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "CAN (26:0:0)" &
        files.dta$long_stockedfert_tz.dta$fert_reliablty %in% "Very reliable"
    )]) *
    100 /
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "CAN (26:0:0)"
    )])
  
  
  # F 4 (iv)
  # SA							only SA
  table_3[[compLevel]][["SA (21:0:0)_Reliable supply (% very reliable)"]] <-
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "SA (21:0:0)" &
        files.dta$long_stockedfert_tz.dta$fert_reliablty %in% "Very reliable"
    )]) *
    100 /
    length(files.dta$long_stockedfert_tz.dta$fert_reliablty[which(
      files.dta$long_stockedfert_tz.dta$agroid %in% compLevel.data$agroid &
        files.dta$long_stockedfert_tz.dta$fert_name %in% "SA (21:0:0)"
    )])
  
  
  # Table data creation end
}


str(table_3)



# 
# table_3[[compLevel]][["standard deviation varieties per agro-dealer"]] <-
#     sum(sapply(compLevel.data$agroid,
#                function(x) {
#                  if (identical(unique(files.dta$long_stockedfert_tz.dta$fert_name[which(
#                    files.dta$long_stockedfert_tz.dta$agroid %in% x )]), "SA (21:0:0)")){
#                    return(1)
#                  } else return(0)
#                }))/dim(compLevel.data)[1]*100