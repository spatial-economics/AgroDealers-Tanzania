# Create Table 1 in agrodealers paper

library(foreign)
library(raster)
library(sf)
library(readxl)
# library(ggplot2)
# library(data.table)


# Table 1 -----------------------------------------------------------------
table_3 <- list()


# Inputs ------------------------------------------------------------------

files.dta_folder <- list.files("data/Esoko safari/data", pattern = "*.dta", full.names = TRUE)

clusters.of.agrodealers <- read.csv("data/ClusterCoords.csv")

agro_clstr_hq_dist.csv <- read.csv("D:/Jordan/AgroDealearsTZ/output/csv/agro_clstr_hq_dist.csv")
maize_vrty_categories.csv <- read.csv("data/MaizeVarietyCategories.csv")
table_3.template <- read_xlsx("data/fwtanzaniaagrodealersclusters/Tanzania paper tables.xlsx", "Table 3")
table_3.rownames <- read.csv("data/Table_3_rownames.csv")

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



for (compLevel in c("Total", unique(agro_clstr_hq_dist.sf$competition))) {
  table_3[[compLevel]] <- list()
  # Total	No competition	1-2 competitors	3-5 competitors	6-10 competitors	>10 competitors	Comments
  
  compLevel1 <- compLevel #Level of competion
  # Select all if compLevel is Total
  if(compLevel == "Total") compLevel1 <- unique(agro_clstr_hq_dist.sf$competition)
  
  # select Data rows for the compLevel
  compLevel.data <- agro_clstr_hq_dist.sf[which(
    agro_clstr_hq_dist.sf$competition %in% compLevel1),]

  
  
# Analysis of the compLevel data ------------------------------------------
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
  # (At this competition Level)
  # M 2 (i)
  # Government varieties							all varieties
  gvmnt.vrty <- 
    unlist(strsplit(maize_vrty_categories.csv$Maize_variety2[which(
                           maize_vrty_categories.csv$Category %in% "Government")],
                    ";"))
  
  table_3[[compLevel]][["Government varieties >= 1"]] <-
    sum(sapply(compLevel.data$agroid,
              function(x) {
                x.vrtys <- files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                  files.dta$long_stockedmaize_tz.dta$agroid %in% x)]
                
                length(x.vrtys[which(x.vrtys %in% gvmnt.vrty)])
                
              }) >= 1) / length(compLevel.data$agroid)*100

  
  # M 2 (ii)
  # Local companies							all varieties
  ntnl.rgnl.vrty <-
    unlist(strsplit(maize_vrty_categories.csv$Maize_variety2[which(
                         maize_vrty_categories.csv$Category %in% "National_Regional")],
                    ";"))
  
  table_3[[compLevel]][["National_Regional varieties >= 1"]] <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 x.vrtys <-
                   files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                     files.dta$long_stockedmaize_tz.dta$agroid %in% x)]
                 
                 length(x.vrtys[which(x.vrtys %in% ntnl.rgnl.vrty)])
                 
               }) >= 1) / length(compLevel.data$agroid)*100
  
  # M 2 (iii)
  # International companies							all varieties
  # (select international varieties and split elements with 2 or more varitie- separated by ;)
  intnl.vrty <- 
    unlist(strsplit(maize_vrty_categories.csv$Maize_variety2[ 
                        which(maize_vrty_categories.csv$Category %in% "International")],
                    ";"))
  table_3[[compLevel]][["International varieties >= 1"]] <-
    sum(sapply(compLevel.data$agroid,
               function(x) {
                 x.vrtys <-
                   files.dta$long_stockedmaize_tz.dta$maizevar_stock[which(
                     files.dta$long_stockedmaize_tz.dta$agroid %in% x)]
                 length(x.vrtys[which(x.vrtys %in% intnl.vrty)])
                 
               }) >= 1) / length(compLevel.data$agroid)*100
  
  
  
  
  # M 3
  # Seed price per 2kg (in TSH)
  # M 3 (i) a
  # Government varieties							varieties available in 10% of stores, see word file for the list
  gvmnt.vrty_30 <-
    unlist(strsplit(maize_vrty_categories.csv$Maize_variety2[
      which( maize_vrty_categories.csv$Category %in% "Government" &
               maize_vrty_categories.csv$N30 %in% 1 )],
      ";"))
  
  table_3[[compLevel]][["Government:MEAN_Seed price per 2kg (in TSH)"]] <-
    mean(unlist(sapply(compLevel.data$agroid,
                function(x) {
                  # Get varieties per agrodealer in 2kg package and in 
                  #   goverment N30 (10% of stores) list
                  x.vrtys.data <-
                    files.dta$long_stockedmaize_tz.dta$seed_pprice[which(
                      files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                        files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack" &
                        files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% gvmnt.vrty_30
                    )]
                  
                  
                })), na.rm = TRUE)
  
  # M 3 (i) b
  # standard deviation
  table_3[[compLevel]][["Government:SD_Seed price per 2kg (in TSH)"]] <-
    sd(unlist(sapply(compLevel.data$agroid,
                     function(x) {
                       # Get varieties per agrodealer in 2kg package and in 
                       #   goverment N30 (10% of stores) list
                       x.vrtys.data <-
                         files.dta$long_stockedmaize_tz.dta$seed_pprice[which(
                           files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                             files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack" &
                             files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% gvmnt.vrty_30
                         )]
                       
                       
                     })), na.rm = TRUE)
  
  # M 3 (ii) a
  # Local companies							varieties available in 10% of stores, see word file for the list
  ntnl.rgnl.vrty_30 <-
    unlist(strsplit(maize_vrty_categories.csv$Maize_variety2[
      which(maize_vrty_categories.csv$Category %in% "National_Regional" &
              maize_vrty_categories.csv$N30 %in% 1 )],
      ";"))
  
  table_3[[compLevel]][["National_Regional:MEAN_Seed price per 2kg (in TSH)"]] <-
    mean(unlist(sapply(compLevel.data$agroid,
                       function(x) {
                         # Get varieties per agrodealer in 2kg package and in 
                         #   National/Regional N30 (10% of stores) list
                         x.vrtys.data <-
                           files.dta$long_stockedmaize_tz.dta$seed_pprice[which(
                             files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                               files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack" &
                               files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% ntnl.rgnl.vrty_30
                           )]
                         
                         
                       })), na.rm = TRUE)
  
  # M 3 (ii) b
  # standard deviation
  table_3[[compLevel]][["National_Regional:SD_Seed price per 2kg (in TSH)"]] <-
    sd(unlist(sapply(compLevel.data$agroid,
                       function(x) {
                         # Get varieties per agrodealer in 2kg package and in 
                         #   National/Regional N30 (10% of stores) list
                         x.vrtys.data <-
                           files.dta$long_stockedmaize_tz.dta$seed_pprice[which(
                             files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                               files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack" &
                               files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% ntnl.rgnl.vrty_30
                           )]
                         
                         
                       })), na.rm = TRUE)
  
  # M 3 (iii) a
  # International companies							varieties available in 10% of stores, see word file for the list
  intnl.vrty_30 <-
    unlist(strsplit(maize_vrty_categories.csv$Maize_variety2[
      which( maize_vrty_categories.csv$Category %in% "International" &
               maize_vrty_categories.csv$N30 %in% 1 )],
      ";"))
  
  table_3[[compLevel]][["International:MEAN_Seed price per 2kg (in TSH)"]] <-
    mean(unlist(sapply(compLevel.data$agroid,
                       function(x) {
                         # Get varieties per agrodealer in 2kg package and in 
                         #   International N30 (10% of stores) list
                         x.vrtys.data <-
                           files.dta$long_stockedmaize_tz.dta$seed_pprice[which(
                             files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                               files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack" &
                               files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% intnl.vrty_30
                           )]
                         
                         
                       })), na.rm = TRUE)
  
  # M 3 (iii) b
  # standard deviation
  table_3[[compLevel]][["International:SD_Seed price per 2kg (in TSH)"]] <-
    sd(unlist(sapply(compLevel.data$agroid,
                     function(x) {
                       # Get varieties per agrodealer in 2kg package and in 
                       #   International N30 (10% of stores) list
                       x.vrtys.data <-
                         files.dta$long_stockedmaize_tz.dta$seed_pprice[which(
                           files.dta$long_stockedmaize_tz.dta$agroid %in% x &
                             files.dta$long_stockedmaize_tz.dta$maize_pack %in% "2kg pack" &
                             files.dta$long_stockedmaize_tz.dta$maizevar_stock %in% intnl.vrty_30
                         )]
                       
                       
                     })), na.rm = TRUE)
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
  table_3[[compLevel]][["Reliable supply (% very reliable):Government varieties >30%"]] <-
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
  table_3[[compLevel]][["Reliable supply (% very reliable):National_Regional >30%"]] <-
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
  table_3[[compLevel]][["Reliable supply (% very reliable):International varieties >30%"]] <-
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
  table_3[[compLevel]][["standard deviation Fertilizer types per agro-dealer"]] <-
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

names(table_3)[which(names(table_3) == "> 10 competitors")] <- ">10 competitors"
table_3.template.df <- as.data.frame(table_3.template)
for (tabledistrct in names(table_3)) {
  for (tablerow in names(table_3[[tabledistrct]])){
    
    table_3.template.df[which(table_3.rownames$Table %in% tablerow),
                        which(grepl(tabledistrct, names(table_3.template)))] <- 
      table_3[[tabledistrct]][[tablerow]]
    
    print(paste(which(grepl(tabledistrct, names(table_3.template))), ": ", tabledistrct ))
    print(paste0(which(table_3.rownames$Table %in% tablerow), ": ", tablerow))
  }
}

table_3.template.df
write.csv(table_3.template.df, "tables/table_3.csv")

