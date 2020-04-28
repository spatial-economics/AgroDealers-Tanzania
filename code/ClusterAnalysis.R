# Calculate Several Measures for Each Agrodealer Cluster
library(foreign)
library(ggplot2)
library(data.table)

source("code/AgroDealearsTZ_functions.R")

stockedfert.dta <- read.dta("data/Esoko safari/data/long_stockedfert_tz.dta")
stockedmaize.dta <- read.dta("data/Esoko safari/data/long_stockedmaize_tz.dta")
agrodealers.clustered <- read.csv("data/tzaagrodealersclustered.csv")
clusters.of.agrodealers <- read.csv("data/ClusterCoords.csv")


clstrs.ferttypes <- getUnique(stockedfert.dta, 
                              agrodealers.clustered, 
                              clusters.of.agrodealers$ClusterID, 
                              "fert_name")

clstrs.maizetypes <- getUnique(stockedmaize.dta, 
                               agrodealers.clustered,
                               clusters.of.agrodealers$ClusterID, 
                               "maizevar_stock")

clstrs.maizetypes_2 <- getUnique(data.frame(stockedmaize_cleaned.sav), 
                                 agrodealers.clustered,
                                 clusters.of.agrodealers$ClusterID,
                                 "maizevar_stock")


clusters.of.agrodealers.output <- getTzaDistrict(clusters.of.agrodealers,
                                                  agrodealers.clustered)

clusters.of.agrodealers.output["fertVrtyNO"] <- as.integer(unlist(lapply(clstrs.ferttypes, length)))
clusters.of.agrodealers.output["fertVrtyType"] <- unlist(lapply(clstrs.ferttypes, toString))

clusters.of.agrodealers.output["mzeVrtyNO"] <- as.integer(unlist(lapply(clstrs.maizetypes, length)))
clusters.of.agrodealers.output["mzeVrtyType"] <- unlist(lapply(clstrs.maizetypes, toString))

clusters.of.agrodealers.output["mzeVrtyNO_2"] <- as.integer(unlist(lapply(clstrs.maizetypes_2, length)))
clusters.of.agrodealers.output["mzeVrtyType_2"] <- unlist(lapply(clstrs.maizetypes_2, toString))


write.csv(clusters.of.agrodealers.output,
          "output/clstrsAgrosAnalysis.csv",
          row.names = FALSE)



maize.max <- max(clusters.of.agrodealers.output$mzeVrtyNO)
fert.max <- max(clusters.of.agrodealers.output$fertVrtyNO)
clstr.maize.max <- max(clusters.of.agrodealers.output$ClusterSize)
clstr.fert.max <- max(clusters.of.agrodealers.output$ClusterSize)

p1 <- ggplot(clusters.of.agrodealers.output) +
  geom_point(aes(x= ClusterSize, y=mzeVrtyNO, colour = Zone)) + 
  coord_cartesian(xlim = c(0, clstr.maize.max), ylim = c(0, maize.max), default = TRUE) 

p2 <- ggplot(clusters.of.agrodealers.output[
  which(clusters.of.agrodealers.output$Zone == "Southern Highlands Zone"),]) +
  geom_point(aes(x= ClusterSize, y=mzeVrtyNO, colour = Zone)) + 
  coord_cartesian(xlim = c(0, clstr.maize.max), ylim = c(0, maize.max), default = TRUE) 

p3 <- ggplot(clusters.of.agrodealers.output[
  which(!clusters.of.agrodealers.output$Zone == "Southern Highlands Zone"),]) +
  geom_point(aes(x= ClusterSize, y=mzeVrtyNO, colour = Zone)) + 
  coord_cartesian(xlim = c(0, clstr.maize.max), ylim = c(0, maize.max), default = TRUE) 

p4 <- ggplot(clusters.of.agrodealers.output) +
  geom_point(aes(x= ClusterSize, y=fertVrtyNO, colour = Zone)) + 
  coord_cartesian(xlim = c(0, clstr.fert.max), ylim = c(0, fert.max), default = TRUE) 

p5 <- ggplot(clusters.of.agrodealers.output[
  which(clusters.of.agrodealers.output$Zone == "Southern Highlands Zone"),]) +
  geom_point(aes(x= ClusterSize, y=fertVrtyNO, colour = Zone)) + 
  coord_cartesian(xlim = c(0, clstr.fert.max), ylim = c(0, fert.max), default = TRUE) 

p6 <- ggplot(clusters.of.agrodealers.output[
  which(!clusters.of.agrodealers.output$Zone == "Southern Highlands Zone"),]) +
  geom_point(aes(x= ClusterSize, y=fertVrtyNO, colour = Zone)) + 
  coord_cartesian(xlim = c(0, clstr.fert.max), ylim = c(0, fert.max), default = TRUE) 

print(p6)

z1 <- gridExtra::marrangeGrob(list(p1,p2,p3), nrow=3, ncol=1, name = "Maize Varieties" )
z2 <- gridExtra::marrangeGrob(list( p4,p5,p6), nrow=3, ncol=1, name = "Fertilizer Varieties"  )
pdf("plot/clusterPlots.pdf")
invisible(lapply(list(z1, z2), print))
dev.off()

png("plot/clusterPlotsMaize.png")
invisible(lapply(list(z1), print))
dev.off()

png("plot/clusterPlotsFert.png")
invisible(lapply(list(z2), print))
dev.off()

ggsave("test.pdf", gridExtra::marrangeGrob(list(p1,p2,p3, p4,p5,p6), nrow=3, ncol=1))






  
