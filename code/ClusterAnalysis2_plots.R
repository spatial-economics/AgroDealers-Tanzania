
clusters.of.agrodealers.output_2c <- clusters.of.agrodealers.output_2
names(clusters.of.agrodealers.output_2c) <- c("ClusterID", "ClusterSize", "Country", "Zone",
                                              "Region", "District", "Longitude", "Latitude",
                                              "Time2Road", "Dist2AllRoad", "Dist2PriRoad", 
                                              "Dist2SecRoad", "Goverment.seed_pprice", 
                                              "Domestic.seed_pprice", "International.seed_pprice",
                                              "Goverment.seed_selprice", "Domestic.seed_selprice", 
                                              "International.seed_selprice", "Goverment.fert_cost", 
                                              "Domestic.fert_cost", "International.fert_cost", 
                                              "Goverment.fertprice", "Domestic.fertprice", 
                                              "International.fertprice", "Very.reliable_count.maize", 
                                              "Not.reliable_count.maize", "Averagely.reliable_count.maize", 
                                              "Very.reliable_rate.maize", "Not.reliable_rate.maize", 
                                              "Averagely.reliable_rate.maize", "Very.reliable_count.fert", 
                                              "Not.reliable_count.fert", "Averagely.reliable_count.fert", 
                                              "Very.reliable_rate.fert", "Not.reliable_rate.fert", 
                                              "Averagely.reliable_rate.fert")


clstr.max <- max(clusters.of.agrodealers.output_2c$ClusterSize)
Dist2AllRoad.max <- max(clusters.of.agrodealers.output_2c$Dist2AllRoad)



p1 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "ClusterSize",
                   yvar = c("Goverment.seed_pprice",
                            "Domestic.seed_pprice",
                            "International.seed_pprice",
                            "Goverment.seed_selprice",
                            "Domestic.seed_selprice",
                            "International.seed_selprice"),
                   xmx = clstr.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$International.seed_selprice, na.rm = TRUE),
                   ylab_ = "Seed Prices")

p2 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "ClusterSize",
                   yvar = c("Goverment.fert_cost", 
                            "Domestic.fert_cost", 
                            "International.fert_cost", 
                            "Goverment.fertprice", 
                            "Domestic.fertprice", 
                            "International.fertprice"),
                   xmx = clstr.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$International.seed_selprice, na.rm = TRUE),
                   ylab_ = "Fertilizer Prices")

p3 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "Dist2AllRoad",
                   yvar = c("Goverment.seed_pprice",
                            "Domestic.seed_pprice",
                            "International.seed_pprice",
                            "Goverment.seed_selprice",
                            "Domestic.seed_selprice",
                            "International.seed_selprice"),
                   xmx = Dist2AllRoad.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$International.seed_selprice, na.rm = TRUE),
                   ylab_ = "Seed Prices")

p4 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "Dist2AllRoad",
                   yvar = c("Goverment.fert_cost", 
                            "Domestic.fert_cost", 
                            "International.fert_cost", 
                            "Goverment.fertprice", 
                            "Domestic.fertprice", 
                            "International.fertprice"),
                   xmx = Dist2AllRoad.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$International.seed_selprice, na.rm = TRUE),
                   ylab_ = "Fertilizer Prices")

p5 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "ClusterSize",
                   yvar = c("Very.reliable_rate.maize",
                            "Not.reliable_rate.maize",
                            "Averagely.reliable_rate.maize"
                   ),
                   xmx = clstr.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$Very.reliable_rate.maize, na.rm = TRUE),
                   ylab_ = "Maize Supply Reliability")

p6 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "ClusterSize",
                   yvar = c("Very.reliable_rate.fert",
                            "Not.reliable_rate.fert",
                            "Averagely.reliable_rate.fert"
                   ),
                   xmx = clstr.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$Very.reliable_rate.fert, na.rm = TRUE),
                   ylab_ = "Fertilizer Supply Reliability")

p7 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "Dist2AllRoad",
                   yvar = c("Very.reliable_rate.maize",
                            "Not.reliable_rate.maize",
                            "Averagely.reliable_rate.maize"
                   ),
                   xmx = Dist2AllRoad.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$Very.reliable_rate.maize, na.rm = TRUE),
                   ylab_ = "Maize Supply Reliability")

p8 <- plot.1toMany(data_ = clusters.of.agrodealers.output_2c,
                   xvar = "Dist2AllRoad",
                   yvar = c("Very.reliable_rate.fert",
                            "Not.reliable_rate.fert",
                            "Averagely.reliable_rate.fert"
                   ),
                   xmx = Dist2AllRoad.max ,
                   ymx = max(clusters.of.agrodealers.output_2c$Very.reliable_rate.fert, na.rm = TRUE),
                   ylab_ = "Fertilizer Supply Reliability")
p <- 0
plots.list <- list()
xvariables <- c("ClusterSize", "Dist2AllRoad", "Dist2PriRoad")
yvariables <-  c("Goverment.seed_pprice", 
                 "Domestic.seed_pprice", "International.seed_pprice",
                 "Goverment.seed_selprice", "Domestic.seed_selprice", 
                 "International.seed_selprice", "Goverment.fert_cost", 
                 "Domestic.fert_cost", "International.fert_cost", 
                 "Goverment.fertprice", "Domestic.fertprice", 
                 "International.fertprice", "Very.reliable_count.maize", 
                 "Not.reliable_count.maize", "Averagely.reliable_count.maize", 
                 "Very.reliable_rate.maize", "Not.reliable_rate.maize", 
                 "Averagely.reliable_rate.maize", "Very.reliable_count.fert", 
                 "Not.reliable_count.fert", "Averagely.reliable_count.fert", 
                 "Very.reliable_rate.fert", "Not.reliable_rate.fert", 
                 "Averagely.reliable_rate.fert")

for (xvariable in xvariables) {
  for (yvariable in yvariables) {
    p <- p + 1
    plots.list[[p]] <- plot.1to1(data_ = clusters.of.agrodealers.output_2c,
                                 xvar = xvariable,
                                 yvar = yvariable,
                                 xmx = max(clusters.of.agrodealers.output_2c[,xvariable], na.rm = TRUE),
                                 ymx = max(clusters.of.agrodealers.output_2c[,yvariable], na.rm = TRUE))
  }
}



ggsave("plot/ClusterPlots_2a.pdf", gridExtra::marrangeGrob(list(p1,p2,p3, p4,p5,p6, p7, p8), nrow=1, ncol=1))

ggsave("plot/ClusterPlots_2b.pdf", gridExtra::marrangeGrob(plots.list, nrow=1, ncol=1))



# print(p6)
# 
# z1 <- gridExtra::marrangeGrob(list(p1,p2,p3), nrow=3, ncol=1, name = "Maize Varieties" )
# z2 <- gridExtra::marrangeGrob(list( p4,p5,p6), nrow=3, ncol=1, name = "Fertilizer Varieties"  )
# pdf("plot/clusterPlots.pdf")
# invisible(lapply(list(z1, z2), print))
# dev.off()
# 
# png("plot/clusterPlotsMaize.png")
# invisible(lapply(list(z1), print))
# dev.off()
# 
# png("plot/clusterPlotsFert.png")
# invisible(lapply(list(z2), print))
# dev.off()
# 