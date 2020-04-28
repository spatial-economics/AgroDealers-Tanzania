segmentSpatialLines <- function(sf.roads, max.road.segment=1000) {
  require(sf)
  require(lwgeom)
  # Segment sf Spatial lines into more lines wil a max length of "max.road.segment"
  print("Segmenting roads")
  newID.count <- 1
  for (j in 1:dim(sf.roads)[1]) {
    print(paste(
      "Spatial line",
      j,
      "of length",
      st_length(sf.roads$geometry[j]),
      "meters"
    ))
    # Get dimensions of spatial line
    road.length <- as.numeric(st_length(sf.roads$geometry[j]))
    road.intervals <-
      seq(0, as.integer(road.length), by = max.road.segment) / as.integer(road.length)
    newID <-
      c(newID.count:(newID.count + length(road.intervals) - 1))
    out_spec <-
      data.frame(
        ID = rep(j, times = length(road.intervals)),
        newID = newID,
        start = road.intervals,
        end = c(road.intervals[-1], 1)
      )
    newID.count <- newID.count + length(road.intervals)
    out_geo <- list()
    # Split spatial line
    for (i in 1:nrow(out_spec)) {
      out_geo[[out_spec$newID[i]]] <-
        lwgeom::st_linesubstring(sf.roads$geometry[out_spec$ID[i]],
                                 out_spec$start[i], out_spec$end[i])[[1]]
    }
    # out <- sf::st_sf(out_spec, sf::st_sfc(out_geo[newID]))
    # Save in on variable
    if (j == 1) {
      all.out_geo <- out_geo[newID]
      all.out_spec <-  out_spec
    }
    if (!j == 1) {
      all.out_geo <- c(all.out_geo, out_geo[newID])
      all.out_spec <- rbind(all.out_spec, out_spec)
    }
    # plot(dplyr::select(out, newID))
    
  }
  # Return as sf object
  geometry <- sf::st_sfc(all.out_geo)
  sf::st_sf(all.out_spec,
            geometry,
            crs = st_crs(tza.primary.road.sf))
}


distance.btw.A.and.B <- function(start_points, end_points, distances, 
                                 graph, edges, nodes, crs_ = 4326) {
  # Get distance between start points and end points, along a network
  require(raster)
  require(sf)
  require(tidygraph)
  require(igraph)
  require(dplyr)
  require(tibble)
  require(ggplot2)
  require(units)
  require(tmap)
  require(osmdata)
  require(rgrass7)
  require(link2GI)
  require(nabor)
  
  all.pathdistance <- list()
  all.starttonode <- list()
  all.endtonode <- list()
  plota <- list()
  for (AandB.id in 1:dim(start_points)[1]) {  
    print(AandB.id)
    agrolocation <- st_point(c(start_points[AandB.id,1], 
                               start_points[AandB.id,2])) %>% st_sfc(crs = crs_)
    
    distrctHQlocation <- st_point(c(end_points[AandB.id,1], 
                                    end_points[AandB.id,2])) %>% st_sfc(crs = crs_)
    
    
    # Coordinates of the origin and destination node, as matrix
    coords_o <- agrolocation %>%
      st_coordinates() %>%
      matrix(ncol = 2)
    
    coords_d <- distrctHQlocation %>%
      st_coordinates() %>%
      matrix(ncol = 2)
    
    # Coordinates of all nodes in the network
    nodes <- graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      st_as_sf()
    
    coords <- nodes %>%
      st_coordinates()
    
    # Calculate nearest points on the network.
    node_index_o <- knn(data = coords, query = coords_o, k = 1)
    node_index_d <- knn(data = coords, query = coords_d, k = 1)
    node_o <- nodes[node_index_o$nn.idx, ]
    node_d <- nodes[node_index_d$nn.idx, ]
    
    # Get distance to node
    # start_to_node <- geosphere::distm(start_points, 
    #                                   sf::st_coordinates(node_o)[,-3])
    # end_to_node <- geosphere::distm(end_points,
    #                                 sf::st_coordinates(node_d)[,-3])
    
    #  we use the ID to calculate the shortest path, 
    path <- shortest_paths(
      graph = graph,
      from = node_o$nodeID, # new origin
      to = node_d$nodeID,   # new destination
      output = 'both',
      weights = graph %>% activate(edges) %>% pull(length)
    )
    
    path_graph <- graph %>%
      subgraph.edges(eids = path$epath %>% unlist()) %>%
      as_tbl_graph()
    
    pathdistance1 <- path_graph %>%
      activate(edges) %>%
      as_tibble() %>%
      summarise(length = sum(length))
    
    pathdistance2 <-  as.data.frame(pathdistance1)
    # print(pathdistance2)
    # print(pathdistance2[1,1])
    all.pathdistance[[AandB.id]] <- pathdistance2
    all.starttonode[[AandB.id]] <- node_o # start_to_node
    all.endtonode[[AandB.id]] <-node_d    # end_to_node
    
  }
  
  # return euclidean distance, distance between start point and nearest node,
  # distance between end point and nearest node, network dictance between start and end nodes
  # and the sum of the above minus eulidean distance
  
  list(euclidean_distance = pointDistance(start_points, 
                                          end_points, 
                                          lonlat=TRUE), 
       networkdistance = unname(unlist(all.pathdistance)),
       all.starttonode,
       all.endtonode)
}


graph_to_distance <- function(graph, edges, nodes) {
  require(sf)
  require(tidygraph)
  require(igraph)
  require(dplyr)
  require(tibble)
  require(ggplot2)
  require(units)
  require(tmap)
  require(osmdata)
  require(rgrass7)
  require(link2GI)
  require(nabor)
  
  graph <- graph %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree()) %>%
    mutate(betweenness = centrality_betweenness(weights = length)) %>%
    activate(edges) %>%
    mutate(betweenness = centrality_edge_betweenness(weights = length))
  
  
  distances <- distances(
    graph = graph,
    weights = graph %>% activate(edges) %>% pull(length)
  )
  
  list(distances, graph) 
}



sf_to_tidygraph = function(x, directed = TRUE) {
  require(sf)
  require(tidygraph)
  require(igraph)
  require(dplyr)
  require(tibble)
  require(ggplot2)
  require(units)
  require(tmap)
  require(osmdata)
  require(rgrass7)
  require(link2GI)
  require(nabor)
  
  # convert sf to tidygraph
  # Step 1: Clean the network before calling this function, by breaking loops: 
  # linesonly
  
  # Step 2: Give each edge a unique index
  edges <- x %>%
    mutate(edgeID = c(1:n()))
  
  # Step 3: Create nodes at the start and end point of each edge
  # Step 4: Give each node a unique index
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    dplyr::select(-xy)
  
  # Step 5: Combine the node indices with the edges
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  # Step 6: Remove duplicate nodes
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    dplyr::select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  # Step 7: Convert to tbl_graph
  graph <- tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
  list("edges"=edges, "nodes"=nodes, "graph"=graph)
}


createSurface <- function(road.shp, toRaster, field_, fun_, travel.rate, surface.col, surface_) {
  # Rasterise roads (lines shapefile) 
  tmp1 <- road.shp[which(road.shp@data[,surface.col] %in% surface_),]
  tmp1[["min_meter"]] <- travel.rate
  rasterize(tmp1, toRaster, field=field_, fun=fun_)
}



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


plot.1to1 <- function(data_, xvar, yvar, xmn=0, xmx, ymn=0, ymx) {
  require(reshape2)
  
  plt <- ggplot(data_) +
    geom_point(aes(x= get(xvar), y= get(yvar), colour = Zone)) +
    coord_cartesian(xlim = c(xmn, xmx), 
                    ylim = c(ymn, ymx), 
                    default = TRUE) +
    ggtitle(paste(xvar, yvar, sep = " vs ")) +
    xlab(xvar) +
    ylab(yvar)
  plt
}

plot.1toMany <- function(data_, xvar, yvar, xmn=0, xmx, ymn=0, ymx, ylab_) {
  require(reshape2)
  
  x.vs.y <- melt(data_,
                 id = xvar,
                 measure = yvar)
  plt <- ggplot(x.vs.y, aes(x= get(xvar), value, colour = variable)) +
    coord_cartesian(xlim = c(xmn, xmx), 
                    ylim = c(ymn, ymx), 
                    default = TRUE) + 
    geom_point() +
    ggtitle(paste(xvar, ylab_, sep = " vs ")) +
    xlab(xvar) +
    ylab(ylab_)
  
  plt
}


getUnique <- function(data_, agrosdata_, clusterids, col_names) {
  # Get Unique entries from col_names columns
  output.list <- list()
  for (clstr in clusterids) {
    clstr.agros.id <- agrosdata_$agroid[which(agrosdata_$cluster %in% clstr)]
    # print(clstr.agros.id)
    clstr.agros.fert.rows <-
      data_[which(data_$agroid %in% clstr.agros.id),]
    # print(clstr.agros.fert.rows)
    
    # Get Unique varieties
    output.list[[clstr]] <- unname(unlist(unique(clstr.agros.fert.rows[col_names])))
    # break()
  }
  output.list
}

getCtgryData <- function(data_, categories_, category_, prices_) {
  # Get the average of prices based on whether they are from Goverment, 
  # local or international 
  output.list <- list()
  i <- 0
  for (clstr_ in data_) {
    i <- i + 1
    
    clstr_1 <- clstr_[which(clstr_[,categories_] == category_),]
    # print(clstr_1[prices_])
    
    if (!is.null(dim(clstr_1))) clstr_2 <- mean(as.numeric(clstr_1[,prices_]))
    if (is.null(dim(clstr_1)))  clstr_2 <- as.numeric(clstr_1[prices_])
    
    # print(clstr_2)
    output.list[[i]] <- clstr_2
    
  }
  unlist(output.list)
  
}


getReliability <- function(data_, agrosdata_, clusterids, col_name, col_values){
  # Get the mean values of selected columns, based on one (selected) column
  output.list <- list()
  for (clstr in clusterids) {
    # Get cluster agrodealears id
    clstr.agros.id <- agrosdata_$agroid[which(agrosdata_$cluster %in% clstr)]
    # print(clstr.agros.id)
    
    # Get cluster agrodealears data
    clstr.agros <-
      data_[which(data_$agroid %in% clstr.agros.id),]
    
    output.list[[clstr]] <- 
      c(sapply(col_values, 
             function(x) length(clstr.agros[,col_name][which(clstr.agros[,col_name] %in% x)])),
        sapply(col_values,
               function(x) {length(clstr.agros[,col_name][
                 which(clstr.agros[,col_name] %in% x)]) / length(clstr.agros[,col_name])}
        ))
    
    
    
  }
  # substr(c(col_values, col_values), 1, 3)
  # t(unname(as.data.frame(output.list)))
  output.df <- t(unname(as.data.frame(output.list)))
  colnames(output.df) <- paste(c(col_values, col_values), 
                      rep(c("count", "rate"), each = length(col_values)),
                      sep = "_")
  output.df
}


getClstrMean <- function(data_, agrosdata_, clusterids, seed_ctgries, 
                         seed.ctgry, cmpny.ctgry, col_names1, col_names2) {
  # Get the mean values of selected columns, based on one (selected) column
  output.list <- list()
  for (clstr in clusterids) {
    # Get cluster agrodealears id
    clstr.agros.id <- agrosdata_$agroid[which(agrosdata_$cluster %in% clstr)]
    # print(clstr.agros.id)
    
    # Get cluster agrodealears data
    clstr.agros <-
      data_[which(data_$agroid %in% clstr.agros.id),]
    # print(clstr.agros)
    
    all.entries <- clstr.agros[,col_names1]
    clstr.list <- list()
    i <- 0
    for ( one.entry in 1:length(all.entries)  ) {
      if (length(all.entries) == 0) {
        clstr.list[[1]] <- c(clstr, NA, NA, NA, NA)
        break()
      }
      i <- i + 1
      
      one.entry_gsub <- gsub(" [(]HYBRID[)]", "", all.entries[one.entry])
      unique.cmpny.ctgry <-
        toString(seed_ctgries[,cmpny.ctgry][which( seed_ctgries[,seed.ctgry] %in%
                                                     one.entry_gsub)])
      # print(one.entry)
      # print(one.entry_gsub)
      clstr.list[[i]] <- as.vector(unlist(c(clstr,
                           all.entries[one.entry],
                           unique.cmpny.ctgry,
                           clstr.agros[i,col_names2]
                                      )))
      
      
    }

    tmp <- unname(t(data.frame(clstr.list)))
    colnames(tmp) <- c("Cluster.id", col_names1, "Category", col_names2)
    output.list[[clstr]] <- tmp 
    
    # break()
  }
  output.list
}


getTzaDistrict <- function(clusterdata, agrosdata) {
  require(raster)
  
  # tza.regions <- getData("GADM", country="TZA", level = 1, path = "data/shp")
  tza.districts <- getData("GADM", country="TZA", level = 2, path = "data/shp")
  output.data.frame <- extract(
    tza.districts,
    SpatialPoints(
      data.frame(clusterdata$Longitude,
                 clusterdata$Latitude)
    )
  )
  output.list <- list()
  for (clstr in clusterdata$ClusterID) {
    clstr.agros.id <- agrosdata$agroid[which(agrosdata$cluster %in% clstr)]
    output.list[[clstr]] <- length(clstr.agros.id)
  }
  
  
  zones.list <- list( `Central Zone` = c("Dodoma", "Singida", "Tabora"), 
                      `Coastal Zone` = c("Dar es Salaam", "Lindi", "Morogoro", 
                                         "Mtwara", "Pwani"), 
                      `Lake Zone` = c("Geita", "Kagera", "Mara", "Mwanza", 
                                      "Shinyanga", "Simiyu"), 
                      `Northern Zone` = c("Arusha", "Kilimanjaro", "Manyara", 
                                          "Tanga"), 
                      `Southern Highlands Zone` = c("Iringa", "Mbeya", "Njombe", 
                                                    "Rukwa", "Ruvuma", "Songwe"), 
                      `Western Zone` = c("Katavi", "Kigoma"), 
                      Zanzibar = c("Mjini Magharibi", "Pemba North", 
                                   "Pemba South", "Unguja North", "Unguja South", 
                                   "Zanzibar North", "Zanzibar South and Central", 
                                   "Zanzibar South and Central", "Zanzibar West")
  )
  
  
  zoneC <- vector(mode = "character", 
                              length = length(output.data.frame$NAME_1))
  
  for (tzazone in names(zones.list)) {
    j <- 0
    for (region in output.data.frame$NAME_1) {
      j <- j + 1 
      if (region %in% zones.list[[tzazone]]) zoneC[j] <- tzazone
    }
    
  }
  
  
  data.frame(ClusterID = clusterdata$ClusterID,
             ClusterSize = unlist(output.list),
             Country = output.data.frame$NAME_0, 
             Zone = zoneC,
             Region = output.data.frame$NAME_1,
             District = output.data.frame$NAME_2,
             Longitude = clusterdata$Longitude,
             Latitude = clusterdata$Latitude
             )
} 



agrodealersWithinTime <- function(time2agro.1hr.sum.wgs, district.id, list1hr) {
  
  names1hr_ <- names(list1hr)
  # Replace "30min" with "1hr"
  half.hour <- FALSE
  print(names1hr_)
  if (grepl("30min", names1hr_[1])) {
    half.hour <- TRUE
    
    names1hr <- unlist(lapply( 1:length(names1hr_),
                        FUN = function(x) gsub("30min", "1hr", names1hr_[x])
                        ))
    print("######")
    print(names1hr)
    names(list1hr) <- names1hr
    print(names(list1hr))
    print("$$$$$$")
  }
  
  
  for (i in 1:length(names1hr)) assign(names1hr[i], list1hr[[names1hr[i]]])
  
  traveltime.1hr.zonal_ <- zonal( district.rural.pop, time2agro.1hr.sum.wgs, 
                                  fun = "sum", digits = 2, na.rm = TRUE )
  traveltime.1hr.zonal <- traveltime.1hr.zonal_[which(traveltime.1hr.zonal_[,2] != 0), ]
  # print(traveltime.1hr.zonal_)
  # print(traveltime.1hr.zonal)
  
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
  
  return.list <- list( agros.in.1hr.is0=agros.in.1hr.is0,
        agros.in.1hr.is1=agros.in.1hr.is1, 
        agros.in.1hr.is2=agros.in.1hr.is2, 
        agros.in.1hr.is3=agros.in.1hr.is3, 
        agros.in.1hr.is4=agros.in.1hr.is4, 
        agros.in.1hr.is5=agros.in.1hr.is5, 
        agros.in.1hr.is2plus=agros.in.1hr.is2plus, 
        agros.in.1hr.is3plus=agros.in.1hr.is3plus, 
        agros.in.1hr.is4plus=agros.in.1hr.is4plus, 
        agros.in.1hr.is5plus=agros.in.1hr.is5plus)
  
  list.names <- names(return.list)
  if (half.hour) {
    list.names <- unlist(lapply(1:length(list.names),
           FUN = function(x)list.names[x] <- gsub("1hr", "30min", list.names[x])
    ))
    print("End")
    print(list.names)
    print("End2")
  }
  names(return.list) <- list.names
  return.list
}





