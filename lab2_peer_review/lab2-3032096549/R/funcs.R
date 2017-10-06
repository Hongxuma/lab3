# Some helper functions used in the project

Sum <- function(x){
  # computes the sum of a vector after removing missing values
  re = sum(x, na.rm = TRUE) / length(x)
  return(re)
}

check_na <- function(x){
  # this function checks if there is any missing value in the input vector x.
  re= FALSE
  if(any(is.na(x)))
    re = TRUE
  return(re)
}

max_count <- function(x){
  # this function detects the element that appears the most time in vector x.
  tab = table(x)
  re = as.numeric(names(tab)[which(tab == max(tab))])
  if(length(re) > 1)
    re = sample(re, 1)
  return(re)
}

plot_cluster <- function(ling_zip_na, county_map, state_map, clus){
  # this function plots the clustering results onto U.S contour map.
  # input args:
  #            ling_zip_na: the full lingustic data aggregated with county information
  #            county: the county map data from "maps" package
  #            state_map: the state map data from "maps" package
  #            clus: the clustering result from clustering algorithms.
  clus_mat = cbind(ling_zip_na %>% select(county), clus)
  colnames(clus_mat)[2] <- "cluster";
  clus_re = clus_mat %>% group_by(county) %>% summarise(Clus = max_count(cluster))
  clus_re[,1] = tolower(as.character(clus_re$county))
  county_cluster = merge(clus_re, county_map, by = "county")
  county_cluster <- arrange(county_cluster, group, order)
  p61 = ggplot( county_cluster, aes( x = long , y = lat , group=group ) ) +
    geom_polygon( colour = "grey" , aes( fill = as.factor(Clus) ), size = 0.2 ) +
    expand_limits( x = county_cluster$long, y = county_cluster$lat ) +
    coord_map( "polyconic" ) + 
    labs(fill="Cluster") + geom_path( data = state_map , colour = "red") + theme_map() + scale_fill_manual(values = c("green", "yellow", "blue", "red"))  + theme(legend.position="none")
  return(p61)
}

plot_cluster_county <- function(ling_county1, county_map, state_map, clus){
  # this function plots the clustering results onto U.S contour map.
  # input args:
  #            ling_zip_na: the full lingustic data aggregated with county information
  #            county: the county map data from "maps" package
  #            state_map: the state map data from "maps" package
  #            clus: the clustering result from clustering algorithms.
  clus_re = cbind(ling_county1 %>% select(county), clus)
  colnames(clus_re)[2] <- "Clus";
  clus_re[,1] = tolower(as.character(clus_re$county))
  county_cluster = merge(clus_re, county_map, by = "county")
  county_cluster <- arrange(county_cluster, group, order)
  p61 = ggplot( county_cluster, aes( x = long , y = lat , group=group ) ) +
    geom_polygon( colour = "grey" , aes( fill = as.factor(Clus) ), size = 0.2 ) +
    expand_limits( x = county_cluster$long, y = county_cluster$lat ) +
    coord_map( "polyconic" ) + 
    labs(fill="Cluster") + geom_path( data = state_map , colour = "red") + theme_map() +scale_fill_manual(values = c("green", "yellow", "blue", "red", "orange", "black")) + theme(legend.position="none")
  return(p61)
}

add_row_noise <- function(data, noise){
  # adding row noise to the data matrix
  m = dim(data)[1]; n = dim(data)[2];
  me = apply(data, 1, mean)
  for(i in 1:m){
    data[i,] = data[i,] + rnorm(n, 0, me[i] * noise)
  }
  return(data)
}

add_col_noise <- function(data, noise){
  # adding column noise to the data matrix
  m = dim(data)[1]; n = dim(data)[2];
  me = apply(data, 2, mean)
  for(i in 1:n){
    data[,i] = data[,i] + rnorm(m, 0, me[i] * noise)
  }
  return(data)
}




