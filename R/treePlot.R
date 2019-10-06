############################### treePlot.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################

# library(dplyr)
# library(ape)
# library(ggplot2)
# library(igraph)
# library(phytools)


###############################

#' @title tree_input_process
#'
#' \code{tree_input_process} preprocess the phylo object from the input tree file with newick format
#'
#' @param inputname the string of the location of the input tree
#' @return data frame containing all information about the input tree file
#'
#' @examples
#'
#' @import ape
#' @import igraph
#' @import dplyr
#' @import phytools
tree_input_process <- function(inputname = "sample1.newick"){
  # inputname = "sample150.newick"
  tree <-(ape::read.tree(inputname))
  rt1 <- dplyr::setdiff(tree$edge[,1],tree$edge[,2]) # find Root from "phylo" object tree
  leaves <- dplyr::setdiff(tree$edge[,2],tree$edge[,1]) #find Leaves from "phylo" object tree

  num_nodes = tree$Nnode # number of internal node tree$Nnode
  num_tips = tree$Nnode + 1 # number of tips tree$Node + 1
  num_edges = nrow(tree$edge) # number of edge also nrow(tree$edge) node + tips - 1

  graph1 <- igraph::graph(t(tree$edge), n = num_nodes+num_tips, directed = TRUE) # construct graph from "phylo" object tree
  max_depth <- max(igraph::bfs(graph1,root = rt1, dist = TRUE)$dist) # find the max depth of the tree
  bfs_result <- igraph::bfs(graph1,root = rt1, father = TRUE, dist = TRUE) # find depth and parent using bfs
  depth <- bfs_result$dist
  parent <- igraph::as_ids(bfs_result$father)
  # Construct the data frame of nodes
  df <- as.data.frame(depth)
  colnames(df) <- 'depth'
  df$id <- c(1:(num_nodes + num_tips))
  df$parent <- parent
  # calculate angles of each nodes and tips, add to dataframe
  df$angle <- -1
  next_list <- c()
  cur_list <-leaves
  while(length(cur_list) > 1) {
    next_list <- c()
    for( i in cur_list){
      if(i %in% leaves) {
        df$angle[i] <- (360/num_tips) * (i-1)
      }
      else {
        child <- dplyr::filter(df, df$parent == i)
        df$angle[i] <- sum(child$angle)/2
      }
      if (!is.na(df$parent[i])) {
        next_list <- c(next_list,df$parent[i])
      }
    }
    cur_list <- next_list
  }
  # add two columns of child1 and child2 store the childrens of the node
  df$c1 <- -1
  df$c2 <- -1
  for(i in df$id) {
    for( j in df$id) {
      # make sure to skip the root with is.na
      if( !is.na(df$parent[i]) && df$id[j] == df$parent[i] ) {
        if(df$c1[j] == -1){
          df$c1[j] <- i
        }else{
          df$c2[j] <- i
        }
      }
    }
  }
  # add two emply columns to store child_angle for each node
  df$c1_a <- -1
  df$c2_a <- -1
  for(i in df$id) {
    if(df$c1[i] != -1){
      c1 <- df$c1[i]
      c2 <- df$c2[i]
      df$c1_a[i] <- df$angle[c1]
      df$c2_a[i] <- df$angle[c2]
    }
  }
  # add the calculated depth_diff between parent and self fo internal nodes
  df$depth_length <- -1
  for(i in df$id) {
    if(!i %in% leaves){ # if it is a internal node
      p_id <- df$parent[i]
      # calculate the absolute value of (parent depth - self depth)
      df$depth_length[i] <- abs(df$depth[p_id] - df$depth[i])
    }else{  # if it is a tip/leave
      p_id <- df$parent[i]
      p_depth <- df$depth[p_id]
      df$depth_length[i] <-max_depth - p_depth
    }
  }
  df$depth_length[rt1] <- 0
  return(df)
}



#' @title circleFun
#'
#' collecting data frame of arc/point based on the diameter/radius and angle according to x-y axis
#' \code{circleFun} circle function for collecting a dataset of dots for a circle
#'
#' @param center center of the circle
#' @param r radius of the circle
#' @param npoints  a sequence of number with a distance of "by" by = ((to - from)/(length.out - 1)),
#' @return data frame of dots x and y coordinate for the circle
#'
#' @examples
#'

# modified based on source code https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center = c(0,0), r = 10, npoints = 100){
  # seq function is used to generate a sequence of number with a distance of "by"
  tt <- seq(0,2*pi,length.out = npoints)
  # get x and y coordinate of each point
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


#' @title getNPoints
#'
#' \code{getNPoints}
#'
#' @param data tree data frame
#' @param refine_factor factor of how many points plotted each circle
#' @return num_p is the number of points of each layer
#'
#' @examples
#'
getNPoints <- function (data,refine_factor = 100){
  # num_p is the number of points of each layer
  num_tips <- (nrow(data) + 1 )/2
  num_p <- num_tips * refine_factor
  return(num_p)
}

#' @title getLayers
#'
#' \code{circleFun} call circle function in this function for
#'
#' @param data tree data frame
#' @param npoint the number of points of each layer
#' @param center the center of the circle
#' @return num_p is the number of points of each layer
#'
#' @examples
#'
getLayers <- function (data, npoint, center = c(0,0)) {

  max_d <- max(data$depth) # n is the max_depth
  unit_r <- 1
  num_tips <- (nrow(data) + 1 )/2
  layers <- data.frame()
  #initiate a empty vector to store layers of data frame of circular plotting data
  for(i in c(1:max_d)){
    # The outer layer just need the tips/points the npoints
    if(i == 1) {
      outer <- circleFun(center = center, r = max_d, npoints = num_tips + 1)
      # layer_id = max_depth means this is the outer layer of the tree for tips
      outer$layer_id <- max_d
      layers <- rbind(layers, outer) #rbind is used to row bind more data frame into one
    }else{
      inner_r <- (max_d - i + 1)
      inner <- circleFun(center = center, r = inner_r, npoints = npoint)
      inner$layer_id <- (max_d - i + 1)
      layers <- rbind(layers, inner)
    }
  }
  return(layers)

}

#' @title getCoordinates
#'
#'  This function take input of the tree data and layer data and autput the tree
#'  data with coordinate info of each node get Coordinates
#'
#' @param data1 tree data frame
#' @param data2 layer data frame contaions all layers data
#' @param npoint the number of points of each layer
#' @return updated tree data frame with x and y coordinates
#'
#' @examples
#'
getCoordinates <- function (data1, data2, npoint) {
  max_d  <- max(data2$layer_id)
  for(i in data1$id){

    if(data1$c1[i] == -1) { # for all tips
      sub <- dplyr::filter(data2,data2$layer_id == max_d)
      data1$x[i] <- sub$x[i]
      data1$y[i] <- sub$y[i]
    }else {  # for all internal nods
      sub <- dplyr::filter(data2,data2$layer_id == data1$depth[i])
      index <- data1$angle[i]/360 * npoint
      data1$x[i] <- sub$x[index]
      data1$y[i] <- sub$y[index]
    }
  }
  root_data <- dplyr::filter(data1, data1$depth == 0)
  root <- root_data$id[1]
  data1$x[root] <- 0
  data1$y[root] <- 0
  #print(data1)
  return(data1)
}


#' @title nodeGroup
#'
#'  This function is adding node plot of a nodeGroup
#'  each time plot one node. a node group include the node,
#'  the arc around it and the path to its parent node
#'
#' @param data1 tree data frame with updated x y coordinate information
#' @param data2 layer data frame contaions all layers data
#' @param idx the index of the point in tree data frame
#' @param plot_arg pass the base plot to adding more layer on top of it
#' @param npoint the number of points of each layer
#' @param tip this flag is for detecting wjether the point is tip or internal node
#'
#' @examples
#'
nodeGroup <- function(data1, data2, idx, plot_arg, npoint, tip = FALSE) {
  # Vertical line / radiate line connect its parent to itself: require depth_length column and angle
  # calculate start point x and y
  target <- dplyr::filter(data1, data1$id == idx)
  st_x <- target$x
  st_y <- target$y
  # parent depth = self depth - depth length
  if(tip == FALSE) {
    p_depth <- target$depth - target$depth_length
  }else{
    max_d = max(data1$depth)
    p_depth <- max_d - target$depth_length
  }
  # calculate end point x and y
  one_layer <- dplyr::filter(data2, data2$layer_id == p_depth)
  index <- floor(target$angle/360 * npoint) + 1

  if(target$depth == 1){
    ep_x <- 0
    ep_y <- 0
  }else {
    ep_x <- one_layer$x[index]
    ep_y <- one_layer$y[index]
  }

  # construct a df with start point and end point coordinates
  node_data <- data.frame(st_x, st_y, ep_x, ep_y)

  plot_res <- plot_arg + ggplot2::geom_segment(aes(x = st_x, y = st_y, xend = ep_x , yend = ep_y, colour = "segment"), data = node_data)

  # Horizontal/arc line connect two support child point require c1_a and c2_a and depth/radius
  if(tip == FALSE) {
    c1_index <- target$c1_a/360 * npoint
    c2_index <- target$c2_a/360 * npoint
    node_df <- dplyr::filter(data2 ,data2$layer_id == target$depth)
    # plot the arc along the internal node
    plot_res <- plot_res + ggplot2::geom_path(data = node_df[c(c1_index:c2_index),], ggplot2::aes(colour = "segment"))
  }

  return(plot_res)
}


#' @title treePlot
#'
#'  The base tree plotting and a while loop which calling the nodeGroup
#'  and add a nodeGroup each time by adding a layer of the ggplot
#'
#' @param xy_data tree data frame with updated x y coordinate information
#' @param layer_data layer data frame contaions all layers data
#' @param npoint the number of points of each layer
#' @param tip this flag is for detecting wjether the point is tip or internal node
#' @return the plot of the tree part
#'
#' @examples
#'
#' @import ggplot2
#' @import dplyr
#'
treePlot <- function(xy_data, layer_data, npoint) {

  root_data <- dplyr::filter(xy_data, xy_data$depth == 0)
  root <- root_data$id[1]
  # remove roots here by filtered the tree df
  no_root_data <- dplyr::filter(xy_data, xy_data$id != root)
  # https://felixfan.github.io/ggplot2-remove-grid-background-margin/
  base_plot <- ggplot2::ggplot(xy_data, aes(x, y)) + ggplot2::coord_fixed(ratio = 1) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  result_plot <- base_plot +
    ggplot2::geom_point(aes(x = 0, y = 0), colour = "blue") +
    ggplot2::geom_point(data = no_root_data, colour = "red", size = 0.3)
  for (i in no_root_data$id){
    result_plot <- nodeGroup(data1 = no_root_data, data2 = layer_data, idx = i, plot_arg = result_plot,np = npoint, tip = (i < root))

  }
  return(result_plot)
}


