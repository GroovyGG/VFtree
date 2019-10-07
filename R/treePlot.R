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

#' treeInputProcess
#'
#' \code{tree_input_process} preprocess the phylo object from the input tree file with newick format
#'
#' @param tree the string of the location of the input tree
#' @return data frame containing all information about the input tree file
#'
#' @examples
#' treeInputProcess(Tree25)
#' treeInputProcess(Tree150)
#'
#' @importFrom ape read.tree
#' @importFrom igraph graph
#' @importFrom igraph bfs
#' @importFrom igraph as_ids
#' @importFrom dplyr setdiff
#' @importFrom dplyr filter
#' @import phytools
#'
#' @export

treeInputProcess <- function(tree){
  # inputname = "inst/extdata/sample150.newick"

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


#' circleFun
#'
#' This function is usend to collecting a data frame of points which together form
#' a circle based on the diameter/radius and angle according to x-y axis
#'
#' the built in "pi" is not exactly equal to qi due to rounding,
#  data of last point will almost covered by the first point with a y distance of "-2.449294e-15"
#'
#' @param center the center of the circle
#' @param r radius of the circle
#' @param npoints  a sequence of number with a distance of "by" by = ((to - from)/(length.out - 1)),
#' @return data frame of points with their x, y coordinates information that form the circle
#'
#' @examples
#' c1 <- circleFun(center = c(0,0), r = 10, npoints = 100)
#'
#'
#' @source
#'  https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
#' @export

circleFun <- function(center = c(0,0), r = 10, npoints = 100){
  # seq function: generate a sequence of number with a distance of "by"
  evenly_divided <- seq(0,2*pi,length.out = npoints)
  # get x and y coordinate of each point
  xx <- center[1] + r * cos(evenly_divided)
  yy <- center[2] + r * sin(evenly_divided)
  return(data.frame(x = xx, y = yy))
}


#' @title getNPoints
#'
#' This function is used to caculate the number of points for each circular
#' shaped data frame such as the rings and each circular layer of the tree
#'
#' The calculation here is based on num_tips is equal to
#'
#' @param ntips this is the input size of the strain/tree tips
#' @param refine_factor factor of how many points plotted each circle
#' @return The total number of points of each circular shape data
#'
#'
getNPoints <- function (ntips, refine_factor = 100){
  # num_p is the number of points of each layer
  num_p <- ntips * refine_factor
  #num_p <- num_tips * refine_factor + 1
  return(num_p)
}


#' getLayers
#'
#' This function is used to generate the layer data frame,
#' The layer data frame composed of x, y coordinate for tree layer
#' of different depth/radius points data.
#'
#' call circle function in this function for each circular data frame
#' and use rbind to stack all data together in one data frame, mark its
#' radius in an additional column named layer_id.
#'
#' @param data tree data frame without x,y coordinate function
#' @param npoint the total number of points of each layer
#' @param ntips this is the input size of the strain/tree tips
#' @param center the center of the circle
#' @param unit_r the radius difference between tree layers
#' @return num_p is the number of points of each layer
#'
#' @examples
#' layer_df <- getLayers(data = tree_df_25 ,
#'                       npoint = 25 * 40,
#'                       ntips = 25,
#'                       center = c(0,0),
#'                       unit_r = 1)
#'
#' @export
#'
getLayers <- function (data, npoint, ntips, center = c(0,0), unit_r = 1) {

  max_d <- max(data$depth)
  layers <- data.frame()
  for(i in c(1:max_d)){
    # The outer layer just need the tips/points the npoints
    if(i == 1) {
      outer <- circleFun(center = center, r = max_d * unit_r, npoints = ntips + 1)
      # layer_id = max_depth means this is the outer layer of the tree for tips
      outer$layer_id <- max_d
      layers <- rbind(layers, outer) #rbind is used to row bind more data frame into one
    }else{
      inner_r <- (max_d - i + 1) * unit_r
      inner <- circleFun(center = center, r = inner_r, npoints = npoint + 1)
      inner$layer_id <- (max_d - i + 1)
      layers <- rbind(layers, inner)
    }
  }
  return(layers)
}


#' getCoordinates
#'
#' This function take input of the tree data and layer data, output the tree
#' data frame with two more columns contain the x,y coordinate info of each node
#'
#' @param tree tree data frame without x,y coordinate
#' @param layers layer data frame contaions all layers data
#' @param npoint the number of points xy coordinates data of each layer
#' @return updated tree data frame two new columns of x, y coordinates
#'
#' @examples
#'
#' xy_data <- getCoordinates(tree = tree_df_25,
#'                layers = tree_layers_25, npoint = 25 * 40)
#'
#' @importFrom dplyr filter
#' @export
#'
getCoordinates <- function (tree, layers, npoint) {
  max_d  <- max(layers$layer_id)
  for(i in tree$id){

    if(tree$c1[i] == -1) { # for all tips
      sub1 <- dplyr::filter(layers,layers$layer_id == max_d)
      tree$x[i] <- sub1$x[i]
      tree$y[i] <- sub1$y[i]
    }else {  # for all internal nods
      sub2 <- dplyr::filter(layers,layers$layer_id == tree$depth[i])
      index <- tree$angle[i]/360 * npoint
      tree$x[i] <- sub2$x[index]
      tree$y[i] <- sub2$y[index]
    }
  }
  root_data <- dplyr::filter(tree, tree$depth == 0)
  root <- root_data$id[1]
  tree$x[root] <- 0
  tree$y[root] <- 0

  return(tree)
}


#' nodeGroup
#'
#' This function is adding a new layer plot of a nodeGroup on the previous plot
#' each time plot one node. a nodeGroup of the internal node is composed of the node,
#' the arc/horisontal line along with it and the path to its parent node, the tips
#' nodeGroup is similar to internal node but without the arc line.
#'
#' @param tree tree data frame with updated x y coordinate information
#' @param layers layer data frame contaions all layers data
#' @param idx the index of the point in tree data frame
#' @param plot_arg pass the base plot to adding more layer on top of it
#' @param npoint the number of points of each layer
#' @param tip this flag is for detecting whether the point is tip or internal node
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @export
#'
nodeGroup <- function(tree, layers, idx, plot_arg, npoint, tip = FALSE) {
  # Vertical line / radiate line connect its parent to itself: require depth_length column and angle
  # calculate start point x and y
  target <- dplyr::filter(tree, tree$id == idx)
  st_x <- target$x
  st_y <- target$y
  # parent depth = self depth - depth length
  if(tip == FALSE) {
    p_depth <- target$depth - target$depth_length
  }else{
    max_d = max(tree$depth)
    p_depth <- max_d - target$depth_length
  }
  # calculate end point x and y
  one_layer <- dplyr::filter(layers, layers$layer_id == p_depth)
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
    node_df <- dplyr::filter(layers ,layers$layer_id == target$depth)
    # plot the arc along the internal node
    plot_res <- plot_res + ggplot2::geom_path(data = node_df[c(c1_index:c2_index),], ggplot2::aes(colour = "segment"))
  }

  return(plot_res)
}


#' treePlot
#'
#' This function is used to inital a base tree ploting using ggplot2
#' This function is composed of two parts: the base tree plotting with
#' only nodes present without any lines/paths and a loop which calling
#' the nodeGroup function to adding data and add a nodeGroup each time
#' by adding a layer of the ggplot
#'
#' @param xy_data tree data frame with updated x y coordinate information
#' @param layer_data layer data frame contaions all layers data
#' @param npoint the number of points of each layer
#' @return the plot of the tree part
#'
#' @import ggplot2
#' @importFrom dplyr filter
#'
#' @export
#'
treePlot <- function(xy_data, layer_data, npoint) {
  np <- npoint
  root_data <- dplyr::filter(xy_data, xy_data$depth == 0)
  root <- root_data$id[1]
  # remove roots here by filtered the tree df
  no_root_data <- dplyr::filter(xy_data, xy_data$id != root)

  # https://felixfan.github.io/ggplot2-remove-grid-background-margin/
  base_plot <- ggplot2::ggplot(xy_data, aes(x = xy_data$x, y = xy_data$y)) + ggplot2::coord_fixed(ratio = 1) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  result_plot <- base_plot +
    ggplot2::geom_point(aes(x = 0, y = 0), colour = "blue") +
    ggplot2::geom_point(data = no_root_data, aes(x = no_root_data$x, y = no_root_data$y), colour = "red", size = 0.3)

  for (i in no_root_data$id){
    result_plot <- nodeGroup(tree = no_root_data, layers = layer_data, idx = i, plot_arg = result_plot, npoint = np, tip = (i < root))
  }
  return(result_plot)

}


