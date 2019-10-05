library(dplyr)
library(ape)
library(ggplot2)
library(igraph)
library(phytools)
library(phangorn)


###########################
# 1.preprocess the phylo object from the input
# require phytools and igraph
# return data frame
input_process <- function(inputname = "sample1.newick"){
  tree <-(read.tree(inputname))
  rt1 <- setdiff(tree$edge[,1],tree$edge[,2]) # find Root from "phylo" object tree
  leaves <- setdiff(tree$edge[,2],tree$edge[,1]) #find Leaves from "phylo" object tree

  num_nodes = tree$Nnode # number of internal node tree$Nnode
  num_tips = tree$Nnode + 1 # number of tips tree$Node + 1
  num_edges = nrow(tree$edge) # number of edge also nrow(tree$edge) node + tips - 1

  graph1 <- graph(t(tree$edge), n = num_nodes+num_tips, directed = TRUE) # construct graph from "phylo" object tree
  max_depth <- max(bfs(graph1,root = rt1, dist = TRUE)$dist) # find the max depth of the tree
  bfs_result <- bfs(graph1,root = rt1, father = TRUE, dist = TRUE) # find depth and parent using bfs
  depth <- bfs_result$dist
  parent <- as_ids(bfs_result$father)

  ###########################
  # 2.Construct the data frame of nodes
  # The data frame should having titles like: depth id parent angle c1 c2 c1_angle c2_angle

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
        child <- filter(df, df$parent == i)
        df$angle[i] <- sum(child$angle)/2
      }
      if (!is.na(df$parent[i])) {
        next_list <- c(next_list,df$parent[i])
      }
    }
    cur_list <- next_list
    #print(cur_list)
  }

  # add two columns of child1 and child2 store the childrens of the node
  df$c1 <- -1
  df$c2 <- -1
  for(i in df$id) {
    for( j in df$id) {
      # make sure to skip the root with is.na
      if( !is.na(df$parent[i]) && df$id[j] == df$parent[i] ) {
        if(df$c1[j] == -1){
          # assign first child to parent
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
  df$depth_length[root] <- 0

  print(df) # example result below
  return(df)
}


###########################
# 3. use a circular function for plot circle/fan/arc shape
# this function take inputs of the center point, the diameter and the # of points of the polygon/circle
# return a data frame of (x,y) of a circle/circular shape/ polygon shape
# plot arc/point based on the diameter/radius and angle according to x-y axis

# source code https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center = c(0,0), r = 10, npoints = 100){
  # seq function is used to generate a sequence of number with a distance of "by"
  # by = ((to - from)/(length.out - 1)),
  tt <- seq(0,2*pi,length.out = npoints)

  # get x and y coordinate of each point
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}



###########################
# 4. Start Plotting using ggplot

# This function is used to add x and y coordinate of all points(nodes + tips)

getNPoints <- function (data,refine_factor = 100){
  # num_p is the number of points of each layer
  num_tips <- (nrow(data) + 1 )/2
  num_p <- num_tips * refine_factor
  return(num_p)
}

#np is the number of points
getLayers <- function (data, npoint) {

  # n is the max_depth
  max_d <- max(data$depth)
  unit_r <- 1
  num_tips <- (nrow(data) + 1 )/2
  # generate x and y coordinate based on layers and tree_fd
  layers <- data.frame()
  #initiate a empty vector to store layers of data frame of circular plotting data
  for(i in c(1:max_d)){
    # The outer layer just need the tips/points the npoints
    if(i == 1) {
      outer <- circleFun(center = center_point, r = max_d, npoints = num_tips + 1)

      # layer_id == depth of the ring means this is the outer layer of the tree for tips
      outer$layer_id <- max_d
      layers <- rbind(layers, outer)
    }else{
      inner_r <- (max_d - i + 1)
      inner <- circleFun(center = center_point, r = inner_r, npoints = npoint)
      inner$layer_id <- (max_d - i + 1)
      layers <- rbind(layers, inner)
    }
  }
  return(layers)

}

#np is the number of points
getCoordinates <- function (data1, data2, npoint) {
  max_d  <- max(data2$layer_id)
  # "layers" is composed of n data_frame where n is the max depth of the tree
  for(i in data1$id){

    if(data1$c1[i] == -1) { # for all tips
      sub <-filter(data2,data2$layer_id == max_d)
      data1$x[i] <- sub$x[i]
      data1$y[i] <- sub$y[i]
    }else {  # for all internal nods
      sub <- filter(data2,data2$layer_id == data1$depth[i])
      index <- data1$angle[i]/360 * npoint
      data1$x[i] <- sub$x[index]
      data1$y[i] <- sub$y[index]
    }
  }
  root_data <- filter(data1, data1$depth == 0)
  root <- root_data$id[1]
  data1$x[root] <- 0
  data1$y[root] <- 0
  print(data1)
  return(data1)
}


###########################
# 5. Plot the tree based on the tree.dataframe we have constructed above
# This part require ggplot2 functions

# data 1 is xy_df data
# data 2 is layer data

nodeGroup <- function(data1, data2, idx, plot_arg, npoint, tip = FALSE) {
  # Vertical line / radiate line connect its parent to itself: require depth_length column and angle
  # calculate start point x and y
  st_x <- data1$x[idx]
  st_y <- data1$y[idx]
  # parent depth = self depth - depth length

  if(tip == FALSE) {
    p_depth <- data1$depth[idx] - data1$depth_length[idx]
    # print("This is node and p_depth is:")
    # print(p_depth)

  }else{
    max_d = max(data1$depth)
    print("max_d is:")
    print(max_d)
    p_depth <- max_d - data1$depth_length[idx]
    # print("This is tip and p_depth is:")
    # print(p_depth)
  }
  print("idk is:")
  print(idx)

  # calculate end point x and y
  filtered <- filter(data2, data2$layer_id == p_depth)
  # print(filtered)
  index <- floor(data1$angle[idx]/360 * npoint) + 1
  print("This is index:")
  print(index)
  if(data1$depth[idx] == 1){
    ep_x <- 0
    ep_y <- 0
  }else {
    ep_x <- filtered$x[index]
    ep_y <- filtered$y[index]
  }

  # construct a df with start point and end point coordinates
  node_data <- data.frame(st_x, st_y, ep_x, ep_y)
  print("this is node_data:")
  print(node_data)
  plot_res <- plot_arg + geom_segment(aes(x = st_x, y = st_y, xend = ep_x , yend = ep_y, colour = "segment"), data = node_data)

  # Horizontal/arc line connect two support child point require c1_a and c2_a and depth/radius
  if(tip == FALSE) {
    c1_index <- df$c1_a[idx]/360 * npoint
    c2_index <- df$c2_a[idx]/360 * npoint
    print("tip == false")

    node_df <- filter(data2 ,data2$layer_id == df$depth[idx])
    # plot the arc along the internal node
    plot_res <- plot_res + geom_path(data = node_df[c(c1_index:c2_index),])
  }

  return(plot_res)
}


treePlot <- function(xy_data, layer_data, npoint) {

  root_data <- filter(xy_data, xy_data$depth == 0)
  root <- root_data$id[1]
  print("root is")
  print(root)
  # initiate a ggplot object here
  #remove roots here by filtered the tree df
  node_df <- filter(xy_data,xy_data$id != root)
  base_plot <- ggplot2::ggplot(xy_data,aes(x, y )) + ggplot2::coord_fixed(ratio = 1)
  result_plot <- base_plot + ggplot2::geom_point(aes(x = 0, y = 0), colour = "blue") + geom_point(data = node_df, colour = "red")
  # plot <- plot + ggplot2::geom_point(aes(x = 0, y = 0), colour = "blue") + geom_point(data = filter(df,df$c1 == -1), colour = "red")
  # plot <- plot + geom_point(data = filter(filtered_df, filtered_df$c1 != -1 ), colour = "green")

  for (i in node_df$id){
    result_plot <- nodeGroup(data1 = xy_data, data2 = layer_data, idx = i, plot_arg = result_plot,np = npoint, tip = (i < root))

  }
  return(result_plot)
}


df <- input_process(inputname = "sample.newick")
refine_f = 100
p <- getNPoints(data = df,refine_factor = refine_f)
layers <- getLayers(data = df, npoint =  p)
xy_df <- getCoordinates(data1 = df, data2 = layers, npoint = p)

final_plot <- treePlot(xy_data = xy_df, layer_data = layers, npoint = p)


final_plot

root <- 26
node_df <- filter(xy_df,xy_df$id != root)
base_plot <- ggplot2::ggplot(xy_df,aes(x, y )) + ggplot2::coord_fixed(ratio = 1)
result_plot <- base_plot + ggplot2::geom_point(aes(x = 0, y = 0), colour = "blue") + geom_point(data = node_df, colour = "red")

# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 1, plot_arg = result_plot, tip = TRUE)
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 2, plot_arg = result_plot, tip = TRUE)
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 3, plot_arg = result_plot, tip = TRUE)
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 4, plot_arg = result_plot, tip = TRUE) # wrong!
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 5, plot_arg = result_plot, tip = TRUE) # wrong!
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 6, plot_arg = result_plot, tip = TRUE) # wrong!
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 8, plot_arg = result_plot, tip = FALSE)
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 9, plot_arg = result_plot, tip = FALSE)
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 10, plot_arg = result_plot, tip = FALSE)
# result_plot <- nodeGroup(data1 = xy_df, data2 = layers, idx = 11, plot_arg = result_plot, tip = FALSE)

