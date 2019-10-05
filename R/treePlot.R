library(dplyr)
library(ape)
library(ggplot2)
library(igraph)
library(phytools)
library(phangorn)


###########################
# 1.preprocess the phylo object from the input
# require phytools and igraph
tree <-(read.tree("sample1.newick"))
root <- setdiff(tree$edge[,1],tree$edge[,2]) # find Root from "phylo" object tree
leaves <- setdiff(tree$edge[,2],tree$edge[,1]) #find Leaves from "phylo" object tree

graph1 <- graph(t(tree$edge), n = 11, directed = TRUE) # construct graph from "phylo" object tree
max_depth <- max(bfs(graph1,root = 7, dist = TRUE)$dist) # find the max depth of the tree
bfs_result <- bfs(graph1,root = 7, father = TRUE, dist = TRUE) # find depth and parent using bfs
depth <- bfs_result$dist
parent <- as_ids(bfs_result$father)

num_nodes = tree$Nnode # number of internal node tree$Nnode
num_tips = tree$Nnode + 1 # number of tips tree$Node + 1
num_edges = nrow(tree$edge) # number of edge also nrow(tree$edge) node + tips - 1

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
      df$angle[i] <- (360/num_tips) * i
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
  print(cur_list)
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
  if(i %in% leaves){ # if it is a internal node
    p_id <- df$parent[i]
    # calculate the absolute value of (parent depth - self depth)
    df$depth_length[i] <- abs(df$depth[p_id] - df$depth[i])
  }else{  # if it is a tip/leave
    p_id <- df$parent[i]
    p_depth <- df$depth[p_id]
    df$depth_length[i] <-max_depth - p_depth
  }
}
print(df) # example result below


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

#try <- circleFun(center = center_point, r = 20, npoints = num_tips + 1)

###########################
# 4. Start Plotting using ggplot

# This function is used to add x and y coordinate of all points(nodes + tips)

getCoordinates <- function (df, refine_factor = 100) {

  # n is the max_depth
  refine_factor <- 100
  max_d <- max(df$depth)
  unit_r <- 1

  num_p <- num_tips * refine_factor  # num_p is the number of points of each layer
  # generate x and y coordinate based on layers and tree_fd
  layers <- data.frame()

  #initiate a empty vector to store layers of data frame of circular plotting data
  for(i in c(1:max_d)){
    # The outer layer just need the tips/points the npoints
    if(i == 1) {
      outer <- circleFun(center = center_point, r = max_d, npoints = num_tips + 1)
      # layer_id == 1 means this is the outer layer of the tree for tips
      outer$layerid <- max_d
      layers <- rbind(layers, outer)
    }else{
      inner_r <- (max_d - i + 1)
      inner <- circleFun(center = center_point, r = inner_r, npoints = num_p)
      inner$layerid <- (max_d - i + 1)
      layers <- rbind(layers, inner)
    }
  }
  # "layers" is composed of n data_frame where n is the max depth of the tree
  for(i in df$id){

    if(i %in% leaves) { # for all tips
      sub <-filter(layers,layers$layerid == max_d)
      df$x[i] <- sub$x[i]
      df$y[i] <- sub$y[i]
    }else {  # for all internal nods
      sub <- filter(layers,layers$layerid == df$depth[i])
      index <- df$angle[i]/360 * num_p
      df$x[i] <- sub$x[index]
      df$y[i] <- sub$y[index]
    }
  }
  return(df)
}

###########################
# 5. Plot the tree based on the tree.dataframe we have constructed above
# This part require ggplot2 functions

treePlot <- function(df) {

  # initiate a ggplot object here
  #remove roots here by filtered the tree df
  node_df <- filter(df,df$id != root)
  plot <- ggplot2::ggplot(df,aes(x, y )) + ggplot2::coord_fixed(ratio = 1)
  plot <- plot + ggplot2::geom_point(aes(x = 0, y = 0), colour = "blue") + geom_point(data = node_df, colour = "red")
  # plot <- plot + ggplot2::geom_point(aes(x = 0, y = 0), colour = "blue") + geom_point(data = filter(df,df$c1 == -1), colour = "red")
  # plot <- plot + geom_point(data = filter(filtered_df, filtered_df$c1 != -1 ), colour = "green")

  for (i in node_df$id){
    if(i %in% leaves){ # plot the tips
      nodeGroup(i)
    }else{ # plot the node_group
      tipGroup(i)
    }

  }
}

nodeGroup <- function(df, layers, i, plot) {


  # Vertical line / radiate line connect its parent to itself
  # Vertical line: require depth_length column and angle



  # Horizontal/arc line connect two support child point
  # Vertical line require c1_a and c2_a and depth/radius
  c1_index <- df$c1_a[i]/360 * num_p
  c2_index <- df$c2_a[i]/360 * num_p

  node_df <- filter(layers ,layers$layerid == df$depth[i])
  # plot the arc along the internal node
  plot <- plot + geom_path(data = node_df[c(c1_index:c2_index),])



}

tipGroup <- function(df, i) {

  # node itself

  # Vertical line / radiate line connect its parent to itself
  # Vertical line require depth_length column and angle



}
