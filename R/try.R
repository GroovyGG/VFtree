

library(ggplot2)
library(ggtree)
library(igraph)
library(ape)
library(phytools)
library(phangorn)
library(dplyr)


# try
temp_tree <- rtree(12)
temp_data <- data.frame(Strain = letters[1:12],vf1 = runif(12,0,1), vf2 = runif(12,0,1))
draw_tree <- ggtree(temp_tree,layout ="circular", branch.length='none',size = 0.1) %<+% temp_data
draw_tree <- draw_tree + geom_tiplab(aes(angle =angle),color = 'blue', size=5, linesize=0.3) + theme_tree2()
plot(draw_tree)

root <- setdiff(temp_tree$edge[,1],temp_tree$edge[,2])




#generate two data inputs
tree <-(read.tree("sample1.newick"))

mydata <- data.frame(Strain = letters[1:6],vf1 = runif(6,0,1), vf2 = runif(6,0,1))

# try plotting now
tree0 <- ggtree(tree,layout ="circular", branch.length='none',size = 0.1) %<+% mydata
tree0 <- tree0 + geom_tiplab(aes(angle =angle),color = 'blue', size=5, linesize=0.3) + theme_tree2()
print(tree0$data)
plot(tree0)

# find Root
root <- setdiff(tree$edge[,1],tree$edge[,2])

#find Leaves
leaves <- setdiff(tree$edge[,2],tree$edge[,1])


# construct graph
#
graph1 <- graph(t(tree$edge), n = 11, directed = TRUE)

# find the max depth of the tree
max_depth <- max(bfs(graph1,root = 7, dist = TRUE)$dist)
bfs_result <- bfs(graph1,root = 7, father = TRUE, dist = TRUE)
depth <- bfs_result$dist
parent <- as_ids(bfs_result$father)

# number of internal node tree$Nnode
num_nodes = tree$Nnode
# number of tips tree$Node + 1
num_tips = tree$Nnode + 1
# number of edge also nrow(tree$edge) node + tips - 1
num_edges = nrow(tree$edge)


# Calculate the unit_length of radiote_plot

# Create a data frame of nodes
# The data frame should having titles like: depth id parent angle c1 c2 c1_angle c2_angle
df <- as.data.frame(depth)
colnames(df) <- 'depth'
df$id <- c(1:(num_nodes + num_tips))
df$parent <- parent

# calculate angles of each nodes and tips
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

# add two empty columns of child1 and child2
df$c1 <- -1
df$c2 <- -1
print(df)
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

# add two emply columns to store child_angle for ploting the arc
df$c1_a <- -1
df$c2_a <- -1


print(df)

# https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2

circleFun <- function(center = c(0,0), diameter = 10, npoints = 100){
  r = diameter / 2
  # seq function is used to generate a sequence of number with a distance of "by"
  # by = ((to - from)/(length.out - 1)),
  tt <- seq(0,2*pi,length.out = npoints)

  # get x and y coordinate of each point
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

center_point <- c(0,0)
unit_length <- 2
base_diameter <- unit_length * max_depth
point_num <- (tree$Nnode + 1) * 60

tree_tips <- circleFun(center_point,base_diameter, num_tips + 1)
depth_four_tips <- circleFun(center_point,base_diameter - unit_length , num_tips + 1)
depth_three_tips <- circleFun(center_point,base_diameter - unit_length , num_tips + 1)

depth_four_data <- circleFun(center_point,base_diameter - unit_length , (num_tips) * 60)
depth_three_data <- circleFun(center_point, base_diameter - 2 * unit_length, (num_tips)*60)
depth_two_data <- circleFun(center_point, base_diameter - 3 * unit_length, (num_tips)*60)
depth_one_data <- circleFun(center_point, base_diameter - 4 * unit_length, (num_tips)*60)

# Cartesian coordinates with fixed "aspect ratio" https://ggplot2.tidyverse.org/reference/coord_fixed.html
# plotting all leaves

# base_circle is composed of a blue center point and n nums of tips point with diameter = unit_length * max_depth of tree
base_circle <- ggplot(tree_tips, aes(x , y)) + geom_point()+ coord_fixed(ratio = 1) + geom_point(aes(x = 0, y = 0), colour = "blue")
base_circle <- base_circle + geom_point(data = depth_four_tips)
base_circle <- base_circle + geom_point(data = depth_four_tips[c(1,4,5,6),])

add_node <- base_circle + geom_point(data = depth_four_data[c(90),], colour = "red") # node 11
add_node <- add_node + geom_path(data = depth_four_data[c(60:120),]) # arc around node 11

add_node <- add_node + geom_point(data = depth_three_data[c((90+180)/2),], colour = "red") # node 10
add_node <- add_node + geom_path(data = depth_three_data[c(90:180),]) # arc around node 10

add_node <- add_node + geom_point(data = depth_two_data[c( ((90+180)/2 + 240)/2 ),], colour = "red") # node 9 parent of 10 and 5
add_node <- add_node + geom_path(data = depth_two_data[c( ((90+180)/2):240),]) # arc around node 9

add_node <- add_node + geom_point(data = depth_one_data[c( (((90+180)/2 + 240)/2 ) + 300)/2,], colour = "red") # node 9 parent of 10 and 5
add_node <- add_node + geom_path(data = depth_one_data[c( ((90+180)/2):240),]) # arc around node 9



#dat2 <- circleFun(c(0,0),9,npoints = 360)
# geom_path() connects the observations in the order in which they appear in the data.
# plot circle use geom_path()
#plot two circles
#p1 <- ggplot(dat1,aes(x,y)) + geom_path () + geom_path(data = dat2[c(1:60),], color = "red") + xlim(c(-6,6)) + ylim(c(-6,6))

#p1 <- ggplot(dat1,aes(x,y)) + geom_dotplot(aes(x,y),dat1, binwidth = 1.5, dotsize = 2)
#p2 <- ggplot(dat1,aes(x , y)) + geom_point()+ xlim(c(-6,6)) + ylim(c(-6,6))

# p1 <- p1 + ggplot(dat1[c(1:4),],aes(x,y)) + geom_path( ) + xlim(c(-6,6)) + ylim(c(-6,6))

# aa <- seq(0,2*pi,length.out = 100)





