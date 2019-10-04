

library(ggplot2)
library(ggtree)
library(igraph)
library(ape)
library(phytools)
library(phangorn)


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
graph1 <- graph(t(tree$edge), n = 11, directed = TRUE)

# find the max depth of the tree
max(bfs(graph1,root = 7, dist = TRUE)$dist)
bfs_result <- bfs(graph1,root = 7, father = TRUE, dist = TRUE)
depth <- bfs_result$dist
parent <- bfs_result$father


# Calculate the unit_length of radiote_plot

# Create a data frame of nodes
df <- as.data.frame(depth)
colnames(df) <- 'depth'
df$parent <- parent
print(df)




# https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
center_point <- c(0,0)
circleFun <- function(center = c(0,0), diameter = 10, npoints = 100){
  r = diameter / 2
  # seq function is used to generate a sequence of number with a distance of "by"
  # by = ((to - from)/(length.out - 1)),
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


dat1 <- circleFun(c(0,0),10,npoints = 20)
#dat2 <- circleFun(c(0,0),9,npoints = 360)
# geom_path() connects the observations in the order in which they appear in the data.
# plot circle use geom_path()
#plot two circles
#p1 <- ggplot(dat1,aes(x,y)) + geom_path () + geom_path(data = dat2[c(1:60),], color = "red") + xlim(c(-6,6)) + ylim(c(-6,6))

#p1 <- ggplot(dat1,aes(x,y)) + geom_dotplot(aes(x,y),dat1, binwidth = 1.5, dotsize = 2)
#p2 <- ggplot(dat1,aes(x , y)) + geom_point()+ xlim(c(-6,6)) + ylim(c(-6,6))
# Cartesian coordinates with fixed "aspect ratio" https://ggplot2.tidyverse.org/reference/coord_fixed.html
# plotting all leaves
p2 <- ggplot(dat1,aes(x , y)) + geom_point()+ coord_fixed(ratio = 1)
p2 <- p2 + geom_point(aes(x = 0, y = 0), colour = "blue")


pp1 <- p1 + ggplot(dat1[c(1:4),],aes(x,y)) + geom_path( ) + xlim(c(-6,6)) + ylim(c(-6,6))
aa <- seq(0,2*pi,length.out = 100)





