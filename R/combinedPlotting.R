#' The main plotting function to combine the two parts of the workflow
#'
#' \code{vftreePlotting} The function will generate a newick file with relationships between genome
#'
#' @param inputCSV the generated csv file with present and absent of VF of each strain
#' @param inputNWK the generated nwk file of the relationship between strains
#' @return A newick file contain the phylogenetic relationships between all strains
#'
#' @examples
#' inputCSV <- system.file("extdata/testdata", "RRHreads.fasta",
#' package = "VFtree")
#' inputNWK <- system.file("extdata/testdata", "RRHreads.fasta",
#' package = "VFtree")
#'
#' @import ggplot2
#' @import ggtree
#' @import dplyr
#'

#

library(ggplot2)
library(ggtree)
library(igraph)
library(ape)
library(phytools)
library(phangorn)

# fp <- "inst/extdata/"
# nwkFile <- read.tree(paste0(fp, "core_gene_tree.newick"))
# fp <- "inst/extdata/"
# meta <- read.csv(paste0(fp, "ggplot-DB-5.csv"),na.strings = c("","NA"), stringsAsFactors=FALSE)

# randomTree <- rtree(25)
tree <-(read.tree("sample1.newick"))
# randomTree <- rtree(6)
mydata <- data.frame(Strain = letters[1:6],vf1 = runif(6,0,1), vf2 = runif(6,0,1))
tree0 <- ggtree(randomTree,layout ="circular", branch.length='none',size = 0.1) %<+% mydata
tree0 <- tree0 + geom_tiplab(aes(angle =angle),color = 'blue', size=5, linesize=0.3) + theme_tree2()
plot(tree0)

# tree1 <- ggtree(randomTree,layout ="circular", branch.length='dN',size = 0.1) %<+% mydata
# tree2 <- ggtree(randomTree,layout ="circular", branch.length='rate',size = 0.1) %<+% mydata
cowplot::plot_grid(tree0, tree1,tree2,ncol = 3, labels = LETTERS[1:3])


new <- subset(meta,select = -c(Category2, Organism.Roary, BapsOne, BapsTwo))

mydata <- data.frame(Strain = letters[1:25],vf1 = runif(25,0,1), vf2 = runif(25,0,1))

# meta <- read.csv(paste0(fp, "ggplot-DB-5.csv"), header = TRUE, na.strings = c("","NA"), stringsAsFactors=FALSE)

#tree <- ggtree(nwkFile, size = 0.05) %<+% meta  + geom_treescale(fontsize = 3, x=100, y=0)
tree <- ggtree(nwkFile,layout ="circular", branch.length='none',size = 0.1) %<+% meta
#tree <- tree + geom_treescale(x=20, y=10, width = 10, offset = 10,color = "red", fontsize = 1)
tree <- tree + geom_tiplab(aes(angle =angle),color = 'blue', size=0.5, linesize=0.3) + theme_tree2()
#plot(tree)
#tree <- ggtree(treeFile, layout = 'circular', size = 0.1) %<+% meta + geom_treescale(fontsize = 3, x=100, y=0)

rownames(meta) <- meta$Strain

#p1 <- gheatmap(tree, meta[, "Category2", drop=F], width = 0.03, offset=10, colnames = FALSE)

p2 <- gheatmap(tree, meta[, "HemolysinIII", drop=F], width=0.03, offset=10, colnames = FALSE)
p3 <- gheatmap(p2, meta[, "HemolysinA", drop=F], width=0.03, offset=12, colnames = FALSE)
p4 <- gheatmap(p3, meta[, "PhospholipaseC", drop=F], width=0.03, offset=14, colnames = FALSE)
p5 <- gheatmap(p4, meta[, "nhe", drop=F], width=0.03, offset=16, colnames = FALSE)
p6 <- gheatmap(p5, meta[, "EntFM", drop=F], width=0.03, offset=18, colnames = FALSE)
p7 <- gheatmap(p6, meta[, "bceT", drop=F], width=0.03, offset=20, colnames = FALSE)
p8 <- gheatmap(p7, meta[, "HBL", drop=F], width=0.03, offset=22, colnames = FALSE)
p9 <- gheatmap(p8, meta[, "Cytk1", drop=F], width=0.03, offset=24, colnames = FALSE)
p10 <- gheatmap(p9, meta[, "HemolysinII", drop=F], width=0.03, offset=26, colnames = FALSE)
p11 <- gheatmap(p10, meta[, "Cereulide", drop=F], width=0.03, offset=28, colnames = FALSE)

plot(p11)


combinedPlotting <- (inputCSV, inputNWK) {

}

#' helper function of compare the elements of csv and nwk return a boolean value for matck or not
#'
#' \code{vftreePlotting} The function will generate a newick file with relationships between genome
#'
#' @param inputCSV the generated csv file with present and absent of VF of each strain
#' @param inputNWK the generated nwk file of the relationship between strains
#' @return a boolean value for wheather the elements are matched or not
#'
#' @examples
#' inputCSV <- system.file("extdata/testdata", "RRHreads.fasta",
#' package = "VFtree")
#' inputNWK <- system.file("extdata/testdata", "RRHreads.fasta",
#' package = "VFtree")
#'
#' @import phytools


elementMatching <- function(inputCSV, inputNWK) {

  #check if the input format are correct
  #read inputs
  phy <- phytools::read.newick(inputNWK)
  element <- read.csv(inputCSV)


  # Check if they contains exactely same elements

  # Assign an ordered list with nwk order to csv file

  # Plot the tree

  # plot the annotation of VF respond to eack strain

  # display


}
elementMatching(inputCSV = "insta/extdata/ref/test.csv",inputNWK = "insta/extdata/ref/test_file.nwk")


# # function from from https://stackoverflow.com/questions/36000716/ggtree-change-radius-scale
# transformtree<-function(tree,radialparameter,repeatparameter,tiplength){
#
#   # radialparameter # # change this to collapse less(0.5) or more (3) and modify repeatparameter together
#   # repeatparameter # # i.e. increase if there are very small branches (levels)
#
#   #number of hierarchical levels in tree
#   df<-as.data.frame(tree$edge)
#   tree1 <- FromDataFrameNetwork(df)# data.tree package
#   levels <- ToDataFrameTable(tree1, "level")
#   edgelevels <- max(levels)-1
#
#   # establish the hierarchy of nodes looking for the children of the children nodes
#
#   centralnode <- getMRCA(tree,1:length(tree$tip.label))
#   childrenlist <- list()
#   childrenlist[1] <- list(phangorn::Children(tree, centralnode))
#
#   for (i in 2:edgelevels){
#     childrenlist[i]<- list(unlist(lapply(unlist(childrenlist[i-1]), function(x) phangorn::Children(tree, x) ) ) )
#   }
#
#   # remove nodes of tips, we do not want to modify their length
#   childrentipsremoved <- lapply(childrenlist, function(x) x[!is.element(x,1:length(tree$tip.label))])
#   # list of inner nodes
#   groupedinnernodes <- rlist::list.clean(childrentipsremoved, fun = function(x) length(x) == 0L)
#
#   # this is the vector that will multiply the inner edges
#   transfvector <- rep(((c(1:(length(groupedinnernodes)/repeatparameter))^(-radialparameter) )*5),
#                       each=repeatparameter)
#
#   # check length of groups of inner nodes and the transformation vector
#   lengths <- unlist(lapply(groupedinnernodes, function(x) length(x)) )
#
#   if(length(lengths)-length(transfvector)>0) {
#     for (i in 1:abs(length(lengths)-length(transfvector) )   ){
#       transfvector <- c(transfvector,transfvector[length(transfvector)])
#     }
#   }
#
#   if(length(lengths)-length(transfvector)<0) {
#     for (i in 1:abs(length(lengths)-length(transfvector) ) ){
#       transfvector <- transfvector[-1]
#     }
#   }
#
#   # create the factor to transform the inner edges
#   vector1<-unlist(mapply(rep, transfvector,lengths) )
#
#   # discard length info, replace all edge length information by 1
#   size<-length(tree$edge.length)
#   tree$edge.length<-rep(1,size)
#
#   # replace edge length for the connecting inner nodes only
#   innernodes<-unlist(groupedinnernodes)
#   tree$edge.length[unlist(lapply(innernodes,function(x,y) which(y==x),y=tree$edge[,2]) )]<-
#     tree$edge.length[unlist(lapply(innernodes,function(x,y) which(y==x),y=tree$edge[,2]) )]*
#     vector1
#
#   # modify length of tip edges # optional decrease for big trees
#   tree$edge.length[tree$edge.length==1]<-tiplength
#   return(tree)
# }

# findRoot
root <- setdiff(tree$edge[,1],randomTree$edge[,2])

# construct graph
graph1 <- graph(tree$edge, n = 49, directed = TRUE)

# find the max depth of the tree
max(bfs(graph1,root = 7, dist = TRUE)$dist)

# Calculate the unit_length of radiote_plot




