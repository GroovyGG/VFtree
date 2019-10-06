############################### combinedPlotting.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################

# library(ggplot2)
# library(igraph)
# library(ape)
# library(phytools)
# library(phangorn)
# library(dplyr)

###############################

source("R/treePlot.R")
source("R/ringPlot.R")


#' combinePlotting
#'
#' The main plotting function to combine the two parts of the workflow
#'
#' This function is used to plot a tree from the newick input, on top of
#' it plot the factor information according to each strain of the tree
#' and plot as rings around the tree
#'
#'
#' @param inputCSV input the generated csv file with pathogenic potential information
#' of each virulence factor of each strain
#' @param inputNWK input the generated nwk file of the taxonomic information between strains
#' @param inputNum the number of input strains
#' @return Combined the plotting of circular tree and factor ring from the
#' data of tree(newick file) and table(csv/tsv file)
#'
#' @examples
#'
#' @import ggplot2
#' @import ape
#' @import phytools
#' @import igraph
#' @import dplyr
#'
#' @export

combinedPlotting <- function(inputCSV, inputTree, inputNum) {
  # inputCSV <- "table100.csv"
  # inputTree <- "sample100.newick"
  tree_refine_f = 40

  df <- treeInputProcess(inputname = inputTree)
  p <- getNPoints(data = df,ntips = inputNum, refine_factor = tree_refine_f)
  tree_layers <- getLayers(data = df, npoint = p,ntips = inputNum)
  xy_df <- getCoordinates(tree = df, layers = tree_layers, npoint = p)
  final_tree_plot <- NULL
  final_tree_plot <- treePlot(xy_data = xy_df, layer_data = tree_layers, npoint = p)

  # table data process
  center_point <- c(0,0)
  table_for_ring <- tableInputProcess(input_name = inputCSV)
  max_d_of_tree <- max(df$depth)
  table_ring_radius <- getRingRadius(ring_table_data = table_for_ring, tree_outer_radius = max_d_of_tree)
  table_length <- nrow(table_for_ring)
  ring_plot <- NULL
  r_data <- structure(as.vector(table_ring_radius$ring_radius), names=as.vector(table_ring_radius$factor))
  ring_plot <- ringPlot(ring_table_data = table_for_ring, radius_data = r_data, tree_max_depth = max_d_of_tree, tree_plot = final_tree_plot )

  return(ring_plot)

}

# ############################### sample test cases

# plot25 <- NULL
# plot25 <- combinedPlotting("inst/extdata/table25.csv", "inst/extdata/sample25.newick", inputNum = 25)
# plot25
# plot100 <- NULL
# plot100 <- combinedPlotting("inst/extdata/table100.csv", "inst/extdata/sample100.newick", inputNum = 100)
# plot100
# plot150 <- NULL
# plot150 <- combinedPlotting("inst/extdata/table150.csv", "inst/extdata/sample150.newick", inputNum = 150)
# plot150

# ############################### function calls start here

# df <- treeInputProcess(inputname = "sample25.newick")
# refine_f = 40
# p <- getNPoints(data = df,refine_factor = refine_f)
# layers <- getLayers(data = df, npoint =  p, center = c(0,0))
# xy_df <- getCoordinates(data1 = df, data2 = layers, npoint = p)
# final_tree_plot <- NULL
# final_tree_plot <- treePlot(xy_data = xy_df, layer_data = layers, npoint = p)
# center_point <- c(0,0)
# table_for_ring <- tableInputProcess(input_name = "table25.csv")
# max_d_of_tree <- max(df$depth)
# table_ring_radius <- getRingRadius(ring_table_data = table_for_ring, tree_outer_radius = max_d_of_tree)
# table_length <- nrow(table_for_ring)
# r_data <- structure(as.vector(table_ring_radius$ring_radius), names=as.vector(table_ring_radius$factor))
# ring_plot <- ringPlot(ring_table_data = table_for_ring, radius_data = r_data, tree_max_depth = max_d_of_tree, tree_plot = final_tree_plot )
#
# ring_plot

