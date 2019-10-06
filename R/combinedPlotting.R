############################### combinedPlotting.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################


library(ggplot2)
library(igraph)
library(ape)
library(phytools)
library(phangorn)
library(dplyr)

source("R/treePlot.R")
source("R/ringPlot.R")


#' The main plotting function to combine the two parts of the workflow
#'
#' \code{combinedPlotting} The function will generate a newick file with relationships between genome
#'
#' @param inputCSV the generated csv file with present and absent of VF of each strain
#' @param inputNWK the generated nwk file of the relationship between strains
#' @return Combined plotting of the data from csv and tree from newick file
#'
#' @examples
#'
#' @import ggplot2
#' @import ape
#' @import phytools
#' @import igraph
#' @import phangorn
#' @import dplyr
combinedPlotting <- function(inputCSV, inputTree) {
  # inputCSV <- "table100.csv"
  # inputTree <- "sample100.newick"
  tree_refine_f = 40

  df <- tree_input_process(inputname = inputTree)
  p <- getNPoints(data = df,refine_factor = tree_refine_f)
  layers <- getLayers(data = df, npoint = p)
  xy_df <- getCoordinates(data1 = df, data2 = layers, npoint = p)

  final_tree_plot <- NULL
  final_tree_plot <- treePlot(xy_data = xy_df, layer_data = layers, npoint = p)

  # table data process
  center_point <- c(0,0)
  table_for_ring <- tableProcess(input_name = inputCSV)
  max_d_of_tree <- max(df$depth)
  table_ring_radius <- getRingRadius(ring_table_data = table_for_ring, tree_outer_radius = max_d_of_tree)
  table_length <- nrow(table_for_ring)
  ring_plot <- NULL
  r_data <- structure(as.vector(table_ring_radius$ring_radius), names=as.vector(table_ring_radius$factor))
  ring_plot <- ringPlot(ring_table_data = table_for_ring, radius_data = r_data, tree_max_depth = max_d_of_tree, tree_plot = final_tree_plot )

  return(ring_plot)

}

#test cases
plot1 <- NULL
plot1 <- combinedPlotting("inst/extdata/table100.csv", "inst/extdata/sample100.newick")
plot1
# plot2 <- NULL
# plot2 <- combinedPlotting("table25.csv", "sample25.newick")
# combinedPlotting("table150.csv", "sample150.newick")


# function calls start here
# df <- input_process(inputname = "sample25.newick")
# refine_f = 40
# p <- getNPoints(data = df,refine_factor = refine_f)
# layers <- getLayers(data = df, npoint =  p, center = c(0,0))
# xy_df <- getCoordinates(data1 = df, data2 = layers, npoint = p)
# final_tree_plot <- NULL
# final_tree_plot <- treePlot(xy_data = xy_df, layer_data = layers, npoint = p)
# center_point <- c(0,0)
# table_for_ring <- tableProcess(input_name = "table25.csv")
# max_d_of_tree <- max(df$depth)
# table_ring_radius <- getRingRadius(ring_table_data = table_for_ring, tree_outer_radius = max_d_of_tree)
# table_length <- nrow(table_for_ring)
# r_data <- structure(as.vector(table_ring_radius$ring_radius), names=as.vector(table_ring_radius$factor))
# ring_plot <- ringPlot(ring_table_data = table_for_ring, radius_data = r_data, tree_max_depth = max_d_of_tree, tree_plot = final_tree_plot )
#
# ring_plot
