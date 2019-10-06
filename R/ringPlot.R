############################### ringPlot.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################

# library(dplyr)
# library(ape)
# library(ggplot2)
# library(igraph)
# library(phytools)
# library(phangorn)

###############################

source("R/treePlot.R")

#' @title tableProcess
#'
#' \code{tree_input_process} preprocess the phylo object from the input tree file with newick format
#'
#' @param inputname the string of the location of the input table/csv
#' @return data frame containing all information about the input table/csv
#'
#' @examples
#'

tableInputProcess <- function(input_name){
  table <- read.csv(input_name)
  return(table)
}

#' @title getRingRadius
#'
#'
#' @param ring_table_data data frame containing all information about the input table/csv
#' @param distance_between_tree_and_ring distance between the tree and the ring
#' @param distance_between_ring_and_ring distance between ring and ring
#' @param tree_outer_radius tree outer radius
#' @return data frame containing responding radius and rings
#'
#' @examples
#'
#'
getRingRadius <- function(ring_table_data, distance_between_tree_and_ring = 1, distance_between_ring_and_ring = 1, tree_outer_radius) {
  list_of_factors <- colnames(ring_table_data)
  radius_factors_df <-as.data.frame(list_of_factors)
  colnames(radius_factors_df) <- 'factor'
  radius_factors_df <- dplyr::filter(radius_factors_df, radius_factors_df$factor != "X")
  radius_factors_df <- dplyr::filter(radius_factors_df, radius_factors_df$factor != "id")
  #create a new column named ring_radius for radius info
  radius_factors_df$ring_radius <- 0
  radius_factors_df$ring_radius[1] <-tree_outer_radius + distance_between_tree_and_ring
  num_rings <- nrow(radius_factors_df)
  for(i in c(2:num_rings)) {
    radius_factors_df$ring_radius[i] <- radius_factors_df$ring_radius[i-1] + distance_between_ring_and_ring
  }
  return(radius_factors_df)
}


#' @title getRingData
#'
#' This function is used to create a data frame about ring x and y coordinates
#'
#'  \code{circleFun}
#'
#' @param center_point center of the ring
#' @param table_data input the table from table process
#' @param tree_depth the max depth of the tree
#' @param ring_radius the radius
#' @param refine_factor_for_ring refine factor of the plotting ring
#' @return data frame containing x and y coordinate of one ring with npoint index
#'
#' @examples
#'
#'
getRingData <- function(center_point = c(0,0), table_data, tree_depth, ring_radius, refine_factor_for_ring = 20) {
  table_length <- nrow(table_data)
  ring_radius
  ring_data_point <- circleFun(center = center_point, r = ring_radius , npoints = table_length)
  return(ring_data_point)
}

#' @title ringPlot
#'
#' This function is used to plot the rings of factoer present/absent of that specific tip
#' based on the data from the table
#' loop the column of the table data frame to plot each ring aroud the tree
#'
#'  \code{getRingData}
#'
#' @param ring_table_data data frame from table_processed
#' @param radius_data  data frame of radius and ring
#' @param tree_max_depth the max depth of the tree
#' @param present_rate the rate for ploting the factor as present/absent
#' @param tree_plot the tree plotting from ggplot
#' @return The ring plot based on the tree plot
#'
#' @examples
#'
ringPlot <- function(ring_table_data, radius_data, tree_max_depth, present_rate = 0.5, tree_plot){
  col_factors <- colnames(ring_table_data)
  ring_table_data[,c(3:ncol(ring_table_data))] <- floor(ring_table_data[,c(3:ncol(ring_table_data))] + present_rate)
  ring_plot <- tree_plot
  for(i in col_factors[c(3:length(col_factors))]){
    temp_data = getRingData(table_data = ring_table_data, tree_depth = tree_max_depth, ring_radius = radius_data[i])
    temp_data[,i] <- ring_table_data[,i]
    ring_plot <- ring_plot + geom_point(data = filter(temp_data,temp_data[,i] == 1),aes(colour = "red"))
  }
  return(ring_plot)
}

# function calls start here
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
# r_data <- structure(as.vector(table_ring_radius$ring_radius), names=as.vector(table_ring_radius$factor))
#
# table_length <- nrow(table_for_ring)
# ring_plot <- ringPlot(ring_table_data = table_for_ring, radius_data = r_data, tree_max_depth = max_d_of_tree, tree_plot = final_tree_plot )
#
# ring_plot

