############################### ringPlot.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################

source("R/treePlot.R")

###############################

#' tableInputProcess
#'
#' \code{tree_input_process} preprocess the phylo object from the input tree file with newick format
#'
#' @param table the string of the location of the input table/csv
#' @return data frame containing all information about the input table/csv
#'
#' @examples
#' ring_table <- tableInputProcess(Table25)
#'

tableInputProcess <- function(table){
  #table <- read.csv(input_name)
  return(table)
}

#' getRingRadius
#'
#'
#' @param ring_table_data data frame containing all information about the input table/csv
#' @param distance_between_tree_and_ring this is the unit distance/ radius difference between the tree and the first ring
#' @param distance_between_ring_and_ring this is the unit distance/ radius difference between rings
#' @param tree_outer_radius tree outer radius
#' @return data frame containing responding radius and rings
#'
#' @examples
#' getRingRadius(ring_table_data = Table25,
#'             distance_between_tree_and_ring = 1,
#'             distance_between_ring_and_ring = 1,
#'             tree_outer_radius = 5)
#'
#' @importFrom dplyr filter
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


#' getRingData
#'
#' This function is used to create a data frame about the points that form the ring
#' with their x, y coordinates
#'
#'  \code{circleFun}
#'
#' @param center_point center of the ring
#' @param table_data input the table from table process
#' @param tree_depth the max depth of the tree
#' @param ring_radius the radius
#' @param ring_refine_factor refine factor of the plotting ring
#' @return data frame containing x and y coordinate of one ring with npoint index
#'
#' @examples
#'getRingData <- function(center_point = c(0,0),
#'                        table_data = Table25,
#'                        tree_depth = 4,
#'                        ring_radius = 10,
#'                        ring_refine_factor = 20)
#'
#'
#'

getRingData <- function(center_point = c(0,0), table_data, tree_depth, ring_radius, ring_refine_factor = 20) {
  table_length <- nrow(table_data)
  ring_data_point <- circleFun(center = center_point, r = ring_radius , npoints = table_length)
  return(ring_data_point)
}


#' ringPlot
#'
#' This function is used to plot the rings of factoer present/absent of that
#' specific strain mapped in the tree based on the data from the table,
#' the same id name mapped to tree data loop the column of the ring_table_data
#' to plot each ring aroud the tree
#'
#'
#' @param ring_table_data data frame from table_processed
#' @param radius_data  data frame of radius and ring
#' @param tree_max_depth the max depth of the tree
#' @param present_rate the rate for ploting the factor as present/absent
#' @param tree_plot the tree plotting from ggplot
#' @return The ring plot based on the tree plot
#'
#' @examples
#' ring_data <- getRingData <- function(center_point = c(0,0),
#'                        table_data = Table25,
#'                        tree_depth = 4,
#'                        ring_radius = 10,
#'                        ring_refine_factor = 20)
#' ringPlot <- function(ring_table_data = Table25,
#'                     radius_data = ring_data,
#'                     tree_max_depth = 5,
#'                     present_rate = 0.5,
#'                     tree_plot = base_plot)
#'
#' @import ggplot2
#'
ringPlot <- function(ring_table_data, radius_data, tree_max_depth, present_rate = 0.5, tree_plot){
  col_factors <- colnames(ring_table_data)
  ring_table_data[,c(3:ncol(ring_table_data))] <- floor(ring_table_data[,c(3:ncol(ring_table_data))] + present_rate)
  ring_plot <- tree_plot
  for(i in col_factors[c(3:length(col_factors))]){
    temp_data = getRingData(table_data = ring_table_data, tree_depth = tree_max_depth, ring_radius = radius_data[i])
    temp_data[,i] <- ring_table_data[,i]
    ring_plot <- ring_plot + ggplot2::geom_point(data = filter(temp_data,temp_data[,i] == 1),aes(colour = "red"))
  }
  return(ring_plot)
}


