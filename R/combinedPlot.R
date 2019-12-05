############################### combinedPlotting.R
# Date: 5 October 2019
# Author: Bihan Zhu <bihan.zhu@mail.utoronto.ca>

###############################

source("R/treePlot.R")
source("R/ringPlot.R")

###############################

#' combinePlot
#'
#' The main plotting function to combine the two parts of the workflow
#'
#' This function is used to plot a tree from the newick input, on top of
#' it plot the factor information according to each strain of the tree
#' and plot as rings around the tree
#'
#'
#' @param inputTable input the generated csv file with pathogenic potential information
#' of each virulence factor of each strain
#' @param inputTree input the generated nwk file of the taxonomic information between strains
#' @param inputNum the number of input strains
#' @return Combined the plotting of circular tree and factor ring from the
#' data of tree(newick file) and table(csv/tsv file)
#'
#' @examples
#' \dontrun{
#' result_plot <- combinedPlot(inputTable = VF25_2,
#'                                 inputTree = Tree25,
#'                                 inputNum = 25)
#' }
#'
#' @import ggplot2
#' @importFrom ape read.tree
#' @import phytools
#' @import dplyr
#'
#' @export

combinedPlot <- function(inputTable, inputTree, inputNum) {

  # Check input length
  if(length(inputTree$tip.label) == inputNum && nrow(inputTable) == inputNum){
    message("All input length are the same")
  }else{
    print(nrow(inputTable))
    print(length(inputTree$tip.label))
    stop("The inputTable should match with type data.frame")
  }

  # Check input class
  if (class(inputTable) != "data.frame"){
    stop("The inputTable should match with type data.frame")
  }

  if (class(inputTable) != "data.frame"){
    stop("The inputTree should match with type phylo")
  }

  # Cross-check labels with each other:
  if(all(inputTable$id %in% inputTree$tip.label)){
    message("All labels in table match with label in newick file")

  }else{
    print(inputTable$id)
    print(inputTree$tip.label)
    stop("The labels from two files are not matching")
  }

  # Cross-check labels with each other:
  if(all(inputTree$tip.label %in% inputTable$id)){
    message("All labels in newick file match with label in table")

  }else{
    print(inputTable$id)
    print(inputTree$tip.label)
    stop("The labels from two files are not matching")
  }

  tree_refine_f = 40

  tree_df <- treeInputProcess(tree = inputTree)
  message("Treeinput Processing")

  p <- getNPoints(ntips = inputNum,
                  refine_factor = tree_refine_f)


  tree_layers <- getLayers(data = tree_df,
                           npoint = p,
                           ntips = inputNum)
  message("Getting Layers of the Tree")

  xy_df <- getCoordinates(tree = tree_df,
                          layers = tree_layers,
                          npoint = p)

  message("Getting Coordinates of the Tree")

  final_tree_plot <- NULL
  final_tree_plot <- treePlot(xy_data = xy_df,
                              layer_data = tree_layers,
                              npoint = p)
  message("Plotting the Tree")

  # table data process
  center_point <- c(0,0)

  max_d_of_tree <- max(tree_df$depth)

  table_ring_radius <- getRingRadius(ring_table_data = inputTable,
                                     tree_outer_radius = max_d_of_tree)
  table_length <- nrow(inputTable)

  message("Getting the ring radius")
  r_data <- structure(as.vector(table_ring_radius$ring_radius),
                      names=as.vector(table_ring_radius$factor))

  ring_plot <- NULL
  ring_plot <- ringPlot(ring_table_data = inputTable,
                        radius_data = r_data,
                        tree_max_depth = max_d_of_tree,
                        tree_plot = final_tree_plot )

  message("Plotting the VF Rings")
  ring_plot <- ring_plot + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                          panel.grid.minor = ggplot2::element_blank(),
                                          panel.border = ggplot2::element_blank())

  message("Plotting the CombinedPlot")
  return(ring_plot)

}


# ############################### sample test cases

# sampleTreePlot150 <- combinedPlot(VF150, Tree150, inputNum = 150)
# sampleTreePlot25 <- combinedPlot(VF25_2, Tree25, inputNum = 25)
