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


combinedPlotting <- (inputCSV,inputNWK) {

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


elementMatching <- function(inputCSV,inputNWK) {

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







