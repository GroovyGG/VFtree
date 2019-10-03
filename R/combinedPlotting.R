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


fp <- "inst/extdata/"
nwkFile <- read.tree(paste0(fp, "core_gene_tree.newick"))
meta <- read.csv(paste0(fp, "ggplot-DB-5.csv"), header = TRUE,
                 na.strings = c("","NA"), stringsAsFactors=FALSE)

#tree <- ggtree(nwkFile, size = 0.05) %<+% meta  + geom_treescale(fontsize = 3, x=100, y=0)
tree <- ggtree(nwkFile,layout ="circular", branch.length='none',size = 0.1) %<+% meta
#tree <- tree + geom_treescale(x=20, y=10, width = 10, offset = 10,color = "red", fontsize = 1)
tree <- tree + geom_tiplab(aes(angle =angle),color = 'blue', size=0.5, linesize=0.3) + theme_tree2()
plot(tree)
#tree <- ggtree(treeFile, layout = 'circular', size = 0.1) %<+% meta + geom_treescale(fontsize = 3, x=100, y=0)



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




