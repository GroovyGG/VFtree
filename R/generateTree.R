#' A function to
#'
#' \code{generateTree} The function will generate a newick file with relationships between genome
#'
#' @param inputFile preprocessed concatenated fasta file contain all genome of the genome
#' @return A newick file contain the phylogenetic relationships between all strains
#'
#' @examples
#' inputFile <- system.file("extdata/testdata", "RRHreads.fasta",
#' package = "VFtree")
#' outputNewick <- system.file("extdata/testdata", "RRHtranscript.fasta",
#' package = "VFtree")
#'
#' @import phangorn
#' @import phytools


generateTree <- (inputFile, outputNewick) {

  # Getting a list of files in a Directory


  treeFile <- read.tree(paste0(fp, "core_gene_tree.newick"))
  meta <- read.csv(paste0(fp, "ggplot-DB-5.csv"), header = TRUE, na.strings=c("","NA"),
                   stringsAsFactors=FALSE)
}
