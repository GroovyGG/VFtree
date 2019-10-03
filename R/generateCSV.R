#' A function to
#'
#' \code{generateTable} The function will generate a the csv file
#'
#' @param inputFile preprocessed fasta file with present and absent of VF of each strain
#' @return A csv file with information of VF present/absents
#' @return A list of list with reference name, alignment position, alignment
#' counts and percentage of introns if there is a verifyDataFrame.
#'
#' @examples
#' inputFile <- system.file("extdata/testdata", "RRHreads.fasta",
#' package = "VFtree")
#' outputNewick <- system.file("extdata/testdata", "RRHtranscript.fasta",
#' package = "VFtree")
#'
#' @import phangorn
#' @import phytools

generateCSV <- function(inputFile, reference, outputName) {


}


