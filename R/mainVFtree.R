# mainVFtree.R

#' An main function to plot the Phylogentic Tree with referenced VF genes.
#'
#' \code{mainNonExonMap} This is the main function for my functions in this
#' package. The function will plot the non-exon position on reference gene and
#' if they are introns (if intronsFile exist).
#'
#' @param readAssemblies The file that store the read sequences, a string.
#' @param readQueries The file that contains the intron sequences, a string.
#' @param style The visualization style of the plot, default is circular style
#' @return A list of list with reference name, alignment position, alignment
#' counts and percentage of introns if there is a verifyDataFrame.
#'
#' @examples
#' readAssemblies <- system.file("extdata/refseq", "GCA_000008005.1_ASM800v1_genomic.fna",
#' package = "VFtree")
#' readQueries <- system.file("extdata/VF", "bceT.fasta",
#' package = "VFtree")
#' \dontrun{
#' mainNonExonMap(readsFile, transcriptsFile)
#' mainNonExonMap(readsFile, transcriptsFile, intronsFile)
#' }
#' @export

plotVFtree <- function(readAssemblies, readQueries, style = "circular") {
  # Set up functions

  # Plot according to styles

  # Delete files

  return(result)

}
