#' \code{concatenate_fna} output the
#'
#' @param fileDirectory
#' @param outputName the output name of the temperary concatenated file

#' @examples
#' fileDirectory <- system.file("/inst/extdata/refseq", package = "VFtree")
#' outputName <- "test_concat.fasta"
#' @import shortread

concatenate_fna <- function(fileDirectory, outputName) {

  pattern <- "fna$"
  concatenated_fasta <- ShortRead::readFasta(fileDirectory, pattern)
  ShortRead::writeFasta(concatenated_fasta, outputName)

}

#' \code{concatenate_fasta} output the
#'
#' @param fileDirectory
#' @param outputName the output name of the temperary concatenated file

#' @examples
#' fileDirectory <- system.file("/inst/extdata/refseq", package = "VFtree")
#' outputName <- "/inst/extdata/refseq/test_concat.fasta"
#' @import shortread

concatenate_fasta <- function(fileDirectory, outputName) {

  pattern <- "fasta$"
  concatenated_fasta <- ShortRead::readFasta(fileDirectory, pattern)
  ShortRead::writeFasta(concatenated_fasta, outputName)

}


# concatenate_fasta(fileDirectory = "inst/extdata/VF", outputName = "inst/extdata/VF/test_output_1.fasta")
# concatenate_fasta(fileDirectory = "inst/extdata/ref", outputName = "inst/extdata/ref/test_concate.fasta")
# concatenate_fna(fileDirectory = "inst/extdata/refseq", outputName = "inst/extdata/refseq/test_output_2.fasta")
