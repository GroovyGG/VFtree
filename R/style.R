#' \code{circular_plot} output the circular phlogenetic Tree
#'
#' @param data mass spetrometry information for the peptide
#' @param modification contain modification information , intensity of ion, amino acide that is modified
#' @param mZmarker_ions maker ion
#' @param search_engine can be Mascot or Tandem
#'
#' modification<-data.frame("type"=c("Carbamidomethyl","Oxidation"),
#' "monomass"=c(57.022, 16.0), "AA"=c("C","M"))
#' result.file <- "/Users/yufei/Desktop/2018fall/BCB410/MSPTM
#' /inst/extdata/output_mouse.2018_12_04_19_57_17.t.xml"
#' uids<-c(12,2,731)
#' library(rTANDEM)
#' result <- GetResultsFromXML(result.file)
#' data<-tandem_get_data(result,modification,uids)
#' intensity_plot(data,modification,mZmarker_ions, search_engine="Tandem")

#' @export

circular_plot <- function(tree,vf,modification) {

}
