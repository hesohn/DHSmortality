# TITLE: DHS Calendar Conversion Functions
# Author: Heeju Sohn
# Date: August 1, 2018
#' Nepalese calendar to Gregorian Calendar (in CMC format) Conversion

#' @param cmc Nepalese Century Month Code defaults to NULL
#' @keywords Nepalese Calendar DHS
#' @author Heeju Sohn
#' @export 
#' @examples 
#' nepal_to_greg()





nepal_to_greg <- function(cmc = NULL){
  gmc <- cmc - 680
  return(gmc)
}
