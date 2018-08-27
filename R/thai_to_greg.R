# TITLE: DHS Calendar Conversion Functions
# Author: Heeju Sohn
# Date: August 1, 2018
#' Thai calendar to Gregorian Calendar (in CMC format) Conversion

#' @param cmc Thai Century Month Code defaults to NULL
#' @keywords Thai Calendar DHS
#' @author Heeju Sohn
#' @export 
#' @examples 
#' thai_to_greg()



thai_to_greg <- function(cmc = NULL){
  outcmc = cmc - (543*12)
  return(outcmc)
}