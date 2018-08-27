# TITLE: DHS Calendar Conversion Functions
# Author: Heeju Sohn
# Date: August 1, 2018
#' Ethiopian calendar to Gregorian Calendar (in CMC format) Conversion

#' @param cmc Ethiopian Century Month Code defaults to NULL
#' @keywords Ethiopian Calendar DHS
#' @author Heeju Sohn
#' @export 
#' @examples 
#' ethiopian_to_greg()




ethiopian_to_greg <- function(cmc = NULL){
  ey = trunc((cmc-1)/12)+1900
  em = cmc-(ey-1900)*12
  ed = 1
  
  joffset = 1723856
  n = 30*(em -1)  + ed - 1 # ed - 1 if actually 0
  jd = joffset + 365 + 365*(ey-1) + trunc(ey/4) + n
  
  z = jd + 0.5
  w = trunc((z-1867216.25)/36524.25)
  x = trunc(w/4)
  a = z+1+w-x
  b = a+1524
  c = trunc((b-122.1)/365.25)
  d = trunc(365.25*c)
  e = trunc((b-d)/30.6001)
  f = trunc(30.6001*e)
  day = b-d-f
  
  month = e-1
  
  month.low  <-(month<=12)*(e-1)
  month.high <-(month>12)*(e-13)
  month = month.low+month.high
  
  year.low <- (month < 3)*(c-4715)
  year.high <-(month>=3)*(c-4716)
  year = year.low + year.high
  
  outcmc = 12*(year - 1900)+month
  return(outcmc)
} 
