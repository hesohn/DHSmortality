# TITLE: DHS Calendar Conversion Functions
# Author: Heeju Sohn
# Date: August 1, 2018
#' Persian calendar to Gregorian Calendar (in CMC format) Conversion
#' @param ydhs Persian year defaults to NULL
#' @param mdhs Persian month defaults to NULL
#' @param cmc Persian Century Month Code defaults to NULL
#' @keywords Persian Calendar DHS
#' @author Heeju Sohn
#' @export 
#' @examples 
#' persian_to_greg()



persian_to_greg <- function(ydhs = NULL, mdhs = NULL, cmc = NULL) 
{
  if(is.null(ydhs) & is.null(mdhs) & is.null(cmc)){
    print("IS NULL")
    return()
  }

  
  if (!is.null(ydhs) & !is.null(mdhs)){
    year = ydhs
    month = mdhs
  }
  
  if(!is.null(cmc)){
    month = cmc %% 12
    month <- replace(month, month == 0, 12)
    year = trunc((cmc-1+15600)/12)
  }
  
  day = 1
  
  persian_epoch = 1948321
  
  
  year.pos <- (year>=0)*(year-474)
  year.neg <- (year<0)*(year-473)
  
  epbase <- year.pos+year.neg
  
  epyear = 474 + (epbase %% 2820)
  
  mdays.early <- (month<=7)*((month-1)*31)
  mdays.late <- (month>7)*(((month-1)*30)+6)
  mdays = mdays.early + mdays.late
  
  jdn = day + mdays +trunc(((epyear*682)-110)/2816) +
    (epyear -1)*365 + trunc(epbase/2820) * 1029983 + (persian_epoch-1)
  
  # #jdn = !iDay + #mdays + Trunc(((#epyear * 682) - 110) / 2816) +
  # (#epyear - 1) * 365 + Trunc(#epbase / 2820) * 1029983 + (#PERSIAN_EPOCH - 1).
  l = jdn + 68569
  n = trunc((4*l)/146097)
  l = l - trunc((146097*n+3)/4)
  i = trunc((4000 * (l + 1)) / 1461001)
  l =l - trunc((1461 *i) / 4) + 31
  j = trunc((80 * l)/2447)
  Gday = l - trunc((2447 * j) / 80)
  l = trunc(j / 11)
  Gmonth = j + 2 - 12 * l
  Gyear = 100 * (n - 49) + i + l
  
  gcal <- paste(Gyear, Gmonth, Gday)   
  
  gcmc  = 12*(Gyear -1900) + Gmonth
  
  return(gcmc)
}

