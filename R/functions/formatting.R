## Functions
## standardize_dates

#' Standardize dates
#'
#' @description
#' Dates in MM/DD/YY format are converted to MM/DD/YYYY
#' 
standardize_dates <- function(date) {
    if ( grepl('^([0-9]+)/([0-9]+)/([0-9]{2})$', date) ) {
        return(format(as.Date(date, format = "%m/%d/%y"), "%m/%d/%Y"))
    } else {
        return(date)
    }
}
