## Functions
## standardize_dates
## days_to_ywd


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


#' Days to Years/Weeks/Days
#' 
days_to_ywd <- function(days) {

    if (is.na(days)) {
        return('')
    }

    years <- (days %/% 365)
    weeks <- (days %% 365) %/% 7
    days <- (days %% 365) %% 7
    if (years >= 1) {
        ywd <- paste0(years, 'y, ', weeks, 'w, ', days, 'd')
    } else if (weeks >= 1) {
        ywd <- paste0(weeks, 'w, ', days, 'd')
    } else {
        ywd <- paste0(days, 'd')
    }
    return(ywd)
}
