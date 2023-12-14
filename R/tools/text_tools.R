## Functions
## dotsep_to_snake_case
## title_to_snake_case


#' Standardize Dot-separated Titles
#' 
#' @description Converts "Column.Title" to column_title
#' 
#' @examples
#' dotsep_to_snake_case('Column.Title')
#' 
dotsep_to_snake_case <- function(text) {
    return(tolower(
        paste(
            unlist(strsplit(text, '[.]')), collapse='_')
        )
    )
}


#' Standardize Space-separated Titles
#' 
#' @description Converts "Column Title" to column_title
#' 
#' @examples
#' title_to_snake_case('Column Title')
#' 
title_to_snake_case <- function(text) {
    return(tolower(
        paste(
            unlist(strsplit(text, '[ ]')), collapse='_')
        )
    )
}
