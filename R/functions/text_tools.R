## Functions
## dotsep_to_snake_case

#' Converts "Column.Title" to column_title
dotsep_to_snake_case <- function(text) {
    return(tolower(
        paste(
            unlist(strsplit(text, '[.]')), collapse='_')
        )
    )
}