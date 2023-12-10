import::here(file.path(wd, 'R', 'utils', 'list_tools.R'),
    'replace_specific_items', .character_only=TRUE)

## Functions
## rename_columns
## fillna
## get_unique_values


#' rename specific dataframe columns
#' 
#' @export
rename_columns <- function(df, columns, inplace=FALSE) {
    colnames(df) <- replace_specific_items(colnames(df), columns)
    if (inplace) {
        # see: https://stackoverflow.com/questions/3969852/update-data-frame-via-function-doesnt-work
        assign('df', df, envir=.GlobalEnv)
    } else {
        return(df)
    }
}


#' fill a specific column with na
#' 
#' @export
fillna <- function(df, cols, val=0, inplace=FALSE) {
    for (col in cols) {
        df[is.na(df[, col]), col] <- val
    }
    if (inplace) {
        # see: https://stackoverflow.com/questions/3969852/update-data-frame-via-function-doesnt-work
        assign('df', df, envir=.GlobalEnv)
    } else {
        return(df)
    }
}


#' get unique values from each column
#' 
#' @export
get_unique_values <- function(df, cols) {
    items <- c()
    for (col in cols) {
        items <- append(items, unique(df[[col]]))
    }
    return(sort(unique(items)))
}
