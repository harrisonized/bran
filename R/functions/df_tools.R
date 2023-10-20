library('readxl')
source(file.path(
    dirname(dirname(this.path::here())),  # wd
    'R', 'functions', 'list_tools.R')
) 

## Functions
## read_excel_or_csv
## rename_columns
## fillna
## get_unique_values


#' switch case to read excel or csv based on the extension
#' 
#' @export
read_excel_or_csv <- function(filepath) {
    ext=tools::file_ext(filepath)
    if (ext == 'xlsx') {
        df <- read_excel(filepath)
    } else if (ext == 'csv') {
        df <- read.csv(filepath, header=TRUE)
    } else {
        log_print(paste(Sys.time(), 'Please enter a xlsx or csv file.'))
        stop()
    }
    return(df)
}


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
