import::here(tidyr, 'pivot_wider')
import::here(file.path(wd, 'R', 'tools', 'list_tools.R'),
    'replace_specific_items', .character_only=TRUE)

## Functions
## get_unique_values
## fillna
## rename_columns


#' Get unique values from multiple dataframe columns
#' 
#' @param df a dataframe
#' @param cols columns to search
#' @return Returns a list of unique items
#' 
#' @examples
#' df <- data.frame(
#'     mouse = c('101', '102'),
#'     father = c("1", "1"),
#'     mother = c("2", "2")
#' )
#' get_unique_values(df, c('father', 'mother'))
#' 
get_unique_values <- function(df, cols) {
    items <- c()
    for (col in cols) {
        items <- append(items, unique(df[[col]]))
    }
    return(sort(unique(items)))
}


#' Fill specific column with NA
#' 
#' @description Mirrors Pandas' \href{https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.fillna.html}{fillna}
#' 
#' @param df a dataframe
#' @param cols a list of columns to replace
#' @param val the value to fill with
#' @param inplace TRUE allows you to avoid re-assigning the variable
#' @return Returns a dataframe.
#' 
#' @examples
#' mtcars['new_col'] <- NA
#' head(fillna(mtcars, c('new_col'), 1))
#' 
fillna <- function(df, cols, val=0, inplace=FALSE) {
    for (col in cols) {
        df[is.na(df[, col]), col] <- val
    }
    if (inplace) {
        assign('df', df, envir=.GlobalEnv)
    } else {
        return(df)
    }
}


#' Rename dataframe columns
#' 
#' @description This is a thin wrapper around replace_specific_items that acts on dataframe columns
#' 
#' @param df a dataframe
#' @param columns a named list of replacements. uses names to match and values to replace
#' @param inplace TRUE allows you to avoid re-assigning the variable
#' @return Returns a dataframe.
#' 
#' @examples
#' head(rename_columns(mtcars, c('mpg'="MPG", 'disp'="DISP")))
#' 
rename_columns <- function(df, columns, inplace=FALSE) {
    colnames(df) <- replace_specific_items(colnames(df), columns)
    if (inplace) {
        assign('df', df, envir=.GlobalEnv)
    } else {
        return(df)
    }
}
