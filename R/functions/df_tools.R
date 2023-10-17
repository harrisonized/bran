source(file.path(
    dirname(dirname(this.path::here())),  # wd
    'R', 'functions', 'list_tools.R')
) 

## Functions
## read_excel_or_csv
## rename_columns
## rev_df
## fillna
## smelt
## reset_index
## filter_dataframe_column_by_list
## pivot


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


#' Reverse a dataframe
#' 
#' @export
rev_df <- function(df, how='row') {
    if (how == 'row') {
        return(df[dim(df)[1]:1,])
    } else if (how == 'col') {
        return(rev(df))
    } else {
        stop("Choose how='row' or how='col'")
    }
}


#' Special Melt
#' 
#' Convert a dataframe from a table to long format
#' This is an alternative to melt that doesn't throw errors
#' See: \href{https://stackoverflow.com/questions/28355377/how-to-add-index-of-a-list-item-after-melt-in-r}{Stack Overflow link}
#' 
#' @export
smelt <- function(
   df,
   rowname='row',
   colname='col',
   valname='val'
) {
   melted <- transform(stack(setNames(df, colnames(df))), id=rownames(df))
   colnames(melted) <- c(valname, colname, rowname)
   return(rev(melted))
}


#' See: https://stackoverflow.com/questions/36396911/r-move-index-column-to-first-column
#' 
#' @export
reset_index <- function(df, index_name='index') {
    df <- cbind(index = rownames(df), df)
    rownames(df) <- 1:nrow(df)
    colnames(df)[colnames(df) == "index"] = index_name
    return (df)
}


#' @export
filter_dataframe_column_by_list <- function(dataframe, colname, items, index_name='index', return_index=FALSE) {
    
    data <- reset_index(dataframe, index_name=index_name)
    rownames(data) <- data[, colname]
    data <- (data[intersect(data[, colname], items),])  # filter genes shared by both gtf files
    rownames(data) <- data[, index_name]  # optional preserve index for troubleshooting
    
    if (return_index==TRUE) {
        return (data)
    } else {
        return (data[, items_in_a_not_b(colnames(data), 'index')])
    }
}


#' tidyr returns a tibble object instead of a dataframe
#' This function returns a dataframe
#' 
#' @export
pivot <- function(df, columns, values) {

    # Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    # See: http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings
    withCallingHandlers({
        tibble_obj = tidyr::pivot_wider(
            df,
            names_from = columns,
            values_from = values,
            values_fn = list,
            names_glue = if (length(values)==1){"{.value}_{.name}"}
        )
    }, warning = function(w) {
        if ( any(grepl("Using an external vector", w)) ) {
            invokeRestart("muffleWarning")
        }
    })
    
    dataframe = data.frame(lapply(tibble_obj, unlist))
    
    return (dataframe)
}
