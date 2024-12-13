# Clean up dataframe

import::here(plyr, 'rbind.fill')
import::here(file.path(wd, 'R', 'tools', 'list_tools.R'),
    'items_in_a_not_b', .character_only=TRUE)
import::here(file.path(wd, 'R', 'tools', 'text_tools.R'),
    'dotsep_to_snake_case', 'title_to_snake_case', .character_only=TRUE)
import::here(file.path(wd, 'R', 'tools', 'df_tools.R'),
    'rename_columns', 'fillna', .character_only=TRUE)
import::here(file.path(wd, 'R', 'functions', 'computations.R'),
    'generate_missing_parents', .character_only=TRUE)


## Objects
## col_to_new_col

## Functions
## impute_required_columns
## clean_column_names
## clean_text_columns
## clean_mouse_ids
## clean_parent_ids
## filter_extinct_families


#' used to match data from sample_ped_tab.csv to transnetyx output
#' 
col_to_new_col = c(
    'father'='father_id',
    'mother'='mother_id',
    'id'='mouse_id',
    'ped'='strain',
    "avail"="dead",
    'affected'='pcr_confirmation',
    "systemid"="system_id"
)


#' Impute missing columns
#' 
#' @description
#' Imputes the following columns:
#' 'dob', 'dod', 'alive', 'chr_m', 'chr_p', 'pcr_confirmation', 'ignore', 'cage_id', 'strain'
#'
impute_required_columns <- function(df) {

    if (!('dob' %in% colnames(df))) {
        df[['dob']] = NA
    }

    if ('dod' %in% colnames(df)) {
        if ('alive' %in% colnames(df)) {
            df[(df['alive']==1), 'alive'] <- as.integer(is.na(df[(df['alive']==1), 'dod']))
        } else {
            df[['alive']] <- as.integer(is.na(df[['dod']]))
        }
    } else {
        df[['dod']] = NA
    }
    if (!('alive' %in% colnames(df))) {
        if ('dead' %in% colnames(df)) {
            df[['alive']] = 1-df[['dead']]
        } else {
            df[['alive']] = 1
        }
    }
    if (!('pcr_confirmation' %in% colnames(df))){
        if ('chr_m' %in% colnames(df)){
            df[['pcr_confirmation']] = df[['chr_m']]
        } else {
            df[['pcr_confirmation']] = NA
        }
    }
    for (chr in c('chr_m', 'chr_p')) {
        if (!(chr %in% colnames(df))){
            df[[chr]] = df[['pcr_confirmation']]
        }
    }
    if (!('ignore' %in% colnames(df))) {
        df[['ignore']] = 0
    }
    if (!('cage_id' %in% colnames(df))) {
        df[['cage_id']] = 1
    }
    if (!('strain' %in% colnames(df))) {
        df[['strain']] = '0 - Unknown'
    }

    return(df)
}


#' Clean Column Names
#' 
clean_column_names <- function(df) {

    # filter extra columns
    df = subset(df, select = items_in_a_not_b(colnames(df), '...1'))

    colnames(df) <- unlist(lapply(colnames(df), dotsep_to_snake_case))
    colnames(df) <- unlist(lapply(colnames(df), title_to_snake_case))
    df <- rename_columns(df, col_to_new_col)

    return(df)
}


#' Standardize Text Columns
#' 
#' @description clean up text fields
#' 
clean_text_columns <- function(df, col='notes') {
    
    if (col %in% colnames(df)) {

        # remove newlines
        df[[col]] <- unlist(
            lapply(df[[col]], function(x) gsub("\r|\n|\r\n", "", x))
        )

        # replace quotes within text field of a csv
        df[[col]] <- unlist(
            lapply(df[[col]], function(x) gsub('"', "'", x))
        )

    }
    return(df)
}


#' Clean Mouse IDs
#' 
clean_mouse_ids <- function(df) {

    # clean mouse ids
    df <- df[!grepl('Count: ', df[['mouse_id']]), ]  # drop filler rows
    if (typeof(df[['mouse_id']]) == "character") {
        df[['mouse_id']] = gsub(' |\\.', '', df[['mouse_id']])
    }
    df[['mouse_id']] <- as.numeric(df[['mouse_id']])
    df <- df[which(!is.na(df[, 'mouse_id'])), ]  # drop mice with missing IDs
    df <- df[!duplicated(df[['mouse_id']]), ]  # drop duplicated mice

    return(df)
}


#' Clean Parent IDs
#' 
clean_parent_ids <- function(df, impute_missing_parents=TRUE) {

    # if multiple parents are listed, eg. "10752; 10753", chooses the first one
    for (col in c('father_id', 'mother_id')) {
        if (is.character(df[[col]])) {
            df[[col]] <- unlist(
                lapply(df[[col]], function(x) strsplit(x, "; ")[[1]][1])
            )
        }
    }

    # fix data types
    for (col in c('father_id', 'mother_id')) {
        if (typeof(df[[col]]) == "character") {
            df[[col]] = gsub(' |\\.', '', df[[col]])
        }
        df[[col]] <- as.numeric(df[[col]])
    }

    # impute missing parents
    if (impute_missing_parents) {
        missing_parents <- generate_missing_parents(df)
        if (!is.null(missing_parents)) {
            df <- plyr::rbind.fill(df, missing_parents)
        }
    }

    # impute missing values
    # NOTE: inplace does not work within functions
    df <- fillna(df, c('father_id', 'mother_id', 'ignore'), 0)
    df <- fillna(df, c('alive'), 1)

    # Subjects must have both a father and mother, or have neither
    df[df[['father_id']] == 0, 'mother_id'] <- 0 
    df[df[['mother_id']] == 0, 'father_id'] <- 0
    
    # remove self parents
    for (col in c('father_id', 'mother_id')) {
        df[df[['mouse_id']]==df[[col]], c('father_id', 'mother_id')] <- 0
    }

    return(df)
}


#' Filter extinct families
#' 
#' requires the following columns: mouse_id, father_id, mother_id, ignore
#' 
filter_extinct_families <- function(df) {

    parents = get_unique_values(df, c('father_id', 'mother_id'))
    n_parents = length(parents)
    n_parents_prev <- 0

    while (n_parents_prev != n_parents) {
        n_parents_prev <- n_parents
        df <- df[(df[['mouse_id']] %in% parents) | (df[['ignore']] == 0), ]  # filter
        parents <- get_unique_values(df, c('father_id', 'mother_id'))
        n_parents = length(parents)
    }

    return(df)
}
