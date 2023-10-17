## Objects
## col_to_new_col

## Functions
## generate_missing_parents
## preprocessing
## read_excel_or_csv


#' used to match data from sample_ped_tab.csv to transnetyx output
col_to_new_col = c(
    'father'='father_id',
    'mother'='mother_id',
    'id'='mouse_id',
    'ped'='strain'
)


#' impute parents if mising
#' 
#' @export
generate_missing_parents <- function(df) {

    gender_for_parent_id = c(
        'father_id'='Male',
        'mother_id'='Female'
    )

    result <- NULL
    for (parent in c('father_id', 'mother_id')) {
        parents <- get_unique_values(df, parent)
        missing_parents <- items_in_a_not_b(parents[parents != 0], df[['mouse_id']])
        if (!identical(missing_parents, integer(0))) {
            missing_parents_df <- data.frame(
                use = 'breeding',
                strain = '1 - B6',
                sex = gender_for_parent_id[parent],
                mouse_id = missing_parents,
                father_id = 0,
                mother_id = 0,
                alive = 1
            )
            result <- rbind(result, missing_parents_df)
        }
    }
    return(result)
}


#' rename columns
#' 
#' @export
preprocessing <- function(df) {

    # rename columns
    colnames(df) <- unlist(lapply(colnames(df), dotsep_to_snake_case))
    colnames(df) <- unlist(lapply(colnames(df), title_to_snake_case))
    df <- rename_columns(df, col_to_new_col)

    # cleanup
    df <- df[which(!(is.na(df[, 'mouse_id']) | is.na(df[, 'strain']))), ]  # drop missing mice
    df <- df[!duplicated(df[['mouse_id']]), ]  # drop duplicated mice

    # impute missing columns
    if (!('alive' %in% colnames(df))){
        if ('dead' %in% colnames(df)) {
            df[['alive']] = 1-df[['dead']]
        } else {
            df[['alive']] = 1
        }
    }
    if (!('pcr_confirmation' %in% colnames(df))){
        df[['pcr_confirmation']] = NA
    }

    # impute missing parents
    missing_parents <- generate_missing_parents(df)
    if (!is.null(missing_parents)) {
        df <- plyr::rbind.fill(df, missing_parents)
    }

    # impute missing values
    # NOTE: inplace does not work within functions
    df <- fillna(df, c('father_id', 'mother_id', 'alive'), 1)

    # remove self parents
    for (col in c('father_id', 'mother_id')) {
        df[df[['mouse_id']]==df[[col]], c('father_id', 'mother_id')] <- 0
    }

    # fix data types
    for (col in c('mouse_id', 'father_id', 'mother_id')) {
        df[[col]] <- as.numeric(df[[col]])
    }
    
    return(df)
}


#' convenience function
#' 
#' @export
read_excel_or_csv <- function(filepath, ext) {
    if (ext == 'xlsx') {
        df <- read_excel(filepath)
        df = subset(df, select = items_in_a_not_b(colnames(df), '...1'))
    } else if (ext == 'csv') {
        df <- read.csv(filepath, header=TRUE)
    } else {
        log_print(paste(Sys.time(), 'Please enter a xlsx or csv file.'))
        stop()
    }
    return(df)
}
