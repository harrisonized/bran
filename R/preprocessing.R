## Objects
## col_to_new_col

## Functions
## generate_missing_parents
## preprocessing


#' used to match data from sample_ped_tab.csv to transnetyx output
col_to_new_col = c(
    'father'='father_id',
    'mother'='mother_id',
    'id'='mouse_id',
    'ped'='strain',
    'affected'='pcr_confirmation'
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

    # filter extra columns
    df = subset(df, select = items_in_a_not_b(colnames(df), '...1'))

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

    # impute missing parents
    missing_parents <- generate_missing_parents(df)
    if (!is.null(missing_parents)) {
        df <- plyr::rbind.fill(df, missing_parents)
    }

    # impute missing values
    # NOTE: inplace does not work within functions
    df <- fillna(df, c('father_id', 'mother_id'), 0)
    df <- fillna(df, c('alive'), 1)

    # remove self parents
    for (col in c('father_id', 'mother_id')) {
        df[df[['mouse_id']]==df[[col]], c('father_id', 'mother_id')] <- 0
    }

    # fix data types
    for (col in c('mouse_id', 'father_id', 'mother_id')) {
        df[[col]] <- as.numeric(df[[col]])
    }

    df <- df[order(df[, 'strain'], df[, 'mouse_id']), ]
    
    # autoassign colors
    if (!('color' %in% colnames(df))){
        strains <- unique(df[['strain']])
        strains_to_color = brewer.pal(n = length(strains), name = "Set1")
        names(strains_to_color) = sort(strains)
        df[['color']] = unlist(lapply(df[['strain']], function(x) strains_to_color[[x]]))
    }

    return(df)
}
