import::here(wrapr, 'orderv')
import::here(file.path(wd, 'R', 'functions', 'cleanup.R'),
    'impute_required_columns', 'clean_column_names', 'clean_text_columns',
    'clean_mouse_ids', 'clean_parent_ids', .character_only=TRUE)
import::here(file.path(wd, 'R', 'functions', 'computations.R'),
    'calculate_age', 'assign_colors', .character_only=TRUE)

## Functions
## preprocessing


#' Main preprocessing function
#'
#' @description
#' Does the following:
#'   1. rename columns, clean up mouse_ids, notes
#'   2. impute missing columns: dob, dod, alive, pcr_confirmation, chr_m, chr_p, ignore, cage_id, strain;
#'   3. fix parents: picks first parent or imputes missing parents;
#'   4. standardize dates;
#'   5. assign colors
#' 
#' @param df a dataframe
#' @param impute_missing_parents TRUE/FALSE. Required if parents are missing.
#' Without it, [kinship2::pedigree()] will throw the following error:
#' Value of 'dadid' (or 'momid') not found in the id list.
#' 
preprocessing <- function(df, impute_missing_parents=TRUE) {

    df <- clean_column_names(df)
    df <- impute_required_columns(df)
    df <- clean_text_columns(df)

    df <- clean_mouse_ids(df)
    df <- clean_parent_ids(df, impute_missing_parents)
    df <- calculate_age(df)

    df <- assign_colors(df)

    # order output
    # see: https://win-vector.com/2021/02/07/it-has-always-been-wrong-to-call-order-on-a-data-frame/
    df <- df[wrapr::orderv(df[, c('strain', 'mouse_id')]), ]

    return(df)
}
