import::here(wrapr, 'orderv')
import::here(plyr, 'rbind.fill')
import::here(RColorBrewer, 'brewer.pal')
import::here(file.path(wd, 'R', 'tools', 'list_tools.R'),
    'items_in_a_not_b', .character_only=TRUE)
import::here(file.path(wd, 'R', 'tools', 'text_tools.R'),
    'dotsep_to_snake_case', 'title_to_snake_case', .character_only=TRUE)
import::here(file.path(wd, 'R', 'tools', 'df_tools.R'),
    'rename_columns', 'fillna', 'get_unique_values', .character_only=TRUE)

## Objects
## col_to_new_col

## Functions
## standardize_dates
## generate_missing_parents
## preprocessing


#' used to match data from sample_ped_tab.csv to transnetyx output
#' 
col_to_new_col = c(
    'father'='father_id',
    'mother'='mother_id',
    'id'='mouse_id',
    'ped'='strain',
    "avail"="dead",
    'affected'='pcr_confirmation'
)


#' Standardize dates
#'
#' @description
#' Dates in MM/DD/YY format are converted to MM/DD/YYYY
#' 
standardize_dates <- function(date) {
    if ( grepl('^([0-9]+)/([0-9]+)/([0-9]{2})$', date) ) {
        return(format(as.Date(date, format = "%m/%d/%y"), "%m/%d/%Y"))
    } else {
        return(date)
    }
}


#' Impute parents if mising
#'
#' @description
#' If the parent is missing, creates a new F0 mouse of strain='0 - Imputed' and assigns an id of 0 for its parents.
#' 
generate_missing_parents <- function(df) {

    gender_for_parent_id = c(
        'father_id'='Male',
        'mother_id'='Female'
    )

    result <- NULL
    for (parent in c('father_id', 'mother_id')) {
        parents <- get_unique_values(df, parent)
        missing_parents <- items_in_a_not_b(parents[parents != 0], df[['mouse_id']])
        if ( !(identical(missing_parents, numeric(0)) |
               identical(missing_parents, integer(0)) ) ) {
            missing_parents_df <- data.frame(
                use = 'Breeding',
                strain = '0 - Imputed',
                sex = gender_for_parent_id[[parent]],
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

    # filter extra columns
    df = subset(df, select = items_in_a_not_b(colnames(df), '...1'))

    # rename columns
    colnames(df) <- unlist(lapply(colnames(df), dotsep_to_snake_case))
    colnames(df) <- unlist(lapply(colnames(df), title_to_snake_case))
    df <- rename_columns(df, col_to_new_col)

    # cleanup mouse ids
    df <- df[!grepl('Count: ', df[['mouse_id']]), ]  # drop filler rows
    if (typeof(df[['mouse_id']]) == "character") {
        df[['mouse_id']] = gsub(' |\\.', '', df[['mouse_id']])
    }
    df[['mouse_id']] <- as.numeric(df[['mouse_id']])
    df <- df[which(!is.na(df[, 'mouse_id'])), ]  # drop mice with missing IDs
    df <- df[!duplicated(df[['mouse_id']]), ]  # drop duplicated mice

    # clean up notes field, remove first newline
    if ('notes' %in% colnames(df)) {
        df[['notes']] <- unlist(
            lapply(df[['notes']], function(x) sub("\r\n*|\n*|\r*", "", x))
        )  
    }

    # ----------------------------------------------------------------------
    # Impute missing columns

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

    # ----------------------------------------------------------------------
    # Fix parents

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

    # remove self parents
    for (col in c('father_id', 'mother_id')) {
        df[df[['mouse_id']]==df[[col]], c('father_id', 'mother_id')] <- 0
    }

    # ----------------------------------------------------------------------
    # Standardize dates
    
    df[['dob']] <- sapply(df[['dob']], standardize_dates)
    df[['dod']] <- sapply(df[['dod']], standardize_dates)

    # update date instead of relying on Transnetyx
    if ('age' %in% colnames(df) & 'dob' %in% colnames(df)) {
        if ('dod' %in% colnames(df)) {
            df[['age']] <- ifelse(
                !is.na(df[['dod']]),
                as.integer(difftime(
                    as.Date(df[['dod']], "%m/%d/%Y"),
                    as.Date(df[['dob']], "%m/%d/%Y"), units ="days")
                ),
                as.integer(difftime(
                    Sys.Date(), 
                    as.Date(df[['dob']], "%m/%d/%Y"), units ="days")
                )
            )
        } else {
            df[['age']] <- as.integer(difftime(
                Sys.Date(),
                as.Date(df[['dob']], "%m/%d/%Y"), units ="days")
            )
        }
    }

    # ----------------------------------------------------------------------
    # Assign colors

    if (!('color' %in% colnames(df))) {
        strains <- sort(unique(df[['strain']]))
        n_strains = max(length(strains), 3)
        strains_to_color = colorRampPalette(
            brewer.pal(n = min(n_strains, 9), name = "Set1")
        )(n_strains)
        names(strains_to_color) = strains
        df[['color']] = unlist(lapply(df[['strain']], function(x) strains_to_color[[x]]))
    }
    df <- fillna(df, c('color'), '#7F7F7F')  # gray

    # see: https://win-vector.com/2021/02/07/it-has-always-been-wrong-to-call-order-on-a-data-frame/
    df <- df[wrapr::orderv(df[, c('strain', 'mouse_id')]), ]

    return(df)
}
