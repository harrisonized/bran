wd_ = dirname(this.path::here())
suppressMessages(library('plyr'))
suppressMessages(library('wrapr'))
library('RColorBrewer')  # brewer.pal
source(file.path(wd_, 'R', 'functions', 'list_tools.R'))  # items_in_a_not_b
source(file.path(wd_, 'R', 'functions', 'text_tools.R'))  # dotsep_to_snake_case, title_to_snake_case
source(file.path(wd_, 'R', 'functions', 'df_tools.R'))  # rename_columns


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
    "avail"="dead",
    'affected'='pcr_confirmation'
)


#' impute parents if mising
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
                strain = 'Imputed',
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


#' main preprocessing function
preprocessing <- function(df, impute_missing_parents=TRUE) {

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
    if (!('ignore' %in% colnames(df))) {
        df[['ignore']] = 0
    }
    if (!('cage_id' %in% colnames(df))) {
        df[['cage_id']] = NA
    }

    # remove first newline
    if ('notes' %in% colnames(df)) {
        df[['notes']] <- unlist(
            lapply(df[['notes']], function(x) sub("\r\n*|\n*|\r*", "", x))
        )  
    }

    # if multiple parents are listed, eg. "10752; 10753", chooses the first one
    for (col in c('father_id', 'mother_id')) {
        if (is.character(df[[col]])) {
            df[[col]] <- unlist(
                lapply(df[[col]], function(x) strsplit(x, "; ")[[1]][1])
            )
        }
    }

    # fix data types
    for (col in c('mouse_id', 'father_id', 'mother_id')) {
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
   
    # update date instead of relying on Transnetyx
    df[['age']] <- as.integer(difftime(Sys.Date(), as.Date(df[['dob']], "%m/%d/%y")))
        
    # autoassign colors
    if (!('color' %in% colnames(df))){
        strains <- unique(df[['strain']])
        strains_to_color = brewer.pal(n = length(strains), name = "Set1")
        names(strains_to_color) = sort(strains)
        df[['color']] = unlist(lapply(df[['strain']], function(x) strains_to_color[[x]]))
    } else {
        df <- fillna(df, c('color'), '#7F7F7F')  # gray
    }

    # see: https://win-vector.com/2021/02/07/it-has-always-been-wrong-to-call-order-on-a-data-frame/
    df <- df[wrapr::orderv(df[, c('strain', 'mouse_id')]), ]

    return(df)
}
