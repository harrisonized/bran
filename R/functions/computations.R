import::here(RColorBrewer, 'brewer.pal')
import::here(file.path(wd, 'R', 'tools', 'df_tools.R'),
    'get_unique_values', .character_only=TRUE)
import::here(file.path(wd, 'R', 'functions', 'formatting.R'),
    'standardize_dates', .character_only=TRUE)

## Functions
## generate_missing_parents
## calculate_age
## assign_colors
## generate_display_text


#' Impute parents if mising
#'
#' @description
#' If the parent is missing, creates a new F0 mouse of strain='0 - Imputed' and assigns an id of 0 for its parents.
#' 
generate_missing_parents <- function(df) {

    result <- NULL
    for (parent in c('father_id', 'mother_id')) {
        parents <- get_unique_values(df, parent)
        missing_parents <- items_in_a_not_b(parents[parents != 0], df[['mouse_id']])
        if ( !(identical(missing_parents, numeric(0)) |
               identical(missing_parents, integer(0)) ) ) {
            missing_parents_df <- data.frame(
                use = 'Breeding',
                strain = '0 - Imputed',
                sex = ifelse(parent=='father_id', 'Male', 'Female'),
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


#' Calculate age
#' 
calculate_age <- function(df) {

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
    return(df)
}


#' Assign colors
#' 
assign_colors <- function(df) {
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
    return(df)
}


#' Generate Display Text
#'
#' @description
#' mouse id, rack name, position, age
#' 
generate_display_text <- function(df) {

    if (basename(opt[['input-file']]) != 'sample_ped_tab.csv') {
        rack_names = gsub(".* ([0-9]+[Aa|Bb]).*", "\\1", df[['rack']])  # regex match Rack 1A
        positions = ifelse((df[['position']]=='Unassigned'), NA, df[['position']])
        display_text = paste0(
            df[['mouse_id']], '\n',
            rack_names, ', ', positions, '\n',
            strftime(as.Date(df[['dob']], "%m/%d/%Y"), "%m/%d/%y"), '\n',
            df[['age']], 'd'
        )
    } else {
        display_text = df[['mouse_id']]
    }

    return(display_text)
}
