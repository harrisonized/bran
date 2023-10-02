## Example code to use kinship2
## See: https://cran.r-project.org/web/packages/kinship2/vignettes/pedigree.html

wd = dirname(this.path::here())  # wd = '~/github/R/harrisonRTools'
library('kinship2')
library('tibble')
library('RColorBrewer')
library('optparse')
library('logr')
source(file.path(wd, 'R', 'functions', 'list_tools.R'))
source(file.path(wd, 'R', 'functions', 'text_tools.R'))
source(file.path(wd, 'R', 'functions', 'df_tools.R'))


# ----------------------------------------------------------------------
# Pre-script settings

# args
option_list = list(
    make_option(c("-i", "--input-file"), default='data/family-tree/mice.csv',
                metavar='data/family-tree/mice.csv',
                type="character",help="path/to/sample_ped_tab.csv"),
   
    make_option(c("-o", "--output-dir"), default="figures/family-tree",
                metavar="figures/family-tree", type="character",
                help="set the output directory for the data"),

    make_option(c("-e", "--use-example"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="use example file instead of real file"),

    make_option(c("-t", "--troubleshooting"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="enable if troubleshooting to prevent overwriting your files")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
troubleshooting = opt[['troubleshooting']]
if (opt[['use-example']]) {
    opt[['input-file']] = 'data/familytree/sample_ped_tab.csv'
}

# Start Log
start_time = Sys.time()
log <- log_open(paste0("plot_familytree-",
                       strftime(start_time, format="%Y%m%d_%H%M%S"), '.log'))
log_print(paste('Script started at:', start_time))


# ----------------------------------------------------------------------
# Pre-script functions

# configs
col_to_new_col = c(
    'father'='father_id',
    'mother'='mother_id',
    'id'='mouse_id',
    'ped'='strain'
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
        if (!identical(missing_parents, integer(0))) {
            missing_parents_df <- data.frame(
                use = 'breeding',
                strain = '1 - B6',
                sex = gender_for_parent_id[parent],
                mouse_id = missing_parents,
                father_id = 0,
                mother_id = 0,
                dead = 0
            )
            result <- rbind(result, missing_parents_df)
        }
    }
    return(result)
}


# rename columns
preprocessing <- function(df) {

    # rename columns
    colnames(df) <- unlist(lapply(colnames(df), dotsep_to_snake_case))
    df <- rename_columns(df, col_to_new_col)

    # impute missing columns
    if (!('pcr_confirmation' %in% colnames(df))){
        df[['pcr_confirmation']] = NA
    }
    if (!('dead' %in% colnames(df))){
        df[['dead']] = 0
    }

    # impute missing parents
    missing_parents <- generate_missing_parents(df)
    if (!is.null(missing_parents)) {
        df <- add_row(df, missing_parents)
    }

    # impute missing values
    # NOTE: inplace does not work within functions
    df <- fillna(df, c('father_id', 'mother_id', 'dead'), 0)

    # remove self parents
    for (col in c('father_id', 'mother_id')) {
        df[df[['mouse_id']]==df[[col]], c('father_id', 'mother_id')] <- 0
    }

    return(df)
}


# ----------------------------------------------------------------------
# Read Data

df <- read.csv(file.path(wd, opt[['input-file']]), header=TRUE)
if (basename(opt[['input-file']]) == 'sample_ped_tab.csv') {
    rename_columns(df, c("affected"="pcr_confirmation", "avail"="dead"), inplace=TRUE)
}
df <- preprocessing(df)

# autoassign colors
strains <- unique(df[['strain']])
strains_to_color = brewer.pal(n = length(strains), name = "Set1")
names(strains_to_color) = sort(strains)
df[['color']] = unlist(lapply(df[['strain']], function(x) strains_to_color[[x]]))

# save
if (!troubleshooting) {
    directory = file.path(wd, dirname(opt[['input-file']]), 'troubleshooting')
    if (!dir.exists(directory)) {
        dir.create(directory, recursive=TRUE)
    }
    filepath = file.path(
        directory,
        paste0('_', tools::file_path_sans_ext(basename(opt[['input-file']])), '.csv')  # filename
    )
    write.table(df, file = filepath, row.names = FALSE, sep = ',' )
}


# ----------------------------------------------------------------------
# Create Pedigree

tree <- pedigree(
    id = df[['mouse_id']],
    dadid = df[['father_id']],
    momid = df[['mother_id']],
    sex = df[['sex']],
    famid = rep(1, nrow(df))
)[1]
if (basename(opt[['input-file']]) == 'sample_ped_tab.csv') {
    names = df[['mouse_id']]
} else {
    names = paste0(df[['mouse_id']], '\n', df[['age']], 'd')
}

if (!troubleshooting) {
    directory = file.path(wd, opt[['output-dir']])
    if (!dir.exists(directory)) {
        dir.create(directory, recursive=TRUE)
    }
    filepath = file.path(
        directory,  # dir
        paste0(tools::file_path_sans_ext(basename(opt[['input-file']])), '.png')  # filename
    )

    png(filepath,
        width = 5000, height = 3000, units = "px", bg = "white",
        res = 1200, pointsize = 5
    )

    plot(tree,
         id = names,
         affected = df[['pcr_confirmation']],
         status = df[['dead']],
         col = df[['color']],
         width = 8, branch = 1,
         symbolsize = 0.7,
         cex = 0.7
    )
}
