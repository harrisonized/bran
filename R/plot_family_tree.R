## Example code to use kinship2
## See: https://cran.r-project.org/web/packages/kinship2/vignettes/pedigree.html

wd = dirname(this.path::here())  # wd = '~/github/R/harrisonRTools'
library("kinship2")
library('optparse')
library('logr')
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

# configs
col_to_new_col = c(
    'father'='father_id',
    'mother'='mother_id',
    'id'='mouse_id',
    'ped'='strain'
)


# ----------------------------------------------------------------------
# Main

# Read data and preprocessing
df <- read.csv(file.path(wd, opt[['input-file']]), header=TRUE)
colnames(df) <- unlist(lapply(colnames(df), dotsep_to_snake_case))
df <- rename_columns(df, col_to_new_col)

# Label F0
fillna(df, 'father_id', 0, inplace=TRUE)
father_ids <- unique(df[['father_id']])
father_ids <- father_ids[father_ids!=0]
missing_fathers <- items_in_a_not_b(father_ids, df[['mouse_id']])

fillna(df, 'mother_id', 0, inplace=TRUE)
mother_ids <- unique(df[['mother_id']])
mother_ids <- mother_ids[mother_ids!=0]
missing_mothers <- items_in_a_not_b(mother_ids, df[['mouse_id']])


# make pedigree object
pedigree_list <- pedigree(
    id = df[['mouse_id']],
    dadid = df[['father_id']],
    momid = df[['mother_id']],
    sex = df[['sex']],
    famid = rep(1, nrow(df))
)

strain=1
if (!troubleshooting) {
    if (!dir.exists(file.path(wd, opt[['output-dir']]))) {
        dir.create(file.path(wd, opt[['output-dir']]), recursive=TRUE)
    }

    filepath = file.path(
        wd, opt[['output-dir']],
        paste0(
            tools::file_path_sans_ext(basename(opt[['input-file']])),
            '-1', '.png')
    )
    png(filepath,
        width = 5000, height = 3000, units = "px", bg = "white",
        res = 1200, pointsize = 5
    )

    plot(pedigree_list[1],
         id = paste0(df[['mouse_id']], '\n', df[['age']], 'd'),
         affected = df[['pcr_confirmation']],
         status = df[['dead']],
         width = 8, branch = 1,
         symbolsize = 0.7,
         cex = 0.7
    )
}


