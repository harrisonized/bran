## Weave the new and old data together
## Automatically outputs to the troubleshooting folder

wd = dirname(this.path::here())  # wd = '~/github/R/bran'
suppressMessages(library('dplyr'))
library('optparse')
library('logr')
source(file.path(wd, 'R', 'preprocessing.R'))
source(file.path(wd_, 'R', 'functions', 'df_tools.R'))  # read_excel_or_csv


# ----------------------------------------------------------------------
# Pre-script settings

# args
option_list = list(
    make_option(c("-i", "--initial-data"), default='data/initial_data.csv',
                metavar='data/initial_data.csv',
                type="character",help="the data you had before, including past annotations"),

    make_option(c("-n", "--new-data"), default='data/My_Mice.xlsx',
                metavar='data/My_Mice.xlsx',
                type="character",help="new file downloaded directly from transnetyx"),

    make_option(c("-t", "--troubleshooting"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="enable if troubleshooting to prevent overwriting your files")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
troubleshooting = opt[['troubleshooting']]

# Start Log
start_time = Sys.time()
log <- log_open(paste0("weave_data-",
                       strftime(start_time, format="%Y%m%d_%H%M%S"), '.log'))
log_print(paste('Script started at:', start_time))


# ----------------------------------------------------------------------
# Main

# read data
old_data = read_excel_or_csv(file.path(wd, opt[['initial-data']]))
old_data <- preprocessing(old_data)
new_data = read_excel_or_csv(file.path(wd, opt[['new-data']]))
new_data <- preprocessing(new_data, impute_missing_parents=FALSE)

# add missing mice
new_data <- rbind(new_data, dplyr::anti_join(old_data, new_data, by='mouse_id'))  # add missing mice from old to new
old_data <- rbind(old_data, dplyr::anti_join(new_data, old_data, by='mouse_id'))  # add new mice

# overwrite update_cols using data from new_data
update_cols = c('age', 'genotype', 'labels', 'cage_id', 'notes', 'rack', 'position')
df <- left_join(
    old_data[, c(items_in_a_not_b(colnames(old_data), update_cols))],
    new_data[, c('mouse_id', update_cols)],
    by='mouse_id'
)
df <- preprocessing(df)
df <- df[, colnames(old_data)]  # reorder columns

# update date instead of relying on Transnetyx
df[['age']] <- as.integer(difftime(Sys.Date(), as.Date(df[['dob']], "%m/%d/%y")))

# save
if (!troubleshooting) {
    directory = file.path(wd, dirname(opt[['initial-data']]), 'troubleshooting')
    if (!dir.exists(directory)) {
        dir.create(directory, recursive=TRUE)
    }

    # merged file
    filepath = file.path(
        directory,
        paste0('_', tools::file_path_sans_ext(basename(opt[['initial-data']])), '.csv')  # filename
    )
    write.table(df, file = filepath, row.names = FALSE, sep = ',' )
}


end_time = Sys.time()
log_print(paste('Script ended at:', Sys.time()))
log_print(paste("Script completed in:", difftime(end_time, start_time)))
log_close()
