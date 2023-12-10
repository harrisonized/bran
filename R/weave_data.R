## Weave the new and old data together
## Automatically outputs to the troubleshooting folder

wd = dirname(this.path::here())  # wd = '~/github/R/bran'
suppressPackageStartupMessages(library('dplyr'))
library('optparse')
library('logr')
import::here(file.path(wd, 'R', 'utils', 'file_io.R'),
    'read_excel_or_csv', .character_only=TRUE)
import::here(file.path(wd, 'R', 'utils', 'list_tools.R'),
    'items_in_a_not_b', .character_only=TRUE)
import::here(file.path(wd, 'R', 'functions', 'preprocessing.R'),
    'generate_missing_parents', 'preprocessing', .character_only=TRUE)


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
missing_mice <- dplyr::anti_join(old_data, new_data, by='mouse_id')
new_mice <- dplyr::anti_join(new_data, old_data, by='mouse_id')

update_cols = c('age', 'genotype', 'labels', 'cage_id', 'notes', 'rack', 'position')

# merge, overwrite update_cols using new_data
new_data <- rbind(new_data, missing_mice)  # add missing_mice to new_data
old_data <- rbind(old_data, new_mice)  # add new_mice to old_data
df <- merge(
    old_data[, c(items_in_a_not_b(colnames(old_data), update_cols))],
    new_data[, c('mouse_id', update_cols)],
    by='mouse_id'
)

df <- preprocessing(df)
df <- df[, colnames(old_data)]  # reorder columns


# save
if (!troubleshooting) {
    directory = file.path(wd, dirname(opt[['initial-data']]), 'troubleshooting')
    if (!dir.exists(directory)) {
        dir.create(directory, recursive=TRUE)
    }

    filepath = file.path(
        directory,
        paste0('_', tools::file_path_sans_ext(basename(opt[['initial-data']])), '.csv')  # filename
    )

    # merged file
    write.table(df, file = filepath, row.names = FALSE, sep = ',' )

    # missing mice
    write.table(
        missing_mice,
        file = file.path(paste0(tools::file_path_sans_ext(filepath), '-missing', '.csv')),
        row.names = FALSE, sep = ','
    )
}


end_time = Sys.time()
log_print(paste('Script ended at:', Sys.time()))
log_print(paste("Script completed in:", difftime(end_time, start_time)))
log_close()
