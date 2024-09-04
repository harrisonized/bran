## Use the new data to update the old data
## Automatically outputs to the troubleshooting folder

wd = dirname(this.path::here())  # wd = '~/github/R/bran'
library('optparse')
suppressPackageStartupMessages(library('logr'))
import::from(dplyr, 'anti_join')
import::here(file.path(wd, 'R', 'tools', 'file_io.R'),
    'read_excel_or_csv', .character_only=TRUE)
import::here(file.path(wd, 'R', 'tools', 'list_tools.R'),
    'items_in_a_not_b', .character_only=TRUE)
import::here(file.path(wd, 'R', 'functions', 'preprocessing.R'),
    'preprocessing', .character_only=TRUE)


# ----------------------------------------------------------------------
# Pre-script settings

# args
option_list = list(
    make_option(c("-i", "--initial-data"), default='data/initial_data.csv',
                metavar='data/initial_data.csv',
                type="character", help="the data you had before, including past annotations"),

    make_option(c("-n", "--new-data"), default='data/My_Mice.xlsx',
                metavar='data/My_Mice.xlsx',
                type="character", help="new file downloaded directly from Transnetyx"),
    
    make_option(c("-o", "--output-dir"), default='data/output',
                metavar='data/output', type="character",
                help="set the output directory"),

    make_option(c("-t", "--troubleshooting"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="enable if troubleshooting to prevent overwriting your files")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
troubleshooting = opt[['troubleshooting']]

cols_to_update = c(
    'use', 'age', 'genotype', 'labels', 'cage_id', 'notes', 'system_id', 'rack', 'position'
)

# Start Log
start_time = Sys.time()
log <- log_open(paste0("update_data-",
                       strftime(start_time, format="%Y%m%d_%H%M%S"), '.log'))
log_print(paste('Script started at:', start_time))


# ----------------------------------------------------------------------
# Main

# read data
old_data = read_excel_or_csv(file.path(wd, opt[['initial-data']]))
old_data <- preprocessing(old_data)

new_data = read_excel_or_csv(file.path(wd, opt[['new-data']]))
new_data <- preprocessing(new_data, impute_missing_parents=FALSE)

missing_mice <- anti_join(old_data, new_data, by='mouse_id')
new_mice <- anti_join(new_data, old_data, by='mouse_id')

# merge, overwrite cols_to_update using new_data
new_data <- rbind(new_data, missing_mice)  # add missing_mice to new_data
old_data <- rbind(old_data, new_mice)  # add new_mice to old_data
df <- merge(
    old_data[, c(items_in_a_not_b(colnames(old_data), cols_to_update))],
    new_data[, c('mouse_id', cols_to_update)],
    by='mouse_id'
)

df <- preprocessing(df)
df <- df[, colnames(old_data)]  # reorder columns


# save
if (!troubleshooting) {
    if (!dir.exists(opt[['output-dir']])) {
        dir.create(opt[['output-dir']], recursive=TRUE)
    }

    filepath = file.path(
        opt[['output-dir']],
        paste0('_', tools::file_path_sans_ext(basename(opt[['initial-data']])), '.csv')  # filename
    )

    # merged file
    write.table(df, file = filepath, row.names = FALSE, sep = ',' )

    # missing mice
    write.table(
        missing_mice[(missing_mice[['alive']]==1), ],
        file = file.path(paste0(tools::file_path_sans_ext(filepath), '-missing', '.csv')),
        row.names = FALSE, sep = ','
    )
}


end_time = Sys.time()
log_print(paste('Script ended at:', Sys.time()))
log_print(paste("Script completed in:", difftime(end_time, start_time)))
log_close()
