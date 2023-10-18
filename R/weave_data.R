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
    make_option(c("-o", "--old-file"), default='data/family-tree/old_data.csv',
                metavar='data/family-tree/old_data.csv',
                type="character",help="template file where you listed your annnotations"),

    make_option(c("-n", "--new-file"), default='data/family-tree/My_Mice.xlsx',
                metavar='data/family-tree/My_Mice.xlsx',
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
old_df = read_excel_or_csv(file.path(wd, opt[['old-file']]))
old_df <- preprocessing(old_df)
new_df = read_excel_or_csv(file.path(wd, opt[['new-file']]))
new_df <- preprocessing(new_df, impute_missing_parents=FALSE)

# add missing mice
new_df <- rbind(new_df, dplyr::anti_join(old_df, new_df, by='mouse_id'))  # add missing mice from old to new
old_df <- rbind(old_df, dplyr::anti_join(new_df, old_df, by='mouse_id'))  # add new mice

# overwrite update_cols using data from new_df
update_cols = c('age', 'genotype', 'labels', 'cage_id', 'notes', 'rack', 'position')
df <- left_join(
    old_df[, c(items_in_a_not_b(colnames(old_df), update_cols))],
    new_df[, c('mouse_id', update_cols)],
    by='mouse_id'
)
df <- preprocessing(df)
df <- df[, colnames(old_df)]  # column order

# save
if (!troubleshooting) {
    directory = file.path(wd, dirname(opt[['old-file']]), 'troubleshooting')
    if (!dir.exists(directory)) {
        dir.create(directory, recursive=TRUE)
    }

    # merged file
    filepath = file.path(
        directory,
        paste0('_', tools::file_path_sans_ext(basename(opt[['old-file']])), '.csv')  # filename
    )
    write.table(df, file = filepath, row.names = FALSE, sep = ',' )
}


end_time = Sys.time()
log_print(paste('Script ended at:', Sys.time()))
log_print(paste("Script completed in:", difftime(end_time, start_time)))
log_close()
