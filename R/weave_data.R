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
# Merge Data

update_cols = c('age', 'genotype', 'labels', 'cage_id', 'notes', 'rack', 'position')

old = read_excel_or_csv(file.path(wd, opt[['old-file']]))
old <- preprocessing(old)
new = read_excel_or_csv(file.path(wd, opt[['new-file']]))
new <- preprocessing(new)

new_mice = dplyr::anti_join(new, old, by='mouse_id')
df <- rbind(old, new_mice)


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
