## Plots a pedigree from Transnetyx output
## See: https://cran.r-project.org/web/packages/kinship2/vignettes/pedigree.html

wd = dirname(this.path::here())  # wd = '~/github/R/bran'
suppressMessages(library('kinship2'))
library('optparse')
library('logr')
source(file.path(wd, 'R', 'preprocessing.R'))
source(file.path(wd_, 'R', 'functions', 'df_tools.R'))  # read_excel_or_csv


# ----------------------------------------------------------------------
# Pre-script settings

# args
option_list = list(
    make_option(c("-i", "--input-file"), default='data/sample_ped_tab.csv',
                metavar='data/sample_ped_tab.csv',
                type="character",help="path/to/input/file"),
   
    make_option(c("-o", "--output-dir"), default="figures",
                metavar="figures", type="character",
                help="set the output directory for the data"),

    make_option(c("-l", "--height"), default=3000,
                metavar="3000", type="integer",
                help="height in px, -h is protected, -l for length"),

    make_option(c("-w", "--width"), default=5000,
                metavar="5000", type="integer",
                help="width in px, max width is 200000"),

    make_option(c("-c", "--cex"), default=0.6,
                metavar="0.6", type="numeric",
                help="can reduce this if text is overlapping"),

    make_option(c("-p", "--ploidy"), default=2,
                metavar="2", type="integer",
                help="ploidy: 1 = homozygous, 2 = heterozygous"),

    make_option(c("-j", "--jpg"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="use jpg instead of png"),  # note: the jpg is larger than the png

    make_option(c("-d", "--show-dead"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="by default, dead mice are automatically hidden"),

    make_option(c("-a", "--show-all"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="unhides all the manually-ignored mice"),

    make_option(c("-t", "--troubleshooting"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="enable if troubleshooting to prevent overwriting your files")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
troubleshooting = opt[['troubleshooting']]
if (opt[['width']] > 200000) {
    opt[['width']] = 200000
}

# Start Log
start_time = Sys.time()
log <- log_open(paste0("plot_family_tree-",
                       strftime(start_time, format="%Y%m%d_%H%M%S"), '.log'))
log_print(paste('Script started at:', start_time))


# ----------------------------------------------------------------------
# Read Data

df = read_excel_or_csv(file.path(wd, opt[['input-file']]))
df <- preprocessing(df)


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


# ignore certain mice
parents = get_unique_values(df, c('father_id', 'mother_id'))
if (opt[['show-all']]) {
    df[['ignore']] = 0
}
if (opt[['show-dead']]) {
    mask = !(df[['mouse_id']] %in% parents) &
            (df[['ignore']] == 1)
} else {
    mask = !(df[['mouse_id']] %in% parents) &
            ((df[['ignore']] == 1) | (df[['alive']] == 0))
}
df <- df[!mask, ]  # filter


tree <- pedigree(
    id = df[['mouse_id']],
    dadid = df[['father_id']],
    momid = df[['mother_id']],
    sex = df[['sex']],
    famid = rep(1, nrow(df))
)[1]


# set names
if (basename(opt[['input-file']]) == 'sample_ped_tab.csv') {
    names = df[['mouse_id']]
} else {
    rack_names = gsub(".* ([0-9]+[Aa|Bb]).*", "\\1", df[['rack']])  # regex match Rack 1A
    positions = ifelse((df[['position']]=='Unassigned'), NA, df[['position']])
    names = paste0(
        df[['mouse_id']], '\n',
        rack_names, ', ', positions, '\n',
        df[['dob']], '\n',
        df[['age']], 'd'
    )
}

# construct affected matrix
if (opt[['ploidy']] == 1) {
    affected <- df[["pcr_confirmation"]]
} else {
    # defaults to 2
    affected <- as.matrix(df[, c('chr_m', 'chr_p')])
}

# save
if (!troubleshooting) {
    directory = file.path(wd, opt[['output-dir']])
    if (!dir.exists(directory)) {
        dir.create(directory, recursive=TRUE)
    }

    # switch
    if (opt[['jpg']]) {
        ext = '.jpg'
        img <- jpeg
    } else {
        ext = '.png'
        img <- png
    }
    filepath = file.path(
        directory,  # dir
        paste0(tools::file_path_sans_ext(basename(opt[['input-file']])), ext)  # filename
    )

    img(filepath,
        width = opt[['width']], height = opt[['height']],
        units = "px", bg = "white",
        res = 1200, pointsize = 5
    )

    plot(tree,
         id = names,
         affected = affected,
         status = 1-df[['alive']],
         col = df[['color']],
         symbolsize = 0.8,
         cex = opt[['cex']],
         branch = 1,
         angle = rep(0, length(df)),
         density = rep(100, length(df))
    )

    dev.off()
}

end_time = Sys.time()
log_print(paste('Script ended at:', Sys.time()))
log_print(paste("Script completed in:", difftime(end_time, start_time)))
log_close()
