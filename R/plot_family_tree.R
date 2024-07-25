## Plots a pedigree from Transnetyx output
## See: https://cran.r-project.org/web/packages/kinship2/vignettes/pedigree.html

wd = dirname(this.path::here())  # wd = '~/github/R/bran'
library('optparse')
suppressPackageStartupMessages(library('logr'))
import::from(kinship2,'pedigree')
import::from(file.path(wd, 'R', 'functions', 'preprocessing.R'),
    'preprocessing', .character_only=TRUE)
import::from(file.path(wd, 'R', 'tools', 'file_io.R'),
    'read_excel_or_csv', .character_only=TRUE)
import::from(file.path(wd, 'R', 'tools', 'df_tools.R'),
    'get_unique_values', 'fillna', .character_only=TRUE)
import::from(file.path(wd, 'R', 'functions', 'computations.R'),
    'generate_display_text', .character_only=TRUE)


# ----------------------------------------------------------------------
# Pre-script settings

# args
option_list = list(
    make_option(c("-i", "--input-file"), default='data/sample_ped_tab.csv',
                metavar='data/sample_ped_tab.csv',
                type="character",help="path/to/input/file"),

    make_option(c("-o", "--output-dir"), default="data/output",
                metavar="data/output", type="character",
                help="set the output directory for the data"),

    make_option(c("-f", "--figures-dir"), default="figures",
                metavar="figures", type="character",
                help="set the output directory for the figures"),

    make_option(c("-l", "--height"), default=3000,
                metavar="3000", type="integer",
                help="height in px"),

    make_option(c("-w", "--width"), default=5000,
                metavar="5000", type="integer",
                help="width in px, max width is 200000"),

    make_option(c("-c", "--cex"), default=0.6,
                metavar="0.6", type="numeric",
                help="can reduce this if text is overlapping"),

    make_option(c("-p", "--ploidy"), default=2,
                metavar="2", type="integer",
                help="Split circle? Choose 1 or 2."),

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

# switch
if (opt[['jpg']]) {
    ext <- '.jpg'
    img <- jpeg
} else {
    ext <- '.png'
    img <- png
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
    if (!dir.exists(opt[['output-dir']])) {
        dir.create(opt[['output-dir']], recursive=TRUE)
    }
    filepath = file.path(
        opt[['output-dir']],
        paste0('_', tools::file_path_sans_ext(basename(opt[['input-file']])), '.csv')  # filename
    )
    write.table(df, file = filepath, row.names = FALSE, sep = ',' )
}


# ----------------------------------------------------------------------
# Create Pedigree

# show or ignore mice
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


display_text <- generate_display_text(df, id_only=FALSE)

# fill in circles
if (opt[['ploidy']] >= 2) {
    fill <- as.matrix(df[, c('chr_m', 'chr_p')])  # default
} else {
    fill <- df[["pcr_confirmation"]]
}

# save
if (!troubleshooting) {
    directory = file.path(wd, opt[['figures-dir']])
    if (!dir.exists(directory)) {
        dir.create(directory, recursive=TRUE)
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

    withCallingHandlers({
        plot(tree,
             id = display_text,
             affected = fill,
             status = 1-df[['alive']],
             col = df[['color']],
             symbolsize = 0.8,
             cex = opt[['cex']],
             branch = 0.8,
             angle = rep(0, length(df)),
             density = rep(100, length(df))
        )
    }, warning=function(w) {
        if ( any(grepl("no non-missing arguments", w)) ) {
            invokeRestart("muffleWarning")
        }
    })

    tmp <- dev.off()
}

end_time = Sys.time()
log_print(paste('Script ended at:', Sys.time()))
log_print(paste("Script completed in:", difftime(end_time, start_time)))
log_close()
