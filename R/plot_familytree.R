## Example code to use kinship2
## See: https://cran.r-project.org/web/packages/kinship2/vignettes/pedigree.html

wd = dirname(this.path::here())  # wd = '~/github/R/harrisonRTools'
library("kinship2")
library('optparse')
library('logr')


# ----------------------------------------------------------------------
# Pre-script settings

# args
option_list = list(
    make_option(c("-i", "--input-file"), default='data/familytree/sample_ped_tab.csv',
                metavar='data/familytree/sample_ped_tab.csv',
                type="character",help="path/to/sample_ped_tab.csv"),
   
    make_option(c("-o", "--output-dir"), default="figures/familytree",
                metavar="figures/familytree", type="character",
                help="set the output directory for the data"),

    make_option(c("-t", "--troubleshooting"), default=FALSE, action="store_true",
                metavar="FALSE", type="logical",
                help="enable if troubleshooting to prevent overwriting your files")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
troubleshooting = opt[['troubleshooting']]

# Start Log
start_time = Sys.time()
log <- log_open(paste0("plot_familytree-",
                       strftime(start_time, format="%Y%m%d_%H%M%S"), '.log'))
log_print(paste('Script started at:', start_time))


# ----------------------------------------------------------------------
# Main

df <- read.csv(file.path(wd, opt[['input-file']]), header=TRUE)

# make pedigree object
pedigree_list <- pedigree(
    id = df[['id']],
    dadid = df[['father']],
    momid = df[['mother']],
    sex = df[['sex']],
    famid = df[['ped']]
)

if (!troubleshooting) {
    if (!dir.exists(file.path(wd, opt[['output-dir']]))) {
        dir.create(file.path(wd, opt[['output-dir']]), recursive=TRUE)
    }
    png(file.path(wd, opt[['output-dir']],
                  sub(".csv", ".png", basename(opt[['input-file']]))),
        width = 4000, height = 4000, units = "px", bg = "white",
        res = 1200, pointsize = 5
    )
    plot(pedigree_list['1'],
         # id = df[['id']],
         status =df[(df['ped']==1), 'avail'], 
         affected = df[(df['ped']==1), 'affected'],
         width = 8, branch = 1,
         symbolsize = 0.8,
         cex = 0.8
    )
    dev.off()
}
