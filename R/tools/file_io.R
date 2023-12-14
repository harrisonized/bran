import::here(readxl, 'read_excel')

## Functions
## read_excel_or_csv


#' Switch case to read excel or csv based on the extension
#'
#' @description Mainly used to simplify scripts
#' 
#' @usage
#' # for 96-well plates:
#' df <- read_csv_from_text(
#'     file_path,
#'     skiprows=3, nrows=8,
#'     skipcols=2, ncols=12,
#'     index=LETTERS[1:8],
#'     columns=seq(1, 12)
#' )
#' 
read_excel_or_csv <- function(filepath) {
    ext=tools::file_ext(filepath)
    if (ext == 'xlsx') {
        df <- read_excel(filepath)
    } else if (ext == 'csv') {
        df <- read.csv(filepath, header=TRUE)
    } else {
        log_print(paste(Sys.time(), 'Please enter a xlsx or csv file.'))
        stop()
    }
    return(df)
}
