## Functions
## check_if_a_in_b
## items_in_a_not_b
## filter_list_for_match
## replace_specific_items


#' https://stackoverflow.com/questions/53086053/how-to-check-if-a-list-contains-a-certain-element-in-r
#' 
#' @export
check_if_a_in_b <- function(a, b) {
    return (as.logical(
        sum(unlist( lapply(b, function(x) ifelse(x==a,1,0)) ))
    ))
}


#' https://stackoverflow.com/questions/10298662/find-elements-not-in-smaller-character-vector-list-but-in-big-list
#' 
#' @export
items_in_a_not_b <- function(a, b) {
    return((new <- a[which(!a %in% b)]))
}


#' return elements of a list matching a particular substring
#'
#' @examples
#' filter_list_for_match(c("gene_id_pat", "gene_id_mat", "count"), "pat")
#' 
#' @export
filter_list_for_match <- function(items, pattern) {
    # filter
    for (i in 1:length(pattern)){
        items <- lapply(items, grep, pattern=pattern[[i]], value=TRUE)
    }
    return (unlist(items[!sapply(items, identical, character(0))]))  # remove character(0)
}


#' Replace specific items
#' Use this to rename columns
#'
#' @export
replace_specific_items <- function(items, replacer) {
    replace_ids <- which(items %in% intersect(names(replacer), items))
    for (idx in replace_ids) {
        items[idx] <- replacer[items[idx]]
    }
    return(items)
}
