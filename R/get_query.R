#' Download data from SCB API
#'
#' @param url SCB API or GUI URL
#' @param max_filter_values Limit number of values per filter
#'
#' @return A tibble with SCB data
#' @export
get_query <- function(url, max_filter_values=0){
  get_scb_data(url=url, print_query = T, max_filter_values = max_filter_values)
}
