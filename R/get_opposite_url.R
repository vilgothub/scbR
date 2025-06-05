#' Send the URL to a SCB table.
#'
#' Get the URL to the corresponding GUI or API table.
#'
#' @param url A string value URL-adress to a SCB table.
#' @return A string value URL to the corresponding GUI or API table.
#' @export
get_opposite_url <- function(url, write_clipboard = TRUE){
  # Apply the function to single values or vectors
  out_urls <- if (length(url) > 1) {
    sapply(url, process_url, USE.NAMES = FALSE)
  } else {
    process_url(url)
  }

  # Optionally write to clipboard (only for single value)
  if (write_clipboard && length(url) == 1) {
    writeClipboard(out_urls)
  }

  return(out_urls)
}

#' Helper function (not exported)
#' @param api_url A string value URL to a SCB table.
#' @return The corresponding url to the GUI table
#' @keywords internal
get_gui_url <- function(api_url){
  gui_url = gsub('https://api.scb.se/OV0104/v1/doris/sv/ssd/', '', api_url)
  separated <- strsplit(gui_url, split='/')[[1]]
  last <- separated[length(separated)]
  gui_url = sub("/[^/]+$", "", gui_url)
  gui_url = gsub("/", "__", gui_url)
  gui_url = paste0('https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__', gui_url, '/', last)
  return(gui_url)
}

#' Helper function (not exported)
#' @param gui_url A string value URL to a SCB table.
#' @return The corresponding url to the API table
#' @keywords internal
get_api_url <- function(gui_url){
  api_url <- gsub('https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__', '', gui_url)
  api_url <- gsub('__','/',api_url)
  api_url = paste0('https://api.scb.se/OV0104/v1/doris/sv/ssd/', api_url)
  return(api_url)
}


#' Helper function (not exported)
#' @param single_url A string value URL to a SCB table.
#' @keywords internal
process_url <- function(single_url) {
  if (grepl('api.scb.se', single_url, fixed = TRUE)) {
    out_url <- get_gui_url(api_url = single_url)
  } else {
    out_url <- get_api_url(gui_url = single_url)
  }
  return(out_url)
}
