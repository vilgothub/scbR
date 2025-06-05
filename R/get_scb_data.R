#' Download data from SCB API
#'
#' @param url SCB API or GUI URL
#' @param query_li Optional query list
#' @param print_query Print and copy R code to clipboard
#' @param max_filter_values Limit number of values per filter
#' @param write_clipboard Copy output to clipboard
#'
#' @return A tibble with SCB data
#' @export
get_scb_data <- function(url, query_li = list(), print_query = FALSE, max_filter_values = 0, write_clipboard = TRUE) {

  if (grepl('https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START', url, fixed = TRUE)) {
    url <- get_opposite_url(url = url, write_clipboard = FALSE)
  }

  variable_list <- get_variable_list(url = url)

  if (length(query_li) == 0) {
    query_li <- get_full_query(url = url, variable_list = variable_list, print_query = print_query,
                               max_filter_values = max_filter_values,
                               write_clipboard = write_clipboard)
  }

  if (!print_query) {
    partitioned_queries <- check_if_partition_needed(query = query_li, variable_list = variable_list)

    queries <- list()
    if(length(partitioned_queries) > length(query_li)){
      for(i in 1:(length(partitioned_queries)/length(query_li))){
        start <- (i - 1) * length(query_li) + 1
        end <- start + length(query_li) - 1
        queries[[length(queries)+1]] <- partitioned_queries[start:end]
      }
    }else{
      queries[[length(queries)+1]] <- partitioned_queries
    }
    # print(queries)

    parsed_queries <- list()
    for(query in queries){
      parsed_query <- list(
        'query' = list(),
        'response' = list(format = "json")
      )
      # print(query)
      # print(class(query))
      # print(names(query))
      for (i in seq_along(names(query))) {
        variable <- names(query)[i]
        values_ <- query[[variable]]
        sub_q <- list(
          code = variable_list$variables$code[i],
          selection = list(
            filter = "item",
            values = values_
          )
        )
        parsed_query$query[[i]] <- sub_q
      }
      parsed_query <- manage_function_list_query(list_query = parsed_query)
      parsed_queries[[length(parsed_queries)+1]] <- parsed_query
    }

    dfli <- list()
    for (i in seq_along(parsed_queries)) {
      if((length(parsed_queries)>1) & (i ==1)){
        total <- length(parsed_queries)
        pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
      }
      json_query <- jsonlite::toJSON(parsed_queries[[i]], auto_unbox = TRUE)
      df <- fetch_data_from_query(url=url, query = json_query)
      dfli[[length(dfli)+1]] <- df

      if(length(parsed_queries)>1){
        utils::setTxtProgressBar(pb, i)  # Update progress bar
      }
    }
    df <- add_text_variables(dfli, variable_list = variable_list)
    return(df)
  }
}
