#' Helper functions (not exported)
#' @param list_query A list containing query elements.
#' @keywords internal
manage_function_list_query <- function(list_query) {
  query <- list_query
  for (i in seq_along(query$query)) {
    values_ <- query$query[[i]]$selection$values
    if (length(values_) == 1) {
      query$query[[i]]$selection$values <- list(values_)
    }
  }
  return(query)
}

#' Get variable list from SCB
#' @param url URL to SCB API
#' @keywords internal
get_variable_list <- function(url) {
  res <- httr::GET(url)
  variable_list <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  return(variable_list)
}

#' Create full SCB query
#' @param url API endpoint
#' @param variable_list List of variables from SCB API
#' @param print_query Logical, whether to print the query
#' @param max_filter_values Integer, maximum number of values to filter
#' @param write_clipboard Logical, write query to clipboard
#' @keywords internal
get_full_query <- function(url, variable_list, print_query = FALSE, max_filter_values = 0, write_clipboard = TRUE) {
  string_query <- "\n# Query list \nquery_li <- list(\n"
  query_li <- list()

  for (i in seq_along(variable_list$variables$code)) {
    code <- variable_list$variables$code[i]
    values <- variable_list$variables$values[[i]]
    labels <- variable_list$variables$valueTexts[[i]]

    colwidth <- max(nchar(labels) + nchar(values) + 6) + 10
    console_width <- getOption('width')
    columns <- pmax(1, floor(console_width / colwidth))

    if (max_filter_values > 0) {
      if (code == 'Tid') values <- rev(values)
      values <- values[1:min(length(values), max_filter_values)]
      labels <- labels[1:min(length(labels), max_filter_values)]
    }

    query_li[[code]] <- values
    string_query <- paste0(string_query, '\t"', code, '" = c(')

    for (j in seq_along(values)) {
      value <- values[j]
      label <- labels[j]
      modulus_threshold <- ifelse(columns == 1, 0, 1)

      if (j %% columns == modulus_threshold) {
        string_query <- paste0(string_query, '\n\t\t')
      }

      if (code == 'Tid') {
        string_query <- paste0(string_query, '"', value, '"')
      } else {
        new_part <- paste0('"', label, '" = "', value, '"')
        spaces_to_add <- colwidth - nchar(new_part)
        string_query <- paste0(string_query, new_part)
      }

      if (j < length(values)) {
        spaces <- if (code == 'Tid') ' ' else strrep(" ", spaces_to_add)
        string_query <- paste0(string_query, ",", spaces)
      } else {
        string_query <- paste0(string_query, '\n\t)')
      }
    }

    string_query <- if (i != length(variable_list$variables$code)) {
      paste0(string_query, ',\n')
    } else {
      paste0(string_query, '\n)\n')
    }
  }

  if (print_query) {
    if (!is.character(url)) {
      print(url)
      stop("The 'url' argument must be a character string.")
    }
    string_query <- paste0(string_query, '\n# Get and store data\nurl <- "', url, '"\ndf <- get_scb_data(url = url, query_li = query_li)\n')
    cat(string_query)
    if (write_clipboard) {
      message("Query written to clipboard")
      utils::writeClipboard(string_query)
    }
  }

  return(query_li)
}

#' Fetch data from SCB query
#' @param url API endpoint
#' @param query Query list
#' @keywords internal
fetch_data_from_query <- function(url, query) {
  res <- httr::POST(url, body = query)
  res_json <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  keys <- lapply(res_json$data$key, function(row) {return(row)})
  values <- lapply(res_json$data$values, function(row) {return(row)})
  df <- tibble::as_tibble(data.frame(cbind(do.call(rbind, keys), do.call(rbind, values))))
  colnames(df) <- res_json$columns$code

  return(df)
}

#' Check if SCB query should be partitioned
#' @param query Query list
#' @param variable_list SCB variable list
#' @param treshold Threshold for max combinations
#' @param out_queries Recursive accumulator
#' @keywords internal
check_if_partition_needed <- function(query, variable_list, treshold = 100000, out_queries=list()) {
  # query <- query_li
  for (i in seq_along(names(query))) {
    variable <- names(query)[i]
    values <- query[[variable]]
    if(length(values)==1){
      if(values == "*"){
        indx <- which(variable_list$variables$code == variable)
        values <- variable_list$variables$values[[i]]
        query[[variable]] <- values
      }
    }
  }
  var_lengths <- sapply(query, length)
  combinations <- unlist(lapply(1:length(var_lengths), function(i) combn(var_lengths, i, FUN = prod, simplify = FALSE)), recursive = FALSE)
  prods <- sapply(combinations, prod)
  indx <- which(prods > treshold)
  max_prod <- max(prods)
  if(max_prod>treshold){
    iter_var <- names(which(rev(sort(max_prod/var_lengths))<treshold)[1])
    if(is.na(iter_var)){
      iter_var <- names(which.max(max_prod/var_lengths)[1])
    }
    temp_query <- query
    for (var_value in query[[iter_var]]) {
      temp_query[[iter_var]] <- var_value
      sub_query <- check_if_partition_needed(query = temp_query, out_queries = list())
      out_queries <- append(out_queries, sub_query)
    }
  }else{
    out_queries <- append(out_queries, query)
  }
  return(out_queries)
}

#' Add readable variable names to SCB dataframe
#' @param dfli List of dataframes from SCB
#' @param variable_list SCB metadata list
#' @keywords internal
add_text_variables <- function(dfli, variable_list) {
  df <- dplyr::bind_rows(dfli)
  column_order <- c()
  value_columns <- c()

  for (i in seq_along(variable_list$variables$code)) {
    column_name <- variable_list$variables$code[i]
    label_name <- variable_list$variables$text[i]

    if (column_name == 'ContentsCode') {
      for (j in seq_along(variable_list$variables$valueTexts[[i]])) {
        value_code <- variable_list$variables$values[[i]][j]
        value_label <- variable_list$variables$valueTexts[[i]][j]
        if (value_code %in% colnames(df)) {
          colnames(df)[colnames(df) == value_code] <- value_label
          value_columns <- c(value_columns, value_label)
          df[[value_label]] <- as.numeric(dplyr::pull(df[value_label]))
        }
      }
    } else {
      column_order <- c(column_order, column_name, label_name)
      if (column_name == 'Tid') {
        column_order <- c(column_order, "datum")
      }

      dft <- data.frame(cbind(variable_list$variables$values[[i]], variable_list$variables$valueTexts[[i]]))
      colnames(dft) <- c(column_name, label_name)
      df <- dplyr::left_join(df, dft, by = column_name)
    }
  }

  if ("år" %in% colnames(df)) {
    df <- dplyr::mutate(df, datum = as.Date(paste0(df$år, "-01-01")))
  } else if ("månad" %in% colnames(df)) {
    df <- dplyr::mutate(df, datum = as.Date(zoo::as.yearmon(df$månad, format = "%YM%m")))
  } else if ("kvartal" %in% colnames(df)) {
    df <- dplyr::mutate(df, datum = as.Date(zoo::as.yearqtr(df$kvartal, format = "%YK%q")))
  } else {
    column_order <- setdiff(column_order, "datum")
  }

  df <- dplyr::relocate(df, dplyr::all_of(column_order))
  return(df)
}
