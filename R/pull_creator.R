#' function to make pull functions
#'
#' @param df_result a dataframe you want filtered by one or multiple filter columns and to pull values from
#' @param filter_col one or multiple columns to filter the dataframe by
#' @param default_pull a default column to pull from the filtered data.
#' @param ... additional argument options to the returned function
#'
#' @return a function to filter the dataframe by a character vector equal in length to the number of filter columns. returns values based on the filter columns/values provided; typically, will be a vector of length 1
#' @export
#'
#' @examples
pull_creator <- function(df_result, filter_col, default_pull = NULL, ...){

  if (!all(filter_col %in% colnames(df_result))){
    stop("Error; some filter columns not present in dataframe")
  }

  # default column to pull
  if (is.null(default_pull)){
    default_pull <- paste0("pct_", tolower(filter_col))
  }

  # function to filter dataframe to string in filter column
  filter_funct <- function(df, filter_val, filter_col){
    df %>%
      dplyr::filter(grepl(filter_val, !!dplyr::sym(filter_col), ignore.case = T))
  }

  # function to filter dataframe based on each filter column and pull the resulting column
  a <- function(filter_val, pull_col = default_pull, ...){

    if (length(filter_val) != length(filter_col)){
      stop("Error; filtervalues provided not equal to number of filter columns")
    }

    if (!pull_col %in% colnames(df_result)){
      stop("Error; pull column missing from dataframe")
    }

    df_cap <- df_result

    purrr::walk2(filter_val, filter_col, ~ {
      # print(filter_col)
      df_cap <<- filter_funct(df_cap, .x, .y)
    })

    if (nrow(df_cap) == 0){
      stop("Error; filtered df is empty")
    }

    pull_vals <- df_cap %>%
      dplyr::pull(!!dplyr::sym(pull_col))

    if (length(pull_vals) == 0 | is.null(pull_vals)){
      stop("Error; returned empty vector")
    }

    if (length(pull_vals) > 1) {
      warning("Length of returned vector greater than 1; likely omitted some columns you want to filter by")
    }

    return(pull_vals)

  }
  return(a)
}
