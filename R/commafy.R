#' Function to comma separate a number
#'
#' @param number a number
#'
#' @return comma separated string
#' @export
#'
#' @examples
commafy <- function(number){

  if (!is.numeric(number)){
    stop("Error; input not a number")
  }

  prettyNum(round(number, 2), ",", scientific = FALSE)
}


#' Number formatter
#' Rounds all numeric columns to 2 decimal places
#'
#' @param df Dataframe to round numeric columns in
#' @param round_dig Number of digits to round numeric columns to
#' @param arrange_col Column to arrange data by
#'
#' @return Dataframe with numeric columns rounded, arranged by numeric column if identified
#' @export
#'
#' @examples
num_formatter <- function(df, round_dig = 2, arrange_col = NULL){

  if (!is.null(arrange_col)){
    return(df %>%
             dplyr::mutate(dplyr::across(where(is.numeric), .fns = ~ round(.x, round_dig))) %>%
             dplyr::arrange(desc(!!sym(arrange_col))))
  }

  else if (is.null(arrange_col)){
    return(df %>%
             dplyr::mutate(dplyr::across(where(is.numeric), .fns = ~ round(.x, 2))))
  }

}

#' Name formatter
#' Function to recode Takoma Park, Montgomery County, PG County, and Maryland
#'
#' @param df ACS dataframe with name column containing Takoma Park, Montgomery County, and/or Maryland or PG County
#'
#' @return Dataframe with location column for Takoma Park, Montgomery County, Prince George's County, or Maryland.
#' @export
#'
#' @examples
name_formatter <- function(df){

  df <- df %>%
    dplyr::mutate(location = dplyr::case_when(grepl("Takoma", name) ~ "Takoma Park",
                                grepl("Montgomery", name) ~ "Montgomery County",
                                grepl("Prince George", name) ~ "PG County",
                                grepl("Maryland", name) ~ "Maryland")) %>%
    dplyr::mutate(location = factor(location, levels = .[["location"]], labels = .[["location"]]))

  if (any(is.na(df[["location"]]))){
    stop("Error; invalid location values")
  }

  df

}

#' Name/number formatter
#' Applies name and number formatter functions to one dataframe
#'
#' @param df Dataframe name/num_formatter functions applied to
#' @param round_dig Number of digits to round numeric columns to
#' @param arrange_col Column to arrange data by
#'
#' @return Dataframe with rounded numeric columns and location column
#' @export
#'
#' @examples
name_num_formatter <- function(df, round_dig = 2, arrange_col = NULL){
  df %>%
    tpfuncts::num_formatter(round_dig = round_dig, arrange_col = arrange_col) %>%
    tpfuncts::name_formatter()
}
