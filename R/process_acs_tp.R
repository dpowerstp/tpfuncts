#' Process ACS data - Takoma Park specific
#' Groups lightly-processed tidycensus/ACS dataframe, and identifies statistical-significance of values comparing Takoma Park to Montgomery County and Maryland.
#'
#' @param df Lightly-processed ACS dataframe with estimate and MOE column.
#' @param group_cols Columns to group ACS dataframe by.
#' @param overall_cols The set of columns to calculate overall results for. E.g., if you want to see how age groups differ from overall rates of access, you would enter the computer access column as the group col.
#' @param name_col Base-name of column to store values in for grouped dataframe.
#' @param bind_overall Whether to add overall results as a row to the processed dataframe. Should enter name of column with groups comparing against (e.g., if comparing ages against overall, should enter age).
#' @param signif_cols Named list with value representing name of the column with significance values, and names representing how significance should be described in the new column (e.g., "Montgomery County" = "signif_mont" would be for a signif_mont column describing the significance of differences with Montgomery County, and Montgomery County is how it would be represented in the new column).
#' @param root_df If the df has already been processed more, the base-dataframe from which the df was processed.
#'
#' @return
#' @export
#'
#' @examples
process_df_tp <- function(df,
                       group_cols,
                       overall_cols,
                       name_col,
                       # tot_derive = NULL,
                       bind_overall = NULL,
                       signif_cols = list("Overall" = "signif_overall",
                                          "MC" = "signif_mont",
                                          "MD" = "signif_maryland"),
                       root_df = NULL) {

  # create column names
  name_pct <- paste0("pct_", name_col)

  # calculate group totals
  processed_df <- df %>%
    tpfuncts::name_num_formatter() %>%
    acsprocess::process_df(group_cols = group_cols,
                           overall_cols = overall_cols,
                           name_col = name_col,
                           bind_overall = bind_overall,
                           root_df = root_df) %>%
    tpfuncts::signif_mont_tp(join_col = group_cols[-1],
                             name_pct = !!sym(name_pct)) %>%
    acsprocess::signif_checker(signif_cols = signif_cols) %>%
    tpfuncts::num_formatter()

  return(processed_df)

}


