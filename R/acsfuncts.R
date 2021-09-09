#' Function to compare statistical significance of results for Takoma Park to Montgomery County and Maryland
#'
#' @param df Processed ACS dataframe with results for Takoma Park, Maryland, and Montgomery County stored in a location column.
#' @param join_col Column by which join restuls for Montgomery County/Maryland back to original dataframe.
#' @param name_pct Name of percentage column you're comparing values for.
#' @param moe_pct Name of moe for percentage column.
#'
#' @return A dataframe with critical values and statistical significance values comparing Takoma Park against Maryland and Montgomery County
#' @export
#'
#' @examples
signif_mont_tp <- function(df,
                           join_col,
                           name_pct,
                           moe_pct = pct_moe){

  df %>%
    acsprocess::signif_compare(filter_col = location,
                   filter_val = "mont",
                   join_col = join_col,
                   est_col = {{ name_pct }},
                   moe_col = {{ moe_pct }}) %>%
    acsprocess::signif_compare(filter_col = location,
                   filter_val = "maryland",
                   join_col = join_col,
                   est_col = {{ name_pct }},
                   moe_col = {{ moe_pct }}) %>%
    tpfuncts::num_formatter()

}

