#' Divide numerator by denominator and round to specified digits
#'
#' @param numerator Numerator in calcualting percentage.
#' @param denominator Denominator in calculating percentage.
#' @param n the number of digits in the field
#'
#' @return number
#' @export
#'
#' @examples
pct_round <- function(numerator, denominator, n = 1){

  if (!(is.numeric(numerator) & is.numeric(denominator))){
    stop("Error; non-numeric numerator or denominator")
  }

  round(numerator * 100 / denominator, digits = n)
}
