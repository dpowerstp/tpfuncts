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
