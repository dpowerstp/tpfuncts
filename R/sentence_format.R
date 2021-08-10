#' Format character vector as sentence
#'
#' Function to format character vector as list. Can define different separator
#'
#' @param char_vector Vector to separate into a sentence format.
#' @param separator What to separate items in the character vector by.
#'
#' @return
#' @export
#'
#' @examples
sentence_format <- function(char_vector, separator= ","){

  if (length(char_vector) > 2){

    val <- purrr::map2_chr(char_vector, seq_along(char_vector), ~ ifelse(.y == length(char_vector),
                                                                  paste0("and ", .x),
                                                                  paste0(.x, separator, " "))) %>%
      paste0(collapse = "")

  }

  if (length(char_vector) == 2){
    val <- paste0(char_vector[1], " and ", char_vector[2])
  }

  if (length(char_vector) == 1){
    val <- char_vector
  }

  return(val)


}
