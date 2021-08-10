

content_dir <- "./data/"

#' Standardize file paths
#'
#' Create path to defined data directory by defining/re-defining content_dir; option to change directory opening for specific purposes.
#'
#' @param ... objects or strings fed to a paste0 function
#' @param dir a root directory
#'
#' @return string
#' @export
#'
#' @examples
#' content_dir <- "~/r_proj/cats/"
#' filename <- "dogs.csv"
#' data_path("data/output/", filename)
data_path <- function(..., dir = content_dir){

  paste0(dir, ...)
}
