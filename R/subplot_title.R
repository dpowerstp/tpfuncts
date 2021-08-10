#' Function to add a subplot title to a plotly subplot
#'
#' @param plot plotly subplot
#' @param title title of subplot
#'
#' @return plotly with subtitle
#' @export
#'
#' @examples
subplot_title <- function(plot, title){

  plot %>%
    plotly::add_annotations(text = paste0("<i><b>", title, "</i></b>"),
                    x = 0.1,
                    y = 1,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "left",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 12))
}
