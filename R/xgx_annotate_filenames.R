#' Append filenames to bottom of the plot 
#' 
#' \code{xgx_annotate_filenames} appends file details to the bottom of a plot 
#' using the plot caption option. 
#' File details to append include the parent directory, the path of the R 
#' script which generated the plot, 
#' and the path of the plot.
#' 
#' @param dirs list containing directories and filenames.  It must contain 
#' five fields
#' \enumerate{
#' \item parent_dir  = Parent directory containing the Rscript and the Results 
#' folder
#' \item rscript_dir = Subdirectory ofparent_dir that contains the Rscript 
#' used to generate the figure
#' \item rscript_name= Name of the Rscript used to generate the figure
#' \item results_dir = Subdirectory ofparent_dir where the figure is stored
#' \item filename    = Filename
#' }
#' @param hjust horizontal justification of the caption
#' @param color font color for caption, default black
#' @param size font size for caption, default 11
#' 
#' @return None
#'
#' @examples
#' dirs <- list(parent_dir = "/your/parent/path/",
#'              rscript_dir = "./Rscripts/",
#'              rscript_name = "Example.R",
#'              results_dir = "./Results/",
#'              filename = "your_file_name.png")
#' data <- data.frame(x = 1:1000, y = rnorm(1000))
#' ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_point() +
#'   xgx_annotate_filenames(dirs)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @export
xgx_annotate_filenames <- function(dirs, hjust = 0.5, color = "black", size = 11) {
  caption <- xgx_dirs2char(dirs)
  return(list(
    ggplot2::labs(caption = caption),
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = hjust, color = color, size = size))
  ))
}
