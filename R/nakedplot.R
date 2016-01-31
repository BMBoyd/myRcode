#' Create a scatter plot with no axes or labels
#'
#' @export nakedplot
nakedplot <- function(...){

  plot(axes=F, ylab="", xlab="", ...)
  box()
}
