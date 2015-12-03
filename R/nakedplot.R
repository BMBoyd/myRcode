#' Create a scatter plot with no axes or labels
#'
#' @export nakedplot
nakedplot <- function(x,y,...){
  plot(x,y, axes=F, ylab="", xlab="",...)
  box()
}
