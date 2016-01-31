#' Plot a custom axis for printing
#'
#' @param side an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
#' @param label.line a numeric value specifying margin line to place tick labels
#' @param append.label a character string with which to append the axis tick label, ex. "\%"
#' @export

myaxis <- function( side, label.line = 0.75, append.label = "", col = "black"){

  axis(side = side, at=axTicks(side), labels=F, col = col)

mtext(text = paste(axTicks(side), append.label, sep = ""), side = side, line = label.line, at=axTicks(side), las = 1, col = col)

}
