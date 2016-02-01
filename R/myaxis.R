#' Plot a custom axis for printing
#'
#' @param side an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
#' @param label.line a numeric value specifying margin line to place tick labels
#' @param append.label a character string with which to append the axis tick label, ex. "\%"
#' @export

myaxis <- function(side, label.line = 0.75, append.label = "", col = "black"){

  ticks <- axTicks(side)
  axis(side = side, at = ticks, labels = F, col = col)
  # constrain writing of labels when plotting in log scale
if( side %in% c(1,3) & par("xlog")){
  ticks <- ticks[ ticks >= 10^ min(par("usr")[1:2]) & ticks <= 10^max(par("usr")[1:2])]
}
  if( side %in% c(2,4) & par("ylog")){
    ticks <- ticks[ ticks <= 10^ min(par("usr")[3:4]) & ticks <= 10^max(par("usr")[3:4])]
  }
mtext(text = paste(ticks, append.label, sep = ""), side = side, line = label.line, at = ticks, las = 1, col = col)

}
