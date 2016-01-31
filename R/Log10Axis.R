#' Plot a log(10) axis
#'
#' @param side an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
#' @param show.labels a logical value specifying whether to show labels
#' @param scientific a logical value specifying whether to print labels as integers or scientific notation
#' @param ... other graphical parameters may also be passed as arguments to this function, particularly, cex.axis, col.axis and font.axis for axis annotation, mgp and xaxp or yaxp for positioning, tck or tcl for tick mark length and direction, las for vertical/horizontal label orientation, or fg instead of col, and xpd for clipping. See par on these.
#' @export Log10Axis

Log10Axis <- function(side = 3, show.labels = TRUE, scientific = FALSE, label.line = 0.5, ...){
# label vector
  at.labels <- c(.01,1,10,100,1000,10000)

  # small ticks
    axis( side, at = c(1*seq(0.1,1,.1), 10*seq(0.1,1,.1), 100*seq(0.1,1,.1),1000*seq(0.1,1,.1)), labels = F, tcl = -0.25, ...)
# large ticks
  axis(side, at = at.labels, labels = F)

  if (show.labels) {
    if (scientific) {
      labels <- format( at.labels, scientific = T)
    } else {
      labels <- prettyNum(at.labels)
    }
    # ind <- at.labels >= plot.coords[1] & at.labels <= plot.coords[2]
    ind <- at.labels >= range(axTicks(side))[1] & at.labels <= range(axTicks(side))[2]
    mtext(text = labels[ind], side = side, line = label.line, at = c(.01,1,10,100,1000,10000)[ind], ...)
  }
}
