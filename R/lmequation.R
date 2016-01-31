#' Print a linear model equation on plot
#'
#' @param side side to plot (1 = bottom, 2 = left, 3 = top, 4 = right).
#' @param line on which MARgin line, starting at 0 counting outwards.
#' @param adj adjustment for each string in reading direction. For strings parallel to the axes, adj = 0 means left or bottom alignment, and adj = 1 means right or top alignment.
#' @param at give location of each string in user coordinates. If the component of at corresponding to a particular text item is not a finite value (the default), the location will be determined by adj.
#' @param ... Further graphical parameters (see par), including family, las and xpd. (The latter defaults to the figure region unless outer = TRUE, otherwise the device region. It can only be increased.)
#' @export lmequation
lmequation <-function(side=1 ,line=NULL,
                       model=NULL, log = NA, ...){
  x <- summary(model)
  if( is.na(log)){
    mtext(side, line, text = bquote(list(y == .(format(round(x$coefficients[2,1],2),nsmall=2))*x + .(format(round(x$coefficients[1,1],2),nsmall=2)),
r^2== .(format(round(x$r.squared,2),nsmall=2)))),...)
  }
  if( !is.na(log) & log == "x"){
    mtext(side, line, t=bquote(list(y == .(format(round(x$coefficients[2,1],2),nsmall=2))*log[10](x) +
                          .(format(round(x$coefficients[1,1],2),nsmall=2)),
                        r^2== .(format(round(x$r.squared,2),nsmall=2)))),...)
  }
}
