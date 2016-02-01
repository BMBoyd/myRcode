#' Print a linear model equation on plot
#'
#' @param side side to plot (1 = bottom, 2 = left, 3 = top, 4 = right).
#' @param line on which MARgin line, starting at 0 counting outwards.
#' @param model a linear model object
#' @param inset fraction of axis length to inset equation. Values >= 0 inset from left; Values < 0 inset from right
#' @param ... Further graphical parameters (see par), including adj, family, las and xpd. (The latter defaults to the figure region unless outer = TRUE, otherwise the device region. It can only be increased.)
#' @export lmequation
lmequation <- function(side=1 ,line=-1,
                       model=NULL, inset = 0.05, at, ...){
  x <- summary(model)
  xlog <- par("xlog")
  pars <- par("usr")

  # If there is no "at" specificed, use inset to place equation
  if (missing(at)) {
    # if side is along x
    if ( side %in% c(1,3)) {
      # get percent of x distance
      xdist <- (pars[2] - pars[1]) * abs(inset)
      # if inset is less than 0 than place equation from right
      if (inset < 0) {
        at <- pars[2] - xdist
      }
      # if inset is greater than 0 than place equation from left
      if (inset >= 0) {
        at <- pars[1] + xdist
      }
      # if plot is log x then transform
      if (xlog) at <- 10 ^ at
    }
    # if side is along y
    #   if( side %in% c(2,4)){
    #   }
    # }
  }

  if (!xlog) {
    mtext(side, line, text = bquote(list(y == .(format(round(x$coefficients[2,1],2),nsmall=2))*x + .(format(round(x$coefficients[1,1],2),nsmall=2)),
                                         r^2== .(format(round(x$r.squared,2),nsmall=2)))), at = at, ...)
  }

  if (xlog) {
    mtext(side, line, t=bquote(list(y == .(format(round(x$coefficients[2,1],2),nsmall=2))*log[10](x) +
                                      .(format(round(x$coefficients[1,1],2),nsmall=2)),
                                    r^2== .(format(round(x$r.squared,2),nsmall=2)))), at = at, ...)
  }
}
