#' Plot Horizontal and Vertical Line Segments on Existing Plot
#'
#' @param x a vector of x coordinates used as a centroid from which to draw segments.
#' @param x.unc a vector of uncertainites added and subracted from x to create segments.
#' @param y a vector of y coordinatesused as a centroid from which to draw segments.
#' @param y.unc a vector of uncertainites added and subracted from y to create segments.
#' @param show.hor logical; if TRUE, function draws horizontal segments
#' @param show.ver logical; if TRUE, function draws vertical segments
#' @param ... further arguments passed to segments
ErrorPoints<- function(x = NA, x.unc = NA, y = NA, y.unc = NA ,show.hor = T, show.ver = T, ...){
  if( show.hor){
    segments(x - x.unc, y , x + x.unc, y, ...)
  }
  if( show.ver){
    segments(x, y - y.unc, x, y + y.unc, ...)
  }
}
