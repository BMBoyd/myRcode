#'  Create a Blank Plot
#'
#' @export emptyplot
emptyplot <- function(){
  old.par <- par("mar")
  on.exit(par("mar" = old.par))
  par(mar=c(0,0,0,0))
  plot(1,1,pch=NA,axes=F)
}
