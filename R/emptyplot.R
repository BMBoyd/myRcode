#'  Create a Blank Plot
#'
emptyplot <- function(){
  old.par <- par("mar")
  on.exit(par("mar" = old.par))
  par(mar=c(0,0,0,0))
  plot(1,1,pch=NA,axes=F)
}
