#' Plot a depth profile
#'
#' @export depthplot
depthplot <- function(x, y, ylim=NULL, xlabel=NULL, ...){
  # par(mar=c(0.5, par()$mar[2], par()$mar[3], 0.5)) # modiy margins for depth plot
  if(!is.null(ylim)){
    plot.default(x, y, ..., ylim=ylim, ylab="", xlab="", axes=F)
  }else{
    plot.default(x, y, ..., ylab="", xlab="", ylim=c(max(y),0), axes=F)}
  axis(2, las=1);axis(3) # add in axes
  mtext( if(is.null(xlabel)) deparse(substitute(x)) else xlabel,
         side=3, line=par()$mgp[1]) # add axis label at pos 3
  mtext('Depth', side=2, line=par()$mgp[1]) # add axis title at pos 2
  box() # draw box around plot
}
