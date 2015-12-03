#' Calculate Excess Pb-210
#'
#'
#' @return Returns a two column vector of numerics with names xs210Pb and xs210Pb.uncer. NAs are placed where data is missing. If show.zero is true (default), then zeros are printed where xsPb210 is determined to be zero.
xsPb210 <- function(){
xs210Pb <- apply(njsg[, c( "X47_Pb210_activity", "X609_Bi214_activity", "X47_Pb210_error")], 1, function(x){
  if(x[1]-x[2] >= 0 & !is.na(x[3])){
    x[1]-x[2]
  }else{
    NA
  }
})
xs210Pb.uncer <- apply( njsg[, c( "X47_Pb210_error", "X609_Bi214_error", "xs210Pb")], 1, function(x) {
  if(is.na(x[3])){
    NA
  }else{
    x[1] + x[2]
  }
})
data.frame( xs210Pb = xs210Pb, xs210.uncer = xs210Pb.uncer)
}
