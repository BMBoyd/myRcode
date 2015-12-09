#' Calculate Excess Pb-210
#'
#' @export
#' @param PbAct vector of total Pb-210 activity values
#' @param PbActUnc vector of total Pb-210 activity error. Note: absolute not percent
#' @param SupPbAct vector or single value to be used for supported Pb-210
#' @param SupPbActUnc vector or single value of supported Pb-210 activity error. Note: absolute not percent
#'
#' @return Returns a two column vector of numerics with names xs210Pb and xs210Pb.uncer. NAs are placed where data is missing. If show.zero is true (default), then zeros are printed where xsPb210 is determined to be zero.
xsPb210 <- function(PbAct = NA, PbActUnc = NA, SupPbAct = NA, SupPbActUnc = NA){
  xs210Pb <- apply( cbind( PbAct, PbActUnc, SupPbAct, SupPbActUnc), 1, function(x){
    ## no Pb
    if( is.na(x[2])){
      NA
    ## no supported
    } else if( is.na(x[4])){
      x[1] # just Pb-210 activity
    ## supported and total
    } else if(sum(x[1],-x[3], na.rm = T) >= 0){
      x[1]-x[3]
    } else {
      NA
    }
  }
  )
  xs210Pb.uncer <- apply( cbind( PbActUnc, SupPbActUnc, xs210Pb), 1, function(x) {
    ## no excess Pb-210
    if(!is.na(x[3])){
      sum(x[1], x[2], na.rm = T)
    }else{
      NA
    }
  }
  )
  data.frame( xs210Pb = xs210Pb, xs210Pb.uncer = xs210Pb.uncer)
}
