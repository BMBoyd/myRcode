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
  xs210Pb <- apply( c( PbAct, SupPbAct, PbActUnc), 1, function(x){
    if(x[1]-x[2] >= 0 & !is.na(x[3])){
      x[1]-x[2]
    }else{
      NA
    }
  }
  )

  xs210Pb.uncer <- apply( c( PbActUnc, SupPbActUnc, xs210Pb), 1, function(x) {
    if(is.na(x[3])){
      NA
    }else{
      x[1] + x[2]
    }
  }
  )

  data.frame( xs210Pb = xs210Pb, xs210.uncer = xs210Pb.uncer)
}
