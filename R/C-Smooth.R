#' Smooth Cyclical Data
#' 
#' @param x Numeric vector to smooth
#' @param k Smoothing parameter
#' 
#' @export

c_smooth = function(x,k) runmed(rep(x,3),k)[(length(x) + 1):(2*length(x))]
