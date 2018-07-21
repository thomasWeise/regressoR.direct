# The internal constant interpolator function
#' @importFrom regressoR.base xy.order
.constant.fitter <- function(xx, yy) {
  or  <- xy.order(xx, yy);
  xx  <- xx[or];
  yy  <- yy[or];
  rm("or");
  xx <- force(xx);
  yy <- force(yy);
  f <- function(x) {
    y       <- x;                   # dummy allocation
    indexes <- findInterval(x, xx); # find indexes
    # the values clip to the first value in xx
    sel     <- (indexes <= 0L);
    y[sel]  <- yy[1L];
    # rest
    y[!sel] <- yy[indexes[!sel]];
    return(y);
  }
  f <- force(f);
  return(f);
}

#' @title Create a Direct Constant Interpolation from the Data
#' @description A direct constant interpolation is computed.
#' @param metric an instance of \code{RegressionQualityMetric}
#' @param transformation.x the transformation along the \code{x}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param transformation.y the transformation along the \code{y}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param metric.transformed the transformed metric for the first fitting step
#' @param q the ignored quality parameter
#' @return On success, an instance of \code{\link{DirectFitModel.new}},
#'   \code{NULL} on failure.
#' @export regressoR.direct.constant
#' @include fitDirect.R
regressoR.direct.constant <- function(metric,
                                      transformation.x=NULL, transformation.y=NULL,
                                      metric.transformed=NULL,
                                      q=0) {
  .fitDirect(metric, .constant.fitter, "constant interpolation",
             transformation.x, transformation.y, metric.transformed);
}
