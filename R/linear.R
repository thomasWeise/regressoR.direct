# The internal linear interpolator function
#' @importFrom regressoR.base xy.order
.linear.fitter <- function(xx, yy) {
  or  <- xy.order(xx, yy);
  xx  <- xx[or];
  yy  <- yy[or];
  rm("or");
  xx <- force(xx);
  yy <- force(yy);
  f <- function(x) {
    y       <- x;                   # dummy allocation
    indexes <- findInterval(x, xx); # find indexes

    # values clipped to first value in xx
    sel     <- (indexes <= 0L);
    y[sel]  <- yy[1L];

    # values that can directly be copied from xx
    rem          <- !sel;
    indexes[sel] <- 1L; #fix for empty/NAs/0s
    sel          <- rem & ((x == xx[indexes]) | (indexes >= length(xx)));
    y[sel]       <- yy[indexes[sel]];

    # remaining values can be linearly interpolated
    rem     <- rem & !sel;
    if(any(rem)) {
      indexes   <- indexes[rem];
      indexesp1 <- indexes + 1L;
      y[rem]    <- yy[indexes] + ((yy[indexesp1] - yy[indexes]) *
                     (x[rem] - xx[indexes]) /
                     (xx[indexesp1] - xx[indexes]));
    }
    return(y);
  }
  f <- force(f);
  return(f);
}

#' @title Create a Direct Linear Interpolation from the Data
#' @description A direct linear interpolation is computed.
#' @param metric an instance of \code{RegressionQualityMetric}
#' @param transformation.x the transformation along the \code{x}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param transformation.y the transformation along the \code{y}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param metric.transformed the transformed metric for the first fitting step
#' @param q the ignored quality parameter
#' @return On success, an instance of \code{\link{DirectFitModel.new}},
#'   \code{NULL} on failure.
#' @export regressoR.direct.linear
#' @include fitDirect.R
regressoR.direct.linear <- function(metric,
                                    transformation.x=NULL, transformation.y=NULL,
                                    metric.transformed=NULL,
                                    q=0) {
  .fitDirect(metric, .linear.fitter, "linear interpolation",
             transformation.x, transformation.y, metric.transformed);
}
