# Perform a Direct Fit
#
# @param metric an instance of \code{RegressionQualityMetric}
# @param fitter a function \code{f(x, y)} returning a fitted function
# @param name the name of the fitter
# @param transformation.x the transformation along the \code{x}-axis, or
#   \code{NULL} if none was applied to the data
# @param transformation.y the transformation along the \code{y}-axis, or
#   \code{NULL} if none was applied to the data
# @param metric.transformed the transformed metric for the first fitting step
#
# @return On success, an instance of \code{\link{DirectFitModel.new}}.
#   \code{NULL} on failure.
#' @importFrom learnerSelectoR learning.checkQuality
#' @include DirectFitModel.R
.fitDirect <- function(metric,
                       fitter,
                       name,
                       transformation.x=NULL, transformation.y=NULL,
                       metric.transformed=NULL) {

  if(is.null(fitter) || (!(is.function(fitter)))) {
    stop("fitter must be a proper function.");
  }

  # First we check the transformations whether they are NULL or identity
  # transformations.
  f.x.i <- is.null(transformation.x);
  if(!f.x.i) {
    f.x <- transformation.x@forward;
    f.x.i <- identical(f.x, identity);
  }

  f.y.i <- is.null(transformation.y);
  if(!f.y.i) {
    f.y <- transformation.y@backward;
    f.y.i <- identical(f.y, identity);
  }


  # initialize result
  f1            <- NULL;
  trafo.complex <- 0L;

  # get result
  if(f.x.i && f.y.i) {
    # Both transformations are NULL or identity transformations
    if(is.null(metric.transformed) ||
       identical(metric.transformed, metric)) {
      # OK, we fit on the original, raw data. The transformations are identity
      # or NULL and the transformed metric is NULL or identical to the actual
      # metric.
      # Then, we directly use the original data and are good
      f1 <- fitter(x=metric@x, y=metric@y);
      if(is.null(f1)) {
        return(NULL);
      }
    } else {
      stop("Transformed metric must be identical to actual metric or NULL if transformations are both NULL or identity.");
    }
  } else {
    if(is.null(metric.transformed)) {
      stop("Transformed metric canot be NULL if at least one transformation is not NULL or identity.");
    }
  }

  if(is.null(f1)) {
    # OK, we need to deal with the transformed data
    # The first fitting step takes place on the transformed data.
    f1 <- fitter(x=metric.transformed@x, y=metric.transformed@y);
    if(is.null(f1)) {
      return(NULL);
    }
  }

  # take the function
  nameAdd <- "";

  # get function for raw data
  if(f.x.i) {
    if(f.y.i) {
      # no transformation
      f2 <- f1;
    } else {
      # x is identity, y is not
      f2 <- function(x) f.y(f1(x));
      trafo.complex <- transformation.y@complexity;
      nameAdd <- " with transformed y";
    }
  } else {
    # x is not identity
    if(f.y.i) {
      # y is identity, x not
      f2 <- function(x) f1(f.x(x));
      trafo.complex <- transformation.x@complexity;
      nameAdd <- " with transformed x";
    } else {
      # neither is
      f2 <- function(x) f.y(f1(f.x(x)));
      trafo.complex <- transformation.x@complexity +
                       transformation.y@complexity;
      nameAdd <- " with transformed xy";
    }
  }

  # compute the quality of the fit
  quality <- metric@quality(f2);
  if(learning.checkQuality(quality)) {
    return(DirectFitModel.new(f2, quality,
           length(metric@x) + trafo.complex,
           paste(name, nameAdd, sep="", collapse="")));
  }

  # ok, the result is somehow invalid
  return(NULL);
}
