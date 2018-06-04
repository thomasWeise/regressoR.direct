# make the default list
#' @include linear.R
#' @include constant.R

# the set of default spline fitters
.default <- c(regressoR.direct.constant, regressoR.direct.linear);

#' @title Get the Default Direct Fitters
#' @description Get the default fitters which base their results directly on the
#'   data.
#' @return the list of default fitters
#' @export regressoR.direct.default
regressoR.direct.default <- function() .default
