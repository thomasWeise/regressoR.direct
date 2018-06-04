#' @title A Spline Model
#' @description This class holds a spline model.
#' @slot name the name of the spline approach
#' @return an instance of \code{\link{DirectFitModel}}
#' @exportClass DirectFitModel
#' @importClassesFrom regressoR.base FittedModel
#' @importFrom methods setClass representation
#' @seealso DirectFitModel.new
DirectFitModel <- setClass(
  Class = "DirectFitModel",
  contains = "FittedModel",
  representation = representation(name="character"),
  validity = function(object) {
    # check the name
    if((!is.character(object@name)) ||
       (length(object@name) != 1L)  ||
       (nchar(object@name) <= 0L)) {
      return("Invalid name.");
    }
    return(TRUE);
  }
)


#' @title Create a New Instance of \code{\link{DirectFitModel}}.
#' @description Create a New Instance of \code{\link{DirectFitModel}}.
#' @param f a function accepting one parameter and returning a value
#' @param quality the quality of the model on the original data, computed by a
#'   quality metric, smaller values are better
#' @param size the size of the model, i.e., the number of parameters
#' @param name the name of the spline model
#' @return the new instance
#' @importFrom methods new validObject
#' @export DirectFitModel.new
DirectFitModel.new <- function(f, quality, size, name) {
  result <- new("DirectFitModel", f=f, quality=quality, size=size,
                name=name);
  result <- force(result);
  result@f <- force(result@f);
  result@name <- force(result@name);
  result@quality <- force(result@quality);
  result@size <- force(result@size);
  result <- force(result);
  validObject(result);
  return(result);
}

#' @title Convert a \code{DirectFitModel} to a String
#' @description well, convert a \code{DirectFitModel} to a String
#' @param x the \code{DirectFitModel}
#' @return the string
#' @export DirectFitModel.as.character
DirectFitModel.as.character <- function(x) x@name

#' @title Convert a \code{\link{DirectFitModel}} to a String
#' @description the \code{as.character} implementation for
#'   \code{\link{DirectFitModel}}
#' @param x the object
#' @return the name of the object
#' @importFrom methods setMethod
#' @name as.character
#' @aliases as.character,DirectFitModel-method
methods::setMethod("as.character", "DirectFitModel", DirectFitModel.as.character)
