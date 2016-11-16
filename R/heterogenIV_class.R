#'@title heterogenIV Object
#'
#' @description This class is used to store and further analyze the results of the heterogenIV function
#' @slot formula - the call of the function.
#' @slot coefficients - the estimated coefficients.
#' @slot obs - number of observations.
#' @slot jtest - the results of the j test 
#' @slot ftest - the results of the 

#' @name heterogenREndo-class
#' @exportClass heterogenREndo
#' @rdname heterogenREndo-class
#'
#' @examples
#' getSlots("heterogenREndo")
#'
#' @importFrom methods setClass
#' @export
#' @keywords internal
setClass(
  
  #Class name
  "heterogenREndo",
  
  #Slots / member vars
  slots = c(
    formula = "formula",
    coefficients = "matrix",
    obs = "numeric",
    jtest = "matrix",
    ftest = "numeric"
    
  ),
  
  prototype = list(
    formula = y~x,
    coefficients = matrix(NA),
    obs = NA_real_,
    jtest = matrix(NA),
    ftest = NA_real_
  )
)



