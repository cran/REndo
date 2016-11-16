#'@title  mixedREndo S4 Object
#'@description defines the slots of class mixedREndo.

#' @slot coefficients - the estimated coefficients. 
#' @slot coefSdErr - stabdard errors of the estimates.
#' @slot formula - the formula used for estimation.
#' @slot vcovMat - variance-covariance matrix.
#' @slot weightMat - the weight matrix used for estimation.


#' @name mixedREndo-class
#' @exportClass mixedREndo
#' @rdname mixedREndo
#' @keywords internal
#' @examples
#' getSlots("mixedREndo")
#'
#' @importFrom methods setClass

setClass("mixedREndo",

  #Slots / member vars
  slots = c(
    
    coefficients = "matrix",
    coefSdErr = "list",
    formula = "formula",
    vcovMat = "matrix",
    weightMat = "matrix"
  ),

  prototype = list(
                  
                   coefficients = matrix(NA),
                   coefSdErr = list(NA),
                   formula = y~x,
                   vcovMat = matrix(NA),
                   weightMat = matrix(NA)
                  
  )
)


