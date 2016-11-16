#'@title latentIV Object
#'
#' @description This class is used to store and further analyze the results of the latentIV function
#' @slot formula - the model formula.
#' @slot coefficients - the estimated coefficients.
#' @slot seCoefficients - standard error of the estimates.
#' @slot groupMeans - the estimates means of the assumed latent groups.
#' @slot seMeans - standard errors of the estimates of the groups' means.
#' @slot sigma - the variance -covariance matrix.
#' @slot probG1 - probability of group 1.
#' @slot seProbG1 - standard error of the estimate of the probability of group 1.
#' @slot initValues - initial values of the parameters.
#' @slot value - the value of the maximum likelihood function.
#' @slot convCode - convergence code. If equalt to zero the model has converged.
#' @slot hessian - the hessian matrix.
#' @slot dataset - a matrix containing the dependent variable and the endogenous regressor.

#' @name livREndo-class
#' @exportClass livREndo
#' @rdname livREndo-class
#' @keywords internal
#' @examples
#' getSlots("livREndo")
#'
#' @importFrom methods setClass
#' @export
setClass(

  #Class name
  "livREndo",


  #Slots / member vars
  slots = c(
    formula = "formula",
    coefficients = "numeric",
    seCoefficients = "numeric",
    groupMeans = "numeric",
    seMeans = "numeric",
    sigma = "matrix",
    probG1 = "numeric",
    seProbG1 = "numeric",
    initValues = "numeric",
    value = "numeric",
    convCode = "integer",
    hessian = "matrix",
    dataset = "matrix"

  ),

  prototype = list(
                   formula = y~x,
                   coefficients = NA_real_,
                   seCoefficients = NA_real_,
                   groupMeans = NA_real_,
                   seMeans = NA_real_,
                   sigma = matrix(NA),
                   probG1 = NA_real_,
                   seProbG1 = NA_real_,
                   initValues = NA_real_,
                   value = NA_real_,
                   convCode = NA_integer_,
                   hessian = matrix(NA),
                   dataset = matrix(NA)
  )
)



