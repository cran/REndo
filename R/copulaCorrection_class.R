#' @title generics for copulaREndo-class
#' @description This class is used to store and further analyze the results of the copulaCorrection function when method=1 is applied
#' @slot copCall - call fo the copulaCorrection function.
#' @slot coefficients - estimated coefficients.
#' @slot coefEndoVar - estimated coefficients of the variables considered endogenous.
#' @slot coefExoVar - estimated coefficients of the variables considered exogenous.
#' @slot coefPStar - estimated coefficient of the additional variable, p*.
#' @slot seCoefficients - standard errors of the coefficients.
#' @slot rho - the estimate of the correlation between the endogenous variable and the error.
#' @slot sigma - standard deviation of the error.
#' @slot value - value of the maximum likelihood.
#' @slot convCode - the convergence code. If is equal to zero, the model converged.
#' @slot regressors - returns the regressors matrix.
#' @slot confint - returns the lower and upper limits of the 95\% confidence interval for the estimates.

#' @name copulaREndo-class
#' @exportClass copulaREndo
#' @keywords internal
#' @rdname copulaRendo-class
#' @examples 
#' getSlots("copulaREndo")
#'
#' @importFrom methods setClass
#
setClass(
  
  #Class name
  "copulaREndo",
    
  #Slots / member vars
  slots = c(
    copCall = "call",
    coefficients = "numeric",
    coefEndoVar = "numeric",
    coefExoVar = "numeric",
    coefPStar = "numeric",
    seCoefficients = "numeric",
    rho = "numeric",
    sigma = "numeric",
    value = "numeric",
    convCode = "numeric",
    regressors = "matrix",
    confint = "matrix"  
    ),
  
  prototype = list(
                  # copCall = "call",
                   coefficients = NA_real_,
                   coefEndoVar = NA_real_,
                   coefExoVar = NA_real_,
                   coefPStar = NA_real_,
                   seCoefficients = NA_real_,
                   rho = NA_real_,
                   sigma = NA_real_,
                   value = NA_real_,
                   convCode = NA_real_,
                   regressors = matrix(NA),
                   confint = matrix(NA)
                  
  )
)




