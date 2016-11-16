#' @title  methods for the heterogenIV function
#' @description prints the coefficients for the model's parameters 
#' @param object an object of class "heterogenREndo"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod( f = "coef", signature= c(object="heterogenREndo"), definition=function(object, ...)
  {
    z <- object   
    cat("Formula:\n")
    print(z@formula)
    cat("\nCoefficients and Standard Errors:\n")
    printCoefmat(z@coefficients, P.values=T, has.Pvalue=T)
    }
)

#' @title  methods for the heterogenIV function
#' @description prints summary for the model's parameters 
#' @param object an object of class "heterogenREndo"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod( f = "summary", signature= c(object="heterogenREndo"), definition=function(object, ...)
{
  z <- object   
  cat("Formula:\n")
  print(z@formula)
  cat("\nCoefficients and Standard Errors:\n")
  printCoefmat(z@coefficients, P.values=T, has.Pvalue=T)
  cat("\nOveridentification Test:\n")
  print(z@jtest)
  cat("\nPartial F-test Statistics for Weak IV Detection:\n")
  print(z@ftest)
  cat("\nNumber of Observations:", z@obs, "\n")
}
)