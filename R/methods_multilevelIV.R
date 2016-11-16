#' @title  methods for the multilevelIV function
#' @description prints the coefficients for the model's parameters 
#' @param object an object of class "mixedREndo"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "coef", signature= c(object="mixedREndo"), definition=function(object, ...){
            
            z <- object
            cat("\nCoefficients:\n")
            coef.table <- z@coefficients
            stats::printCoefmat(coef.table)
       
     }
)

#' @title  methods for the multilevelIV function
#' @description prints the summary for the model's parameters 
#' @param object    object of class multilevelIV
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "summary", signature= c(object="mixedREndo"), definition=function(object, ...){
  
            z <- object
            cat("\nFormula:\n")
            print(z@formula)
            ans <- z@coefSdErr  # estimates and their sd.errors and z-scores
            cat("\nCoefficients:\n")
            print(ans)
}
)

