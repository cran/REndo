 #' @title  methods for the multilevelIV function
#' @description prints the coefficients for the model's parameters 
#' @param object an object of class "mixedREndo"
#' @param ... extra named arguments 
#' @export
#' @docType methods
#' @aliases coef-methods
#' @aliases coef, ANY-method
#' @aliases coef, mixedREndo-method
#' @keywords internal
setMethod(f = "coef", signature= c(object="mixedREndo"), definition=function(object, ...){
            
            z <- object
            coef.table <- z@coefficients
            return(coef.table)
       
     }
)

#' @title  methods for the multilevelIV function
#' @description prints the summary for the model's parameters 
#' @param object    object of class multilevelIV
#' @param model     for which of the estimated models should the coefficients be displayed. If \code{model="all"}, the coefficients of all estimated models will be printed.
#' @export
#' @docType methods
#' @aliases summary-methods
#' @aliases summary, ANY-method
#' @aliases summary, mixedREndo-method
#' @keywords internal
setMethod(f = "summary", signature= c(object="mixedREndo"), definition=function(object, model){
  
            z <- object
            mcl <- match.call()  
            if ("bFE_L2" %in% mcl$model){
              ans <- list(Formula = z@formula, Coefficients = z@coefSdErr$bFE_Lev2) 
            }
          #  ans <- list(Formula = z@formula, Coefficients = z@coefSdErr)  # estimates and their sd.errors and z-scores
            if ("bFE_L3" %in% mcl$model){
              ans <- list(Formula = z@formula, Coefficients = z@coefSdErr$bFE_Lev3) 
            } 
            
            if ("bGMM_L2" %in% mcl$model){
              ans <- list(Formula = z@formula, Coefficients = z@coefSdErr$bGMM_Lev2) 
            }
            if ("bGMM_L3" %in% mcl$model){
              ans <- list(Formula = z@formula, Coefficients = z@coefSdErr$bGMM_Lev3) 
            }
            if ("bRE" %in% mcl$model){
              ans <- list(Formula = z@formula, Coefficients = z@coefSdErr$b_REF ) 
            }
            if ("all" %in% mcl$model){
              ans <- list(Formula = z@formula, Coefficients = z@coefSdErr ) 
            }
            return(ans)
            
}
)

