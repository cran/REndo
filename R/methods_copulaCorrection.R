# Raluca Gui 02.06.2016
# methods for copulaCorrection


#' @title  coefficients for the copulaCorrection method
#' @description prints the estimated coefficients for the model's parameters
#' @param  object     an object of class "copulaREndo"
#' @param ...     extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "coef", signature= c(object="copulaREndo"), definition=function(object, ...)
  {
             mcl <- object@copCall

            if ("1" %in% mcl$method){
              z <- object
              estEnd <- z@coefEndoVar  # estimates value
              estEx <- z@coefExoVar
              coef.table <- estEx
              coef.table <- append(coef.table, estEnd)
              coef.table <- append(coef.table, z@rho)
              coef.table <- append(coef.table, z@sigma)
              coef.table <- matrix(coef.table, , ncol=1)
              rownames(coef.table) <- append(colnames(z@regressors),c("rho","sigma"))
              colnames(coef.table) <- c("Estimate")
              cat("\nCoefficients:\n")
              printCoefmat(coef.table)
            }else
            if ("2" %in% mcl$method) {
              z <- object
              coef.table <- matrix(z@coefficients,,ncol=1)
              rownames(coef.table) <- colnames(z@regressors)
              colnames(coef.table) <- c("Estimate")
              cat("\nCoefficients:\n")
              printCoefmat(coef.table)


              } else
                if ("discrete" %in% mcl$type) {
                    z <- object
                    est <- z@coefficients # estimates value
                    coef.table <- est
                    coef.table <- matrix(coef.table, , ncol=1)
                    rownames(coef.table) <- colnames(z@regressors)
                    colnames(coef.table) <- c("Estimate")
                    cat("\nCoefficients:\n")
                    printCoefmat(coef.table) # print the coefficient


                }

          }
)

#' @title  summary for the copulaCorrection method
#' @description returns the summary of the estimated model
#' @param  object an object of class "copulaREndo"
#' @param ... extra named arguments 
#' @export
#' @keywords internal
setMethod(f = "summary", signature= c(object="copulaREndo"),
definition = function(object, ...)            
          
  {         
            mcl <- object@copCall    
            
            if ("1" %in% mcl$method) {
              z <- object
              estEnd <- z@coefEndoVar  # estimates value
              estEx <- z@coefExoVar 
              coef.table <- estEx
              coef.table <- append(coef.table, estEnd)
              coef.table <- append(coef.table, z@rho)
              coef.table <- append(coef.table, z@sigma)
              coef.table <- matrix(coef.table, , ncol=1) 
              rownames(coef.table) <- append(colnames(z@regressors),c("rho","sigma"))
              colnames(coef.table) <- c("Estimate")
              cat("\nCoefficients:\n")
              printCoefmat(coef.table) # print the coefficient 
              #cat("\n Initial parameter values:\n", z@param)  # print logLik values
              #cat("\n")
              cat("\n The Value of the log likelihood function:\n", z@value)  # print logLik values
              cat("\n")
              cat("\n Convergence Code:\n", z@convCode)  # print comvergence code
            } else 
              if ("2" %in% mcl$method){
                      z <- object
                      coef.table <- z@coefficients
                      coef.table <- cbind(coef.table, z@seCoefficients)
                      # t-values
                      t_val_table <- z@coefficients/z@seCoefficients  # t-score endogenous variable
                      coef.table <- cbind(coef.table, t_val_table)
                      # p-values
                      pval <- 2*pt(q=(-abs(t_val_table)), df=(length(z@regressors[,1])-1))  
                      coef.table <- cbind(coef.table, pval)
                      rownames(coef.table) <- colnames(z@regressors)
                      colnames(coef.table) <- c("Estimate","Std. Error", "z-score", "Pr(>|z|)")
                      cat("\nCoefficients:\n")
                      printCoefmat(coef.table)
                    } else 
                       if ("discrete" %in% mcl$type){
                              z <- object
                              est <- z@coefficients  # coef. estimates 
                              #estExo <- z@coefExoVar    # coeff estimates of exogenous variables
                              coef.table <- est
                              coef.table <- cbind(coef.table, z@confint) 
                              rownames(coef.table) <- colnames(z@regressors)
                              colnames(coef.table) <- c("Estimate","Low_95%CI","Up_95%CI")
                              cat("\nCoefficients:\n")
                             printCoefmat(coef.table) # print the coefficient 
                             
                       }
}           
)





