#'@title Fitting Linear Models with One Enedogenous Regressor using Gaussian Copula
#'
#'@description  Estimates a linear model with one endogenous variable using Gaussian copula. The optimization is done via maximum likelihood, using the BFLG algorithm.
#
# Arguments

#'@param    y  a vector or matrix containing the dependent variable. 
#'@param    X  a data frame or matrix containing the regressors of the model, both exogeneous and endogeneous. The last column should contain the endogeneous variable.
#'@param    P  a vector containing the continuous, non-normally distributed endogeneous variable.
#'@param    param  Initial values for the parameters to be optimized over.  
#'@param    intercept  It should be specified whether the model should be estimated with or without intercept. 
#'If no intercept is desired or the regressors matrix \code{X} contains already
#'a column of ones, intercept should be given the value "FALSE", otherwise the value "TRUE".
#'@param data  optional data frame or matrix containing the variables of the model.
 
#
# Return Value
#'@return    Returns a list with the best set of parameters found. 
#'@keywords
#'internal
#'copula
#'instrumental variables
copulaCont1 <- function(y,X,P, param = NULL, intercept = NULL, data = NULL){

  
    if (intercept==TRUE) {
      X <- cbind(rep(1,nrow(X)),X)
      k <- ncol(X) 
    } else {
        k <- ncol(X)
     }

    k1 <- k+2
    
    if (is.null(param)){
      datalm <- data.frame(X)
      if (intercept==FALSE) {
      l <- stats::lm(y ~ .-1, data=datalm)
      } else
        {
      l <- stats::lm(y ~ ., data=datalm)
      }
      par1 <- rep(0,ncol(datalm))
      for (i in 1:(ncol(datalm))){
        par1[i] <- l$coefficients[[i]]
      }
      #parameter are composed of the OLS estimates, while for rho and sigma the default value are 0.5 and 0.9 
      param <- c(par1,0.5,0.9)
    }
   
   b <- optimx::optimx(par=param,fn=logLL,y=y, X=X, P=P, method="BFGS", control=list(trace=0)) 
   results <- list(k=k,k1=k1,coef_cop=b, reg=as.matrix(X), param = param) 
 
    return(results)
  
  }

