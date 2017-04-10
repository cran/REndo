#'@title  Fitting Linear Models Endogeneous Regressors using Gaussian Copula
#
# Description
#'@description  Fits linear models with continuous or discrete endogeneous regressors using Gaussian copulas, method presented in Park and Gupta (2012).
#'This is a statistical technique to address the endogeneity problem, where no external instrumental variables are needed. The important assumption of the model is 
#'that the endogeneous variables should NOT be normally distributed.
#'@usage copulaCorrection(y,X,P,param,type, method, intercept, data)
# Arguments
#'@param  y  the vector or matrix containing the dependent variable. 
#'@param  X  the data frame or matrix containing the regressors of the model, both \emph{exogeneous and endogeneous}. The \emph{last column/s should contain the endogenous variable/s}.
#'@param  P  the matrix.vector containing the endogenous variables.
#'@param  param  the vector of initial values for the parameters of the model to be supplied to the optimization algorithm. The parameters to be estimated are \code{theta = \{b,a,rho,sigma\}}, where
#' \code{b} are the parameters of the exogenous variables, \code{a} is the parameter of the endogenous variable, \code{rho} is the parameter for the correlation between the error and the endogenous regressor, while 
#' \code{sigma} is the standard deviation of the structural error.
#'@param type  the type of the endogenous regressor/s. It can take two values, "continuous" or "discrete".
#'@param method  the method used for estimating the model. It can take two values, "1" or "2", where "1" is the ML approach described in Park and Gupta (2012),
#'and "2" is the equivalent OLS approach described in the same paper. "1" can be applied when there is just a single, continous endogenous variable. 
#'With one discrete or more than one continuous endogenous regressors, the second method is applied by default.
#'@param  intercept  optional parameter. The model is estimated by default with 
#'intercept. If no intercept is desired or the regressors matrix \code{X} contains already
#'a column of ones, intercept should be given the value "no".  
#'@param data  data frame or matrix containing the variables of the model.

#'@details The maximum likelihood estimation is performed by the "BFGS" algorithm. When there are two endogenous regressors, there is no need for initial parameters since the method applied is by default the augmented OLS, which
#' can be specified by using method two - "method="2"".
# Return Value
#'@return  Depending on the method and the type of the variables, it returns the optimal values of the parameters and their standard errors. When the method one is used, the standard 
#'errors returned are obtained bootstrapping over 10 samples. If more bootstraping samples are desired, the standard errors can be obtained using the \code{\link{boots}} function from the same package.
#'The following are being returned and can be saved: 
#'\item{coefficients}{the estimated coefficients.}
#'\item{standard errors}{the corresponding estimated coefficients standard errors.}
#'\item{fitted.values}{the fitted values.}
#'\item{residuals}{the estimated residuals.} 
#'\item{logLik}{the estimated log likelihood value in the case of method 1.}
#'\item{AIC}{Akaike Information Criterion in the case of method 1.}
#'\item{BIC}{Bayesian Information Criterion in the case of method 1.}
#'@author The implementation of the model by Raluca Gui based on the paper of Park and Gupta (2012).
#'@references   Park, S. and Gupta, S., (2012), 'Handling Endogeneous Regressors by Joint Estimation Using Copulas', Marketing Science, 31(4), 567-86. 
#'@seealso \code{\link{higherMomentsIV}}
#'@examples
#'#load dataset dataCopC1, where P is endogenous, continuous and not normally distributed
#'
#'data(dataCopC1)
#'y <- dataCopC1[,1]
#'X <- dataCopC1[,2:5]
#'P <- dataCopC1[,5]
#'\dontrun{
#'c1 <- copulaCorrection(y, X, P, type = "continuous", method = "1", intercept=FALSE)
#'summary(c1)
#'}
#'
#'# an alternative model can be obtained using "method ="2"".
#'c12 <- copulaCorrection(y, X, P, type = "continuous", method = "2", intercept=FALSE)
#'summary(c12)
#'
#'# with 2 endogeneous regressors no initial parameters are needed, the default is the augmented OLS.
#'data(dataCopC2)
#'y <- dataCopC2[,1]
#'X <- dataCopC2[,2:6]
#'P <- dataCopC2[,5:6]
#'c2 <- copulaCorrection(y, X, P, type = "continuous" ,method="2", intercept=FALSE)
#'summary(c2)
#'
#'# load dataset with 1 discrete endogeneous variable. 
#'# having more than 1 discrete endogenous regressor is also possible
#' data(dataCopDis)
#' y <- dataCopDis[,1]
#' X <- dataCopDis[,2:5]
#' P <- dataCopDis[,5]
#' c3 <- copulaCorrection(y, X, P, type = "discrete", intercept=FALSE, data = dataCopDis)
#' summary(c3)
#'@export


copulaCorrection <- function(y,X,P,param=NULL,type=NULL,method=NULL, intercept = NULL, data = NULL){

  mcl <- match.call()  
  obj <- methods::new("copulaREndo")
  if ("continuous" %in% mcl$type){ 
         if ("1" %in% mcl$method )
        {   
              print("Attention! The endogeneous regressor should be continuous and NOT normally distributed")
            
              cC1 <- copulaCont1(y,X,P,param, intercept, data)
              seC1 <- boots(10, y, X, P, cC1$param, intercept, data)    # run bottstrap for SE
              k <- cC1$k    # position of the endogenous regressor - always on the last column of X
              k1 <- cC1$k1    # k1=k+2
              obj@coefEndoVar  <- cC1$coef_cop[,k]
              obj@coefExoVar <- as.numeric(cC1$coef_cop[,1:(k-1)])
              obj@seCoefficients <- seC1
              obj@rho <- cC1$coef_cop[,k1-1]
              obj@sigma <- cC1$coef_cop[,k1]
              obj@param <- cC1$param
              obj@logLik <- (-1)*cC1$coef_cop[,"fevals"]
              obj@AIC <- (-2)*(obj@logLik) + 2*length(cC1$param)  # Akaike Information Criterion
              obj@BIC <- (-2)*(obj@logLik) + length(y)*length(cC1$param)
              obj@convCode <- cC1$coef_cop[,"convcode"]
              obj@regressors <- cC1$reg
              obj@fitted.values <- t(as.matrix(as.numeric(cC1$coef_cop[,1:k]))) %*% t(X) 
              obj@residuals <- as.matrix(y-obj@fitted.values)
              obj@copCall <- mcl
             
            
         } else 
             if ("2" %in% mcl$method) {
               cC2 <- copulaMethod2(y,X,P,intercept) 
               obj@coefficients <- round(cC2$f$coefficients,5)
               obj@seCoefficients <- as.matrix(summary(cC2$f)$coefficients[,2])
               obj@regressors <- cC2$reg
               obj@copCall <- mcl
               obj@residuals <- as.matrix(as.numeric(cC2$resid))
               obj@fitted.values <- as.matrix(cC2$fitted.values)
          
             
               }
  } else
   if ("discrete" %in% mcl$type) {
  
     cD <- copulaDiscrete(y,X,P,intercept, data)
     obj@coefficients  <- cD$coefficients
     obj@residuals <- as.matrix(as.numeric(cD$resid))
     obj@fitted.values <- as.matrix(cD$fitted.values)
     obj@confint <- cD$CI
     obj@regressors <- cD$reg
     obj@copCall <- mcl
     
     }
 
 return(obj)
}
