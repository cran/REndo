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
#'@param data optional data frame or matrix containing the variables of the model.

#'@details The maximum likelihood estimation is performed by the "BFGS" algorithm. When there are two endogenous regressors, there is no need for initial parameters since the method applied is by default the augmented OLS, which
#' can be specified by using method two - "method="2"".

# Return Value
#'@return  Depending on the method and the type of the variables, it returns the optimal values of the parameters and their standard errors in the case of the second method. 
#'With one endogenous variable, if the maximum likelihood approach is chosen, the standard errors can be computed by bootsptrapping using the \code{\link{boots}} function from the same package.

#'@author The implementation of the model by Raluca Gui based on the paper of Park and Gupta (2012).
#'@references   Park, S. and Gupta, S., (2012), 'Handling Endogeneous Regressors by Joint Estimation Using Copulas', Marketing Science, 31(4), 567-86. 
#'@seealso \code{\link{higherMomentsIV}}, \code{\link{latentIV}}
#'@examples
#'#load dataset dataCopC1, where P is endogenous, continuous and not normally distributed
#'data(dataCopC1)
#'y <- dataCopC1[,1]
#'X <- dataCopC1[,2:5]
#'P <- dataCopC1[,5]
#'c1 <- copulaCorrection(y, X, P, type = "continuous", method = "1", intercept=FALSE)
#'summary(c1)
#'# to obtain the standard errors use the boots() function
#'# se.c1 <- boots(10, y, X, P, param = c(1,1,-2,-0.5,0.2,1), intercept=FALSE)
#'
#'# an alternative model can be obtained using "method ="2"".
#'c12 <- copulaCorrection(y, X, P, type = "continuous", method = "2", intercept=FALSE)
#'summary(c12)
#'
#'# load dataset with 2 continuous, non-normally distributed endogeneous regressors.
#'# with 2 endogeneous regressors no initial parameters needed, the default is the augmented OLS.
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
#' c3 <- copulaCorrection(y, X, P, type = "discrete", intercept=FALSE)
#' summary(c3)
#'@export


copulaCorrection <- function(y,X,P,param=NULL,type=NULL,method=NULL, intercept = NULL, data=NULL){

  mcl <- match.call()  
  obj <- new("copulaREndo")
  if ("continuous" %in% mcl$type){ 
         if ("1" %in% mcl$method )
        {   
             print("Attention! The endogeneous regressor should be continuous and NOT normally distributed")
             
              cC1 <- copulaCont1(y=y,X=X,P=P,param=param, intercept=intercept, data=data)
              k <- cC1$k    # position of the endogenous regressor - always on the last column of X
              k1 <- cC1$k1    # k1=k+2
              obj@coefEndoVar  <- cC1$coef_cop[,k]
              obj@coefExoVar <- as.numeric(cC1$coef_cop[,1:(k-1)])
              obj@rho <- cC1$coef_cop[,k1-1]
              obj@sigma <- cC1$coef_cop[,k1]
              obj@value <- cC1$coef_cop[,"fevals"]
              obj@convCode <- cC1$coef_cop[,"convcode"]
              obj@regressors <- cC1$reg
              obj@copCall <- mcl
             
            
         } else 
             if ("2" %in% mcl$method) {
               cC2 <- copulaMethod2(y=y,X=X,P=P,intercept=intercept) 
               obj@coefficients <- cC2$f$coefficients
               obj@seCoefficients <- summary(cC2$f)$coefficients[,2]
               obj@regressors <- cC2$reg
               obj@copCall <- mcl
          
             
               }
  } else
   if ("discrete" %in% mcl$type) {
  
     cD <- copulaDiscrete(y=y,X=X,P=P,intercept=intercept)
     obj@coefficients  <- cD$coefficients
     obj@confint <- cD$CI
     obj@regressors <- cD$reg
     obj@copCall <- mcl
     
     }
 
 return(obj)
}
