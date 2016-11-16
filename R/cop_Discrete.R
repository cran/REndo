#'@title Fitting Linear Models with Endogeneous Discrete Regressors using Internal Instrumental Variables
# Description
#'@description  Fits linear models with discrete, endogeneous regressors using the approach described in Park and Gupta (2012). Due to the variablility 
#'in pStar, a simulation was needed in order to obtain the coefficient estimates. Then, in order to get the Z-scores and p-values the sum of the Z method was used
#'as described in  Zaykin, D V.(2011). "Optimally weighted Z-test is a powerful method for combining probabilities in meta-analysis". Journal of Evolutionary Biology, 24:1836-1841.

#
# Arguments
#'@param  y  the vector containing the dependent variable. 
#'@param  X  the matrix containing the regressors, with the endogenous variables occupying the last columns.
#'@param  P  the matrix containing the discrete endogeneous regressors.
#'@param  intercept  the intercept of the model. It should be specified whether the model should be estimated with or without intercept. 
#'If no intercept is desired, intercept should be given the value "FALSE", otherwise the value "TRUE".
#'@param data  the matrix or data frame containing the dataset used.

#
# Return Value
#'@return  Returns an object of class "lm".
#'@references Park, S. and Gupta, S., (2012), 'Handling Endogeneous Regressors by Joint Estimation Using Copulas', Marketing Science, 31(4), 567-86.

#'@keywords internal

copulaDiscrete <- function(y,X,P, intercept = NULL, data=NULL) {


 sim <- 250  
 k1 <- ncol(X)   
# P is the endogeneous regressor
 P1 <- as.matrix(P)
 colnames(P1) <- colnames(X)[-c(1:(ncol(X)-ncol(P1)))]

#  empirical cumulative distribution function
	 getEcdf <- function(x){
    H.p <- stats::ecdf(x)
    H.p1 <- H.p(x)
    H.p0 <- H.p(x-1)
    H.p0 <- ifelse(H.p0==0,0.0000001,H.p0)
    H.p0 <- ifelse(H.p0==1,0.9999999,H.p0)
    H.p1 <- ifelse(H.p1==0,0.0000001,H.p1)
    H.p1 <- ifelse(H.p1==1,0.9999999,H.p1)
        
    gecdf <- list(H.p = H.p, H.p1=H.p1, H.p0 = H.p0)
    
    return(gecdf)
  }


# each element of pecdf had 3 levels - H.p, H.p0 and H.p1. 
# Nr. of elements of pecdf = Nr. columns P1
 pecdf <- apply(P1,2,getEcdf)


  U.p <- matrix(NA, dim(P1)[1], dim(P1)[2])
  nc <- ncol(X) + ncol(P1)
  # create a list with nc elements (number of regressors + Pstar) , for each regressor save the confint for each lm out of sim simulations
  conf.int <- rep(list(matrix(0,sim,2)), nc)

  for (k in 1:dim(P1)[2]){
  
    U.p[,k] <- stats::runif(dim(P1)[1],min=pecdf[[k]]$H.p0,max=pecdf[[k]]$H.p1)
  }  
  p.star <- apply(U.p, 2,qnorm)
  colnames(p.star) <- paste("PStar",1:ncol(P1),sep=".")
  
  X1 <- cbind(X,p.star)
  X1 <- as.matrix(X1)
  k2 <- ncol(X1)
  dataCopula <- X1
  
  
  if (intercept==FALSE){  # no intercept 
    meth.2 <- stats::lm(y ~.-1,data=data.frame(dataCopula))
    bhat <-   stats::coef(summary(meth.2))[,1] # estimated coefficients
    
  } else {
    meth.2 <- stats::lm(y ~.,data=data.frame(dataCopula))  # with intercept
    bhat <- stats::coef(summary(meth.2))[,1]   # estimated coefficients
  } 
  names(bhat) <- colnames(dataCopula)
  
  
  for (i in 1:sim){
  for (k in 1:dim(P1)[2]){
    
    U.p[,k] <- stats::runif(dim(P1)[1],min=pecdf[[k]]$H.p0,max=pecdf[[k]]$H.p1)
  }  
  p.star <- apply(U.p, 2,qnorm)
  colnames(p.star) <- paste("PStar",1:ncol(P1),sep=".")
     
  X1 <- cbind(X,p.star)
  X1 <- as.matrix(X1)
  k2 <- ncol(X1)
  dataCopula <- X1

  if (intercept==FALSE){  # no intercept 
  meth.2 <- stats::lm(y ~.-1,data=data.frame(dataCopula))
  for (s in 1:nc) {
           conf.int[[s]][i,] <- confint(meth.2, level=0.95)[s,]
           }
    } else {
    meth.2 <- stats::lm(y ~.,data=data.frame(dataCopula))  # with intercept
   for (s in 1:nc) {
      conf.int[[s]][i,] <- confint(meth.2, level=0.95)[s,]
      
    }
  }
}

CI <- matrix(0,nc,2)
colnames(CI) <- c("Lower95%", "Upper95%")
for (i in 1:nc)
  CI[i,] <- apply(conf.int[[i]],2,mean)


res <- list(coefficients= bhat, CI=CI, reg=dataCopula)
return(res)
}