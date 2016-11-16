# Raluca Gui - 11.04.2016
# adapt from Kim and Frees 2007
# multilevelIV for 2 and 3 levels
#'@title Multilevel GMM Estimation
#'@aliases multilevelIV
# Description
#'@description  Estimates multilevel models (max. 3 levels) employing the GMM approach presented in Kim and Frees (2007). One of the important features is that, using the hierarchical 
#'structure of the data, no external instrumental variables are needed, unlike traditional instrumental variable techniques.
# Arguments
#'@param    formula  an object of type 'formula': a symbolic description of the model to be fitted. 
#'@param    endoVar  a matrix or data frame containing the variables assumed to be endogenous.  
#'@param    data  optional data frame or list containing the variables of the model.
#details
#'@details    When all model variables are assumed exogenous the GMM estimator is the usual GLS estimator. 
#'While the GLS model assumes all explanatory variables are uncorrelated with the random intercepts and slopes in the model,
#'fixed effects models allow for endogeneity of all effects but sweeps out the random components as well as the explanatory variables at the same levels. 
#'The more general estimator presented here allows for some of the explanatory variables to be endogenous and uses this information to build internal instrumental variables.
#'The multilevel GMM estimator uses both the between and within variations of the exogenous variables, but only the within variation of the variables assumed 
#' endogenous. The mixed GMM estimator equals the random effects estimator when all variables are assumed exogenous and is equal to the fixed effects estimator 
#' when all variables are assumed endogenous. In between different GMM estimators are obtained for different sets of endogenous/exogenous variables.  
#'@return    returns the estimated coefficients together with their standard errors and p-values. It also returns the variance -covariance matrix and the weigth matrix used in estimation.
#'\item{coefficients}{the estimated coefficients.}
#'\item{coefSdErr}{the standard errors of the estimated coefficients.}
#'\item{vcovMat}{the variance-covariance matrix.}
#'\item{weigthMat}{the weight matrix used in estimation.}
#'\item{formula}{the formula of the estimated model.}
#'@keywords  endogeneity
#'@keywords  GMM
#'@keywords  instrumental
#'@keywords  instruments
#'@author The implementation of the model formula by Raluca Gui based on the paper of Kim and Frees (2007).
#'@references   Kim, Jee-Seon and Frees, Edward W. (2007). 'Multilevel Modeling with Correlated Effects'. \emph{Psychometrika},72(4), 505-533.
#'@seealso \code{\link{internalIV}}, \code{\link[AER]{ivreg}}, \code{\link{latentIV}},\code{\link{copulaCorrection}}
#'@examples
#'\dontrun{
#'data(tScores)
#'endoVars <- tScores[,5:7] 
#'formula <- 
#'TLI ~ GRADE_3 + RETAINED + SWITCHSC + S_FREELU + FEMALE + BLACK + HISPANIC + 
#'OTHER + C_COHORT + T_EXPERI + CLASS_SI + P_MINORI + 
#'(1+GRADE_3 | CID) + (1 | SID)
#'model1<- multilevelIV(formula, endoVars, data=tScores)
#'coef(model1)
#'}
#'@export
multilevelIV <- function(formula, endoVar, data = NULL) {

print("Attention! The endogeneous regressor names in endoVar have to match the names as they appear in the data")
  
Interim <-suppressWarnings(multilevelGMM(formula, endoVar = endoVar, data = data))
Levels <- Interim$Levels
W <- Interim$Wmat
V <- Interim$Vmat
X <- Interim$X
y <- Interim$y
Z <- Interim$Z
HIVc1 <- Interim$HIVc1
HIVc2 <- Interim$HIVc2
HIVs1 <- Interim$HIVs1
HIVs2 <- Interim$HIVs2
predictors <- Interim$pred
obsid <- Interim$obsid
nTot <- Interim$nTot
endo.env <- Interim$envir

#  GMM estimation 
printGMM <- function(y,X,HIV,id,Wmat, Vmat, nLevels, nTot, obsid, envir){
  
  estimGMM <- GmmEstim(y,X,HIV,id,Wmat, Vmat, nLevels, nTot, obsid, envir)
  b <- estimGMM$bIV
  se <- round(estimGMM$MSdError,3)
  z <- round(b/se,3)
  pval <- round(2*stats::pnorm(-abs(z)),3)
  printRes <- list(b=b,se=se, z=z, pval=pval)
  return(printRes)
}

if (Levels==3) {

  resFEc <- printGMM(y,X,HIVc1,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bFEc <- resFEc$b
  sebFEc <- resFEc$se
  zFEc <- resFEc$z
  pvalFEc <- resFEc$pval
  
  resGMMc <- printGMM(y,X,HIVc2,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bGMMc <- resGMMc$b
  sebGMMc <- resGMMc$se
  zGMMc <- resGMMc$z
  pvalGMMc <- resGMMc$pval
  
  resFEs <- printGMM(y,X,HIVs1,id=2,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bFEs <- resFEs$b
  sebFEs <- resFEs$se
  zFEs <- resFEs$z
  pvalFEs <- resFEs$pval

  resGMMs <- printGMM(y,X,HIVs2,id=2,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bGMMs <- resGMMs$b
  sebGMMs <- resGMMs$se
  zGMMs <- resGMMs$z
  pvalGMMs <- resGMMs$pval

  HREE <- W %*% as.matrix(X)
  resREE <- printGMM(y,X,HREE,id=0,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bREE <- resREE$b
  sebREE <- resREE$se
  zREE <- resREE$z
  pvalREE <- resREE$pval


} else if (Levels==2){

  resFEc <- printGMM(y,X,HIVc1,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bFEc <- resFEc$b
  sebFEc <- resFEc$se
  zFEc <- resFEc$z
  pvalFEc <- resFEc$pval
 
  resGMMc <- printGMM(y,X,HIVc2,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bGMMc <- resGMMc$b
  sebGMMc <- resGMMc$se
  zGMMc <- resGMMc$z
  pvalGMMc <- resGMMc$pval
  # 
  HREE <- W %*% as.matrix(X)
  
  resREE <- printGMM(y,X,HREE,id=0,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bREE <- resREE$b
  sebREE <- resREE$se
  zREE <- resREE$z
  pvalREE <- resREE$pval
}
# Print results

print.res <- function(coeff, stderr, z,pval){
  z[!is.finite(z)] <- NA
  pval[!is.finite(pval)] <- NA
  b <- cbind(coeff,stderr,z,pval)
  colnames(b) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(b) <- predictors
  return(b)
}

if (Levels==3) {
  
  bFE_Lev2 <- print.res(bFEc,sebFEc, zFEc, pvalFEc)
  bFE_Lev3 <- print.res(bFEs,sebFEs, zFEs, pvalFEs)
  bGMM_Lev2 <- print.res(bGMMc,sebGMMc,zGMMc, pvalGMMc)
  bGMM_Lev3 <- print.res(bGMMs,sebGMMs,zGMMs, pvalGMMs)
  b_REF <- print.res(bREE,sebREE,zREE, pvalREE)
  
  
  coef <- cbind(bFEc,bFEs,bGMMc, bGMMs, bREE)
  colnames(coef) <- c("FE_L2","FE_L3","GMM_L2","GMM_L3","RandomEffects")
  rownames(coef) <- predictors
  coefSdErr <- list(bFE_Lev2 = bFE_Lev2, bFE_Lev3 = bFE_Lev3, bGMM_Lev2 = bGMM_Lev2, bGMM_Lev3 = bGMM_Lev3, b_REF = b_REF)
  
  results <- list(coefficients=coef, coefSdErr=coefSdErr,vcovMat = V, weightMat = W, formula = formula, model = data)

  } else {
 
   bFE_Lev2 <- print.res(bFEc,sebFEc, zFEc, pvalFEc)
   bGMM_Lev2 <- print.res(bGMMc,sebGMMc,zGMMc, pvalGMMc)
   b_REF <- print.res(bREE,sebREE,zREE, pvalREE)
 
  coefSdErr <- list(bFE_Lev2 = bFE_Lev2, bFE_Lev3 = bFE_Lev3, bGMM_Lev2 = bGMM_Lev2, bGMM_Lev3 = bGMM_Lev3, b_REF = b_REF)
  coef <- cbind(bFEc, bGMMc, bREE)
  colnames(coef) <- c("FixedEffects","GMM","RandomEffects")
  rownames(coef) <- predictors
  results <- list(coefficients=coef, coefSdErr = coefSdErr,vcovMat = V, weightMat = W, formula = formula, model = data)
}

object <- methods::new("mixedREndo")

object@coefficients <- results$coefficients    # only coefficients 
object@coefSdErr <- results$coefSdErr    # coefficients with sdErrors and t-values
object@vcovMat <- results$vcovMat
object@weightMat <- results$weightMat
object@formula <- formula
object
}

