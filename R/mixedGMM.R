# Raluca Gui - 11.04.2016
# adapt from Kim and Frees 2007
# mixedGMM for 2 and 3 levels
#'@title Multilevel GMM Estimation
#'@aliases mixedGMM
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
#'@examples
#'\dontrun{
#'data(tScores)
#'endoVars <- tScores[,5:7] 
#'formula <- 
#'TLI ~ GRADE_3 + RETAINED + SWITCHSC + S_FREELU + FEMALE + BLACK + HISPANIC + 
#'OTHER + C_COHORT + T_EXPERI + CLASS_SI + P_MINORI + 
#'(1+GRADE_3 | CID) + (1 | SID)
#'model1<- mixedGMM(formula, endoVars, data=tScores)
#'coef(model1)
#'}
#'@export
mixedGMM <- function(formula, endoVar, data = NULL) {

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

if (Levels==3) {

gmmFEc <- GmmEstim(y,X,HIVc1,id=1,Wmat=W, Vmat=V, nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
bFEc <- gmmFEc$bIV   # FixedEffects estimation at L2
sebFEc <-  round(gmmFEc$MSdError,3)    #Sd Error
zFEc <- round(bFEc/sebFEc,3)   # z-score
pvalFEc <- round(2*stats::pnorm(-abs(zFEc)),3)

gmmGMMc <- GmmEstim(y,X,HIVc2,id=1,Wmat=W, Vmat=V,nLevels= Levels, nTot=nTot, obsid=obsid,envir=endo.env)
bGMMc <-gmmGMMc$bIV   # GMM estimation at L2
sebGMMc <- round(gmmGMMc$MSdError,3)
zGMMc <- round(bGMMc/sebGMMc,3)
pvalGMMc <- round(2*stats::pnorm(-abs(zGMMc)),3)

gmmFEs <- GmmEstim(y,X,HIVs1,id=2,Wmat=W, Vmat=V,nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
bFEs <- gmmFEs$bIV   # FE estimation at L3
sebFEs <- gmmFEs$MSdError
zFEs <- round(bFEs/sebFEs,3)
pvalFEs <- round(2*stats::pnorm(-abs(zFEs)),3)

gmmGMMs <-  GmmEstim(y,X,HIVs2,id=2,Wmat=W, Vmat=V,nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
bGMMs <-gmmGMMs$bIV   # GMM estimation at L3
sebGMMs <- round(gmmGMMs$MSdError,3)
zGMMs <- round(bGMMs/sebGMMs,3)
pvalGMMs <- round(2*stats::pnorm(-abs(zGMMs)),3)

HREE <- W %*% as.matrix(X)
gmmREE <- GmmEstim(y,X,HREE,id=0,Wmat=W, Vmat=V,nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
bREE <-gmmREE$bIV   # RandomEff estimation
sebREE <- gmmREE$MSdError
zREE <- round(bREE/sebREE,3)
pvalREE <- round(2*stats::pnorm(-abs(zREE)),3)

} else if (Levels==2){

  gmmFEc <- GmmEstim(y,X,HIVs1,id=1,Wmat=W, Vmat=V,nLevels= Levels, nTot=nTot, obsid=obsid,envir=endo.env)
  bFEc <- gmmFEc$bIV   # FE at L2
  sebFEc <-  gmmFEc$MSdError
  zFEc <- round(bFEc/sebFEc,3)
  pvalFEc <- round(2*stats::pnorm(-abs(zFEc)),3)
 
  gmmGMMc <- GmmEstim(y,X,HIVs2,id=1,Wmat=W, Vmat=V,nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bGMMc <- gmmGMMc$bIV   # GMM estimation at L2
  sebGMMc <- round(gmmGMMc$MSdError,3)
  zGMMc <- round(bGMMc/sebGMMc,3)
  pvalGMMC <- round(2*stats::pnorm(-abs(zGMMc)),3)
  
  HREE <- W %*% as.matrix(X)
  
  gmmREE <- GmmEstim(y,X,HREE,id=0,Wmat=W, Vmat=V,nLevels= Levels, nTot=nTot, obsid=obsid, envir=endo.env)
  bREE <- gmmREE$bIV   # RandomEffects estimation
  sebREE <- round(gmmREE$MSdError,3)
  zREE <- round(bREE/sebREE,3)    # z-score
  pvalREE <- round(2*stats::pnorm(-abs(zREE)),3)
}
# Print results

if (Levels==3) {
  
  zFEc[!is.finite(zFEc)] <- NA
  bFE_Lev2 <- cbind(bFEc,sebFEc, zFEc, pvalFEc)
  colnames(bFE_Lev2) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(bFE_Lev2) <- predictors
  
  zFEs[!is.finite(zFEs)] <- NA
  bFE_Lev3 <- cbind(bFEs,sebFEs, zFEs, pvalFEs)
  colnames(bFE_Lev3) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(bFE_Lev3) <- predictors
  
  zGMMc[!is.finite(zGMMc)] <- NA
  bGMM_Lev2 <- cbind(bGMMc,sebGMMc,zGMMc, pvalGMMc)
  colnames(bGMM_Lev2) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(bGMM_Lev2) <- predictors
  
  zGMMs[!is.finite(zGMMs)] <- NA
  bGMM_Lev3 <- cbind(bGMMs,sebGMMs,zGMMs, pvalGMMs)
  colnames(bGMM_Lev3)<- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(bGMM_Lev3) <- predictors
  
  b_REF <- cbind(bREE,sebREE,zREE, pvalREE)
  colnames(b_REF) <- c("Coefficients","ModelSE","z-score","Pr|>z|" )
  rownames(b_REF) <- predictors
  coefSdErr <- list(bFE_Lev2 = bFE_Lev2, bFE_Lev3 = bFE_Lev3, bGMM_Lev2 = bGMM_Lev2, bGMM_Lev3 = bGMM_Lev3, b_REF = b_REF)
  
  coef <- cbind(bFEc,bFEs,bGMMc, bGMMs, bREE)
  colnames(coef) <- c("FE_L2","FE_L3","GMM_L2","GMM_L3","RandomEffects")
  rownames(coef) <- predictors
  
  results <- list(coefficients=coef, coefSdErr=coefSdErr,vcovMat = V, weightMat = W, formula = formula, model = data)

  } else {
  bFE_Lev2 <- cbind(bFEc,sebFEc,zFEc, pvalFEc)
  colnames(bFE_Lev2) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(bFE_Lev2) <- predictors
  
  bGMM_Lev2 <- cbind(bGMMc,sebGMMc,zGMMc, pvalGMMc)
  colnames(bGMM_Lev2) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(bGMM_Lev2) <- predictors
  b_REF <- cbind(bREE,sebREE,zREE, pvalREE)
  colnames(b_REF) <- c("Coefficients","ModelSE","z-score", "Pr|>z|")
  rownames(b_REF) <- predictors
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

