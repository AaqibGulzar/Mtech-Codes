#'Weighted-Multiple-Linear Regression Program (WREG)
#'
#'@description The \code{WREG.GLS} function executes the multiple linear 
#'  regression analysis using generalized least-squares regression.
#'  
#'@param Y A numeric vector of the dependent variable of interest, with any 
#'  transformations already applied.
#'@param X A numeric matrix of the independent variables in the regression, with
#'  any transformations already applied.  Each row represents a site and each 
#'  column represents a particular independe variable.  (If a leading constant 
#'  is used, it should be included here as a leading column of ones.)  The rows 
#'  must be in the same order as the dependent variables in \code{Y}.
#'@param x0 A vector containing the independent variables (as above) for a 
#'  particular target site.  This variable is only used for ROI analysis.
#'@param recordLengths A numeric matrix whose rows and columns are in the same 
#'  order as \code{Y}.  Each \code{(r,c)} element represents the length of 
#'  concurrent record between sites \code{r} and \code{c}.  The diagonal 
#'  elements therefore represent each site's full record length.
#'@param LP3 A numeric matrix containing the fitted Log-Pearson Type III 
#'  standard deviate, standard deviation and skew for each site.  The columns of
#'  the matrix represent S, K, G, and an option regional skew value \code{GR} 
#'  required by WREG.GLS with regSkew = TRUE. The order of the rows must be the 
#'  same as \code{Y}.
#'@param alpha A numeric. \code{alpha} is a parameter used in the estimated 
#'  cross-correlation between site records.  See equation 20 in the WREG v. 1.05
#'  manual.  The arbitrary, default value is 0.01.  The user should fit a 
#'  different value as needed.
#'@param theta A numeric. \code{theta} is a parameter used in the estimated 
#'  cross-correlation between site records.  See equation 20 in the WREG v. 1.05
#'  manual.  The arbitrary, default value is 0.98.  The user should fit a 
#'  different value as needed.
#'@param basinChars A dataframe containing three variables: \code{StationID} is 
#'  the identifier of each site, \code{Lat} is the latitude of the site, in 
#'  decimal degrees, and \code{Long} is the longitude of the site, in decimal 
#'  degrees.  The sites must be presented in the same order as \code{Y}.
#'@param transY A required character string indicating if the the 
#'  dependentvariable was transformed by the common logarithm ('log10'), 
#'  transformed by the natural logarithm ('ln') or untransformed ('none').
#'@param MSEGR A numeric. The mean squared error of the regional skew.  Required
#'  only if \code{regSkew = TRUE}.
#'@param TY A numeric.  The return period of the event being modeled.  Required 
#'  only for \dQuote{GLSskew}.  The default value is \code{2}.  (See the 
#'  \code{Legacy} details below.)
#'@param peak A logical.  Indicates if the event being modeled is a peak flow 
#'  event or a low-flow event.  \code{TRUE} indicates a peak flow, while 
#'  \code{FALSE} indicates a low-flow event.
#'@param distMeth A numeric. A value of \code{1} indicates that the "Nautical 
#'  Mile" approximation should be used to calculate inter-site distances.  A 
#'  value of \code{2} designates the Haversine approximation.  See 
#'  \code{\link{Dist.WREG}}.  The default value is \code{2}.  (See the 
#'  \code{Legacy} details below.)
#'@param regSkew A logical vector indicating if regional skews are provided with
#'  an adjustment required for uncertainty therein (\code{TRUE}).  The default 
#'  is \code{FALSE}.
#'@param legacy A logical.  A value of \code{TRUE} forces the WREG program to 
#'  behave identically to WREG v. 1.05, with BUGS and all.  It will force 
#'  \code{TY=2} and \code{DistMeth=1}.  For ROI regressions, it will also 
#'  require a specific calculation for weighing matrices in \dQuote{WLS} 
#'  (\code{\link{Omega.WLS.ROImatchMatLab}}), \dQuote{GLS}, and \dQuote{GLSskew}
#'  (see \code{\link{Omega.GLS.ROImatchMatLab}}) Further details are provided in
#'  \code{\link{WREG.RoI}}
#'  
#'@details In this implementation, the weights for generalized least-squares 
#'  regression are defined by intersite correlations and record lengths.  See 
#'  manual for details.
#'  
#'  The logical handle \code{Legacy} has been included to test that this program
#'  returns the same results as WREG v. 1.05.  In the development of this code, 
#'  some idiosyncrasies of the MatLab code for WREG v. 1.05 became apparent. 
#'  Setting \code{Legacy} equal to \code{TRUE} forces the code to use the same 
#'  idiosycrasies as WREG v. 1.05.  Some of these idosyncrasies may be bugs in 
#'  the code.  Further analysis is needed.  For information on the specific 
#'  idiosyncrasies, see the notes for the \code{Legacy} input and the links to 
#'  other functions in this package.
#'  
#'@return All outputs are returned as part of a list.  The elements of the list 
#'  depend on the type of regression performed.  The elements of the list may 
#'  include: \item{Coefs}{A data frame composed of four variables: (1) 
#'  \code{Coefficient} contains the regression coefficeints estimated for the 
#'  model, (2) \code{Standard Error} contains the standard errors of each
#'  regression coefficient, (3) \code{tStatistic} contains the Student's 
#'  T-statistic of each regression coefficient and (4) \code{pValue} contains 
#'  the significance probability of each regression coefficient.} 
#'  \item{ResLevInf}{A data frame composed of four variables for each site in 
#'  the regression.  \code{Residual} contains the model residuals. 
#'  \code{Leverage} contains the leverage of each site.  \code{Influence} 
#'  contains the influence of each site.  \code{VarPred} contains the variance
#'  of prediction at each site.} \item{LevLim}{The critical value of leverage. 
#'  See \code{\link{Leverage}}} \item{InflLim}{The critical value of influence. 
#'  See \code{\link{Influence}}} \item{LevInf.Sig}{A logical matrix indicating
#'  if the leverage (column 1) is significant and the influence (column 2) is
#'  significant for each site in the regression.} \item{PerformanceMetrics}{A
#'  list of not more than ten elements.  All regression types return the mean
#'  squared error of residuals (\code{MSE}), the coefficient of determination
#'  (\code{R2}), the adjusted coefficient of determination (\code{R2_adj}) and
#'  the root mean squared error (\code{RMSE}, in percent).  The pseudo
#'  coefficient of regression (\code{R2_pseudo}), the average variance of
#'  prediction (\code{AVP}), the standard error of prediction (\code{Sp}, in
#'  percent), a vector of the individual variances of prediction for each site
#'  (\code{VP.PredVar}), the model-error variance (\code{ModErrVar}) and the
#'  standardized model error variance (\code{StanModErr}, in percent) are also
#'  returned.  Details on the appropriateness and applicability of performance
#'  metrics can be found in the WREG manual.} \item{X}{The input predictors.}
#'  \item{Y}{The input observations.} \item{fitted.values}{A vector of model
#'  estimates from the regression model.} \item{residuals}{A vector of model
#'  residuals.} \item{Weighting}{The weighting matrix used to develop regression
#'  estimates.} \item{Input}{A list of input parameters for error searching. 
#'  Currently empty.}
#'@import stats
#'  
#'@examples
#' # Import some example data
#' peakFQdir <- paste0(
#'   file.path(system.file("exampleDirectory", package = "WREG"),
#'     "pfqImport"))
#' gisFilePath <- file.path(peakFQdir, "pfqSiteInfo.txt")
#' importedData <- importPeakFQ(pfqPath = peakFQdir, gisFile = gisFilePath)
#' 
#' # Organizing input data
#' lp3Data <- importedData$LP3f
#' lp3Data$K <- importedData$LP3k$AEP_0.5
#' Y <- importedData$Y$AEP_0.5
#' X <- importedData$X[c("Sand", "OutletElev", "Slope")]
#' recordLengths <- importedData$recLen
#' basinChars <- importedData$BasChars
#' transY <- "none"
#' 
#' # Run GLS regression
#' result <- WREG.GLS(Y, X, recordLengths, LP3 = lp3Data, basinChars, transY)
#'
#'@export

WREG.GLS <- function(Y,X,recordLengths,LP3,basinChars,transY,
                     x0=NA,alpha=0.01,theta=0.98,peak=T,distMeth=2,
                     regSkew=FALSE,MSEGR=NA,TY=2,legacy=FALSE) {
  # William Farmer, USGS, January 05, 2015
  # 11/9/16 Greg Petrochenkov: Changed validation scheme
  
  warn("clear")
  # Some upfront error handling
  wregValidation((!missing(X)&!missing(Y))&&(length(Y)!=nrow(X)), "eq", FALSE,
                 paste0("The length of Y must be the same as ",
                        "the number of rows in X."), warnFlag = TRUE)
  
  if (!wregValidation((!missing(X)&!missing(Y))&&(length(Y)!=nrow(X)), "eq", FALSE,
                      "Dependent variable (Y) must be provided", warnFlag = TRUE)) {
    
    if (!wregValidation(Y, "numeric", message = 
                        "Dependent variable (Y) must be provided as class numeric",
                        warnFlag = TRUE)) {
      
      wregValidation(sum(is.na(Y)), "eq", 0 ,
                     paste0("The depedent variable (Y) contains missing ",
                            "values.  These must be removed."),
                     warnFlag = TRUE)
      
      wregValidation(sum(is.infinite(Y)), "eq", 0 ,
                     paste0("The depedent variable (Y) contains infinite ",
                            "values.  These must be removed."),
                     warnFlag = TRUE)
    }
  }
  
  if (!wregValidation(missing(X), "eq", FALSE,
                      "Independent variables (X) must be provided.", warnFlag = TRUE)) {
    
    if (!wregValidation((length(unique(apply(X,FUN=class,MARGIN=2)))!=1)|
                        (unique(apply(X,FUN=class,MARGIN=2))!="numeric"), "eq", FALSE,
                        "Independent variables (X) must be provided as class numeric.", warnFlag = TRUE)){
      
      wregValidation(sum(is.na(as.matrix(X))), "eq", 0,
                     paste0("Some independent variables (X) contain missing ",
                            "values.  These must be removed."), warnFlag = TRUE)
      
      wregValidation(sum(is.infinite(as.matrix(X))), "eq", 0,
                     paste0("Some independent variables (X) contain infinite ",
                            "values.  These must be removed."), warnFlag = TRUE)
    }
  }
  
  wregValidation(!is.logical(regSkew), "eq", FALSE,
                 paste0("regSkew must be either TRUE to for skew correction",
                        "or FALSE for no skew correction."), warnFlag = TRUE)
  
  if(!wregValidation(missing(transY)|!is.character(transY), "eq", FALSE,
                     "transY must be included as a character string", warnFlag = TRUE)) {
    
    wregValidation(!is.element(transY,c("none","log10","ln")), "eq", FALSE,
                   "transY must be either 'none', 'log10' or 'ln'", warnFlag = TRUE)
  }
  
  ## Add controls to meet legacy demands
  wregValidation(!is.logical(legacy), "eq", FALSE,
                 paste0("legacy must be either TRUE to force matching with previous",
                        "versions or FALSE for correct computations."), warnFlag = TRUE)
  
  ##    NOTE: legacy forces program to return the same results as WREG v 1.05.
  if (legacy) { # If legacy is indicated, override custom inputs.
    TY <- 2 # WREG v1.05 does not read this input correctly.
    distMeth <- 1 # WREG v1.05 uses "Nautical Mile" approximation
  }
  
  ## Determine if ROI is being applied
  if (is.na(sum(x0))) { # ROI regression is not used.
    ROI <- F
  } else { # ROI regression is used.
    ROI <- T
  }
  
  wregValidation(ROI&&(length(x0)!=ncol(X)), "eq", FALSE,
                 paste0("The length of x0 must be the same as ",
                        "the number of columns in X"), warnFlag = TRUE)
  
  wregValidation(ROI&&(!is.numeric(x0)), "eq", FALSE,
                 "The input x0 must be of the numeric class", warnFlag = TRUE)
  
  ## Just initial values for control.
  var.modelerror.k <- NA
  CustomModelError <- FALSE
  
  #Convert X and Y from dataframes to matrices to work with matrix operations below
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  ## Weighting matrix
  if (!is.na(MSEGR)) {
    
    wregValidation(length(MSEGR)!=1, "eq", FALSE,
                   "MSEGR must be a single value", warnFlag = TRUE)
    
    wregValidation(MSEGR, "numeric", 
                   message="MSEGR must be a single value", warnFlag = TRUE)
  }
  if (warn("check")) {
    stop('Invalid inputs were provided. See warnings().', warn("get"))
  }
  if (regSkew==FALSE) {MSEGR<-NA}
  ### Estimate k-variable model-error variance and final weighting matrix
  GLS.Weighting <- Omega.GLS(alpha=alpha,theta=theta,independent=basinChars,X=X,Y=Y,recordLengths=recordLengths,LP3=LP3,MSEGR=MSEGR,TY=TY,peak=peak,distMeth=distMeth) # Subroutine to calculate GLS weighting by iteration
  var.modelerror.k <- GLS.Weighting$GSQ # k-variable model-error variance
  Omega <- GLS.Weighting$Omega # GLS/GLSskew weighting matrix
  ### Estimate 0-order model-error variance
  X.0 <- matrix(1,ncol=1,nrow=length(Y)) # Constant predictor
  GLS.Weighting <- Omega.GLS(alpha=alpha,theta=theta,independent=basinChars,X=X.0,Y=Y,recordLengths=recordLengths,LP3=LP3,MSEGR=MSEGR,TY=TY,peak=peak,distMeth=distMeth) # Subroutine to calculate GLS weighting by iteration
  var.modelerror.0 <- GLS.Weighting$GSQ # constant-model model-error variance
  
  ## Basic regression calculations
  B_hat <- solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%solve(Omega)%*%Y # Estimated regression coefficients. Eq 7 (9, 11, and 18)
  Y_hat <- X%*%B_hat # Model estimates. Eq 8
  e <- Y-Y_hat # Model residuals. Eq 30
  
  ## Performance metrics
  MSE <- sum(e^2)/(nrow(X)-ncol(X)) # Mean square-error. Eq 31
  SSR <- sum(e^2) # Residual sum of squares. Eq 36
  SST <- sum((Y-mean(Y))^2) # Total sum of squares. Eq 37
  R2 <- 1 - SSR/SST # Coefficient of determination. Eq 35
  R2_adj <- 1 - SSR*(nrow(X)-1)/SST/(nrow(X)-ncol(X)) # Adjusted coefficient of determination. Eq 38
  RMSE <- NA
  if (transY=='log10') {
    RMSE <-100*sqrt(exp(log(10)*log(10)*MSE)-1) # Root-mean-squared error, in percent. Eq 34
  } else if (transY=='ln') {
    RMSE <-100*sqrt(exp(MSE)-1) # Root-mean-squared error, in percent. transformed for natural logs.
  }
  PerfMet <- list(MSE=MSE,R2=R2,R2_adj=R2_adj,RMSE=RMSE) # Performance metrics for output (basic, for OLS)
  R2_pseudo <- 1 - var.modelerror.k/var.modelerror.0 # Pseudo coefficient of determination. Eq 39
  AVP <- var.modelerror.k + mean(diag(X%*%solve(t(X)%*%solve(Omega)%*%X)%*%t(X))) # Average varaince of prediction. Eq 32
  VP <- vector(length=length(Y)) # Empty vector for individual variances of prediction
  for (i in 1:length(VP)) { # Individual variances of prediction
    VP[i] <- var.modelerror.k + X[i,]%*%solve(t(X)%*%solve(Omega)%*%X)%*%X[i,] # Individual variance of prediction.  Based on Eq 32.
  }
  VP <- data.frame(VP); names(VP) <- 'PredVar' # Formating the VP vector for output
  Sp <- Se <- NA
  if (transY=='log10') {
    Sp <- 100*sqrt(exp(log(10)*log(10)*AVP)-1) # Standard error of predictions. Eq 33.
    Se <-100*sqrt(exp(log(10)*log(10)*var.modelerror.k)-1) # Standard model error. Not noted in the manual, but included as output in WREG 1.05.  Based on Eq 33.
  } else if (transY=='ln') {
    # corrected for natural logs
    Sp <- 100*sqrt(exp(AVP)-1)
    Se <-100*sqrt(exp(var.modelerror.k)-1)
  }
  PerfMet <- c(PerfMet,R2_pseudo=R2_pseudo,AVP=AVP,Sp=Sp,VP=VP,ModErrVar=var.modelerror.k,StanModErr=Se) # Performance metrics for output
  
  
  ## Leverage and influence statistics
  Lev <- Leverage(X=X,Omega=Omega,x0=x0,ROI=ROI) # Leverage subroutine
  Infl <- Influence(e=e,X=X,Omega=Omega,Beta=B_hat,ROI=ROI,Lev=Lev$Leverage) # Influence subroutine
  
  ## Significance of regression parameters
  B_var <- diag(solve(t(X)%*%solve(Omega)%*%X)) # Covariances of regression coefficients. Eq 46
  
  B_tval <- B_hat/sqrt(B_var) # T-value statistics of regression coefficients. Eq 45.
  B_pval <- 2*stats::pt(-abs(B_tval),df=(nrow(X)-ncol(X))) # Significnace of regression coefficients
  
  ## Create summary tables
  Coefs <- data.frame(cbind(B_hat,sqrt(B_var),B_tval,B_pval)) # Regression coefficient table for output
  names(Coefs) <- c('Coefficient','Standard Error','tStatistic','pValue')
  ResLevInf <- data.frame(cbind(e,Lev$Leverage,Infl$Influence, VP)) # Residuals, leverage and influence of each varaible for output
  names(ResLevInf) <- c('Residual','Leverage','Influence', 'VarPred')
  LevInf.Sig<-data.frame(cbind(Lev$Significant,Infl$Significant)) # Indication of significance for leverage and Influence for output
  names(LevInf.Sig) <- c('SignificantLeverage','SignificantInfluence')
  
  ## Handling output
  Output <- list(Coefs=Coefs,ResLevInf=ResLevInf,LevLim=Lev$Limit,
                 InflLim=Infl$Limit,LevInf.Sig=LevInf.Sig,
                 PerformanceMetrics=PerfMet,X=X,Y=Y,fitted.values=Y_hat,residuals=e,
                 Weighting=Omega,Inputs=list(legacy=legacy,transY=transY))
  if (ROI) { # Appended at-site estimates for ROI calculations
    Y_est <- as.matrix(x0)%*%B_hat # ROI site estimate
    Output <- c(Output,Y.ROI=Y_est,x0.ROI=x0)
  }
  
  
  if (regSkew==FALSE) {
    class(Output) <- 'WREG.GLS'
  } else if (regSkew==TRUE) {
    class(Output) <- 'WREG.GLSs'
  }
  
  return(Output)
}