###
### (c) Antonio Gasparrini 2015-2017
#
################################################################################
# FUNCTION FOR COMPUTING ATTRIBUTABLE MEASURES FROM DLNM
#   REQUIRES dlnm VERSION 2.2.0 AND ON
################################################################################
#
# DISCLAIMER:
#   THE CODE COMPOSING THIS FUNCTION HAS NOT BEEN SYSTEMATICALLY TESTED. THE
#   PRESENCE OF BUGS CANNOT BE RULED OUT. ALSO, ALTHOUGH WRITTEN GENERICALLY
#   FOR WORKING IN DIFFERENT SCENARIOS AND DATA, THE FUNCTION HAS NOT BEEN
#   TESTED IN CONTEXTS DIFFERENT THAN THE EXAMPLE INCLUDED IN THE PAPER.
#   IT IS RESPONSIBILITY OF THE USER TO CHECK THE RELIABILITY OF THE RESULTS IN
#   DIFFERENT APPLICATIONS.
#
# Version: 25 January 2017
# AN UPDATED VERSION CAN BE FOUND AT:
#   https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata
#
################################################################################
# SEE THE PDF WITH A DETAILED DOCUMENTATION AT www.ag-myresearch.com
#
#   - x: AN EXPOSURE VECTOR OR (ONLY FOR dir="back") A MATRIX OF LAGGED EXPOSURES
#   - basis: THE CROSS-BASIS COMPUTED FROM x
#   - cases: THE CASES VECTOR OR (ONLY FOR dir="forw") THE MATRIX OF FUTURE CASES
#   - model: THE FITTED MODEL
#   - coef AND vcov: COEF AND VCOV FOR basis IF model IS NOT PROVIDED
#   - type: EITHER "an" OR "af" FOR ATTRIBUTABLE NUMBER OR FRACTION
#   - dir: EITHER "back" OR "forw" FOR BACKWARD OR FORWARD PERSPECTIVES
#   - tot: IF TRUE, THE TOTAL ATTRIBUTABLE RISK IS COMPUTED
#   - cen: THE REFERENCE VALUE USED AS COUNTERFACTUAL SCENARIO
#   - range: THE RANGE OF EXPOSURE. IF NULL, THE WHOLE RANGE IS USED
#   - sim: IF SIMULATION SAMPLES SHOULD BE RETURNED. ONLY FOR tot=TRUE
#   - nsim: NUMBER OF SIMULATION SAMPLES
################################################################################
attrdl <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,type="af",
  dir="back",tot=TRUE,cen,range=NULL,sim=FALSE,nsim=5000) {
################################################################################
#
  # CHECK VERSION OF THE DLNM PACKAGE
  if(packageVersion("dlnm")<"2.2.0") 
    stop("update dlnm package to version >= 2.2.0")
#
  # EXTRACT NAME AND CHECK type AND dir
  name <- deparse(substitute(basis))
  type <- match.arg(type,c("an","af"))
  dir <- match.arg(dir,c("back","forw"))
#
  # DEFINE CENTERING
  if(missing(cen) && is.null(cen <- attr(basis,"argvar")$cen))
    stop("'cen' must be provided")
  if(!is.numeric(cen) && length(cen)>1L) stop("'cen' must be a numeric scalar")
  attributes(basis)$argvar$cen <- NULL
#  
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen
#
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    at <- if(dir=="back") tsModel:::Lag(x,seq(lag[1],lag[2])) else 
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(at <- x)!=diff(lag)+1) 
      stop("dimension of 'x' not compatible with 'basis'")
  }
#
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF cases PROVIDED AS A MATRIX, TAKE THE ROW AVERAGE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases)!=diff(lag)+1) stop("dimension of 'cases' not compatible")
    den <- sum(rowMeans(cases,na.rm=TRUE),na.rm=TRUE)
    cases <- rowMeans(cases)
  } else {
    den <- sum(cases,na.rm=TRUE) 
    if(dir=="forw") 
      cases <- rowMeans(as.matrix(tsModel:::Lag(cases,-seq(lag[1],lag[2]))))
  }
#
################################################################################
#
  # EXTRACT COEF AND VCOV IF MODEL IS PROVIDED
  if(!is.null(model)) {
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- dlnm:::getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- dlnm:::getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- dlnm:::getlink(model,model.class)
    if(model.link!="log") stop("'model' must have a log link function")
  }
#
  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")
#
################################################################################
#
  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else dlnm:::seqlag(lag)
#  
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON typebasis)
  if(typebasis=="cb") {
    Xpred <- dlnm:::mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- dlnm:::mkXpred(typebasis,basis,x,predvar,predlag,cen)
  }
#  
  # CHECK DIMENSIONS  
  if(length(coef)!=ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov)!=c(length(coef),length(coef)))) 
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis=="one" && dir=="back")
    stop("only dir='forw' allowed for reduced estimates")
#
################################################################################
#
  # COMPUTE AF AND AN 
  af <- 1-exp(-drop(as.matrix(Xpredall%*%coef)))
  an <- af*cases
#
  # TOTAL
  #   - SELECT NON-MISSING OBS CONTRIBUTING TO COMPUTATION
  #   - DERIVE TOTAL AF
  #   - COMPUTE TOTAL AN WITH ADJUSTED DENOMINATOR (OBSERVED TOTAL NUMBER)
  if(tot) {
    isna <- is.na(an)
    af <- sum(an[!isna])/sum(cases[!isna])
    an <- af*den
  }
#
################################################################################
#
  # EMPIRICAL CONFIDENCE INTERVALS
  if(!tot && sim) {
    sim <- FALSE
    warning("simulation samples only returned for tot=T")
  }
  if(sim) {
    # SAMPLE COEF
    k <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(rnorm(length(coef)*nsim),nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)
    # RUN THE LOOP
    # pre_afsim <- (1 - exp(- Xpredall %*% coefsim)) * cases # a matrix
    # afsim <- colSums(pre_afsim,na.rm=TRUE) / sum(cases[!isna],na.rm=TRUE)
    afsim <- apply(coefsim,2, function(coefi) {
      ani <- (1-exp(-drop(Xpredall%*%coefi)))*cases
      sum(ani[!is.na(ani)])/sum(cases[!is.na(ani)])
    })
    ansim <- afsim*den
  }
#
################################################################################
#
  res <- if(sim) {
    if(type=="an") ansim else afsim
  } else {
    if(type=="an") an else af    
  }
#
  return(res)
}

#
