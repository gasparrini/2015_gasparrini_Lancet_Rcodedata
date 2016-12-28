################################################################################
# SUPPLEMENTAL MATERIAL of the article:
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#
# This code reproduces the analysis with the subset of data only including UK
#
# 17 March 2016
# * an updated version of this code, (hopefully) compatible with future
#   versions of the software, is available at the personal website of the
#   first author (www.ag-myresearch.com)
################################################################################

################################################################################
# MULTIVARIATE META-ANALYSIS OF THE REDUCED COEF AND THEN COMPUTATION OF BLUP
################################################################################

# CREATE AVERAGE TEMPERATURE AND RANGE AS META-PREDICTORS
avgtmean <- sapply(dlist,function(x) mean(x$tmean,na.rm=T))
rangetmean <- sapply(dlist,function(x) diff(range(x$tmean,na.rm=T)))

################################################################################
# META-ANALYSIS
# NB: COUNTRY EFFECT IS NOT INCLUDED IN THIS EXAMPLE

mv <- mvmeta(coef~avgtmean+rangetmean,vcov,data=cities,control=list(showiter=T))
summary(mv)
# NB: IN THIS EXAMPLE THE MV-META MODEL IS CLEARLY OVERPARAMETERIZED

################################################################################

# FUNCTION FOR COMPUTING THE P-VALUE OF A WALD TEST
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}

# TEST THE EFFECTS
fwald(mv,"avgtmean")
fwald(mv,"rangetmean")

################################################################################
# OBTAIN BLUPS

blup <- blup(mv,vcov=T)

################################################################################
# RE-CENTERING

# GENERATE THE MATRIX FOR STORING THE RESULTS
minperccity <- mintempcity <- rep(NA,length(dlist))
names(mintempcity) <- names(minperccity) <- cities$city

# DEFINE MINIMUM MORTALITY VALUES: EXCLUDE LOW AND VERY HOT TEMPERATURE
for(i in seq(length(dlist))) {
  data <- dlist[[i]]
  predvar <- quantile(data$tmean,1:99/100,na.rm=T)
  # REDEFINE THE FUNCTION USING ALL THE ARGUMENTS (BOUNDARY KNOTS INCLUDED)
  argvar <- list(x=predvar,fun=varfun,
    knots=quantile(data$tmean,varper/100,na.rm=T),degree=vardegree,
    Bound=range(data$tmean,na.rm=T))
  bvar <- do.call(onebasis,argvar)
  minperccity[i] <- (1:99)[which.min((bvar%*%blup[[i]]$blup))]
  mintempcity[i] <- quantile(data$tmean,minperccity[i]/100,na.rm=T)
}

# COUNTRY-SPECIFIC POINTS OF MINIMUM MORTALITY
(minperccountry <- median(minperccity))

#
