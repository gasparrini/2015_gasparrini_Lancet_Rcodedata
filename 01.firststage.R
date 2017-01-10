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
# FIRST-STAGE ANALYSIS: RUN THE MODEL IN EACH CITY, REDUCE AND SAVE
################################################################################

################################################################################
# CREATE THE OBJECTS TO STORE THE RESULTS

# COEFFICIENTS AND VCOV FOR OVERALL CUMULATIVE SUMMARY
coef <- matrix(NA,nrow(cities),length(varper)+vardegree,
  dimnames=list(cities$city))
vcov <- vector("list",nrow(cities))
names(vcov) <- cities$city

################################################################################
# RUN THE LOOP

# LOOP
time <- proc.time()[3]
for(i in seq(length(dlist))) {

  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  data <- dlist[[i]]

  # DEFINE THE CROSSBASIS
  argvar <- list(fun=varfun,knots=quantile(data$tmean,varper/100,na.rm=T),
    degree=vardegree)
  cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
    arglag=list(knots=logknots(lag,lagnk)))
  #summary(cb)
  
  # RUN THE MODEL AND OBTAIN PREDICTIONS
  # NB: NO CENTERING NEEDED HERE, AS THIS DOES NOT AFFECT COEF-VCOV
  model <- glm(formula,data,family=quasipoisson,na.action="na.exclude")
  pred <- crosspred(cb,model)

  # REDUCTION TO OVERALL CUMULATIVE
  red <- crossreduce(cb,model)
  coef[i,] <- coef(red)
  vcov[[i]] <- vcov(red)
  
}
proc.time()[3]-time

#
