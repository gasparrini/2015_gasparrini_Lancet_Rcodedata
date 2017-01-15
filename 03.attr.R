################################################################################
# Updated version of the code for the analysis in:
#
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#   http://www.ag-myresearch.com/2015_gasparrini_lancet.html
#
# Update: 15 January 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata
################################################################################

################################################################################
# COMPUTE THE ATTRIBUTABLE DEATHS FOR EACH CITY, WITH EMPIRICAL CI
# ESTIMATED USING THE RE-CENTERED BASES
################################################################################

# LOAD THE FUNCTION FOR COMPUTING THE ATTRIBUTABLE RISK MEASURES
source("attrdl.R")

# CREATE THE VECTORS TO STORE THE TOTAL MORTALITY (ACCOUNTING FOR MISSING)
totdeath <- rep(NA,nrow(cities))
names(totdeath) <- cities$city

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE DEATHS
matsim <- matrix(NA,nrow(cities),3,dimnames=list(cities$city,
  c("glob","cold","heat")))

# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
nsim <- 1000

# CREATE THE ARRAY TO STORE THE CI OF ATTRIBUTABLE DEATHS
arraysim <- array(NA,dim=c(nrow(cities),3,nsim),dimnames=list(cities$city,
  c("glob","cold","heat")))

################################################################################

# RUN THE LOOP
for(i in seq(dlist)){
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  data <- dlist[[i]]
  
  # DERIVE THE CROSS-BASIS
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  argvar <- list(x=data$tmean,fun=varfun,knots=quantile(data$tmean,
    varper/100,na.rm=T),degree=vardegree)
  cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
    arglag=list(knots=logknots(lag,lagnk)))
  
  # COMPUTE THE ATTRIBUTABLE DEATHS
  # NB: THE REDUCED COEFFICIENTS ARE USED HERE
  matsim[i,"glob"] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
    vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i])
  matsim[i,"cold"] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
    vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
    range=c(-100,mintempcity[i]))
  matsim[i,"heat"] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
    vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
    range=c(mintempcity[i],100))

  # COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE DEATHS
  # USED TO DERIVE CONFIDENCE INTERVALS
  arraysim[i,"glob",] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
    vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],sim=T,nsim=nsim)
  arraysim[i,"cold",] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
    vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
    range=c(-100,mintempcity[i]),sim=T,nsim=nsim)
  arraysim[i,"heat",] <- attrdl(data$tmean,cb,data$death,coef=blup[[i]]$blup,
    vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
    range=c(mintempcity[i],100),sim=T,nsim=nsim)
  
  # STORE THE DENOMINATOR OF ATTRIBUTABLE DEATHS, I.E. TOTAL OBSERVED MORTALITY
  # CORRECT DENOMINATOR TO COMPUTE THE ATTRIBUTABLE FRACTION LATER, AS IN attrdl
  totdeath[i] <- sum(data$death,na.rm=T)
}

################################################################################
# ATTRIBUTABLE NUMBERS

# CITY-SPECIFIC
ancity <- matsim
ancitylow <- apply(arraysim,c(1,2),quantile,0.025)
ancityhigh <- apply(arraysim,c(1,2),quantile,0.975)
rownames(ancity) <- rownames(ancitylow) <- rownames(ancityhigh) <- cities$cityname

# TOTAL
# NB: FIRST SUM THROUGH CITIES
antot <- colSums(matsim)
antotlow <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.025)
antothigh <- apply(apply(arraysim,c(2,3),sum),1,quantile,0.975)

################################################################################
# TOTAL MORTALITY

# BY COUNTRY
totdeathtot <- sum(totdeath)

################################################################################
# ATTRIBUTABLE FRACTIONS

# CITY-SPECIFIC
afcity <- ancity/totdeath*100
afcitylow <- ancitylow/totdeath*100
afcityhigh <- ancityhigh/totdeath*100

# TOTAL
aftot <- antot/totdeathtot*100
aftotlow <- antotlow/totdeathtot*100
aftothigh <- antothigh/totdeathtot*100

#
