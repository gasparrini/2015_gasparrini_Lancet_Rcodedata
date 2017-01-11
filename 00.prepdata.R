################################################################################
# SUPPLEMENTAL MATERIAL of the article:
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#
# This code reproduces the analysis with the subset of data only including UK
#
# Update: 11 January 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata
################################################################################

################################################################################
# PREPARE THE DATA
################################################################################

# LOAD THE PACKAGES
library(dlnm) ; library(mvmeta) ; library(splines) ; library(tsModel)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATASET (INCLUDING THE 10 UK REGIONS ONLY)
regEngWales <- read.csv("regEngWales.csv",row.names=1)
regEngWales$date <- as.Date(regEngWales$date)

# ARRANGE THE DATA AS A LIST OF DATA SETS
regions <- as.character(unique(regEngWales$regnames))
dlist <- lapply(regions,function(x) regEngWales[regEngWales$regnames==x,])
names(dlist) <- regions

# METADATA FOR LOCATIONS
cities <- data.frame(
  city = regions,
  cityname = c("North East","North West","Yorkshire & Humber","East Midlands",
    "West Midlands","East","London","South East","South West","Wales")
)

# ORDER
ord <- order(cities$cityname)
dlist <- dlist[ord]
cities <- cities[ord,]

# REMOVE ORIGINALS
rm(regEngWales,regions,ord)

################################################################################

# SPECIFICATION OF THE EXPOSURE FUNCTION
varfun = "bs"
vardegree = 2
varper <- c(10,75,90)

# SPECIFICATION OF THE LAG FUNCTION
lag <- 21
lagnk <- 3

# DEGREE OF FREEDOM FOR SEASONALITY
dfseas <- 8

# COMPUTE PERCENTILES
per <- t(sapply(dlist,function(x) 
  quantile(x$tmean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))

# MODEL FORMULA
formula <- death~cb+dow+ns(date,df=dfseas*length(unique(year)))

#
