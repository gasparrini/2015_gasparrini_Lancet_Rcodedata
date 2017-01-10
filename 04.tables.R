################################################################################
# SUPPLEMENTAL MATERIAL of the article:
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#
# This code reproduces the analysis with the subset of data only including UK
#
# 10 January 2017
# * an updated version of this code, (hopefully) compatible with future
#   versions of the software, is available at the personal website of the
#   first author (www.ag-myresearch.com)
################################################################################

################################################################################
# TABLES
################################################################################

################################################################################
# RELATED PART OF TABLE 1

tmeanuk <- sapply(dlist,function(city) mean(city$tmean,na.rm=T))
c(Country="UK",
  Period=paste(range(dlist[[1]]$year),collapse="-"),
  Deaths=sum(sapply(dlist,function(x) sum(x$death,na.rm=T))),
  Temperature=paste0(formatC(mean(tmeanuk),dig=1,
    format="f")," (",paste(formatC(range(tmeanuk),dig=1,format="f"),
      collapse="-"),")"))
  
################################################################################
# RELATED PART OF TABLE 2

# MMP
minperccountry

# ATTRIBUTABLE FRACTION
t(cbind(aftot,aftotlow,aftothigh))

################################################################################
# RELATED PART OF TABLE S4

# DEATHS
sapply(dlist,function(x) sum(x$death,na.rm=T))

# MINIMUM MORTALITY TEMPERATURE PERCENTILE AND ABSOLUTE TEMPERATURE
minperccity
mintempcity

# ATTRIBUTABLE FRACTION
afcity 

#
