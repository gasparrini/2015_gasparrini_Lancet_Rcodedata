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
# TABLES
################################################################################

################################################################################
# RELATED PART OF TABLE 1

tmeanuk <- sapply(dlist,function(city) mean(city$tmean,na.rm=T))
c(Country="UK",
  Period=paste(range(dlist[[1]]$year),collapse="-"),Deaths=totdeathtot,
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
totdeath

# MINIMUM MORTALITY TEMPERATURE PERCENTILE AND ABSOLUTE TEMPERATURE
minperccity
mintempcity

# ATTRIBUTABLE FRACTION
afcity 

#
