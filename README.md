
### Updated R code and data from Gasparrini Lancet 2015

--------------------------------------------------------------------------------

An examples partly reproducing the results of an analysis of the excess risk attributable to non-optimal outdoot temperature in a multi-country dataset, published in:


Gasparrini A, Guo Y, Hashizume M, Lavigne E, Zanobetti A, Schwartz J, Tobias A, Tong S, Rocklöv J, Forsberg B, Leone M, De Sario M, Bell ML, Guo YLL, Wu CF, Kan H, Yi SM, de Sousa Zanotti Stagliorio Coelho M, Saldiva PH, Honda Y, Kim H, Armstrong B. Mortality risk attributable to high and low ambient temperature: a multicountry observational study. *The Lancet*. 2015;**386**(9991):369-375. [[freely available here](http://www.ag-myresearch.com/2015_gasparrini_lancet.html)]

--------------------------------------------------------------------------------

The code:

  * *regEngWales* stores the daily time series data from 10 locations corresponding to regions of England and Wales in the period 1993--2006
  * *attrdl.R* creates the function for computing the attributable risk measures
  * *attrdl.pdf* is the help page for the function attrdl
  * the numbered files from *00.prepdata.R* to *05.plots.R*, reproduce the results of an example with the subset of data
  
Download as a ZIP file using the green button *Clone or download* above