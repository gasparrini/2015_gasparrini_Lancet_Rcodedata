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
# PLOTS
################################################################################

################################################################################
# SIMILAR TO FIGURE 1

xlab <- expression(paste("Temperature (",degree,"C)"))

pdf("figure1.pdf",width=8,height=9)
layout(matrix(c(0,1,1,2,2,0,rep(3:8,each=2),0,9,9,10,10,0),ncol=6,byrow=T))
par(mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)

for(i in seq(length(dlist))) {
  data <- dlist[[i]]
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  argvar <- list(x=data$tmean,fun=varfun,degree=vardegree,
    knots=quantile(data$tmean,varper/100,na.rm=T))
  bvar <- do.call(onebasis,argvar)
  pred <- crosspred(bvar,coef=blup[[i]]$blup,vcov=blup[[i]]$vcov,
    model.link="log",by=0.1,cen=mintempcity[i])
  plot(pred,type="n",ylim=c(0,2.5),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
    main=cities$cityname[i])
  ind1 <- pred$predvar<=mintempcity[i]
  ind2 <- pred$predvar>=mintempcity[i]
  lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
  lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)
  mtext(cities$countryname[i],cex=0.7,line=0)
  #axis(1,at=-8:8*5)
  axis(2,at=1:5*0.5)
  breaks <- c(min(data$tmean,na.rm=T)-1,seq(pred$predvar[1],
    pred$predvar[length(pred$predvar)],length=30),max(data$tmean,na.rm=T)+1)
  hist <- hist(data$tmean,breaks=breaks,plot=F)
  hist$density <- hist$density/max(hist$density)*0.7
  prop <- max(hist$density)/max(hist$counts)
  counts <- pretty(hist$count,3)
  plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
    breaks=breaks,freq=F,add=T)
  axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
  #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
  abline(v=mintempcity[i],lty=3)
  abline(v=c(per[i,c("2.5%","97.5%")]),lty=2)
}

dev.off()

#
