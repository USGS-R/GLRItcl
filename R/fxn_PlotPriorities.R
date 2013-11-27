#' Function to help prioritization of sites based on available data
#' 
#' Combination boxplot and points graphing function. This orders the boxplots and points first by the number of data points per site and then by the mean for each site.
#'
#' @param df Dataframe with observations and conditioning variable
#' @param parm Parameter in df to be plotted
#' @param xLabelParm Parameter in df to be used as a conditioning variable for graphing
#' @param statParm Summary statistic to be used to order xLabelParm for the x-axis. Usually this is the montiroing site short name
#' @param countThresh Threshold number of observations for determining high-count compared to low-count sites
#' @param concThresh Threshold concentration for prioritization and coloring x-axis labels
#' @param plotTitle Main title of the plot
#' @param ylab y-axis label
#' @param subsetVar If df is to be subset, this is the column name to be used. Commonly a character or factor
#' @param subsetValue The value of subsetVar used for subsetting df
#' @param logy Boolean variable to designate log y-axis. TRUE if log, or FALSE if linear (the default).
#' @param addY Value to add to parm values before plotting or "min10" to find the minimum nonzero value, divide by 10, and add to the parm values. This is useful to plot on log scale when there are zero values.
#' @keywords prioritize ordered boxplot counts
#' @return NULL
#' @export
#' @examples
#' x<-1
PlotPriorities <- function(df,parm,xLabelParm,statParm,countThresh,concThresh,plotTitle,ylab,subsetVar=NA,subsetValue=NA,logy=FALSE,addY=0){
  options(scipen=10) #avoid scientific notation for y axis
  
  ############################
  # Define internal functions
  ############################
  
  count <- function(x) sum(!is.na(x)) #Count valid observations
  
  drop.unused.factors <- function(factordf) {
    ncols <- length(names(factordf))
    for (i in 1:ncols) {
      if(class(factordf[,i])[1]=="factor")
        factordf[,i] <- as.factor(as.character(factordf[,i]))
    }
    return(factordf)
  }
  
  # End function definitions
  ##########################
  logyCh <- ifelse(logy,"y","") 
  if(!is.na(subsetVar)) df <- subset(df,df[,subsetVar]==subsetValue)
  if(addY=="min10") addY <- min(df[which(df[,parm]>0),parm])/10
  if(min(df[,parm],na.rm=TRUE)==0){
  df[which(df[,parm]==0),parm] <- addY
  }
  
  countParm <- "count"
  
  df[,xLabelParm] <- as.factor(as.character(df[,xLabelParm]))
  df <- df[which(!is.na(df[,parm])),]
  df <- drop.unused.factors(df)
  
  meanTotal <- aggregate(df[,parm],by=list(df[,xLabelParm]),FUN=mean,na.rm=TRUE)
  countTotal<- aggregate(df[,parm],by=list(df[,xLabelParm]),FUN=count)
  
  names(meanTotal) <- c(xLabelParm,statParm)
  names(countTotal) <- c(xLabelParm,countParm)
  dfSummary <- merge(meanTotal,countTotal)
  plotColors <- c("blue","black")
#  plotColors <- c("brown4",plotColors)
  
  dfSummary <- dfSummary[order(dfSummary[,countParm] > countThresh,dfSummary[,statParm],decreasing=TRUE),]
  dfSummary[,xLabelParm]<- factor(dfSummary[,xLabelParm],levels=dfSummary[,xLabelParm])

  df2 <- df
  df2[,xLabelParm] <- "All"
  df2 <- rbind(df,df2)
  df2[,xLabelParm] <- factor(df2[,xLabelParm],levels=c("All",as.character(dfSummary[,xLabelParm])))
  
  #df[,xLabelParm] <- factor(df[,xLabelParm],levels=dfSummary[,xLabelParm])
  highSites <- as.character(dfSummary[dfSummary[,countParm]> countThresh,xLabelParm])
  highSamples <- which(as.character(df2[,xLabelParm]) %in% highSites)
  dfPlot <- df[highSamples,]
  dfPlot <- subset(df2,df2[,xLabelParm]=="All")
  
  par(mar=c(8,5,4,2),oma=c(0,0,0,0))
  bp <- boxplot(as.numeric(dfPlot[,parm])~dfPlot[,xLabelParm],las=2,xaxt="n",yaxt="n",cex.axis=0.65,log=logyCh,col="brown4",pch=20)
  dfPlot <- df2[highSamples,]
  par(new=TRUE)
  if(dim(dfPlot)[1]>1){
  bp <- boxplot(as.numeric(dfPlot[,parm])~dfPlot[,xLabelParm],las=2,xaxt="n",yaxt="n",cex.axis=0.65,log=logyCh,col="lightgoldenrod3",pch=20)
  #  bp <- boxplot(as.numeric(dfPlot[,parm])~dfPlot[,"All"],las=2,xaxt="n",cex.axis=0.65,log=logyCh,col="lightgoldenrod3",pch=20)
  }
  numSites <- length(levels(dfPlot[,xLabelParm]))
  tickLocs <- seq(1,numSites,5)
  axis(side=3,at=tickLocs,tck=0.02,labels=rep("",length(tickLocs)))
  tickLocs2 <- seq(1,numSites)
  axis(side=3,at=tickLocs2,tck=0.01,labels=rep("",length(tickLocs2)))
  tickLocs <- seq(1,numSites,5)
  axis(side=1,at=tickLocs,tck=0.02,labels=rep("",length(tickLocs)))
  tickLocs2 <- which(dfSummary[,statParm] > concThresh)
  axis(side=1,at=(tickLocs2+1),tck=0.01,labels=dfSummary[tickLocs2,xLabelParm],las=2,cex.axis=0.65, mgp=c(0,1.3,0),col.axis=plotColors[1])
  tickLocs2 <- which(dfSummary[,statParm] <= concThresh)
  axis(side=1,at=(tickLocs2+1),tck=0.01,labels=dfSummary[tickLocs2,xLabelParm],las=2,cex.axis=0.65, mgp=c(0,1.3,0),col.axis=plotColors[2])
  axis(side=1,at=(1),tck=0.01,labels="All",las=2,cex.axis=0.7, mgp=c(0,1.8,0),col.axis="Brown4")
  
  ytickLocs <- axisTicks(par("usr")[3:4],log=logy,nint=8)
  ytickLabels <- prettyNum(ytickLocs)
  if(!is.character(addY) & addY > 0){ ytickLocs <- c(addY,ytickLocs[-1])
                                      ytickLabels <- c("ND",ytickLabels[-1])
  }
  axis(side=2,at=ytickLocs,tck=0.01,labels=ytickLabels,las=2,cex.axis=0.65, mgp=c(0,1.1,0),col.axis=plotColors[2])
  
  nObsSite <- c(sum(dfSummary[,countParm]),dfSummary[,countParm])
  mtext(nObsSite, at = seq_along(bp$n), line = 0.2, side = 1,cex=0.6,las=2)
  mtext("n = ",side=1,at=-1,line=0.2,cex=0.6)
  
  if(length(highSamples)>0){
    dfPlot <- df2[-highSamples,]
    abline(v=length(highSites)+1.5,lty=2,col="orange")
  }else dfPlot <- df2
  dfPlot <- subset(dfPlot,dfPlot[,xLabelParm] != "All")
  points(as.numeric(dfPlot[,parm])~dfPlot[,xLabelParm],cex=1,pch=20,col="blue")
  for (i in tickLocs) abline(v=tickLocs,lty=4,col="grey")
  tickLocs2 <- seq(1,numSites)
  for (i in tickLocs2) abline(v=tickLocs2,lty=3,col="lightgrey")
  mtext(side=3,plotTitle,line=1.3,font=2)
  mtext(side=2,ylab,line=3,font=2)
  abline(h=concThresh,lty=2,col="springgreen4")
  
}

