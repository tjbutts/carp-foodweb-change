## Size Spectra Analysis - Dummy Dataset ## 
library(tidyverse)

n = 1000
b.known = -2
xmin.known = 1 
xmax.known = 1000
set.seed(42)

x = rPLB(n, 
         b = b.known, 
         xmin = xmin.known, 
         xmax = xmax.known)
hist(x)

num.bins = 8 # Suggested number for standard method; Daan et al. used 8 bins 

# Make the standard histogram with a break in the y-axis (first fit using the Llin method since that returns the rquired bin breaks, counts, etc.)
  # hAAA is the h(istogram) results for the method AAA 
  # Llin method = plotting binned data on log-linear axes then fitting regression 
hLlin = Llin.method(x, 
                    num.bins = num.bins)
gap.barplot.cust(hLlin$counts,
                 midpoints = hLlin$mids,
                 breakpoints = hLlin$breaks,
                 xlim = c(-10,max(hLlin$breaks)+10),
                 col = rep("grey", length(hLlin$counts)),
                 xaxs = "i",
                 yaxs = "i"
)

eight.results = eightMethodsMEE(x, num.bins = num.bins)
eight.methods.plot(eight.results)

MLE.plots.recommend(x = x,
                    b.MLE = eight.results$hMLE.list$b,
                    confVals.MLE = eight.results$hMLE.list$confVals,
                    hLBNbiom.list = eight.results$hLBNbiom.list)




## Calculate the number of log2 bins, taken from Edwards et al. 2017 - assign bins
log2bins_butts = function(x = NULL, counts = NULL)
{
  if(!is.null(x) & !is.null(counts)) {
    stop("need only one of x or counts in log2bins") }
  if(is.null(x) & is.null(counts)) {
    stop("need x or counts in log2bins") }
  if(!is.null(x)) {
    if(!is.vector(x))stop("x not a vector in log2bins")
    if(anyNA(x)) stop("x contains NA's in log2bins")
    if(min(x) <= 0)stop("x needs to be >0 in log2bins")
  }
  if(!is.null(counts))  {
    if(dim(counts)[2] != 2)stop("counts needs two cols in log2bins")
    if(min(counts[,1]) < 0) {
      stop("x values in counts need to be >= 0 in log2bins") }
    if(min(counts[,2]) < 0) {
      stop("numbers in counts need to be >= 0 in log2bins") }
  }
  # As for LBNbiom.method(), could write code that would make
  #  use of the counts dataframe explicitly, but actually quite easy
  #  to just create the longer vector x (though may be slightly slower
  #  computationally), to save writing extensive new code.
  if(is.null(x))
  {x = rep(counts[,1], counts[,2]) }
  #
  binBreaks = 2^( floor(log2(min(x))) : ceiling(log2(max(x))) )
  
  indiv = data.frame(x)       # dataframe with one row for each individual
  indiv$binMid =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-length(binBreaks)] + 0.5*diff(binBreaks))
  indiv$binMin =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-length(binBreaks)])
  indiv$binMax =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-1])
  # indiv$binWidth =cut(x, breaks=binBreaks, right=FALSE,
  #    include.lowest=TRUE, labels = diff(binBreaks))
  # indiv = mutate(indiv, binWidth = binMax - binMin)
  # Above commands avoid any problems with bins with 0 counts.
  # Don't really need all of them, but include for completeness.
  indiv$binMid = as.numeric(as.character(indiv$binMid))
  indiv$binMin = as.numeric(as.character(indiv$binMin))
  indiv$binMax = as.numeric(as.character(indiv$binMax))
  # Now calculate biomass in each bin class:
  binVals = dplyr::summarise(dplyr::group_by(indiv, binMid),
                             binMin = unique(binMin),
                             binMax = unique(binMax),
                             binWidth = binMax - binMin,
                             binCount = length(x),
                             binCountNorm = binCount / binWidth,
                             binSum = sum(x),
                             binSumNorm = binSum / binWidth )
  # binWidth uses new columns binMax and binMin
  binVals = binVals[order(binVals$binMid),]   # order by binMid
  #
  if(dim(indiv)[1] < 10^6) {       # only save indiv if not too big
    y = list(indiv = indiv, binVals = binVals)
  } else
  {
    y = list(binVals = binVals)
  }
  return(y)
}

# Rewrote the base function removing the log10 bit that was causing errors, only need the log2 part anyway
log2bins.list = log2bins_butts(df1$biomass)
df1$BINMID <- as.factor(log2bins.list$indiv$binMid)
df1$BINMIN <- log2bins.list$indiv$binMin
df1$BINMAX <- log2bins.list$indiv$binMax
df1
