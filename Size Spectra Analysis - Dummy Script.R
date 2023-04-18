## Size Spectra Analysis - Dummy ## 

# Install sizeSpectra package from Edwards et al. 2017 # 
if (!require(devtools)) install.packages('devtools')
library(devtools)

install.packages('sizeSpectra 1.0.0.0.tar.gz', repos = NULL)
library(sizeSpectra)

library(tidyverse)

browseVignettes("sizeSpectra")

n = 1000
b.known = -2
xmin.known = 1
xmax.known = 1000
set.seed(42)

dummy = rPLB(n, 
             b = b.known, 
             xmin = xmin.known, 
             xmax = xmax.known)
dummy

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
log2bins.list = log2bins_butts(dummy)
dum = as.data.frame(dummy)
dum$BINMID <- as.factor(log2bins.list$indiv$binMid)
dum$BINMIN <- log2bins.list$indiv$binMin
dum$BINMAX <- log2bins.list$indiv$binMax
dum

#Something in the middle bins is creating high density 
stripchart(dummy~BINMID, data=dum, vertical = T)

dum.combined = dum %>% 
  group_by(BINMID) %>%
  summarize(density = n()) %>%
  ungroup()

dum.combined

dum.combined$BINMID = as.numeric(as.character(dum.combined$BINMID))
#BINMIN and BINMAX are included here but will not do anything, 
## but by including they will be carried through to the summary table 
## and will be usuable for relative density

dum.combined = as_tibble(dum.combined) # Save in a tidy format
dum.combined

# log 2 binning # 

dum.binned = dum.combined %>%
  group_by(BINMID) %>%
  mutate(BINMID_LOG = log2(BINMID),DENS_LOG = log2(density))
dum.binned

# Check if relationship is as expected # 
plot(DENS_LOG ~ BINMID_LOG, data = dum.binned)
mod = lm(DENS_LOG ~ BINMID_LOG, data = dum.binned)
abline(mod)
summary(mod)
