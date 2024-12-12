##_MLEbin estimation of slope ##==============================

library(sizeSpectra)
library(tidyverse)

set.seed(42)
x <- rPLB(n = 1000,
          b = -2,
          xmin = 1,
          xmax = 1000)

x.binned <- binData(x,
                    binWidth = "2k")  # 1, 2, 5 or "2k"

head(x.binned$indiv)
x.binned$binVals

num.bins = nrow(x.binned$binVals)

# bin breaks are the minima plus the max of the final bin # 
binBreaks = c(dplyr::pull(x.binned$binVals, binMin), 
              dplyr::pull(x.binned$binVals, binMax)[num.bins])
binCounts = dplyr::pull(x.binned$binVals, binCount)

MLEbin.res = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1.5,
                      w = binBreaks, 
                      d = binCounts, 
                      J = length(binCounts), # = num.bins 
                      vecDiff = 1) # increase this if hit a bound 
# NA/Inf warnings are fine - likelihood function is blowing up in a very unlikely region of parameter space

# Plotting the data #===========================
ISD_bin_plot_nonoverlapping(binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.res$MLE, 
                            b.confMin = MLEbin.res$conf[1],
                            b.confMax = MLEbin.res$conf[2])

  # The y-axis is (a) lienar and (b) logarithmic. 
    # For each bin, the horizontal green line shows the range of body masses of that bin
        # this plotting method properly represents the discrete binned data on a continuous scale

# Plotting the data if stored as a tibble 
x.binned.tib = x.binned$binVals
x.binned.tib
ISD_bin_plot_nonoverlapping(binValsTibble = x.binned$binVals,
                            b.MLE = MLEbin.res$MLE, 
                            b.confMin = MLEbin.res$conf[1],
                            b.confMax = MLEbin.res$conf[2],
                            yBig.inc = 500)

# Test with Carp Lakes Data # 
silver18.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Silver' & year == 2018)
silver18.test # now extract the 'binned' data 

bincharac.test = silver18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

silver18.trim = silver18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, silver18.trim, by = 'BINMID')
join1

silver18.prepped = join1[!duplicated(join1), ]
silver18.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(silver18.prepped)
binBreaks = c(dplyr::pull(silver18.prepped, BINMIN), 
              dplyr::pull(silver18.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(silver18.prepped, binCount)

MLEbin.silver18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1.5, 
                      w = binBreaks, 
                      d = binCounts, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1) # increase this if hit a bound 


ISD_bin_plot_nonoverlapping(binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.silver18$MLE,
                            b.confMin = MLEbin.silver18$conf[1],
                            b.confMax = MLEbin.silver18$conf[2], 
                            yBig.max = 328)

# Try doing your normal size spectra binning method but targeted towards Silver 18 see if it looks better #
# Silver18 binning #==============================

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
#browseVignettes("sizeSpectra")


# Load in SSA dataset #===========================
output = read_csv('SSA start_zoop.miv-biomass.csv')
output

# Replace lake number with lake name #
levels(output$lake) = c(levels(output$lake), 'Blue', 'Center', 
                         'Five Island', 'North Twin', 
                         'Silver', 'Storm', 'South Twin')

output$lake[output$lake == '12'] <- 'Blue'
output$lake[output$lake == '19'] <- 'Center' 
output$lake[output$lake == '36'] <- 'Five Island' 
output$lake[output$lake == '90'] <- 'North Twin'
output$lake[output$lake == '105'] <- 'Silver'
output$lake[output$lake == '113'] <- 'Storm'
output$lake[output$lake == '406'] <- 'South Twin'
output

# Remove Blue Lake - geologically distinct, almost no BMB # 
output = filter(output, lake != 'Blue')
output

# filter to Silver Lake 2018 # 
output.si18 = output %>%
  filter(lake == 'Silver' & year == 2018)
output.si18

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
log2bins.list = log2bins_butts(output.si18$biomass_g)
output.si18$BINMID <- as.factor(log2bins.list$indiv$binMid)
output.si18$BINMIN <- log2bins.list$indiv$binMin
output.si18$BINMAX <- log2bins.list$indiv$binMax
output.si18

silver18.test = output.si18 # output from Step 2, Line 99
silver18.test # now extract the 'binned' data 

bincharac.test = silver18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

silver18.trim = silver18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, silver18.trim, by = 'BINMID')
join1

silver18.prepped = join1[!duplicated(join1), ]
silver18.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(silver18.prepped)
binBreaks = c(dplyr::pull(silver18.prepped, BINMIN), 
              dplyr::pull(silver18.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(silver18.prepped, binCount)

MLEbin.silver18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1.5, 
                      w = binBreaks, 
                      d = binCounts, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1) # increase this if hit a bound 


ISD_bin_plot_nonoverlapping(yBig.inc =100000,
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.silver18$MLE,
                            b.confMin = MLEbin.silver18$conf[1],
                            b.confMax = MLEbin.silver18$conf[2])


# okay absolutely no difference...interesting 

# Now let's pick a different lake and see what pulls up # 
# Test with Carp Lakes Data # 
silver19.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Silver' & year == 2019)
silver19.test # now extract the 'binned' data 

bincharac.test = silver19.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

silver19.trim = silver19.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, silver19.trim, by = 'BINMID')
join1

silver19.prepped = join1[!duplicated(join1), ]
silver19.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(silver19.prepped)
binBreaks = c(dplyr::pull(silver19.prepped, BINMIN), 
              dplyr::pull(silver19.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(silver19.prepped, binCount)

MLEbin.silver19 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1.5, 
                      w = binBreaks, 
                      d = binCounts, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1) # increase this if hit a bound 


ISD_bin_plot_nonoverlapping(yBig.inc =10000,
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.silver19$MLE,
                            b.confMin = MLEbin.silver19$conf[1],
                            b.confMax = MLEbin.silver19$conf[2], 
                            yBig.max = 328)


