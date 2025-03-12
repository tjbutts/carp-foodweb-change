## Step 7_MLEbin estimation of slope ##==============================

# Must install sizeSpectra package from GitHub remotes; uncomment code below 
  # intsall.packages("remotes")  # If you do not already have the "remotes" package
  # remotes::install_github("andrew-edwards/sizeSpectra")

# If you get errors try 
  # remotes::install_github("andrew-edwards/sizeSpectra", build_vignettes = FALSE)

# For more information on the package:  https://github.com/andrew-edwards/sizeSpectra 
  # Package is detailed in the publication cited below: 
    # Edwards, A.M. (2020). sizeSpectra: R package for fitting size spectra to ecological data (including binned data).
library(sizeSpectra)
library(tidyverse)

# Create the 'output' object to be used for the MLE estimation 
output = get_data("edi.1926.1", filenum = 2)

# Replace lake number with lake name #
levels(output$lake) = c(levels(output$lake),  'Center', 
                         'Five Island', 'North Twin', 
                         'Silver', 'Storm', 'South Twin')

output$lake[output$lake == '19'] <- 'Center' 
output$lake[output$lake == '36'] <- 'Five Island' 
output$lake[output$lake == '90'] <- 'North Twin'
output$lake[output$lake == '105'] <- 'Silver'
output$lake[output$lake == '113'] <- 'Storm'
output$lake[output$lake == '406'] <- 'South Twin'
output


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
log2bins.list = log2bins_butts(output$biomass_g)
output$BINMID <- as.factor(log2bins.list$indiv$binMid)
output$BINMIN <- log2bins.list$indiv$binMin
output$BINMAX <- log2bins.list$indiv$binMax
output

# Adjust sizeSpectra functions for plotting purposes ==============================

# ISD bin Plot adjusted code #

ISD_bin_plot.adj <- function(data.year,
                         b.MLE,
                         b.confMin,
                         b.confMax,
                         year = NA,
                         xlim = NA,
                         xmin = NA,
                         xmax = NA,
                         yScaling = 0.75,
                         MLE.round = 2,
                         xLabel.small = c(5, 50, 500, 5000),
                         yBig.inc = 1000,
                         yBig.max = 10,
                         ySmall.inc = NA,
                         ySmall.tcl = -0.2,
                         xLab = expression(paste("Body mass (", italic(x), "), g")),
                         yLab = expression(paste("Number of ", values >= italic(x))),
                         # inset.a = c(0, 0),
                         # inset.year = c(0, 0.04),
                         seg.col = "green",
                         rect.col = "grey",
                         fit.col = "red",
                         fit.lwd = 2,
                         conf.lty = 2,
                         # par.mai = c(0.4, 0.5, 0.05, 0.3),
                         par.cex = 0.7,
                         mgp.vals = c(1.6,0.5,0),
                         IBTS_MEPS_figs = FALSE,
                         x.PLB = NA
                         )
  {
  sumNumber = sum(data.year$Number)

  par(cex = par.cex)  # Affects all figures

  if(is.na(ySmall.inc)){
    ySmall.inc = yBig.inc/4
  }

  if(is.na(xlim[1])){
    xlim = c(min(data.year$wmin),
             max(data.year$wmax))  # Range of axis
  }

  if(is.na(xmin)){
    xmin = min(data.year$wmin)
  }

  if(is.na(xmax)){
    xmax = max(data.year$wmax)
  }

  # x values to plot PLB if not provided; need high resolution for both plots.
  if(is.na(x.PLB)){
    #  First option is just to keep the exact original code used in MEPS
    #  figures,
    #  second option is probably more generally useful (for example, when using
    #  very small size classes like for zooplankton data)
    ifelse((IBTS_MEPS_figs),
           x.PLB <- seq(xmin,
                        xmax,
                        length=10000),
           x.PLB <- exp(seq(log(xmin),
                            log(xmax),
                            length = 10000))
           )

    #  Need to insert value close to xmax to make log-log curve go down further;
    #   since log(1 - pPLB(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
    x.PLB.length = length(x.PLB)
    x.PLB = c(x.PLB[-x.PLB.length],
              0.9999999999 * x.PLB[x.PLB.length],
              x.PLB[x.PLB.length])
  }

  y.PLB = (1 - pPLB(x = x.PLB,
                    b = b.MLE,
                    xmin = min(x.PLB),
                    xmax = max(x.PLB))) * sumNumber
  # To add curves for the limits of the 95% confidence interval of b:
  y.PLB.confMin = (1 - pPLB(x = x.PLB,
                   b = b.confMin,
                   xmin = min(x.PLB),
                   xmax = max(x.PLB))) * sumNumber
  y.PLB.confMax = (1 - pPLB(x = x.PLB,
                   b = b.confMax,
                   xmin = min(x.PLB),
                   xmax = max(x.PLB))) * sumNumber

  # yRange = c(min(data.year$lowCount), max(data.year$highCount))
  # The above does not work because first val is 0 which is not permissable on
  #  log axis. Which also means that the rectangle that goes to 0 has to be
  #  added manually (below). Picking the y-axis to go down to 0.75 of the
  #  minimum value of CountGTEwmin.
  yRange = c(yScaling * min(data.year$countGTEwmin), max(data.year$highCount))

  # y-axis logged
  # empty plot:
  plot(data.year$wmin,
       data.year$countGTEwmin,
       log = "xy",
       xlab = xLab,
       ylab = yLab,
       xlim = xlim,
       ylim = yRange,
       type = "n",
       axes = FALSE,
       mgp = mgp.vals)

  xLim = 10^par("usr")[1:2]
  yLim = 10^par("usr")[3:4]

  logTicks(xLim,
           yLim,
           xLabelSmall = xLabel.small)

  rect(xleft = data.year$wmin,
       ybottom = data.year$lowCount,
       xright = data.year$wmax,
       ytop = data.year$highCount,
       col = rect.col)

  # Need to manually draw the rectangle with lowCount = 0 since it doesn't
  #  get plotted on log-log plot
  extra.rect = dplyr::filter(data.year,
                             lowCount == 0)
  # if(nrow(extra.rect) > 1) stop("Check rows of extra rect.")
  rect(xleft = extra.rect$wmin,
       ybottom = rep(0.01 * yRange[1], nrow(extra.rect)),
       xright = extra.rect$wmax,
       ytop = extra.rect$highCount,
       col = rect.col)

  segments(x0 = data.year$wmin,
           y0 = data.year$countGTEwmin,
           x1 = data.year$wmax,
           y1 = data.year$countGTEwmin,
           col = seg.col)

  lines(x.PLB,
        y.PLB,
        col = fit.col,
        lwd = fit.lwd)
  lines(x.PLB,
        y.PLB.confMin,
        col = fit.col,
        lty = conf.lty)
  lines(x.PLB,
        y.PLB.confMax,
        col = fit.col,
        lty = conf.lty)
  # legend("topright",
  #        "(b)",
  #        bty="n",
  #        inset = inset.a)
  box()       # to redraw axes over any boxes
}

ISD_bin_plot_nonoverlapping.adj <- function(binValsTibble = NULL,
                                        binBreaks = NULL,
                                        binCounts = NULL,
                                        b.MLE,
                                        b.confMin,
                                        b.confMax,
                                        ...){
  stopifnot(
    "Need binValsTibble OR both binBreaks and binCounts to be NULL" =
    (!is.null(binValsTibble) & is.null(binBreaks) & is.null(binCounts)) |
    (is.null(binValsTibble) & !is.null(binBreaks) & !is.null(binCounts))
  )

  # Create a tibble with the desired columns, to go into ISD_bin_plot:
  ifelse(!is.null(binValsTibble),
    # Adapt the existing tibble into the required form:
    ifelse("binMin" %in% names(binValsTibble),
      binTibble <- dplyr::select(binValsTibble,
                                 wmin = binMin,
                                 wmax = binMax,
                                 Number = binCount),
      binTibble <- dplyr::select(binValsTibble,
                                 wmin,
                                 wmax,
                                 Number = binCount)),
    # Else no tibble, so create one from the vectors binBreaks and binCounts:
    binTibble <- dplyr::tibble(wmin = binBreaks[-length(binBreaks)],
                               wmax = binBreaks[-1],
                               Number = binCounts))

    # Have to do not with dplyr:
    wmin.vec <- binTibble$wmin
    wmax.vec <- binTibble$wmax
    num.vec <- binTibble$Number

    # Here, highCount and countGWEwmin will be the same since only one set of
    # bin breaks; but do them both to save adapting ISD_bin_plot()
    countGTEwmin <- rep(NA, length(num.vec)) # to do a manual count
    lowCount <- countGTEwmin
    highCount <- countGTEwmin

    for(iii in 1:length(countGTEwmin))
    {
      countGTEwmin[iii] <- sum( (wmin.vec >= wmin.vec[iii]) * num.vec)
      lowCount[iii] <-     sum( (wmin.vec >= wmax.vec[iii]) * num.vec)
      highCount[iii] <-    sum( (wmax.vec >  wmin.vec[iii]) * num.vec)
    }

# When having working, check if can just to mutate here; think can: - can't,
#  but there is another command
    binTibble <- cbind(binTibble,
                      "countGTEwmin" = countGTEwmin,
                      "lowCount" = lowCount,
                      "highCount" = highCount)
    binTibble <- tibble::as_tibble(binTibble) # This is one of the desired input for
                                         #  the plotting function below
  ISD_bin_plot.adj(data.year = binTibble,
               b.MLE = b.MLE,
               b.confMin = b.confMin,
               b.confMax = b.confMax,
               ...)
}


### Storm Lake ##====================

#### 2018 ##=======================
# Test with Carp Lakes Data # 
storm18.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Storm' & year == 2018)
storm18.test # now extract the 'binned' data 

bincharac.test = storm18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

storm18.trim = storm18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, storm18.trim, by = 'BINMID')
join1

storm18.prepped = join1[!duplicated(join1), ]
storm18.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(storm18.prepped)
binBreaks = c(dplyr::pull(storm18.prepped, BINMIN), 
              dplyr::pull(storm18.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(storm18.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.storm18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)
windows(height = 10, width = 6)
par(mfrow = c(6, 3), mar = c(0.5,.5,1,1), oma = c(4,4,.5,.5))
ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.storm18$MLE,
                            b.confMin = MLEbin.storm18$conf[1],
                            b.confMax = MLEbin.storm18$conf[2])
mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
mtext(side = 2, line = 3, 'Storm')
mtext(side = 3, line = 0, '2018')

storm18.slope = MLEbin.storm18$MLE
storm18.conf = MLEbin.storm18$conf

#### 2019 ##=======================
# Test with Carp Lakes Data # 
storm19.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Storm' & year == 2019)
storm19.test # now extract the 'binned' data 

bincharac.test = storm19.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

storm19.trim = storm19.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, storm19.trim, by = 'BINMID')
join1

storm19.prepped = join1[!duplicated(join1), ]
storm19.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(storm19.prepped)
binBreaks = c(dplyr::pull(storm19.prepped, BINMIN), 
              dplyr::pull(storm19.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(storm19.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.storm19 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.storm19$MLE,
                            b.confMin = MLEbin.storm19$conf[1],
                            b.confMax = MLEbin.storm19$conf[2])

storm19.slope = MLEbin.storm19$MLE
storm19.conf = MLEbin.storm19$conf
# mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
# mtext(side = 2, line = 3, 'Storm')
mtext(side = 3, line = 0, '2019')

#### 2020 ##=======================
# Test with Carp Lakes Data # 
storm20.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Storm' & year == 2020)
storm20.test # now extract the 'binned' data 

bincharac.test = storm20.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

storm20.trim = storm20.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, storm20.trim, by = 'BINMID')
join1

storm20.prepped = join1[!duplicated(join1), ]
storm20.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(storm20.prepped)
binBreaks = c(dplyr::pull(storm20.prepped, BINMIN), 
              dplyr::pull(storm20.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(storm20.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.storm20 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.storm20$MLE,
                            b.confMin = MLEbin.storm20$conf[1],
                            b.confMax = MLEbin.storm20$conf[2])
# mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
# mtext(side = 2, line = 3, 'Storm')
mtext(side = 3, line = 0, '2020')

storm20.slope = MLEbin.storm20$MLE
storm20.conf = MLEbin.storm20$conf

### South Twin Lake ##========================
#### 2018 ##=======================
# Test with Carp Lakes Data # 
south.twin18.test = output %>% # output from Step 2, Line 99
  filter(lake == 'South Twin' & year == 2018)
south.twin18.test # now extract the 'binned' data 

bincharac.test = south.twin18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

south.twin18.trim = south.twin18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, south.twin18.trim, by = 'BINMID')
join1

south.twin18.prepped = join1[!duplicated(join1), ]
south.twin18.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(south.twin18.prepped)
binBreaks = c(dplyr::pull(south.twin18.prepped, BINMIN), 
              dplyr::pull(south.twin18.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(south.twin18.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.south.twin18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.south.twin18$MLE,
                            b.confMin = MLEbin.south.twin18$conf[1],
                            b.confMax = MLEbin.south.twin18$conf[2])
mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
mtext(side = 2, line = 3, 'South Twin')
#mtext(side = 3, line = 0, '2018')

south.twin18.slope = MLEbin.south.twin18$MLE
south.twin18.conf = MLEbin.south.twin18$conf

#### 2019 ##=======================
# Test with Carp Lakes Data # 
south.twin19.test = output %>% # output from Step 2, Line 99
  filter(lake == 'South Twin' & year == 2019)
south.twin19.test # now extract the 'binned' data 

bincharac.test = south.twin19.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

south.twin19.trim = south.twin19.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, south.twin19.trim, by = 'BINMID')
join1

south.twin19.prepped = join1[!duplicated(join1), ]
south.twin19.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(south.twin19.prepped)
binBreaks = c(dplyr::pull(south.twin19.prepped, BINMIN), 
              dplyr::pull(south.twin19.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(south.twin19.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.south.twin19 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.south.twin19$MLE,
                            b.confMin = MLEbin.south.twin19$conf[1],
                            b.confMax = MLEbin.south.twin19$conf[2])
#mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
#mtext(side = 2, line = 3, 'Storm')
#mtext(side = 3, line = 0, '2018')

south.twin19.slope = MLEbin.south.twin19$MLE
south.twin19.conf = MLEbin.south.twin19$conf

#### 2020 ##=======================
# Test with Carp Lakes Data # 
south.twin20.test = output %>% # output from Step 2, Line 99
  filter(lake == 'South Twin' & year == 2020)
south.twin20.test # now extract the 'binned' data 

bincharac.test = south.twin20.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

south.twin20.trim = south.twin20.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, south.twin20.trim, by = 'BINMID')
join1

south.twin20.prepped = join1[!duplicated(join1), ]
south.twin20.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(south.twin20.prepped)
binBreaks = c(dplyr::pull(south.twin20.prepped, BINMIN), 
              dplyr::pull(south.twin20.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(south.twin20.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.south.twin20 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)

ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.south.twin20$MLE,
                            b.confMin = MLEbin.south.twin20$conf[1],
                            b.confMax = MLEbin.south.twin20$conf[2])

south.twin20.slope = MLEbin.south.twin20$MLE
south.twin20.conf = MLEbin.south.twin20$conf

### Center Lake ##=====================
#### 2018 ##=======================
# Test with Carp Lakes Data # 
center18.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Center' & year == 2018)
center18.test # now extract the 'binned' data 

bincharac.test = center18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

center18.trim = center18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, center18.trim, by = 'BINMID')
join1

center18.prepped = join1[!duplicated(join1), ]
center18.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(center18.prepped)
binBreaks = c(dplyr::pull(center18.prepped, BINMIN), 
              dplyr::pull(center18.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(center18.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.center18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.center18$MLE,
                            b.confMin = MLEbin.center18$conf[1],
                            b.confMax = MLEbin.center18$conf[2])
mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
mtext(side = 2, line = 3, 'Center')
#mtext(side = 3, line = 0, '2018')

center18.slope = MLEbin.center18$MLE
center18.conf = MLEbin.center18$conf

#### 2019 ##=======================
# Test with Carp Lakes Data # 
center19.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Center' & year == 2019)
center19.test # now extract the 'binned' data 

bincharac.test = center19.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

center19.trim = center19.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, center19.trim, by = 'BINMID')
join1

center19.prepped = join1[!duplicated(join1), ]
center19.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(center19.prepped)
binBreaks = c(dplyr::pull(center19.prepped, BINMIN), 
              dplyr::pull(center19.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(center19.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.center19 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.center19$MLE,
                            b.confMin = MLEbin.center19$conf[1],
                            b.confMax = MLEbin.center19$conf[2])

center19.slope = MLEbin.center19$MLE
center19.conf = MLEbin.center19$conf

#### 2020 ##=======================
# Test with Carp Lakes Data # 
center20.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Center' & year == 2020)
center20.test # now extract the 'binned' data 

bincharac.test = center20.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

center20.trim = center20.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, center20.trim, by = 'BINMID')
join1

center20.prepped = join1[!duplicated(join1), ]
center20.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(center20.prepped)
binBreaks = c(dplyr::pull(center20.prepped, BINMIN), 
              dplyr::pull(center20.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(center20.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.center20 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 2, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.center20$MLE,
                            b.confMin = MLEbin.center20$conf[1],
                            b.confMax = MLEbin.center20$conf[2])

center20.slope = MLEbin.center20$MLE
center20.conf = MLEbin.center20$conf


### Five Island ##========================
#### 2018 ##=======================
# Test with Carp Lakes Data # 
five.island18.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Five Island' & year == 2018)
five.island18.test # now extract the 'binned' data 

bincharac.test = five.island18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

five.island18.trim = five.island18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, five.island18.trim, by = 'BINMID')
join1

five.island18.prepped = join1[!duplicated(join1), ]
five.island18.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(five.island18.prepped)
binBreaks = c(dplyr::pull(five.island18.prepped, BINMIN), 
              dplyr::pull(five.island18.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(five.island18.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.five.island18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)

ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.five.island18$MLE,
                            b.confMin = MLEbin.five.island18$conf[1],
                            b.confMax = MLEbin.five.island18$conf[2])
mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
mtext(side = 2, line = 3, 'Five Island')
#mtext(side = 3, line = 0, '2018')

five.island18.slope = MLEbin.five.island18$MLE
five.island18.conf = MLEbin.five.island18$conf

#### 2019 ##=======================
# Test with Carp Lakes Data # 
five.island19.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Five Island' & year == 2019)
five.island19.test # now extract the 'binned' data 

bincharac.test = five.island19.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

five.island19.trim = five.island19.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, five.island19.trim, by = 'BINMID')
join1

five.island19.prepped = join1[!duplicated(join1), ]
five.island19.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(five.island19.prepped)
binBreaks = c(dplyr::pull(five.island19.prepped, BINMIN), 
              dplyr::pull(five.island19.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(five.island19.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.five.island19 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.five.island19$MLE,
                            b.confMin = MLEbin.five.island19$conf[1],
                            b.confMax = MLEbin.five.island19$conf[2])

five.island19.slope = MLEbin.five.island19$MLE
five.island19.conf = MLEbin.five.island19$conf

#### 2020 ##=======================
# Test with Carp Lakes Data # 
five.island20.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Five Island' & year == 2020)
five.island20.test # now extract the 'binned' data 

bincharac.test = five.island20.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

five.island20.trim = five.island20.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, five.island20.trim, by = 'BINMID')
join1

five.island20.prepped = join1[!duplicated(join1), ]
five.island20.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(five.island20.prepped)
binBreaks = c(dplyr::pull(five.island20.prepped, BINMIN), 
              dplyr::pull(five.island20.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(five.island20.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.five.island20 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.five.island20$MLE,
                            b.confMin = MLEbin.five.island20$conf[1],
                            b.confMax = MLEbin.five.island20$conf[2])

five.island20.slope = MLEbin.five.island20$MLE
five.island20.conf = MLEbin.five.island20$conf

### North Twin Lake ##=========================
#### 2018 ##=======================
# Test with Carp Lakes Data # 
north.twin18.test = output %>% # output from Step 2, Line 99
  filter(lake == 'North Twin' & year == 2018)
north.twin18.test # now extract the 'binned' data 

bincharac.test = north.twin18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

north.twin18.trim = north.twin18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, north.twin18.trim, by = 'BINMID')
join1

north.twin18.prepped = join1[!duplicated(join1), ]
north.twin18.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(north.twin18.prepped)
binBreaks = c(dplyr::pull(north.twin18.prepped, BINMIN), 
              dplyr::pull(north.twin18.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(north.twin18.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.north.twin18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 2, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.north.twin18$MLE,
                            b.confMin = MLEbin.north.twin18$conf[1],
                            b.confMax = MLEbin.north.twin18$conf[2])
mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
mtext(side = 2, line = 3, 'North Twin')
#mtext(side = 3, line = 0, '2018')

north.twin18.slope = MLEbin.north.twin18$MLE
north.twin18.conf = MLEbin.north.twin18$conf

#### 2019 ##=======================
# Test with Carp Lakes Data # 
north.twin19.test = output %>% # output from Step 2, Line 99
  filter(lake == 'North Twin' & year == 2019)
north.twin19.test # now extract the 'binned' data 

bincharac.test = north.twin19.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

north.twin19.trim = north.twin19.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, north.twin19.trim, by = 'BINMID')
join1

north.twin19.prepped = join1[!duplicated(join1), ]
north.twin19.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(north.twin19.prepped)
binBreaks = c(dplyr::pull(north.twin19.prepped, BINMIN), 
              dplyr::pull(north.twin19.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(north.twin19.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.north.twin19 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.north.twin19$MLE,
                            b.confMin = MLEbin.north.twin19$conf[1],
                            b.confMax = MLEbin.north.twin19$conf[2])

north.twin19.slope = MLEbin.north.twin19$MLE
north.twin19.conf = MLEbin.north.twin19$conf

#### 2020 ##=======================
# Test with Carp Lakes Data # 
north.twin20.test = output %>% # output from Step 2, Line 99
  filter(lake == 'North Twin' & year == 2020)
north.twin20.test # now extract the 'binned' data 

bincharac.test = north.twin20.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

north.twin20.trim = north.twin20.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, north.twin20.trim, by = 'BINMID')
join1

north.twin20.prepped = join1[!duplicated(join1), ]
north.twin20.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(north.twin20.prepped)
binBreaks = c(dplyr::pull(north.twin20.prepped, BINMIN), 
              dplyr::pull(north.twin20.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(north.twin20.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.north.twin20 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.north.twin20$MLE,
                            b.confMin = MLEbin.north.twin20$conf[1],
                            b.confMax = MLEbin.north.twin20$conf[2])

north.twin20.slope = MLEbin.north.twin20$MLE
north.twin20.conf = MLEbin.north.twin20$conf

### Silver Lake ##==============================
#### 2018 ##=======================
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

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.silver18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)

ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.silver18$MLE,
                            b.confMin = MLEbin.silver18$conf[1],
                            b.confMax = MLEbin.silver18$conf[2])
mtext(side = 2, line = 1.8, expression(paste("Number of ", values >= italic(x))), cex = 0.7)
mtext(side = 2, line = 3, 'Silver')
#mtext(side = 3, line = 0, '2018')
mtext(side = 1, line = 1.8, expression(paste("Body mass (", italic(x), "), g")), cex = 0.7)

silver18.slope = MLEbin.silver18$MLE
silver18.conf = MLEbin.silver18$conf

#### 2019 ##=======================
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

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.silver19 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 1, 
                      vecInc = 0.01) # increase this if hit a bound 
# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.silver19$MLE,
                            b.confMin = MLEbin.silver19$conf[1],
                            b.confMax = MLEbin.silver19$conf[2])
mtext(side = 1, line = 1.8, expression(paste("Body mass (", italic(x), "), g")), cex = 0.7)

silver19.slope = MLEbin.silver19$MLE
silver19.conf = MLEbin.silver19$conf

#### 2020 ##=======================
# Test with Carp Lakes Data # 
silver20.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Silver' & year == 2020)
silver20.test # now extract the 'binned' data 

bincharac.test = silver20.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

silver20.trim = silver20.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, silver20.trim, by = 'BINMID')
join1

silver20.prepped = join1[!duplicated(join1), ]
silver20.prepped

# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(silver20.prepped)
binBreaks = c(dplyr::pull(silver20.prepped, BINMIN), 
              dplyr::pull(silver20.prepped, BINMAX)[num.bins])
binCounts = dplyr::pull(silver20.prepped, binCount)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.silver20 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1, 
                      w = binBreaks_scaled, 
                      d = binCounts_scaled, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 2, 
                      vecInc = 0.01) # increase this if hit a bound 

# Ignore the NA/Inf eror the model is breaking in an unlikely parameter space (according to Edwards notes on vignette)


ISD_bin_plot_nonoverlapping.adj(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.silver20$MLE,
                            b.confMin = MLEbin.silver20$conf[1],
                            b.confMax = MLEbin.silver20$conf[2])
mtext(side = 1, line = 1.8, expression(paste("Body mass (", italic(x), "), g")), cex = 0.7)

silver20.slope = MLEbin.silver20$MLE
silver20.conf = MLEbin.silver20$conf

# Plot slopes and confidence intervals of the MLE fit # ==============================
  # though some lake/years have better fits than others # 
a = data.frame(slope = storm18.slope, year = 2018, lake = 'storm', upr = storm18.conf[1], lwr = storm18.conf[2])
b = data.frame(slope = storm19.slope, year = 2019, lake = 'storm', upr = storm19.conf[1], lwr = storm19.conf[2])
c = data.frame(slope = storm20.slope, year = 2020, lake = 'storm', upr = storm20.conf[1], lwr = storm20.conf[2]) 

d = data.frame(slope = south.twin18.slope, year = 2018, lake = 'south.twin', upr = south.twin18.conf[1], lwr = south.twin18.conf[2])
e = data.frame(slope = south.twin19.slope, year = 2019, lake = 'south.twin', upr = south.twin19.conf[1], lwr = south.twin19.conf[2])
f = data.frame(slope = south.twin20.slope, year = 2020, lake = 'south.twin', upr = south.twin20.conf[1], lwr = south.twin20.conf[2])

g = data.frame(slope = center18.slope, year = 2018, lake = 'center', upr = center18.conf[1], lwr = center18.conf[2])
h = data.frame(slope = center19.slope, year = 2019, lake = 'center', upr = center19.conf[1], lwr = center19.conf[2])
i = data.frame(slope = center20.slope, year = 2020, lake = 'center', upr = center20.conf[1], lwr = center20.conf[2])

j = data.frame(slope = five.island18.slope, year = 2018, lake = 'five.island', upr = five.island18.conf[1], lwr = five.island18.conf[2])
k = data.frame(slope = five.island19.slope, year = 2019, lake = 'five.island', upr = five.island19.conf[1], lwr = five.island19.conf[2])
l = data.frame(slope = five.island20.slope, year = 2020, lake = 'five.island', upr = five.island20.conf[1], lwr = five.island20.conf[2])

m = data.frame(slope = north.twin18.slope, year = 2018, lake = 'north.twin', upr = north.twin18.conf[1], lwr = north.twin18.conf[2])
n = data.frame(slope = north.twin19.slope, year = 2019, lake = 'north.twin', upr = north.twin19.conf[1], lwr = north.twin19.conf[2])
o = data.frame(slope = north.twin20.slope, year = 2020, lake = 'north.twin', upr = north.twin20.conf[1], lwr = north.twin20.conf[2])

p = data.frame(slope = silver18.slope, year = 2018, lake = 'silver', upr = silver18.conf[1], lwr = silver18.conf[2])
q = data.frame(slope = silver19.slope, year = 2019, lake = 'silver', upr = silver19.conf[1], lwr = silver19.conf[2])
r = data.frame(slope = silver20.slope, year = 2020, lake = 'silver', upr = silver20.conf[1], lwr = silver20.conf[2])

slope.dat.mle = full_join(a, 
              full_join(b, 
               full_join(c, 
                full_join(d, 
                 full_join(e, 
                  full_join(f, 
                   full_join(g, 
                    full_join(h, 
                     full_join(i, 
                      full_join(j, 
                       full_join(k, 
                        full_join(l,
                         full_join(m, 
                          full_join(n, 
                           full_join(o, 
                            full_join(p, 
                             full_join(q,r))))))))))))))))) 
slope.dat.mle = as_tibble(slope.dat)
slope.dat.mle

# Plotting OLS SLope-MLE Slope Dynamics #==========================
# Load in data # 
library(tidyverse)
library(lubridate)

# Color # 
# ========= PLOTTING COLORS ===== # 
# Output graphs with linear fits # 
# Reference - no removal # South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

# removal, removal, null # Center, Five Island 
rrn_col_18 = rgb(37, 111, 92, max = 255, alpha = 100) 
rrn_col_19 = rgb(37, 111, 92, max = 255, alpha = 180)
rrn_col_20 = rgb(37, 111, 92, max = 255, alpha = 255)

# null, removal, removal # North Twin, Silver
nrr_col_18 = rgb(77, 77, 77, max = 255, alpha = 100) 
nrr_col_19 = rgb(77, 77, 77, max = 255, alpha = 180)
nrr_col_20 = rgb(77, 77, 77, max = 255, alpha = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

## Reference SLOPE - OLS ##======================

windows(height = 3.5, width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,3,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

max(fits_dat$slp_u95ci)
min(fits_dat$slp_l95ci)

# STORM # 
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm')
storm.su_dat$year = c(2017.8, 2018.8, 2019.8)
storm.su_dat

plot(slope~year, data = storm.su_dat, pch = 19, ylim = c(-3.05, -0.10),   xlim = c(2017.4, 2020.4), cex = 2, col = ref_col_20, xaxt = 'n', ylab = '')
arrows(x0=storm.su_dat$year, y0=storm.su_dat$slp_l95ci,
       x1=storm.su_dat$year, y1=storm.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2, col = ref_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#abline(h = -1, lty =3)
mtext('Reference', side = 3, line = 0)
mtext('OLS Slope', side = 2, line = 1.8)
#mtext('Storm', side = 3, line = 0)
text(2017.55, -0.12, 'A', cex = 1.2)

# STORM # 
# storm.su_dat = fits_dat %>%
#   filter(lake == 'Storm') 
# storm.su_dat
# 
# plot(slope~year, data = storm.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19, col.axis = transparent, xaxt = 'n')
# arrows(x0=storm.su_dat$year, y0=storm.su_dat$slp_l95ci, col = ref_col_19,
#        x1=storm.su_dat$year, y1=storm.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
# axis(side = 1, at = c(2018, 2019, 2020), labels = F)
# abline(h = -1, lty =3)
# mtext(side=3, 'Storm', line = 0)
# storm.sp_dat = fits_dat %>%
#   filter(lake == 'Storm') %>%
#   filter(season == 'spring')
# storm.sp_dat
# 
# points(slope~year, data = storm.sp_dat, pch = 22, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
# arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$slope-storm.sp_dat$slp_se,
#        x1=storm.sp_dat$year, y1=storm.sp_dat$slope+storm.sp_dat$slp_se, code = 3, angle=90, length=0)


# SOUTH TWIN # 
st.su_dat = fits_dat %>%
  filter(lake == 'South Twin') 
st.su_dat$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = st.su_dat, pch = 19, ylim = c(-3.05, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.su_dat$year, y0=st.su_dat$slp_l95ci, col = ref_col_18,
       x1=st.su_dat$year, y1=st.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#segments(2017, -1, 2018.5, -1, lty = 3)
#abline(h = -1, v = c(2017.5, 2017.8), lty =3)
#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, -0.12, 'B', cex = 1.2)

# Legend - Storm & South Twin # 
legend('bottomright', legend = c('Storm', 'South Twin'), col = c(ref_col_20, ref_col_18), pch = 19, bty = 'n', cex = 1.1)

## RRN OLS SLOPE ##======================
# CENTER # 
center.su_dat = fits_dat %>%
  filter(lake == 'Center') 
center.su_dat$year = c(2017.8, 2018.8, 2019.8)

plot(slope~year, data = center.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n', ylab = '')
arrows(x0=center.su_dat$year, y0=center.su_dat$slp_l95ci, col = rrn_col_20,
       x1=center.su_dat$year, y1=center.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Removal 2018, 2019', side = 3, line = 0)
#mtext(side =2, 'Slope', line = 1.8 )
#segments(2017, -1, 2018.55, -1, lty = 3)
#abline(h = -1, lty = 3)
abline(v = 2017.65)
abline(v = 2018.5)
text(2017.53, -0.12, 'B', cex = 1.2)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five Island') 
five.island.su_dat$year =  c(2018.2, 2019.2, 2020.2)

points(slope~year, data = five.island.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$slp_l95ci, col = rrn_col_18,
       x1=five.island.su_dat$year, y1=five.island.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext(side = 3, 'Five Island', line = 0)
#abline(h = -1, lty =3)
#abline(v = 2017.65)
#abline(v = 2018.5)
#text(2017.53, -0.12, 'D', cex = 1.2)

# Legend Center & Five Island 
legend('bottomright', legend = c('Center', 'Five Island'), col = c(rrn_col_20, rrn_col_18), pch = 19, bty = 'n', cex = 1.1)

## NRR OLS SLOPE ##==========================
# NORTH TWIN # 
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North Twin') 
north.twin.su_dat$year = c(2017.8, 2018.8, 2019.8)

plot(slope~year, data = north.twin.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n', ylab = '')
arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$slp_l95ci, col = nrr_col_20,
       x1=north.twin.su_dat$year, y1=north.twin.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Removal 2019, 2020', side = 3, line = 0)
#mtext(side =2, 'Slope', line = 1.8)
#mtext(side = 1, 'Year', line = 2)
text(2017.53, -0.12, 'C', cex = 1.2)
legend('bottomleft', legend = c('North Twin', 'Silver'), col = c(nrr_col_20, nrr_col_18), pch = 19, cex = 1.1, bty = 'n')
segments(x0 = 2018.5, y0 = 0.0, x1 = 2018.5, y1 = -2.2)
abline(v = 2019.5)


# SILVER # 
silver.su_dat = fits_dat %>%
  filter(lake == 'Silver') 
silver.su_dat$year = c(2018.2, 2019.2, 2020.2)

points(slope~year, data = silver.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=silver.su_dat$year, y0=silver.su_dat$slp_l95ci, col = nrr_col_18,
       x1=silver.su_dat$year, y1=silver.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
#axis(side = 1, at = c(2018, 2019, 2020), labels = T)
#mtext(side = 3, 'Silver', line = 0)
#mtext(side = 1, 'Year', line = 2)

#abline(h = -1, lty =3)
#abline(v = 2018.5)
#abline(v = 2019.5)
#text(2017.53, -0.12, 'F', cex = 1.2)


## Reference MLE Slope ##==========================
slope.dat.mle

min(slope.dat.mle$upr)
max(slope.dat.mle$lwr)

# STORM #
storm.mle = slope.dat.mle %>% 
  filter(lake == 'storm')
storm.mle$year = c(2017.8, 2018.8, 2019.8)
plot(slope~year, data = storm.mle, pch = 17, ylim = c(-3.05, -0.1), xlim = c(2017.5, 2020.5), 
     cex = 2, col = ref_col_20, xaxt = 'n', ylab = '')
arrows(x0=storm.mle$year, y0=storm.mle$lwr, col = ref_col_20,
 x1=storm.mle$year, y1=storm.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext(side = 2, 'MLE Slope', line = 1.8)
#mtext('Height', side = 2, line = 1.8)
#mtext(side =3, line = 0, 'Storm')
text(2017.55, -.12, 'D', cex = 1.2)

# storm.sp_dat = fits_dat %>%
#   filter(lake == 'Storm') %>%
#   filter(season == 'spring')
# storm.sp_dat

# 
# points(height~year, data = storm.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
# #arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$height-storm.sp_dat$int_se,
# #  x1=storm.sp_dat$year, y1=storm.sp_dat$height+storm.sp_dat$int_se, code = 3, angle=90, length=0)

# SOUTH TWIN # 
st.mle = slope.dat.mle %>% 
  filter(lake == 'south.twin')
st.mle$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = st.mle, pch = 17, ylim = c(-3.05, -0.1),
       xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.mle$year, y0=st.mle$lwr, col = ref_col_18, 
 x1=st.mle$year, y1=st.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, 15.9, 'B', cex = 1.2)


## RRN MLE Slope ##===========================
# CENTER # 
center.mle = slope.dat.mle %>% 
  filter(lake == 'center')
center.mle$year = c(2017.8, 2018.8, 2019.8)
plot(slope~year, data = center.mle, pch = 17, ylim = c(-3.05, -0.1), xlim = c(2017.5, 2020.5), 
     cex = 2, col = rrn_col_20, xaxt = 'n', ylab = '')
arrows(x0=center.mle$year, y0=center.mle$lwr, col = rrn_col_20,
 x1=center.mle$year, y1=center.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext(side = 2, 'MLE Slope', line = 1.8)
#mtext('Height', side = 2, line = 1.8)
#mtext(side =3, line = 0, 'center')
text(2017.55, -.12, 'E', cex = 1.2)
abline(v = 2017.65)
abline(v = 2018.5)

# FIVE ISLAND # 
fi.mle = slope.dat.mle %>% 
  filter(lake == 'five.island')
fi.mle$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = fi.mle, pch = 17, ylim = c(-3.05, -0.1),
       xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt='n', col.axis = transparent)
arrows(x0=fi.mle$year, y0=fi.mle$lwr, col = rrn_col_18, 
 x1=fi.mle$year, y1=fi.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, 15.9, 'B', cex = 1.2)


## NRR MLE Slope ##========================
# NORTH TWIN # 
north.twin.mle = slope.dat.mle %>% 
  filter(lake == 'north.twin')
north.twin.mle$year = c(2017.8, 2018.8, 2019.8)
plot(slope~year, data = north.twin.mle, pch = 17, ylim = c(-3.05, -0.1), xlim = c(2017.5, 2020.5), 
     cex = 2, col = nrr_col_20, xaxt = 'n', ylab = '')
arrows(x0=north.twin.mle$year, y0=north.twin.mle$lwr, col = nrr_col_20,
 x1=north.twin.mle$year, y1=north.twin.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
#mtext(side = 2, 'MLE Slope', line = 1.8)
#mtext('Height', side = 2, line = 1.8)
#mtext(side =3, line = 0, 'north.twin')
text(2017.55, -.12, 'F', cex = 1.2)
abline(v= 2018.5) 
abline(v = 2019.5)

# SILVER # 
silver.mle = slope.dat.mle %>% 
  filter(lake == 'silver')
silver.mle$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = silver.mle, pch = 17, ylim = c(-3.05, -0.1),
       xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt='n', col.axis = transparent)
arrows(x0=silver.mle$year, y0=silver.mle$lwr, col = nrr_col_18, 
 x1=silver.mle$year, y1=silver.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, 15.9, 'B', cex = 1.2)

# Long version of Above Plot #===============================
## Reference SLOPE - OLS ##======================

windows(height = 6.5, width = 5) 

# Set dimensions for figure array # 
par(mfrow =c(3,2), mar = c(0.5,3,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

max(fits_dat$slp_u95ci)
min(fits_dat$slp_l95ci)

# STORM # 
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm')
storm.su_dat$year = c(2017.8, 2018.8, 2019.8)
storm.su_dat

plot(slope~year, data = storm.su_dat, pch = 19, ylim = c(-3.05, -0.10),   xlim = c(2017.4, 2020.4), cex = 2, col = ref_col_20, xaxt = 'n', ylab = '')
arrows(x0=storm.su_dat$year, y0=storm.su_dat$slp_l95ci,
       x1=storm.su_dat$year, y1=storm.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2, col = ref_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#abline(h = -1, lty =3)
mtext('Reference', side = 2, line = 3)
mtext('Slope', side = 2, line = 1.8)
mtext('OLS Slope', side = 3, line = 0)
#mtext('Storm', side = 3, line = 0)
text(2017.5, -0.12, 'A', cex = 1.2)

# STORM # 
# storm.su_dat = fits_dat %>%
#   filter(lake == 'Storm') 
# storm.su_dat
# 
# plot(slope~year, data = storm.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19, col.axis = transparent, xaxt = 'n')
# arrows(x0=storm.su_dat$year, y0=storm.su_dat$slp_l95ci, col = ref_col_19,
#        x1=storm.su_dat$year, y1=storm.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
# axis(side = 1, at = c(2018, 2019, 2020), labels = F)
# abline(h = -1, lty =3)
# mtext(side=3, 'Storm', line = 0)
# storm.sp_dat = fits_dat %>%
#   filter(lake == 'Storm') %>%
#   filter(season == 'spring')
# storm.sp_dat
# 
# points(slope~year, data = storm.sp_dat, pch = 22, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
# arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$slope-storm.sp_dat$slp_se,
#        x1=storm.sp_dat$year, y1=storm.sp_dat$slope+storm.sp_dat$slp_se, code = 3, angle=90, length=0)


# SOUTH TWIN # 
st.su_dat = fits_dat %>%
  filter(lake == 'South Twin') 
st.su_dat$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = st.su_dat, pch = 19, ylim = c(-3.05, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.su_dat$year, y0=st.su_dat$slp_l95ci, col = ref_col_18,
       x1=st.su_dat$year, y1=st.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#segments(2017, -1, 2018.5, -1, lty = 3)
#abline(h = -1, v = c(2017.5, 2017.8), lty =3)
#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, -0.12, 'B', cex = 1.2)

# Legend - Storm & South Twin # 
legend('bottomright', legend = c('Storm', 'South Twin'), col = c(ref_col_20, ref_col_18), pch = 19, bty = 'n', cex = 1.1)

## Reference MLE Slope ##==========================
slope.dat.mle

min(slope.dat.mle$upr)
max(slope.dat.mle$lwr)

# STORM #
storm.mle = slope.dat.mle %>% 
  filter(lake == 'storm')
storm.mle$year = c(2017.8, 2018.8, 2019.8)
plot(slope~year, data = storm.mle, pch = 17, ylim = c(-3.05, -0.1), xlim = c(2017.5, 2020.5), 
     cex = 2, col = ref_col_20, xaxt = 'n', ylab = '')
arrows(x0=storm.mle$year, y0=storm.mle$lwr, col = ref_col_20,
 x1=storm.mle$year, y1=storm.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 3, 'MLE Slope', line = 0)
#mtext('Height', side = 2, line = 1.8)
#mtext(side =3, line = 0, 'Storm')
text(2017.55, -.12, 'B', cex = 1.2)

# storm.sp_dat = fits_dat %>%
#   filter(lake == 'Storm') %>%
#   filter(season == 'spring')
# storm.sp_dat

# 
# points(height~year, data = storm.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
# #arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$height-storm.sp_dat$int_se,
# #  x1=storm.sp_dat$year, y1=storm.sp_dat$height+storm.sp_dat$int_se, code = 3, angle=90, length=0)

# SOUTH TWIN # 
st.mle = slope.dat.mle %>% 
  filter(lake == 'south.twin')
st.mle$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = st.mle, pch = 17, ylim = c(-3.05, -0.1),
       xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.mle$year, y0=st.mle$lwr, col = ref_col_18, 
 x1=st.mle$year, y1=st.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, 15.9, 'B', cex = 1.2)

## RRN OLS SLOPE ##======================
# CENTER # 
center.su_dat = fits_dat %>%
  filter(lake == 'Center') 
center.su_dat$year = c(2017.8, 2018.8, 2019.8)

plot(slope~year, data = center.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n', ylab = '')
arrows(x0=center.su_dat$year, y0=center.su_dat$slp_l95ci, col = rrn_col_20,
       x1=center.su_dat$year, y1=center.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Removal 2018, 2019', side = 2, line = 3)
mtext(side =2, 'Slope', line = 1.8 )
#segments(2017, -1, 2018.55, -1, lty = 3)
#abline(h = -1, lty = 3)
abline(v = 2017.65)
abline(v = 2018.5)
text(2017.53, -0.12, 'C', cex = 1.2)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five Island') 
five.island.su_dat$year =  c(2018.2, 2019.2, 2020.2)

points(slope~year, data = five.island.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$slp_l95ci, col = rrn_col_18,
       x1=five.island.su_dat$year, y1=five.island.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext(side = 3, 'Five Island', line = 0)
#abline(h = -1, lty =3)
#abline(v = 2017.65)
#abline(v = 2018.5)
#text(2017.53, -0.12, 'D', cex = 1.2)

# Legend Center & Five Island 
legend('bottomright', legend = c('Center', 'Five Island'), col = c(rrn_col_20, rrn_col_18), pch = 19, bty = 'n', cex = 1.1)

## RRN MLE Slope ##===========================
# CENTER # 
center.mle = slope.dat.mle %>% 
  filter(lake == 'center')
center.mle$year = c(2017.8, 2018.8, 2019.8)

plot(slope~year, data = center.mle, pch = 17, ylim = c(-3.05, -0.1), xlim = c(2017.5, 2020.5), 
     cex = 2, col = rrn_col_20, xaxt = 'n', ylab = '')
arrows(x0=center.mle$year, y0=center.mle$lwr, col = rrn_col_20,
 x1=center.mle$year, y1=center.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
#mtext(side = 2, 'MLE Slope', line = 1.8)
#mtext('Height', side = 2, line = 1.8)
#mtext(side =3, line = 0, 'center')
text(2017.55, -.12, 'D', cex = 1.2)
abline(v = 2017.65)
abline(v = 2018.5)

# FIVE ISLAND # 
fi.mle = slope.dat.mle %>% 
  filter(lake == 'five.island')
fi.mle$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = fi.mle, pch = 17, ylim = c(-3.05, -0.1),
       xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt='n', col.axis = transparent)
arrows(x0=fi.mle$year, y0=fi.mle$lwr, col = rrn_col_18, 
 x1=fi.mle$year, y1=fi.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)

#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, 15.9, 'B', cex = 1.2)


## NRR OLS SLOPE ##==========================
# NORTH TWIN # 
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North Twin') 
north.twin.su_dat$year = c(2017.8, 2018.8, 2019.8)

plot(slope~year, data = north.twin.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n', ylab = '')
arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$slp_l95ci, col = nrr_col_20,
       x1=north.twin.su_dat$year, y1=north.twin.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)

mtext('Removal 2019, 2020', side = 2, line = 3)
mtext(side =2, 'Slope', line = 1.8)
mtext(side = 1, 'Year', line = 1.8)
text(2017.53, -0.12, 'E', cex = 1.2)
legend('bottomleft', legend = c('North Twin', 'Silver'), col = c(nrr_col_20, nrr_col_18), pch = 19, cex = 1.1, bty = 'n')
segments(x0 = 2018.5, y0 = 0.0, x1 = 2018.5, y1 = -2.4)
abline(v = 2019.5)

# SILVER # 
silver.su_dat = fits_dat %>%
  filter(lake == 'Silver') 
silver.su_dat$year = c(2018.2, 2019.2, 2020.2)

points(slope~year, data = silver.su_dat, pch = 19, ylim = c(-3.05, -0.10),  
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=silver.su_dat$year, y0=silver.su_dat$slp_l95ci, col = nrr_col_18,
       x1=silver.su_dat$year, y1=silver.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
#axis(side = 1, at = c(2018, 2019, 2020), labels = T)
#mtext(side = 3, 'Silver', line = 0)
#mtext(side = 1, 'Year', line = 2)

#abline(h = -1, lty =3)
#abline(v = 2018.5)
#abline(v = 2019.5)
#text(2017.53, -0.12, 'F', cex = 1.2)

## NRR MLE Slope ##========================
# NORTH TWIN # 
north.twin.mle = slope.dat.mle %>% 
  filter(lake == 'north.twin')
north.twin.mle$year = c(2017.8, 2018.8, 2019.8)

plot(slope~year, data = north.twin.mle, pch = 17, ylim = c(-3.05, -0.1), xlim = c(2017.5, 2020.5), 
     cex = 2, col = nrr_col_20, xaxt = 'n', ylab = '')
arrows(x0=north.twin.mle$year, y0=north.twin.mle$lwr, col = nrr_col_20,
 x1=north.twin.mle$year, y1=north.twin.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
#mtext(side = 2, 'MLE Slope', line = 1.8)
#mtext('Height', side = 2, line = 1.8)
#mtext(side =3, line = 0, 'north.twin')
text(2017.55, -.12, 'F', cex = 1.2)
abline(v= 2018.5) 
abline(v = 2019.5)

# SILVER # 
silver.mle = slope.dat.mle %>% 
  filter(lake == 'silver')
silver.mle$year = c(2018.2, 2019.2, 2020.2)

points(slope~year,data = silver.mle, pch = 17, ylim = c(-3.05, -0.1),
       xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt='n', col.axis = transparent)
arrows(x0=silver.mle$year, y0=silver.mle$lwr, col = nrr_col_18, 
 x1=silver.mle$year, y1=silver.mle$upr, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 1, 'Year', line = 1.8)
#mtext(side = 3, 'South Twin', line = 0)
#text(2017.53, 15.9, 'B', cex = 1.2)




# ========= PLOTTING COLORS ===== # 
# Output graphs with linear fits # 
# Reference - no removal #  South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

# removal, removal, null # Center, Five Island 
rrn_col_18 = rgb(37, 111, 92, max = 255, alpha = 100) 
rrn_col_19 = rgb(37, 111, 92, max = 255, alpha = 180)
rrn_col_20 = rgb(37, 111, 92, max = 255, alpha = 255)

# null, removal, removal # North Twin, Silver
nrr_col_18 = rgb(77, 77, 77, max = 255, alpha = 100) 
nrr_col_19 = rgb(77, 77, 77, max = 255, alpha = 180)
nrr_col_20 = rgb(77, 77, 77, max = 255, alpha = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

# Set dimensions for figure array # 
par(mfrow =c(3,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


slope.dat
storm = slope.dat %>%
  filter(lake == 'storm') 

plot(slope~year, data = storm, ylim = c(-3.1, -0.7),
     cex = 2.5, pch = 19, xaxt = 'n', xlim = c(2017.5, 2020.5), 
     col = ref_col_20)
arrows(x0=storm$year, y0=storm$lwr,
       x1=storm$year, y1=storm$upr, code = 3, angle=90, length=0, lwd = 2, col = ref_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext('MLE Slope', side = 2, line = 3)
mtext('Reference', side = 2, line = 1.8)
mtext('Storm', side = 3, line = 0)
text(2017.53, -.8, 'A', cex = 1.2)

south.twin = slope.dat %>%
  filter(lake == 'south.twin') 

plot(slope~year, data = south.twin, ylim = c(-3.1, -0.7), xlim = c(2017.5, 2020.5),
     cex = 2.5, col = ref_col_19, pch = 19, xaxt = 'n', col.axis = transparent)
arrows(x0=south.twin$year, y0=south.twin$lwr,
       x1=south.twin$year, y1=south.twin$upr, code = 3, angle=90, length=0, lwd = 2, col = ref_col_19)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 3, 'South Twin', line = 0)
text(2017.53, -.8, 'B', cex = 1.2)

center = slope.dat %>%
  filter(lake == 'center') 

plot(slope~year, data = center, ylim = c(-3.1, -0.7), xlim = c(2017.5, 2020.5),
     cex = 2.5, pch = 19, col = rrn_col_20, xaxt = 'n')
arrows(x0=center$year, y0=center$lwr,
       x1=center$year, y1=center$upr, code = 3, angle=90, length=0, lwd = 2, col = rrn_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext('MLE Slope', side = 2, line = 3)
mtext('Removal 2018, 2019', side = 2, line = 1.8)
mtext(side =3, 'Center', line = 0 )
abline(v = 2017.65, lty = 3)
abline(v = 2018.5, lty = 3)
text(2017.53, -.8, 'C', cex = 1.2)

five.island = slope.dat %>%
  filter(lake == 'five.island') 

plot(slope~year, data = five.island, ylim = c(-3.1, -0.7),  xlim = c(2017.5, 2020.5),
     cex = 2.5, pch = 19, col = rrn_col_19, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island$year, y0=five.island$lwr,
       x1=five.island$year, y1=five.island$upr, code = 3, angle=90, length=0, lwd = 2, col = rrn_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 3, 'Five Island', line = 0)
abline(v = 2017.65, lty = 3)
abline(v = 2018.5, lty = 3)
text(2017.53, -.8, 'D', cex = 1.2)

north.twin = slope.dat %>%
  filter(lake == 'north.twin') 

plot(slope~year, data = north.twin, ylim = c(-3.1, -0.7), xlim = c(2017.5, 2020.5),
     cex = 2.5, pch = 19, col = nrr_col_20, xaxt = 'n')
arrows(x0=north.twin$year, y0=north.twin$lwr,
       x1=north.twin$year, y1=north.twin$upr, code = 3, angle=90, length=0, lwd = 2, col = nrr_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext('MLE Slope', side = 2, line = 3)
mtext('Removal 2019, 2020', side = 2, line = 1.8)
mtext(side =3, 'North Twin', line = 0)
abline(v = 2018.5, lty = 3)
abline(v = 2019.5, lty = 3)
text(2017.53, -.8, 'E', cex = 1.2)
mtext(side = 1, 'Year', line = 2)

silver = slope.dat %>%
  filter(lake == 'silver') 

plot(slope~year, data = silver, ylim = c(-3.1, -0.7), xlim = c(2017.5, 2020.5),
     cex = 2.5, pch = 19, col = nrr_col_19, xaxt = 'n', col.axis = transparent)
arrows(x0=silver$year, y0=silver$lwr,
       x1=silver$year, y1=silver$upr, code = 3, angle=90, length=0, lwd = 2, col = nrr_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext(side = 3, 'Silver', line = 0)
mtext(side = 1, 'Year', line = 2)

abline(v = 2018.5, lty = 3)
abline(v = 2019.5, lty = 3)
text(2017.53, -.8, 'F', cex = 1.2)