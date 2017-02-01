rm(list=ls())
require(xts)
require(ggplot2)
require(dplyr)
require(PerformanceAnalytics)
require(quantmod)
Sys.setenv(TZ="UTC")

setwd('//londonfile1/Group/Solutions Investment Office/PORTFOLIO CONSTRUCTION AND RISK/Portfolio Construction/2016 12 Dynamic Asset Allocation/')
data <- read.csv('S&P500data.csv', header = T)
dataXTS <- xts(data[,-1],as.Date(data[,1],'%d/%m/%Y'))# convert to xts
# get dates
dates <- data$Date
datesXTS <- index(dataXTS)
years <- data.frame(year = format(datesXTS, "%Y"))

# parameters
l <- nrow(data)
lambda <- 0.948078 # for a half life of 13 weeks
n <- 180

# custom function to calculate time period weights based on a desired half life

funWeights <- function(lambda, n){
 wt <- rep(0, n)
 wt[1] <- (1-lambda)/(1-lambda^n)
  for (i in 2:n){
    wt[i] <- wt[i-1]*lambda
  }
    wt
}

wghts <- funWeights(lambda, n) # get weights for calc of STV

# calculate vol time-series under STV (EWMA) and MTV
STV <- rollapply(dataXTS, n, FUN = function(x) sqrt(sum((x^2)*rev(wghts)))*sqrt(52),  by.column = FALSE, align = 'right')
MTV <- rollapply(dataXTS, n, FUN = function(x) sd(x)*sqrt(52), by.column = FALSE, align = 'right')

#####
# generic functions to aid dplyr use
dftoxts <- function(df) {
  xts(df[, 2:ncol(df)], as.POSIXct(df[,1]))
}
dftoxts1 <- function(df) {
  xts(df[, 2:ncol(df)], as.Date(df[,1],'%d/%m/%Y'))
}
xtstodf <- function(xts) {
  data.frame(Date = index(xts), as.matrix(xts), check.rows = F, check.names = F)
}

##### 
# set parameters for backtest simulation
volTarget <- 0.12 # this is the target vol
tradeSize <- 0.05 # max amount we can regear in any timestep
volLB <- 0.11 # set bounds inside which we don't trade
volUB <- 0.13 # set bounds inside which we don't trade
lag <- 2 # lag due to risk model availability
tCost = 0.001 # transaction cost set at 10bps

volXTS = STV

##### custom function that backtests trading strategy for time window #####
# Summary of rules
# 1. There is a lag upon receiving the risk model controlled by the lag variable
# 2. There is a limit on how much we can regear by each timestep set by the tradeSize variable
# 3. If the vol is within volLB and volUB we don't take any action
# The function works by for each time step calculating the amount we would need to degear by to get to the target vol
# except if we are in the bounds we do not trade. Further, the most we can gear up by is constrained by the trade size.
# However if we are above the vol upper limit we degear fully to the target vol in one time step.

## Original function ##
simulation <- function(volXTS){
  zero <- rep(0, length(volXTS))
  df <- data.frame(vol = volXTS, portVolStart = zero, percentDegear = zero, traded = zero, riskyWeight = zero, regear = zero, newPortVol = zero)
  df$riskyWeight[1:(n + lag - 1)] <- 1 # set first few periods inside lag window to be S&P until start calc
  
  for (i in (n + lag):length(volXTS)){
    df$portVolStart[i] <- df$vol[i - lag]*df$riskyWeight[i - 1]
    
    # set regearing amount (note these are expressed as negative values)
    # if we are below lower vol bound will suggest regearing even though we can't leverage - this is controlled in 
    # calc of percentDegear with max(df$percentDegear[i - 1] + df$regear[i], 0)
    if(abs(1 - volTarget/df$vol[i - lag] - df$percentDegear[i - 1]) > tradeSize){
      df$regear[i] <- -tradeSize
    } else {
      df$regear[i] <- (1 - volTarget/df$vol[i - lag] - df$percentDegear[i - 1])
    }
    
    # set percent we are degeared
    if (df$portVolStart[i] > volUB){
      df$percentDegear[i] <- (1 - volTarget/df$vol[i - lag])
    } else if (between(df$portVolStart[i], volLB, volUB)){
      df$percentDegear[i] <- df$percentDegear[i - 1]
    } else {
      df$percentDegear[i] <- max(df$percentDegear[i - 1] + df$regear[i], 0)
    }
    
    df$traded[i] <- abs(df$percentDegear[i] - df$percentDegear[i - 1]) # diff between 2 dates
    df$riskyWeight[i] <- (1 - df$percentDegear[i]) # weight in risky assets
    df$newPortVol[i] <- df$riskyWeight[i]*df$vol[i - lag] # calc new port vol at timestep
    
  }
  df  
}

system.time(out <- simulation(STV)) # calculate vol controlled strategy
# Runs in 0.22s
# Or running 1000 times
# system.time(replicate(1000, simulation(STV)))
# Running 1000 times takes 211 seconds.

rwI <- na.omit(out) # risky weight as numeric without NAs
dfI <- rwI[-c(1:2),] %>% select(riskyWeight)
# write.csv(dfI, 'MW.csv')

######################

# Attempt 1: trying to write vectorised code.
# Analysing the function above, it seems we are only trying to calculate the riskweight of the portfolio/
# alternatively, the percentDegear. Everything else can be calculated after the iteration in a vectorised 
# format, which will be much faster. Since we are only calcuting one variable that is dependent on path before it,
# we should be able to write this as a single function. Ideally, if this was a simple enough function, 
# we could use inbuilt mutate functions in dplyr which are written in C++ and highly optimised. Examples include
# cumsum, pmax etc. Alternatively, we will need to write a loop on just a single vector.

# Other observations: the riskweight at each time step is only dependent on vol variable at a particular lag.
# The updated riskweight compares itself to previous time, calculated portfolio vol (with the lagged vol)
# and then updates accordingly. The regear value should therefore only be calculated if it is required.
# R skips running any code not in conditional statements.

# Start with just vol.
# join vols and lag one so they are aligned in same row, convert to df
voldf = merge.xts(volXTS, lag.xts(volXTS,lag)) %>% na.omit() %>% xtstodf()
names(voldf) <- c("Date","Vol","VolLag")
volLag <- voldf$VolLag
  
# New function to iterate and produce the next element of riskweight.
rwUpdate <- function(rw_prev, volLag, volLB, volUB, volTarget, tradeSize) {
    percentDegear_prev = 1 - rw_prev
    vol_port_prev = volLag * rw_prev
    if (vol_port_prev > volUB) {
      percentDegear <- (1 - volTarget / volLag)
    } else if (between(vol_port_prev, volLB, volUB)) {
      percentDegear <- percentDegear_prev
    } else {
      # Move regear code to here
      if(abs(1 - volTarget / volLag - percentDegear_prev) > tradeSize){
        regearTemp <- -tradeSize
      } else {
        regearTemp <- (1 - volTarget / volLag - percentDegear_prev)
      }
      percentDegear <- max(percentDegear_prev + regearTemp, 0)
    }
    rw <- 1 - percentDegear
  return(rw)
}

# Now we iterate over the path dependent part
rwIterate <- function(volLag, volLB, volUB, volTarget, tradeSize) {
    n = length(volLag)
    rw <- rep(1, n)
    for (i in 2:n) {
      rw[i] <- rwUpdate(rw[i-1], volLag[i], volLB, volUB, volTarget, tradeSize)
    }
    return(rw)
}

# Check it works (out by a lag of two, check why - MW)
rwR <- rwIterate(volLag,volLB,volUB,volTarget,tradeSize)
dfR <- data.frame(Date = voldf$Date, rwR) # RF output for comparison
# write.csv(dfR, 'RF.csv')

# Now run 1000 times.
system.time(replicate(1000, rwIterate(volLag, volLB, volUB, volTarget, tradeSize)))

# Running this method only takes ~7 seconds. 
# Then we can calculate the other components as vectorised code.
# Note that these functions don't add on any time at all really.
system.time(replicate(1000,{
  out <- voldf %>% mutate(riskweight = rwIterate(volLag,volLB,volUB,volTarget,tradeSize)) %>%
    mutate(percentDegear = 1 - riskweight,
           traded = abs(riskweight-lag(riskweight)),
           portVol = riskweight * volLag)
}))

# Attempt 2: We look the make the code even faster now using C++ within R. 
# There are 2 main ways to do this using the Rcpp package. You will need Rtools also installed for this to 
# work. Also, be careful when installing Rtools that you configure the path correctly at the installation 
# stage. Google if necessary.
# I got all info on this from: http://adv-r.had.co.nz/Rcpp.html
# One option is to load a single function directly with cppFunction. However, we need 2 functions to replicate
# above and therefore need to save them in a script .cpp and source the script with sourceCpp.

# C++ has some similar syntax to R. if else loops are exactly the same. 
# Every execution line needs to end in a ;
# Need to declare every variable. 
# The script itself needs a few extra bits of code I just copied from a template.

library(Rcpp)
sourceCpp("cplusplus.cpp")

# Now calculate the risk weights using the C function that can be called from within R.
rwC <- rwIterateC(volLag, volLB, volUB, volTarget, tradeSize)
dfC <- data.frame(Date = voldf$Date, rwC) # RF output for comparison

# Now run 1000 times. 
system.time(replicate(1000,rwIterateC(volLag,volLB,volUB,volTarget,tradeSize)))

# Summary: For 1000 iterations: 220s vs 7s in R and 0.03 in C++ !!!

# Check output - all the same now
resComp <- data.frame(rwI = dfI, rwR = dfR$rwR, rwC = dfC$rwC) 
write.csv(resComp, 'resComp.csv')

chart.TimeSeries(as.xts(out[,c(1,3,7)])) # plot amount degeared and vols

#####
# calculate trading cost per year
tcPerYear <- years %>% 
  bind_cols(simulation(STV)) %>% 
  group_by(year) %>% 
  summarise(cost = sum(traded))

##### Backtest and summary of backtest results

# weights for backtest
bWeights <- data.frame(date = data$Date) %>%
  bind_cols(data.frame(normal = rep(1, l), deGear = (1 - out$percentDegear)))  
# df containing performance of both strategies including TC
perf <- as.data.frame(apply(bWeights[,-1], 2, function(x) x*data$Return)) %>%
  bind_cols(data.frame(cost = out$traded*tCost)) %>% # cost to trade
  transmute(normal = normal, deGearTC = deGear - cost)
# chart of cumulative returns
res <- data.frame(date = dates, perf) %>%
  dftoxts1 %>%
  chart.CumReturns(wealth.index = T, legend.loc = 'top', main = "Cumulative Returns")
# table of downside risk
data.frame(date = dates, perf) %>%
  dftoxts1 %>%
  table.DownsideRisk()
# table of annualised returns
data.frame(date = dates, perf) %>%
  dftoxts1 %>%
  table.AnnualizedReturns()
# analysis of STV autocorrelation
acf <- acf(na.omit(STV))
pacf <- pacf(na.omit(STV))