### Current runtime of loop is ~7 mins for 10,000 trials
rm(list=ls()) # wipeout all variables

require(xts)
require(doParallel)
require(readr)
require(data.table)

setwd('//londonfile1/Group/Solutions Investment Office/PORTFOLIO CONSTRUCTION AND RISK/SCR optimisation framework/SCR Pricer')

### Parameters ###
n <- 36 # number of monthly historical returns
nESG <- 120 # number of ESG simulated months

### read in historical data ###
eqHist <- read.csv('Reference Data/EquityHistoricalData.csv', header = T) 
hist3y <- last(eqHist, n)

### read in ESG data and sort headers out ###
eqESG <- read.csv('ESG Data/Equity.csv', header = T, row.names = NULL)
colnames(eqESG) <- c(colnames(eqESG)[-1], NULL)
eqESG <- eqESG[,-ncol(eqESG)]
colnames(eqESG) <- c('Trial', 'Timestep', colnames(eqHist)[-1])
eqESG <- eqESG[!(eqESG$Timestep == 0),] # cut t=0 timestep from ESG as always 0
eqESGSplit <- split(eqESG, eqESG$Trial) # split ESG data

### main loop for each ESG trial ####
### old (slow) calc which works okay ###

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
ptm <- proc.time()

### Calculate symmetric adjustment firstly ###
r <- foreach (i = 1:length(eqESGSplit), .combine = 'rbind', .packages = c('zoo', 'xts')) %dopar% {
    
ret <- eqESGSplit[[i]][,3:4] # need to alter when try sapply
combinedRet <- rbind(hist3y[,-1], ret )
rownames(combinedRet) <- seq(1:(n + nESG))
level <- cumprod(1 + combinedRet)*100

symAdjFunction <- function(x) max(min(((last(x, 1) - mean(x))/mean(x) - 0.08)*0.5, 0.1), -0.1) # calc symmetric adjustment
rollSymAdj <- function(x) rollapply(x, 36, FUN = symAdjFunction, by = 1, align = 'right')
symAdj <- apply(level, 2, rollSymAdj)

res <- data.frame(Trial = i, Timestep = seq(0, nESG,length.out = (nESG + 1)), symAdj)
return(res)

}

proc.time() - ptm
stopCluster(cl = cl)

annualResultsSA <- subset(r, r$Timestep %% 12 == 0)
rownames(annualResultsSA) <- seq(1:nrow(annualResultsSA))

############# TEST ###################
### try: sapply(eqESGSplit, UDF) and enclose current loop in the UDF

# udf <- function(input){
#     t <- as.data.frame(input)
# 
#     symAdjFunction <- function(x) max(min(((last(x, 1) - mean(x))/mean(x) - 0.08)*0.5, 0.1), -0.1) # calc symmetric adjustment
#     rollSymAdj <- function(x) rollapply(x, 36, FUN = symAdjFunction, by = 1, align = 'right')
# 
#     ret <- t[,3:4] # need to alter when try sapply
#     colnames(ret) <- colnames(eqESG[,3:4])
#     combinedRet <- rbind(hist3y[,-1], ret) # combine returns
#     rownames(combinedRet) <- seq(1:(n + nESG))
#     level <- cumprod(1 + combinedRet)*100 # create price index
# 
#     symAdj <- apply(t[,3:4], 2, rollSymAdj)
#     return(symAdj)
# 
# }

# system.time(t1 <- sapply(eqESGSplit[1:2], FUN = udf))

############# END TEST ###################

### Combine results into SCR_Equity: type 1 base 39%, type 2 49%
SCRTypes <- data.frame(annualResultsSA[,1:2], EquityType1 = annualResultsSA[,3] + 0.39, EquityType2 = annualResultsSA[,4] + 0.49)

write.csv(SCRTypes, 'Output/SCREquity.csv', row.names = FALSE)
# write.csv(SCR, 'Output/SCREquityCalc.csv', row.names = FALSE)