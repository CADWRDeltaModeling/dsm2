# Pre-processor for generating South Delta transition probabilities for the ECO-PTM using the USGS routing model.
# Doug Jackson
# doug@QEDAconsulting.com
library(rhdf5)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(doParallel)
library(imputeTS)
library(slider)
library(msm)
####################################################################################################
# Constants
####################################################################################################
workingDir <- "/Users/djackson/Documents/QEDA/DWR/programs/routingPreprocessor"

outputDir <- file.path(workingDir, "output")
tideFile <- "/Users/djackson/Documents/QEDA/DSM2_tideFiles/routing_model_dsm2_output_15apr22/routing_model_dsm2_15apr22.h5"
stationLocFile <- file.path(workingDir, "stationLoc.csv")

sampleTime_min <- 15

TClag_min <- 90

flowScalingFile <- file.path(workingDir, "flowScaling.csv")
coefsFile <- file.path(workingDir, "coefs.csv")
covMeansFile <- file.path(workingDir, "covMeans.csv") 

HORstationNames <- c("HOR_U", "HOR_D", "HOR_T")
TCstationNames <- c("TC_U", "TC_D", "TC_T")

# Range of dates to calculate transition probabilities
transProbsStartDate <- "01jan2011"
transProbsEndDate <- "31dec2017"

plotHORstartDate <- "18may2013"
plotHORstartTime <- "08:00"
plotHORendDate <- "20may2013"
plotHORendTime <- "08:00"
plotTCstartDate <- "20apr2016"
plotTCstartTime <- "00:00"
plotTCendDate <- "23apr2016"
plotTCendTime <- "00:00"

# Number of CPU cores to use
numCores <- 8

figWidth <- 10
figHeight <- 5
figDPI <- 300
####################################################################################################
# Functions
####################################################################################################
# Godin filter from Mike Dodrill
# Based on code from https://github.com/ODWG/ODWGtools 
smoothGodin = function(x, increment="15 mins", kind = c("mean", "max", "min")) {
    kind = match.arg(kind, c("mean", "max", "min"))
    roll_fun = switch(kind,
                      "mean" = mean,
                      "max" = max,
                      "min" = min
    )
    increment = string_to_difftime(increment)
    inc.units = units(increment)
    d25 = as.difftime(25L, units = "hours")
    d24 = as.difftime(24L, units = "hours")
    
    w25 = as.numeric(d25, units = inc.units) / as.numeric(increment)
    w24 = as.numeric(d24, units = inc.units) / as.numeric(increment)
    w25.before = sum(seq(w25) < ceiling(w25 / 2))
    w25.after = sum(seq(w25) > ceiling(w25 / 2))
    w24.before = sum(seq(w24) < ceiling(w24 / 2))
    w24.after = sum(seq(w24) > ceiling(w24 / 2))
    
    offset = as.numeric(as.difftime(1L, units = "hours"),
                        units = inc.units) / as.numeric(increment)
    
    (slide_dbl(lag(x, offset), roll_fun, .complete = TRUE,
               .before = w24.before, .after = w24.after) +
            slide_dbl(lead(x, offset), roll_fun, .complete = TRUE,
                      .before = w24.before, .after = w24.after) +
            slide_dbl(x, roll_fun, .complete = TRUE,
                      .before = w25.before, .after = w25.after)) / 3.0
}

# Function to determine day/night from Mike Dodrill
day_light <- function(date_time, loc = "old_river"){
    
    if(loc == "old_river"){
        # junction at head of Old River
        lat = 37.808168
        lon = -121.327446
    }
    
    if(loc == "turner_cut"){
        # junction at turner cut
        lat = 38.0060934678955
        lon = -121.45293254879358
    } 
    
    date_in = as.Date(date_time)
    
    # in Rebecca's meta data it says "All times are in Pacific Local Time (PLT)" -- I think this is correct
    sun = suncalc::getSunlightTimes(date = date_in, lat = lat, lon = lon,
                                    keep = c("sunrise", "sunset"), tz = "Etc/GMT+8")
    
    out = ifelse(date_time > sun$sunrise & date_time < sun$sunset, "day", "night")
    return(out)
}

# Barrier state from Mike Dodrill
defineBarrier <- function() {
    # head of old river barrier info:
    # See: Q:\QFES\Delta\sth six year study\DATA\7COVAR\Barrier status\OMR BARRIER STATUS.xlsx
    
    # dates are from "closed" to "breached" for "spring" only (not fall). Note: I've
    # added one day to the closed and subtracted one day from breached, to be
    # conservative on then the barrier was in place and working fully. If not
    # "closed", then "open"
    b1 = seq.Date(from = as.Date("2012-04-02"), to = as.Date("2012-06-03"), by = "day")
    # 2013 - no barrier
    b2 = seq.Date(from = as.Date("2014-04-09"), to = as.Date("2014-06-08"), by = "day")
    b3 = seq.Date(from = as.Date("2015-04-04"), to = as.Date("2015-05-31"), by = "day")
    b4 = seq.Date(from = as.Date("2016-04-02"), to = as.Date("2016-05-31"), by = "day")
    
    barrier = data.frame(date = c(b1, b2, b3, b4), status = "closed")
    return(barrier)
}

string_to_difftime = function(s) {
    value = as.numeric(strsplit(s, " ")[[c(1, 1)]])
    increment = strsplit(s, " ")[[c(1, 2)]]
    increment = match.arg(increment,
                          c("secs", "mins", "hours", "days", "weeks"))
    as.difftime(value, units = increment)
}

# Calculate the 15-minute transition probabilities at Head of Old River
# Based on /Users/djackson/Documents/QEDA/DWR/fromXiao/routingSouthDelta_27jan22/Guide_HOR.html
calcProbsHOR <- function(yes2011, barrierOpen, daytime,
                         scaled_net_flow_D, scaled_tidal_D, 
                         scaled_net_flow_T, scaled_tidal_T,
                         logest, log_slopes, covMeans) {
    
    cov_vec = c(yes_11 = yes2011 - covMeans["yes_11", "covMean"],
                barrier_2Open = barrierOpen - covMeans["barrier_2Open", "covMean"],
                s_g_cfs_3_new = scaled_net_flow_T - covMeans["s_g_cfs_3", "covMean"],
                s_t_cfs_3_new = scaled_tidal_T - covMeans["s_t_cfs_3", "covMean"],
                day2_day = daytime - covMeans["day2_day", "covMean"],
                s_g_cfs_2_new = scaled_net_flow_D - covMeans["s_g_cfs_2", "covMean"],
                s_t_cfs_2_new = scaled_tidal_D - covMeans["s_t_cfs_2", "covMean"])

    for(i in 1:length(log_slopes)){
        logest <- logest + log_slopes[[i]] * cov_vec[i]
    }
    
    mat <- exp(logest)
    diag(mat) <- 0
    diag(mat) <- -rowSums(mat)
    
    p <- MatrixExp(as.matrix(mat), 1)
    
    return(p)
}

# Calculate the 15-minute transition probabilities in Turner Cut
# Based on /Users/djackson/Documents/QEDA/DWR/fromXiao/routingSouthDelta_27jan22/Guide_TC.html
calcProbsTC <- function(scaled_net_flow_U, scaled_tidal_U,
                        scaled_net_flow_D, scaled_tidal_D, 
                        scaled_net_flow_T, scaled_tidal_T,
                        logest, log_slopes, covMeans) {
  
    # Subtract covariate means to be consistent with pmatrix.msm
    cov_vec <- c(s_g_l_cfs_3_new = scaled_net_flow_T - covMeans["s_g_l_cfs_3", "covMean"],
                 s_t_l_cfs_3_new = scaled_tidal_T - covMeans["s_t_l_cfs_3", "covMean"],
                 s_g_l_cfs_2_new = scaled_net_flow_D - covMeans["s_g_l_cfs_2", "covMean"],
                 s_t_l_cfs_2_new = scaled_tidal_D - covMeans["s_t_l_cfs_2", "covMean"],
                 s_g_l_cfs_1_new = scaled_net_flow_U - covMeans["s_g_l_cfs_1", "covMean"],
                 s_t_l_cfs_1_new = scaled_tidal_U - covMeans["s_t_l_cfs_1", "covMean"])

    for(i in 1:length(log_slopes)){
        logest <- logest + log_slopes[[i]] * cov_vec[i]
    }

    mat <- exp(logest)
    diag(mat) <- 0
    diag(mat) <- -rowSums(mat)

    p <- MatrixExp(as.matrix(mat), 1)

    return(p)
}

# Rename coefs data frame to use State 1, State 2, State 3 convention
renameCoefsDF <- function(coefsDF) {
    row.names(coefsDF) <- coefsDF$fromState
    coefsDF <- coefsDF[c("fromU", "fromD", "fromT"), ]
    row.names(coefsDF) <- c("State 1", "State 2", "State 3")
    coefsDF <- coefsDF[, c("toU", "toD", "toT")]
    names(coefsDF) <- c("State 1", "State 2", "State 3")
    return(coefsDF)
}
####################################################################################################
# Run
####################################################################################################
setwd(workingDir)

# Set up parallel processing
registerDoParallel(cores=numCores)

# Read the locations of the stations
stationLoc <- read.csv(stationLocFile)
stationLoc$channelFrac <- stationLoc$channelDist_ft/stationLoc$channelLen_ft
stationNames <- unique(stationLoc$stationName)

# Read the start and end dates and times
envVar <- h5read(tideFile, "hydro/input/envvar")
startDate <- envVar[envVar$name=="START_DATE", "value"]
startTime <- envVar[envVar$name=="START_TIME", "value"]
endDate <- envVar[envVar$name=="END_DATE", "value"]
endTime <- envVar[envVar$name=="END_TIME", "value"]
timeStep_min <- h5readAttributes(tideFile, "hydro")$'Time interval'

# All times are in PST (GMT+8)
startDatetime <- dmy_hm(paste0(startDate, startTime), tz="Etc/GMT+8")
endDatetime <- dmy_hm(paste0(endDate, endTime), tz="Etc/GMT+8")
transProbsStartDatetime <- dmy(transProbsStartDate, tz="Etc/GMT+8")
transProbsEndDatetime <- dmy(transProbsEndDate, tz="Etc/GMT+8")
plotHORstartDatetime <- dmy_hm(paste0(plotHORstartDate, plotHORstartTime), tz="Etc/GMT+8")
plotHORendDatetime <- dmy_hm(paste0(plotHORendDate, plotHORendTime), tz="Etc/GMT+8")
plotTCstartDatetime <- dmy_hm(paste0(plotTCstartDate, plotTCstartTime), tz="Etc/GMT+8")
plotTCendDatetime <- dmy_hm(paste0(plotTCendDate, plotTCendTime), tz="Etc/GMT+8")

cat("Range of data available in tide file:", as.character(startDatetime), "to", as.character(endDatetime), "\n")
cat("Range of data to extract:", as.character(transProbsStartDatetime), "to", as.character(transProbsEndDatetime), "\n")

# Read flows for all channels
channelFlows <- h5read(tideFile, "/hydro/data/channel flow")

# Read the (external) channel numbers
channelNums = h5read(tideFile, "/hydro/geometry/channel_number")

# Create data frame with channelNums and channelFracs for each time step and station
datetimes <- seq(startDatetime, endDatetime, sampleTime_min*60)
datetimes <- data.frame(datetime=datetimes)
datetimes$year <- year(datetimes$datetime)

flowDatetimes <- seq(startDatetime, endDatetime, length.out=dim(channelFlows)[3])

# Expand stationLoc to include entries for all years
stationLocByYear <- data.frame(year=seq(min(datetimes$year), max(datetimes$year)), ID=1)
stationNamesDF <- data.frame(stationName=stationNames, ID=1)
stationLocByYear <- full_join(stationLocByYear, stationNamesDF, by="ID")
stationLocByYear$ID <- NULL

for(i in 1:nrow(stationLoc)) {
    thisStationLoc <- stationLoc[i, ]
    thisIndices <- stationLocByYear$stationName==thisStationLoc$stationName & 
        stationLocByYear$year>=thisStationLoc$startYear & stationLocByYear$year<=thisStationLoc$endYear
    stationLocByYear[thisIndices, "channelNum"] <- thisStationLoc$extChannelNum
    stationLocByYear[thisIndices, "channelFrac"] <- thisStationLoc$channelFrac
}

stationLocByDatetime <- left_join(datetimes, stationLocByYear, by="year")

stationChannelNums <- unique(stationLocByDatetime$channelNum)

# Extract flows for all channels
cat("Extracting flow data from tide file.\n")
DSM2flowList <- list()
for(i in 1:length(stationChannelNums)) {
    thisChannelNum <- stationChannelNums[i]
    thisChannelIndex <- which(channelNums==thisChannelNum)
    
    thisFlow <- data.frame(datetime=flowDatetimes,
                           upFlow=channelFlows[1, thisChannelIndex, ],
                           downFlow=channelFlows[2, thisChannelIndex, ])
    
    # Upsample
    thisFlow <- left_join(datetimes, thisFlow, by="datetime")
    thisFlow$upFlow <- na_interpolation(thisFlow$upFlow)
    thisFlow$downFlow <- na_interpolation(thisFlow$downFlow)
    
    thisFlow$channelNum <- thisChannelNum
    
    DSM2flowList[[i]] <- thisFlow 
}
DSM2flows <- bind_rows(DSM2flowList)

# Combine flows with station location information and calculate interpolated flow
stationFlow <- left_join(stationLocByDatetime, DSM2flows, by=c("datetime", "year", "channelNum"))
stationFlow <- stationFlow %>% mutate(flow=upFlow + channelFrac*(downFlow-upFlow))

# Apply the Godin filter
cat("Applying Godin filter.\n")
stationFlow <- stationFlow %>% group_by(stationName) %>% mutate(netFlow=smoothGodin(flow))
stationFlow$tidalFlow <- stationFlow$flow - stationFlow$netFlow

stationFlow <- as.data.frame(stationFlow)

# Apply lag at Turner Cut
for(stationName in TCstationNames) {
    stationFlow[stationFlow$stationName==stationName, "datetime"] <- 
        stationFlow[stationFlow$stationName==stationName, "datetime"] + minutes(TClag_min)
}

stationFlow <- stationFlow %>% select(datetime, stationName, netFlow, tidalFlow) %>% filter(!is.na(tidalFlow))

# Load routing model coefficients and scaling data
flowScaling <- read.csv(flowScalingFile)

# Apply flow scaling
stationFlow <- left_join(stationFlow, flowScaling, by="stationName")
stationFlow <- stationFlow %>% mutate(netFlowScaled=(netFlow-netMean)/netStd,
                                      tidalFlowScaled=(tidalFlow-tidalMean)/tidalStd)

cat("Saving flow data to", file.path(outputDir, "stationFlow.csv"), "\n")
write.csv(stationFlow, file.path(outputDir, "stationFlow.csv"), row.names=F)

# Plot HOR
cat("Saving HOR flow plot to", file.path(outputDir, "plotHORflow.png"), "\n")
plotHORflow <- stationFlow %>% filter(datetime>=plotHORstartDatetime, datetime<=plotHORendDatetime, stationName %in% HORstationNames)
p <- ggplot(plotHORflow) + geom_line(aes(x=datetime, y=netFlow, color=stationName), linetype="dashed") + 
    geom_line(aes(x=datetime, y=tidalFlow, color=stationName)) + 
    ylim(-2000, 2100) + labs(title="Head of Old River") + theme_light()
ggsave(file.path(outputDir, "plotHORflow.png"), width=figWidth, height=figHeight, dpi=figDPI)

# Plot Turner Cut
cat("Saving Turner Cut flow plot to", file.path(outputDir, "plotTCflow.png"), "\n")
plotTCflow <- stationFlow %>% filter(datetime>=plotTCstartDatetime, datetime<=plotTCendDatetime, stationName %in% TCstationNames)
p <- ggplot(plotTCflow) + geom_line(aes(x=datetime, y=netFlow, color=stationName), linetype="dashed") + 
    geom_line(aes(x=datetime, y=tidalFlow, color=stationName)) +
    labs(title="Turner Cut") + theme_light()
ggsave(file.path(outputDir, "plotTCflow.png"), width=figWidth, height=figHeight, dpi=figDPI)

# Load model coefficients
coefs <- read.csv(coefsFile)

# Load covariate means
covMeans <- read.csv(covMeansFile)

# Define barrier status
barrier <- defineBarrier()

# Create a logfile to monitor progress
logFile <- file.path(workingDir, "routingPreprocessor.log")
cat("Calculating transition probabilities. Track progress in logfile:", logFile, "\n")
cat("routingPreprocessor.R logfile\n", file=logFile)
######################################################
# Calculate Head of Old River transition probabilities
coefsHOR <- coefs %>% filter(junction=="HOR")
parameters <- unique(coefsHOR$parameter)
parameters <- parameters[parameters!="logBaseline"]
covMeansHOR <- covMeans %>% filter(junction=="HOR")
rownames(covMeansHOR) <- covMeansHOR$parameter

logestHOR <- coefsHOR %>% filter(parameter=="logBaseline") %>% select(fromState, toU, toD, toT)
logestHOR <- renameCoefsDF(logestHOR)

log_slopesHOR <- list()
for (thisParameter in parameters) {
    thisCoefs <- coefsHOR %>% filter(parameter==thisParameter) %>% select(fromState, toU, toD, toT)
    log_slopesHOR[[thisParameter]] <- renameCoefsDF(thisCoefs)
}

# Create flows data frame for calculating transition probabilities
modelHORflow <- stationFlow %>% filter(datetime>transProbsStartDatetime, datetime<transProbsEndDatetime, stationName %in% HORstationNames) %>% 
    select(datetime, stationName, netFlowScaled, tidalFlowScaled) %>% 
    pivot_wider(id_cols=datetime, names_from=stationName, values_from=netFlowScaled:tidalFlowScaled)

for (transition in c("qUD", "qUT", "qDU", "qDT", "qTU", "qTD")) {
    modelHORflow[, transition] <- NA
}
modelHORflow <- as.data.frame(modelHORflow)

# Calculate transition probabilities
r <- foreach(i=1:nrow(modelHORflow), .combine=rbind) %dopar% {
    
    if(i%%1000 == 0) {
        cat("HOR: calculating transition probabilities for row", i, "of", nrow(modelHORflow), "\n",
            file=logFile, append=T)
    }
    
    thisYear <- year(modelHORflow[i, "datetime"])
    thisYear2011 <- ifelse(thisYear==2011, 1, 0)
    
    # In Mike Dodrill's original model, barrier_2Open was a factor with closed=1 and open=2. msm.parse.covariates converts this
    # to a 0 and 1, respectively (see 23may22 Evernote)
    thisBarrierOpen <- ifelse(date(modelHORflow$datetime[i]) %in% barrier$date, 0, 1)
    
    # In Mike Dodrill's original model, day2_day was a factor with night=1 and day=2. msm.parse.covariates converts this 
    # to a 0 and 1, respectively (see 23may22 Evernote)
    thisDaytime <- ifelse(day_light(modelHORflow$datetime[i])=="day", 1, 0)
    
    thisProbs <- calcProbsHOR(thisYear2011, thisBarrierOpen, thisDaytime,
                              modelHORflow[i, "netFlowScaled_HOR_D"], modelHORflow[i, "tidalFlowScaled_HOR_D"],
                              modelHORflow[i, "netFlowScaled_HOR_T"], modelHORflow[i, "tidalFlowScaled_HOR_T"],
                              logestHOR, log_slopesHOR, covMeansHOR)
    
    modelHORflow$qUD[i] <- thisProbs[1, 2]
    modelHORflow$qUT[i] <- thisProbs[1, 3]
    modelHORflow$qDU[i] <- thisProbs[2, 1]
    modelHORflow$qDT[i] <- thisProbs[2, 3]
    modelHORflow$qTU[i] <- thisProbs[3, 1]
    modelHORflow$qTD[i] <- thisProbs[3, 2]
    modelHORflow[i, ]
}
rownames(r) <- c()
modelHORflow <- r
######################################################
# Calculate Turner Cut transition probabilities
coefsTC <- coefs %>% filter(junction=="TC")
parameters <- unique(coefsTC$parameter)
parameters <- parameters[parameters!="logBaseline"]
covMeansTC <- covMeans %>% filter(junction=="TC")
rownames(covMeansTC) <- covMeansTC$parameter

logestTC <- coefsTC %>% filter(parameter=="logBaseline") %>% select(fromState, toU, toD, toT)
logestTC <- renameCoefsDF(logestTC)

log_slopesTC <- list()
for (thisParameter in parameters) {
    thisCoefs <- coefsTC %>% filter(parameter==thisParameter) %>% select(fromState, toU, toD, toT)
    log_slopesTC[[thisParameter]] <- renameCoefsDF(thisCoefs)
}

# Create flows data frame for calculating transition probabilities
modelTCflow <- stationFlow %>% filter(datetime>transProbsStartDatetime, datetime<transProbsEndDatetime, stationName %in% TCstationNames) %>% 
    select(datetime, stationName, netFlowScaled, tidalFlowScaled) %>% 
    pivot_wider(id_cols=datetime, names_from=stationName, values_from=netFlowScaled:tidalFlowScaled)

for (transition in c("qUD", "qUT", "qDU", "qDT", "qTU", "qTD")) {
    modelTCflow[, transition] <- NA
}
modelTCflow <- as.data.frame(modelTCflow)

# Calculate transition probabilities
#for (i in 1:nrow(modelTCflow)) {
r <- foreach(i=1:nrow(modelTCflow), .combine=rbind) %dopar% {

    if(i%%1000 == 0) {
        cat("TC: calculating transition probabilities for row", i, "of", nrow(modelHORflow), "\n",
            file=logFile, append=T)
    }
    
    thisProbs <- calcProbsTC(modelTCflow[i, "netFlowScaled_TC_U"], modelTCflow[i, "tidalFlowScaled_TC_U"],
                             modelTCflow[i, "netFlowScaled_TC_D"], modelTCflow[i, "tidalFlowScaled_TC_D"],
                             modelTCflow[i, "netFlowScaled_TC_T"], modelTCflow[i, "tidalFlowScaled_TC_T"],
                             logestTC, log_slopesTC, covMeansTC)
    
    modelTCflow$qUD[i] <- thisProbs[1, 2]
    modelTCflow$qUT[i] <- thisProbs[1, 3]
    modelTCflow$qDU[i] <- thisProbs[2, 1]
    modelTCflow$qDT[i] <- thisProbs[2, 3]
    modelTCflow$qTU[i] <- thisProbs[3, 1]
    modelTCflow$qTD[i] <- thisProbs[3, 2]
    modelTCflow[i, ]
}
rownames(r) <- c()
modelTCflow <- r
######################################################
# Combine HOR and TC transition probabilities and write to an output file
transProbsHOR <- modelHORflow %>% select(datetime, qUD, qUT, qDU, qDT, qTU, qTD) %>% mutate(junction="HOR")
transProbsTC <- modelTCflow %>% select(datetime, qUD, qUT, qDU, qDT, qTU, qTD) %>% mutate(junction="TC")
transProbs <- bind_rows(transProbsHOR, transProbsTC)

cat("Saving transition probabilities to", file.path(outputDir, "transProbs.csv"), "\n")
transProbs <- transProbs %>% pivot_longer(cols=qUD:qTD, names_to="transition", values_to="transProb") %>% 
    select(junction, datetime, transition, transProb) %>% arrange(datetime, junction, transition)
write.csv(transProbs, file=file.path(outputDir, "transProbs.csv"), row.names=F, quote=F)
