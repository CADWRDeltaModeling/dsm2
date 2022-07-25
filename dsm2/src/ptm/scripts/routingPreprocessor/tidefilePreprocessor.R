# Alter grid topology (connectivity) in a DSM2 *.h5 tidefile so it can be used 
# with the ECO-PTM South Delta routing model
# Doug Jackson
# doug@QEDAconsulting.com
library(rhdf5)
library(tidyverse)

####################################################################################################
# Constants
####################################################################################################
workingDir <- "/Users/djackson/Documents/QEDA/DWR/programs/routingPreprocessor"

tidefile <- "/Users/djackson/Documents/QEDA/DSM2_tideFiles/routing_model_dsm2_output_15apr22/routing_model_dsm2_15apr22.h5"

# Specify list of channels and nodes in which flow should be zero (external channel and node numbers).
# These are used to balance flows in orphaned nodes and channels
zeroFlowChannels <- c()
zeroFlowNodes <- c()

####################################################################################################
# Run
####################################################################################################
setwd(workingDir)

# Save original attributes
qfAttrs <- h5readAttributes(tidefile, "hydro/data/channel flow")
cfAttrs <- h5readAttributes(tidefile, "hydro/data/channel flow")
h5closeAll()

h5f <- H5Fopen(tidefile)

########################################
# Modify connectivity
# Load the channel modifications
modifyChannel <- read.csv("modifyChannel.csv")
cutEnds <- modifyChannel %>% select(chan_no, length, cutEnd) %>% filter(cutEnd!="")
modifyChannel$cutEnd <- NULL

# Read the original channel information
channelH <- h5f&'hydro/input/channel'
channel <- channelH[]

if(nrow(modifyChannel)>0) {
    # Determine which nodes might have changed connectivity; only include nodes whose connectivity has changed
    origChannel <- channel[which(channel$chan_no %in% modifyChannel$chan_no), ]
    allNodes <- unique(c(origChannel$upnode, origChannel$downnode, modifyChannel$upnode, modifyChannel$downnode))
    modNodes <- c()
    for(i in 1:length(allNodes)) {
        thisNode <- allNodes[i]
        if(!(identical(unique(origChannel$chan_no[origChannel$upnode==thisNode]), unique(modifyChannel$chan_no[modifyChannel$upnode==thisNode]))
             & identical(unique(origChannel$chan_no[origChannel$downnode==thisNode]), unique(modifyChannel$chan_no[modifyChannel$downnode==thisNode])))) {
            modNodes[length(modNodes)+1] <- thisNode
        }
    }
    
    # Modify connectivity, channel lengths, etc.
    for(i in 1:nrow(modifyChannel)) {
        thisMod <- modifyChannel[i, ]
        row <- which(channel$chan_no==thisMod$chan_no)
        channel[row, ] <- thisMod
    }
}

channel$length <- as.integer(channel$length)

h5delete(h5f, "hydro/input/channel")
h5write(channel, file=h5f, name="hydro/input/channel")

########################################
# Interpolate and mass balance flows

# Load external flows
extFlowNames <- h5f&"hydro/geometry/external_flow_names"
extFlowNames <- gsub(" ", "", extFlowNames[])
extFlow <- h5f&"hydro/data/qext flow"
extFlow <- extFlow[]

# Load channel flows
flow <- h5f&"hydro/data/channel flow"
flow <- flow[]

if(nrow(modifyChannel)>0) {
    # Calculate channel flow interpolation factor at cut location
    interpFac <- left_join(cutEnds, origChannel, by="chan_no", suffix=c("_new", "_orig"))
    interpFac$interpFac <- interpFac$length_new/interpFac$length_orig
    interpFac <- interpFac %>% select(chan_no, cutEnd, interpFac)
    
    # Apply interpolation factor
    for (i in 1:nrow(interpFac)) {
        thisInterpFac <- interpFac[i, ]
        chanInd <- which(channel$chan_no==thisInterpFac$chan_no)
        interpFlow <- flow[1, chanInd, ] + (flow[2, chanInd, ] - flow[1, chanInd, ])*thisInterpFac$interpFac
        if(thisInterpFac$cutEnd=="upNode") {
            flow[1, chanInd, ] <- interpFlow
        } else if(thisInterpFac$cutEnd=="downNode") {
            flow[2, chanInd, ] <- interpFlow
        } else {
            cat("Incorrect cutEnd in modifyChannel.csv:", thisInterpFac$cutEnd, "\n")
        }
    }
    
    # Set dicu flows at a node equal to the sum of all channel flows
    for (node in modNodes) {
        upChan <- channel %>% filter(upnode==node)
        downChan <- channel %>% filter(downnode==node)
        upInd <- which(channel$chan_no %in% upChan$chan_no)
        downInd <- which(channel$chan_no %in% downChan$chan_no)
        
        upFlow <- -flow[1, upInd, , drop=F]
        downFlow <- flow[2, downInd, , drop=F]
        sumUpFlow <- apply(upFlow, 3, sum)
        sumDownFlow <- apply(downFlow, 3, sum)
        
        # Obtain dicu_drain and dicu_div flows, if present
        divName <- paste0("dicu_div_", node)
        drainName <- paste0("dicu_drain_", node)
        dicuFlow <- divFlow <- drainFlow <- numeric(length(sumDownFlow))
        if(divName %in% extFlowNames) {
            divFlowInd <- which(extFlowNames==divName)
            divFlow <- extFlow[divFlowInd, ]
        }
        if(drainName %in% extFlowNames) {
            drainFlowInd <- which(extFlowNames==drainName)
            drainFlow <- extFlow[drainFlowInd, ]
        }
        dicuFlow <- divFlow + drainFlow
        
        sumFlow <- sumUpFlow + sumDownFlow + dicuFlow
        
        # Set seep flow equal to the sum of the channel outflows
        seepName <- paste0("dicu_seep_", node)
        if(seepName %in% extFlowNames) {
            extFlowInd <- which(extFlowNames==seepName)
            extFlow[extFlowInd, ] <- -sumFlow
        } else {
            cat("No dicu_seep for node", node, "=> could not use seep to adjust mass balance.\n")
        }
    }
}

# Zero net DICU flow in specified nodes
for(thisNode in zeroFlowNodes) {
    # Obtain dicu_drain and dicu_div flows, if present
    divName <- paste0("dicu_div_", thisNode)
    drainName <- paste0("dicu_drain_", thisNode)
    dicuFlow <- divFlow <- drainFlow <- numeric(dim(extFlow)[2])
    if(divName %in% extFlowNames) {
        divFlowInd <- which(extFlowNames==divName)
        divFlow <- extFlow[divFlowInd, ]
    }
    if(drainName %in% extFlowNames) {
        drainFlowInd <- which(extFlowNames==drainName)
        drainFlow <- extFlow[drainFlowInd, ]
    }
    dicuFlow <- divFlow + drainFlow
    
    sumFlow <- dicuFlow
    
    # Set seep flow equal to the sum of the DICU flows
    seepName <- paste0("dicu_seep_", thisNode)
    if(seepName %in% extFlowNames) {
        extFlowInd <- which(extFlowNames==seepName)
        extFlow[extFlowInd, ] <- -sumFlow
    } else {
        cat("No dicu_seep for node", node, "=> could not use seep to zero net DICU flows.\n")
    }
}

h5delete(h5f, "hydro/data/qext flow")
h5createDataset(file=h5f, dataset="hydro/data/qext flow", 
                dims = dim(extFlow), storage.mode="double", 
                chunk=c(nrow(extFlow), 16), level=6)
cat("Writing modified qext flow data\n")
h5write(extFlow, file=h5f, name="hydro/data/qext flow")

# Add original attributes back to qext
qfD <- H5Dopen(h5f, "hydro/data/qext flow")
for(thisAttr in names(qfAttrs)) {
    h5writeAttribute(attr=qfAttrs[[thisAttr]], h5obj=qfD, name=thisAttr)
}
H5Dclose(qfD)

# Zero flow in specified channels
for(thisChannel in zeroFlowChannels) {
    index <- which(channel$chan_no==thisChannel)
    flow[, index, ] <- 0
}

# Load channel balance relationships
balanceChannels <- read.csv(file.path(workingDir, "balanceChannels.csv"))
for(adjChannel in unique(balanceChannels$adjust_chan_no)) {
    cat("Mass balance by adjusting flows in channel", adjChannel, "\n")
    thisAdj <- balanceChannels %>% filter(adjust_chan_no==adjChannel)
    thisOutUpNode <- thisAdj %>% filter(balance_chan_connect=="upNode")
    thisOutDownNode <- thisAdj %>% filter(balance_chan_connect=="downNode")
    
    # Sum flows at upNodes of outflow channels
    outUpNodeInd <- which(channel$chan_no %in% thisOutUpNode$balance_chan_no)
    outUpNodeFlow <- flow[1, outUpNodeInd, , drop=F]
    sumOutUpNodeFlow <- apply(outUpNodeFlow, 3, sum)
    
    outDownNodeInd <- which(channel$chan_no %in% thisOutDownNode$balance_chan_no)
    outDownNodeFlow <- flow[2, outDownNodeInd, , drop=F]
    sumOutDownNodeFlow <- apply(outDownNodeFlow, 3, sum)
    
    # Set flow at downnode of inflow channel equal to the sum of the outflow
    inInd <- which(channel$chan_no==adjChannel)
    flow[2, inInd, ] <- sumOutUpNodeFlow - sumOutDownNodeFlow
}

h5delete(h5f, "hydro/data/channel flow")
h5createDataset(file=h5f, dataset="hydro/data/channel flow", 
                dims = dim(flow), storage.mode="double", 
                chunk=c(2, dim(flow)[2], 16), level=6)
cat("Writing modified channel flow data\n")
h5write(flow, file=h5f, name="hydro/data/channel flow")

# Add original attributes back to channel flow
cfD <- H5Dopen(h5f, "hydro/data/channel flow")
for(thisAttr in names(cfAttrs)) {
    h5writeAttribute(attr=cfAttrs[[thisAttr]], h5obj=cfD, name=thisAttr)
}
H5Dclose(cfD)

h5closeAll()
