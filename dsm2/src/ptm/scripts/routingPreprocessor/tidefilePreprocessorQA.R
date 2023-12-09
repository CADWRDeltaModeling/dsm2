# Spot check QA of files processed by tidefilePreprocessor.R
# Doug Jackson
# doug@QEDAconsulting.com
library(rhdf5)
library(tidyverse)
library(ggplot2)

####################################################################################################
# Constants
####################################################################################################
workingDir <- "/Users/djackson/Documents/QEDA/DWR/programs/routingPreprocessor"

# Location of original files that haven't been preprocessed
origDir <- "/Users/djackson/Documents/QEDA/DWR/ECO_PTM_scenarios/ECO_PTM_calibration_08dec23/v1.1.88.4_byYear_08dec23_orig"

# Location of preprocessed files
newDir <- "/Users/djackson/Documents/QEDA/DWR/ECO_PTM_scenarios/ECO_PTM_calibration_08dec23/v1.1.88.4_byYear_08dec23_post_tidefilePreprocessor"

# Channels to check flow in
checkFlowUpChan <- c(31)
checkFlowDownChan <- c(25, 172)
checkFlowChan <- c(checkFlowUpChan, checkFlowDownChan)
####################################################################################################
# Run
####################################################################################################
setwd(workingDir)
outputDir <- file.path(newDir, "QA")
dir.create(outputDir, showWarnings=F)

newFiles <- list.files(origDir, pattern="*.h5")

########################################
# Check channel lengths and flows
for(f in newFiles) {
    
    origFH <- H5Fopen(file.path(origDir, f))
    newFH <- H5Fopen(file.path(newDir, f))
    
    origChannelH <- origFH&'hydro/input/channel'
    origChannel <- origChannelH[]
    newChannelH <- newFH&'hydro/input/channel'
    newChannel <- newChannelH[]
    
    bothChannel <- right_join(origChannel, newChannel, by="chan_no", suffix=c("_orig", "_new"))
    
    bothChannel <- bothChannel %>% select(chan_no, length_orig, length_new) %>% filter(length_new!=length_orig) %>% 
        pivot_longer(cols=-chan_no, names_to="version", values_to="length")
    
    p <- ggplot(bothChannel) + geom_point(aes(x=chan_no, y=length, color=version, group=version)) +
        labs(x="channel number", y="length") +
        theme_light()
    ggsave(file.path(outputDir, paste0(f, "_channelLength.png")))
    
    # Load channel flows
    cat("Reading flow data.\n")
    origFlowH <- origFH&"hydro/data/channel flow"
    origFlow <- origFlowH[]
    newFlowH <- newFH&"hydro/data/channel flow"
    newFlow <- newFlowH[]
    
    for(chan_no in checkFlowChan) {
        origChanInd <- which(origChannel$chan_no==chan_no)
        newChanInd <- which(newChannel$chan_no==chan_no)
        
        thisOrigFlowUp <- origFlow[1, origChanInd, ]
        thisOrigFlowDown <- origFlow[2, origChanInd, ]
        thisNewFlowUp <- newFlow[1, newChanInd, ]
        thisNewFlowDown <- newFlow[2, newChanInd, ]
        
        thisFlowUp <- data.frame(t=seq(1, length(thisOrigFlowUp)),
                               origFlowUp=thisOrigFlowUp,
                               newFlowUp=thisNewFlowUp)
        thisFlowDown <- data.frame(t=seq(1, length(thisOrigFlowDown)),
                                   origFlowDown=thisOrigFlowDown,
                                   newFlowDown=thisNewFlowDown)
              
        if(chan_no %in% checkFlowUpChan) {
            p <- ggplot(thisFlowUp) + geom_point(aes(x=origFlowUp, y=newFlowUp)) +
                geom_abline(slope=1, intercept=0, color="red") +
                theme_light()
            ggsave(file.path(outputDir, paste0(f, "_flowUp_chan_", chan_no, ".png")))
        }                 

        if(chan_no %in% checkFlowDownChan) {
            p <- ggplot(thisFlowDown) + geom_point(aes(x=origFlowDown, y=newFlowDown)) +
                geom_abline(slope=1, intercept=0, color="red") +
                theme_light()
            ggsave(file.path(outputDir, paste0(f, "_flowDown_chan_", chan_no, ".png")))
            
        }

    }
    
    h5closeAll()
}