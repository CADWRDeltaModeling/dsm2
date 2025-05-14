# Spot check QA of files processed by tidefilePreprocessor.R
# Doug Jackson
# doug@QEDAconsulting.com
library(yaml)
####################################################################################################
# Constants
####################################################################################################
args <- commandArgs(trailingOnly=T)
if(length(args)==0) {
    cat("Reading hard-coded path to configuration file.\n")
    configFile <- "" # "/Users/djackson/Documents/QEDA/DWR/programs/dsm2_master/dsm2/dsm2/src/ptm/scripts/routingPreprocessor/config_preprocessors.yaml"
    workingDir <- "" # "/Users/djackson/Documents/QEDA/DWR/programs/dsm2_master/dsm2/dsm2/src/ptm/scripts/routingPreprocessor"
} else {
    cat("Reading path to configuration file as a command line argument\n")
    configFile <- args[1]
    workingDir <- getwd()
}
cat("Reading configuration from ", configFile, "\n")
config <- read_yaml(configFile)$tidefilePreprocessorQA

# Channels to check flow in
checkFlowUpChan <- c(31)
checkFlowDownChan <- c(25, 172)
checkFlowChan <- c(checkFlowUpChan, checkFlowDownChan)

####################################################################################################
# Install packages that aren't available through conda
####################################################################################################
cat("=========================================================\n")
cat("Running tidefilePreprocessor.R\n")
cat("---------------------------------------------------------\n")
cat("Installing packages that are not available through conda.\n")
if(config$runInCondaEnv) {
    # Use the library associated with the active R installation as the default.
    # This ensures that R is using packages installed in the current conda environment.
    .libPaths(R.home("library"))
}

# Check if rhdf5 is installed. If not, install it.
if(!require("rhdf5", quietly=T)) {
    cat("Installing rhdf5...\n")
    options(install.packages.compile.from.source="always")
    install.packages("BiocManager", repos="http://cran.us.r-project.org", quiet=T)
    BiocManager::install("rhdf5")
}
cat("Done installing packages that are not available through conda.\n")

library(rhdf5)
library(tidyverse)
library(ggplot2)

####################################################################################################
# Functions
####################################################################################################
# Load variables from config file after verifying they exist
loadVar <- function(varName) {
    if(is.null(config[[varName]])) {
        stop(paste(varName, "not found in configuration file."))
    }
    else {return (config[[varName]])}
}
####################################################################################################
# Run
####################################################################################################
setwd(workingDir)

outputDir <- file.path(workingDir, "output")
dir.create(outputDir, showWarnings=F, recursive=T)

origFile <- loadVar("origFile")
preprocessedFile <- loadVar("preprocessedFile")

########################################
# Check channel lengths and flows
origFH <- H5Fopen(origFile)
newFH <- H5Fopen(preprocessedFile)
f <- basename(preprocessedFile)

origChannelH <- origFH&'hydro/input/channel'
origChannel <- origChannelH[]
newChannelH <- newFH&'hydro/input/channel'
newChannel <- newChannelH[]

bothChannel <- right_join(origChannel, newChannel, by="chan_no", suffix=c("_orig", "_preprocessed"))

bothChannel <- bothChannel %>% select(chan_no, length_orig, length_preprocessed) %>% filter(length_preprocessed!=length_orig) %>% 
    pivot_longer(cols=-chan_no, names_to="version", values_to="length") %>% 
    mutate(chan_no=as.factor(chan_no))

p <- ggplot(bothChannel) + geom_bar(aes(x=chan_no, y=length, fill=version, group=version), 
                                    stat="identity", position="dodge", width=0.5) +
    labs(x="channel number", y="length") +
    theme_light()
ggsave(file.path(outputDir, paste0(f, "_channelLength.png")), width=8, height=6)

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
        ggsave(file.path(outputDir, paste0(f, "_flowUp_chan_", chan_no, ".png")), width=7, height=7)
    }                 

    if(chan_no %in% checkFlowDownChan) {
        p <- ggplot(thisFlowDown) + geom_point(aes(x=origFlowDown, y=newFlowDown)) +
            geom_abline(slope=1, intercept=0, color="red") +
            theme_light()
        ggsave(file.path(outputDir, paste0(f, "_flowDown_chan_", chan_no, ".png")), width=7, height=7)
        
    }

}

h5closeAll()

cat("QA output saved to", outputDir, "\n")