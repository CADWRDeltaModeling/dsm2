#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Split transProbs file by year

Doug Jackson
doug@QEDAconsulting.com
"""
import os
import re
import numpy as np
import pandas as pd
import datetime as dt

inputFile = "/Users/djackson/Documents/QEDA/DWR/ECO_PTM_scenarios/ECO_PTM_calibration_08dec23/transProbs.csv"
startYear = 2009
endYear = 2017

transProbs = pd.read_csv(inputFile)

transProbs["dt"] = pd.to_datetime(transProbs["datetime"])
transProbs["year"] = [d.year for d in transProbs["dt"]]


for year in np.arange(startYear, endYear+1):
    print(f"Processing {year}")
    
    thisTransProbs = transProbs.loc[transProbs["year"]==year].copy()
    
    thisTransProbs = thisTransProbs[["junction", "datetime", "transition", "transProb"]]
    thisTransProbs.to_csv(inputFile.replace(".csv", f"_{year}.csv"), index=False)
    
    

