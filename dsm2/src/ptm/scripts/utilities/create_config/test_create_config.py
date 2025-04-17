#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: djackson
"""
import os
workingDir = r"C:\Users\admin\Documents\QEDA\DWR\programs\ECO_PTM_SouthDelta\dsm2\src\ptm\scripts\utilities\create_config"

inputFileCSV = "exampleCreateConfigInput.csv"
outputDir = r"C:\Users\admin\Documents\QEDA\DWR\ECO_PTM_runs\test_createConfig_17apr25"

from create_config import CreateConfig

cC = CreateConfig(inputFileCSV, outputDir)
cC.run()