#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: djackson
"""
workingDir = r"C:\Users\admin\Documents\QEDA\DWR\programs\ECO_PTM_SouthDelta\dsm2\src\ptm\scripts\utilities"

inputFileCSV = "exampleCreateConfigInput.csv"
outputFileYAML = r"C:\Users\admin\Documents\QEDA\DWR\ECO_PTM_runs\test_createConfig_17apr25\ptmConfig.yaml"

from create_config import CreateConfig

cC = CreateConfig(inputFileCSV, outputFileYAML)
cC.run()