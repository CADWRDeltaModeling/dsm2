#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 28 10:58:58 2025

@author: djackson
"""
workingDir = r"C:\Users\admin\Documents\QEDA\DWR\programs\ECO_PTM_SouthDelta\dsm2\src\ptm\scripts\utilities\convert_config"

DSM2configFile = r"C:\Users\admin\Documents\QEDA\DWR\ECO_PTM_runs\test_master_FortranIO_24oct24\dsm2_config.inp"
PTMconfigFile = r"C:\Users\admin\Documents\QEDA\DWR\ECO_PTM_runs\test_master_FortranIO_24oct24\ptm_config.inp"
PTMbehaviorFile = r"C:\Users\admin\Documents\QEDA\DWR\ECO_PTM_runs\test_master_FortranIO_24oct24\ptm_behavior_inputs.inp"
outputFile = r"C:\Users\admin\Documents\QEDA\DWR\ECO_PTM_runs\test_convertConfig_17apr25\ptmConfig.yaml"

from convert_config import ConvertConfig

cC = ConvertConfig(DSM2configFile, PTMconfigFile, PTMbehaviorFile, outputFile)
cC.run()