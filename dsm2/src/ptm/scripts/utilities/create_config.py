#!usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script to create a YAML config file from a CSV file containing a subset of the config variables
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

import os
import re
import yaml
from pyparsing import (Word, alphas, nums, alphanums, Suppress, ZeroOrMore, OneOrMore, Group, Optional, python_style_comment,
    Regex, Keyword, Literal, Optional, LineStart)
import pyparsing as pp
import numpy as np
import pandas as pd

class CreateConfig:
    
    def __init__(self, inputFileCSV, outputFileYAML):
        """Initialize CreateConfig object."""
        self.workingDir = os.path.dirname(os.path.realpath(__file__))
        
        self.inputFileCSV = inputFileCSV
        self.outputFileYAML = outputFileYAML

    def run(self):
        """Create output file."""
        
        self.writeOutput()
    
    def writeOutput(self):
        """Write YAML configuration file."""
        try:
            with open(self.outputFileYAML, "w") as fH:
                yaml.dump(self.config, fH, default_flow_style=None, indent=4, sort_keys=False)
                
            print("Finished converting config file.")
            print(f"Output: {self.outputFileYAML}")
        except:
            print(f"Could not write output to:{self.outputFile}. Aborting")
    
if __name__=="__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Utility to create a YAML config file from a CSV file containing a subset of the config variables")
    parser.add_argument("--inputFileCSV", action="store", dest="inputFileCSV", required=True)
    parser.add_argument("--outputFileYAML", action="store", dest="outputFileYAML", required=True)
    
    args = parser.parse_args()

    cC = CreateConfig(args.inputFileCSV, args.outputFileYAML)
    cC.run()
    