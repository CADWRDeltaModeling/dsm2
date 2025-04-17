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
import pandas as pd

class CreateConfig:
    
    def __init__(self, inputFileCSV, outputDir):
        """Initialize CreateConfig object."""
        self.workingDir = os.path.dirname(os.path.realpath(__file__))
        
        self.inputFileCSV = inputFileCSV
        self.outputDir = outputDir

    def run(self):
        """Create output file."""
        os.makedirs(self.outputDir, exist_ok=True)

        # Read config file templates
        with open("ptmConfigTemplate_SouthDelta.yaml", "r") as fH:
            templateSouthDelta = fH.read()
        with open("ptmConfigTemplate_NorthDelta.yaml", "r") as fH:
            templateNorthDelta = fH.read()
        
        # Read input file
        self.inputs = pd.read_csv(self.inputFileCSV)

        configSouthDelta = self.replacePlaceholders(templateSouthDelta)
        configNorthDelta = self.replacePlaceholders(templateNorthDelta)
        
        with open(os.path.join(self.outputDir, "ptmConfig_SouthDelta.yaml"), "w") as fH:
            print(configSouthDelta, end="", file=fH)
        with open(os.path.join(self.outputDir, "ptmConfig_NorthDelta.yaml"), "w") as fH:
            print(configNorthDelta, end="", file=fH)

    def replacePlaceholders(self, template):
        """Replace placeholders in template with specified values"""
        template = self.replacePlaceholder(template, "TIDEFILE_PLACEHOLDER", "tidefile")

        return template

    def replacePlaceholder(self, template, placeholder, variable):
        """Replace single placeholder in template with specified value."""
        try:
            template = template.replace(placeholder, self.inputs.loc[self.inputs["variable"]==variable, "value"].values[0]) 
        except:
            print(f"Could not find {variable} in inputs. Leaving placeholder in output files")
        
        return template

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
    parser.add_argument("--outputDir", action="store", dest="outputDir", required=True)
    
    args = parser.parse_args()

    cC = CreateConfig(args.inputFileCSV, args.outputDir)
    cC.run()
    