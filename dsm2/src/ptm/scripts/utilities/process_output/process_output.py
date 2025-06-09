"""script to do some basic post-processing of eco-ptm outputs
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@qedaconsulting.com"
import os
import sys
import xarray as xr
import pandas as pd
import argparse
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime as dt
from datetime import timedelta
import yaml

class ProcessOutput:

    def __init__(self):
        """Initialize a ProcessOutput object""" 
        self.figWidth = 6
        self.figHeight = 6
    
    def createFluxDat(self, fluxOutputDir, fluxFiles, fluxSimLoc, fluxDatLocs, fluxDatDays):
        """Create dat file of flux outputs
        
        Keyword arguments:
        fluxOutputDir (str) -- full path to the directory where the dat file should be created
        fluxFiles (list) -- list of paths to the netCDF flux output files
        fluxSimLoc (str) -- insertion location
        fluxDatLocs (list) -- list of flux locations
        fluxDatDays (list) -- list of integer days from simulation start to record flux
        """
        print("="*80)

        # Make sure the output directory exists
        os.makedirs(fluxOutputDir, exist_ok=True)

        # Read the scenario from the first file
        try:
            ds = xr.open_dataset(fluxFiles[0])
            scenario = ds["simulation_scenario"].item().decode("utf8")
            ds.close()
        except:
            scenario = "NA"

        for days in fluxDatDays:

            outputFile = os.path.join(fluxOutputDir, f"ptm_fate_results_{days}day.dat")

            with open(outputFile, "w") as fH:
                print(f"Saving flux output to {outputFile}")
                print(f"{days}-day PTM Output - {scenario}", file=fH)
                header = "SimPeriod,SimLoc"
                for loc in fluxDatLocs:
                    header+=f",{loc.upper()}"
                print(header, file=fH)

            for fluxFile in fluxFiles:
                print(f"Creating *.dat flux output file using outputs in {fluxFile}")

                ds = xr.open_dataset(fluxFile)

                nodeFlux = ds["nodeFlux"].to_pandas()
                nodeFlux.columns = [c.decode("utf8") for c in nodeFlux.columns]
                nodeFlux.index = [i.decode("utf8") for i in nodeFlux.index]
                
                nodes = nodeFlux.columns
                nodeFlux["datetime"] = [dt.strptime(d, "%m/%d/%Y %H:%M:%S") for d in nodeFlux.index]
                
                groupFlux = ds["groupFlux"].to_pandas()
                groupFlux.columns = [c.decode("utf8") for c in groupFlux.columns]
                groupFlux.index = [i.decode("utf8") for i in groupFlux.index]
                
                groups = groupFlux.columns
                groupFlux["datetime"] = [dt.strptime(d, "%m/%d/%Y %H:%M:%S") for d in groupFlux.index]
                
                ds.close()

                flux = pd.merge(nodeFlux, groupFlux, on="datetime", how="outer")
                flux["datetime"] = pd.to_datetime(flux["datetime"])
                startDatetime = flux["datetime"].min()
                flux["daysFromStart"] = [(d - startDatetime).total_seconds()/timedelta(days=1).total_seconds() for d in flux["datetime"]]

                thisFlux = flux[flux["daysFromStart"]>=days].iloc[0]

                with open(outputFile, "a") as fH:
                    row = f"{dt.strftime(startDatetime, '%d%b%Y').upper()}, {fluxSimLoc}"
                    for loc in fluxDatLocs:
                        try:
                            row+=f", {thisFlux[loc.upper()]}"
                        except:
                            row+=","
                    print(row, file=fH)
                
    def createSurvDat(self, survOutputDir, survFiles, survDatLocs):
        """Create dat file of survival estimates
        
        Keyword arguments:
        survOutputDir (str) -- full path to the directory where the dat file should be created
        survFiles (list) -- list of patsh to the netCDF survival output files
        survDatLocs (list) -- list of survival estimates to include
        """
        print("="*80)

        # Make sure the output directory exists
        os.makedirs(survOutputDir, exist_ok=True)

        outputFile = os.path.join(survOutputDir, "eco-ptm_survival.dat")

        with open(outputFile, "w") as fH:
            print(f"Saving survival output to {outputFile}")
            header = "Date,Scenario"
            for loc in survDatLocs:
                header+=f",{loc}"
            print(header, file=fH)

        for survFile in survFiles:
            print(f"Creating *.dat survival output file using outputs from {survFile}")

            ds = xr.open_dataset(survFile)

            try:
                startDate = ds["ptm_start_date"].item().decode("utf8")
            except:
                startDate = "NA"

            try:
                scenario = ds["simulation_scenario"].item().decode("utf8")
            except:
                scenario = "NA"

            surv = ds["surv"].to_dataframe().reset_index()
            surv["survGroup"] = [s.decode("utf8") for s in surv["survGroup"]]

            ds.close()

            with open(outputFile, "a") as fH:
                row = f"{startDate},{scenario}"
                for loc in survDatLocs:
                    try:
                        thisSurv = surv.loc[surv["survGroup"]==loc, "surv"].values[0]
                    except:
                        print(f"Could not read survival for {loc}")
                        thisSurv = "NA"
                    row+=f",{thisSurv}"
                print(row, file=fH)

    def processSurvival(self, survivalFile):
        """Process survival output

        Keyword arguments:
        survivalFile (str) -- path to the netCDF survival output file
        """
        print("="*80)
        print(f"Processing survival output in {survivalFile}")

        ds = xr.open_dataset(survivalFile)
        surv = ds["surv"].to_dataframe().reset_index()
        surv["survGroup"] = [s.decode("utf8") for s in surv["survGroup"]]

        outputPath = os.path.join(os.path.dirname(survivalFile), "surv.csv")
        surv.to_csv(outputPath, index=False)
        print(f"Saved survival to {outputPath}")

        fig, ax = plt.subplots(figsize=[self.figWidth, self.figHeight])
        ax.bar(surv["survGroup"], surv["surv"])
        ax.set_xlabel("survival group")
        ax.set_ylabel("survival fraction")
        outputPath = os.path.join(os.path.dirname(survivalFile), "surv.png")
        plt.savefig(outputPath)
        print(f"Saved survival plot to {outputPath}")

        if "survDetails" in ds:
            survDetails = ds["survDetails"].to_dataframe().reset_index()
            survDetails["survDetailsKey"] = [s.decode("utf8") for s in survDetails["survDetailsKey"]]
            survDetails["survDetails"] = [s.decode("utf8") for s in survDetails["survDetails"]]
            survDetails[["component", "variable"]] = survDetails["survDetailsKey"].str.split("-", expand=True)
            survDetails = survDetails.rename(columns={"survDetails":"value"})
            survDetails = survDetails[["component", "variable", "value"]]
            survDetails.sort_values(by=["component", "variable"], inplace=True)
            outputPath = os.path.join(os.path.dirname(survivalFile), "survDetails.csv")
            survDetails.to_csv(outputPath, index=False)
            print(f"Saved survival details to {outputPath}")

    def printConfig(self, echoConfigNetCDF):
        """Print configuration values

        Keyword arguments:
        echoConfigNetCDF (str) -- path to the netCDF echoConfig output file 
        """
        print("="*80)
        print(f"Echoing configuration values in {echoConfigNetCDF}")

        ds = xr.open_dataset(echoConfigNetCDF)
        for varName in ["tidefile", "particle_type", "time_zone", "use_new_random_seed", "travel_time_output_path",
                        "sunrise", "sunset", "random_assess", "output_path_entrainment", "trans_probs_path",
                        "output_path_flux", "survival_output_path", "simulation_start_date", "show_route_survival_detail",
                        "route_survival_output_path", "display_simulation_timestep_write_all", "flux_write_all",
                        "entrainment_write_all", "survival_write_all", "ptm_start_date", "ptm_start_time",
                        "ptm_end_date", "ptm_end_time", "ptm_time_step", "display_intvl", "ptm_ivert",
                        "ptm_itrans", "ptm_iey", "ptm_iez", "ptm_iprof", "ptm_igroup", "ptm_flux_percent", 
                        "ptm_group_percent", "ptm_flux_cumulative"]:
            self.printVal(ds, varName)
        
        # Scalars
        for varName in ["stst_threshold", "tidal_cycles_to_calculate_channel_direction", "confusion_probability_constant",
                        "max_confusion_probability", "confusion_probability_slope", "assess_probability", "stuck_threshold",
                        "dicu_filter_efficiency", "theta", "ptm_random_seed", "ptm_trans_constant", "ptm_vert_constant", 
                        "ptm_trans_a_coef", "ptm_trans_b_coef", "ptm_trans_c_coef", "ptm_num_animated"]:
            self.printScalar(ds, varName)
        
        # Arrays 
        for varName in ["travel_time", "release_groups:release_loc", "release_groups:releases", "swimming_vel",
                        "channel_groups", "channel_name_lookup", "special_behavior", "barriers", "fish_screens",
                        "survival_groups:start_stations", "survival_groups:end_stations", "survival_groups:exchangeable_start_stations",
                        "survival_groups:survival_params", "particle_flux", "individual_route_survival", "route_survival_equations",
                        "individual_reach_survival", "particle_group_output", "particle_flux_output", "groups", "io_file"]:
            print("-"*80)
            thisVar = self.decodeDF(ds[varName])
            print(thisVar)

        exitStations = [c.decode("utf8") for c in ds["exit_stations"].to_pandas()]
        print(f"exit_stations: {exitStations}")

    def printVal(self, ds, varName):
        """Read a value from the output and print it"""
        try:
            thisVal = ds[varName].item().decode("utf8")
            print(f"{varName}: {thisVal}")
        except:
            print(f"Unable to read {varName}")
    
    def printScalar(self, ds, varName):
        """Read a scalar value from the output and print it"""
        try:
            thisVal = np.double(ds[varName])
            print(f"{varName}: {thisVal}")
        except:
            print(f"Unable to read {varName}")

    def decodeDF(self, dF):
        """Decode all elements of a data frame from UTF-8"""
        dF = dF.to_pandas()
        dF.columns = [c.decode("utf8") for c in dF.columns]
        dF = dF.map(lambda x: x.decode("utf-8") if isinstance(x, bytes) else x)
        dF.reset_index(inplace=True)
        return dF

if __name__=="__main__":
    import argparse
    import yaml 

    # Read in command line arguments
    parser = argparse.ArgumentParser(description="Script to perform basic post-processing of ECO-PTM output.")
    parser.add_argument("--configFile", action="store", dest="configFile", required=True)
    args = parser.parse_args()

    p = ProcessOutput()

    configFile = args.configFile

    # Read YAML configuration file
    try:
        with open(configFile) as fH:
            config = yaml.safe_load(fH)
    except IOError as e:
        print(f"Could not load configuration file {configFile}. Does it exist? {e}")
        sys.exit()
    except yaml.YAMLError as e:
        print(f"Error while parsing process_output configuration file: {e}")
        sys.exit()

    if config["createFluxDat"]:
        p.createFluxDat(config["fluxOutputDir"], config["fluxFiles"], config["fluxSimLoc"], config["fluxDatLocs"], config["fluxDatDays"])
    
    if config["createSurvDat"]:
        p.createSurvDat(config["survOutputDir"], config["survFiles"], config["survDatLocs"])

    if  config["processSurvival"]:
        p.processSurvival(config["survivalFile"])
    
    if config["echoConfig"]:
        p.printConfig(config["echoConfigNetCDF"])