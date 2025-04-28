"""Script to do some basic post-processing of ECO-PTM outputs
"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"
__version__ = "0.1"
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
        fluxFiles (list) -- list of paths to the netCDF flux output files
        """
        print("="*80)

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
                
    def processSurvival(self, survivalFile):
        """Process survival output

        Keyword arguments:
        survivalFile (str) -- path to the netCDF survival output file
        """
        print("="*80)
        print(f"Processing survival output in {survivalFile}")

        dat = xr.open_dataset(survivalFile)
        surv = dat["surv"].to_dataframe().reset_index()
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

        if "survDetails" in dat:
            survDetails = dat["survDetails"].to_dataframe().reset_index()
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

        dat = xr.open_dataset(echoConfigNetCDF)
        for varName in ["tidefile", "particle_type", "time_zone", "use_new_random_seed", "travel_time_output_path",
                        "sunrise", "sunset", "random_assess", "output_path_entrainment", "trans_probs_path",
                        "output_path_flux", "survival_output_path", "simulation_start_date", "show_route_survival_detail",
                        "route_survival_output_path", "display_simulation_timestep_write_all", "flux_write_all",
                        "entrainment_write_all", "survival_write_all", "ptm_start_date", "ptm_start_time",
                        "ptm_end_date", "ptm_end_time", "ptm_time_step", "display_intvl", "ptm_ivert",
                        "ptm_itrans", "ptm_iey", "ptm_iez", "ptm_iprof", "ptm_igroup", "ptm_flux_percent", 
                        "ptm_group_percent", "ptm_flux_cumulative"]:
            self.printVal(dat, varName)
        
        # Scalars
        for varName in ["stst_threshold", "tidal_cycles_to_calculate_channel_direction", "confusion_probability_constant",
                        "max_confusion_probability", "confusion_probability_slope", "assess_probability", "stuck_threshold",
                        "dicu_filter_efficiency", "theta", "ptm_random_seed", "ptm_trans_constant", "ptm_vert_constant", 
                        "ptm_trans_a_coef", "ptm_trans_b_coef", "ptm_trans_c_coef", "ptm_num_animated"]:
            self.printScalar(dat, varName)
        
        # Arrays 
        for varName in ["travel_time", "release_groups:release_loc", "release_groups:releases", "swimming_vel",
                        "channel_groups", "channel_name_lookup", "special_behavior", "barriers", "fish_screens",
                        "survival_groups:start_stations", "survival_groups:end_stations", "survival_groups:exchangeable_start_stations",
                        "survival_groups:survival_params", "particle_flux", "individual_route_survival", "route_survival_equations",
                        "individual_reach_survival", "particle_group_output", "particle_flux_output", "groups", "io_file"]:
            print("-"*80)
            thisVar = self.decodeDF(dat[varName])
            print(thisVar)

        exitStations = [c.decode("utf8") for c in dat["exit_stations"].to_pandas()]
        print(f"exit_stations: {exitStations}")

    def printVal(self, dat, varName):
        """Read a value from the output and print it"""
        try:
            thisVal = dat[varName].item().decode("utf8")
            print(f"{varName}: {thisVal}")
        except:
            print(f"Unable to read {varName}")
    
    def printScalar(self, dat, varName):
        """Read a scalar value from the output and print it"""
        try:
            thisVal = np.double(dat[varName])
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

    if  config["processSurvival"]:
        p.processSurvival(config["survivalFile"])
    
    if config["echoConfig"]:
        p.printConfig(config["echoConfigNetCDF"])