#!usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script to convert *.inp config files into YAML config file
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

class ConvertConfig:
    
    def __init__(self, DSM2configFile, PTMconfigFile, PTMbehaviorFile, outputFile):
        """Initialize ConvertConfig object."""
        self.workingDir = os.path.dirname(os.path.realpath(__file__))
        
        self.DSM2configFile = DSM2configFile
        self.PTMconfigFile = PTMconfigFile
        self.PTMbehaviorFile = PTMbehaviorFile
        self.outputFile = outputFile
    
    def run(self):
        """Run all conversion methods and create output file."""
        # Initialize config
        self.config = {}
        
        with open(self.DSM2configFile, "r") as fH:
            self.DSM2configTextOrig = fH.read()
        with open(self.PTMconfigFile, "r") as fH:
            self.PTMconfigTextOrig = fH.read()
        with open(self.PTMbehaviorFile, "r") as fH:
            self.behaviorTextOrig = fH.read()
            
        commentFilter = python_style_comment.suppress()
        self.DSM2configTextNoComment = commentFilter.transform_string(self.DSM2configTextOrig)
        self.PTMconfigTextNoComment = commentFilter.transform_string(self.PTMconfigTextOrig)
        self.behaviorTextNoComment = commentFilter.transform_string(self.behaviorTextOrig)
        
        self.subVariables()
        
        self.extractVariables()
        
        self.writeOutput()
    
    def subVariables(self):
        """Repeatedly substitute variables with defined values until all variables are resolved or we've exceeded 10 attempts."""
        
        foundVariables = True
        attempts = 0
        while foundVariables and attempts<10:
            # Find all variables, e.g., ${DSM2OUTPUTDIR}
            varNames = set()
            for t in [self.DSM2configTextNoComment, self.PTMconfigTextNoComment, self.behaviorTextNoComment]:
                varNames = varNames | set(re.findall(r"(?<=\$\{)\w+(?=\})", t))
            foundVariables = len(varNames)>0
                
            # Extract the variables
            self.variables = {}
            for v in varNames:
                for i, t in enumerate([self.DSM2configTextNoComment, self.PTMconfigTextNoComment, self.behaviorTextNoComment]):
                    val = self.extractVar(v, t)
                    
                    # If we've found the variable and it doesn't depend on another variable, save it
                    if val is not None and v not in self.variables and re.search(r"\$", val) is None:
                        self.variables[v] = val
                
            if "HYDROTIDEFILE" in self.variables:
                self.config["tidefile"] = self.variables["HYDROTIDEFILE"]
            
            # Perform variable substitutions
            for v in self.variables:
                self.DSM2configTextNoComment = re.sub(r"\$\{" + v + r"\}", self.variables[v], self.DSM2configTextNoComment)
                self.PTMconfigTextNoComment = re.sub(r"\$\{" + v + r"\}", self.variables[v], self.PTMconfigTextNoComment)
                self.behaviorTextNoComment = re.sub(r"\$\{" + v + r"\}", self.variables[v], self.behaviorTextNoComment)
            
            attempts+=1
    
    def parseScalars(self, scalars, varType, text):
        """Parse scalars and add to config."""
        for varName in scalars:
            parser = Group(Suppress(f"{scalars[varName]}" + Optional(":")) +
                           Word(alphanums + "-:._")(varName))
            result = parser.search_string(text)
            
            try:
                val = result[0][0][varName]
                
                if varType=="float":
                    val = float(val)
                elif varType=="int":
                    val = int(val)
                elif varType=="boolean":
                    val = {"TRUE":True, "FALSE":False, "T":True, "F":False}[val.upper()]
                    
                self.config[varName] = val
                
            except:
                self.reportFailure(varName)
                continue

    def parsePaths(self, paths, text):
        """Parse paths and add to config."""
        for varName in paths:
            parser = Group(Suppress(f"{paths[varName]}:") + 
                Word(alphanums + r'._\\/:')(varName))
            result = parser.search_string(text)
            
            try:
                val = result[0][0][varName]
                
                self.config[varName] = val
            except:
                self.reportFailure(varName)

    def extractVar(self, varName, text):
        """Extract variable and return."""
        parser = Group(LineStart() + Suppress(ZeroOrMore(" ")) + Suppress(varName + Optional(":")) +
                       Word(alphanums + "${}-:./\\_")(varName))
        result = parser.search_string(text)
        
        try:
            val = result[0][0][varName]
        except:
            val = None
        
        return val
    
    def extractVariables(self):
        """Extract variables from config file."""
        
        # particle_type
        try:
            particleTypeParser = Word(alphas + "_")
            particle_type_inputs = Suppress("Particle_Type_Inputs")
            end_particle_type_inputs = Suppress("End_Particle_Type_Inputs")
            
            parser = Group(Suppress("Particle_Type_Inputs") + 
                            particleTypeParser("particle_type") +
                            Suppress("End_Particle_Type_Inputs"))
            
            result = parser.search_string(self.behaviorTextNoComment)
            particle_type = result[0][0]["particle_type"]
            self.config["particle_type"] = particle_type
        except:
            self.reportFailure("particle_type")
        
        # time_zone
        try:
            parser = Group(Suppress("Time_Zone") + 
                            Word(alphas)("time_zone") +
                            Suppress("End_Time_Zone"))
            result = parser.search_string(self.behaviorTextNoComment)
            self.config["time_zone"] = result[0][0]["time_zone"]
        except:
            self.reportFailure("time_zone")
            
        # use_new_random_seed
        try:
            parser = Group(Suppress("Use_New_Random_Seed:") +
                            Word(alphas)("use_new_random_seed"))
            result = parser.search_string(self.behaviorTextNoComment)
            use_new_random_seed = {"YES":True, "NO":False}[result[0][0]["use_new_random_seed"].upper()]
            self.config["use_new_random_seed"] = use_new_random_seed
        except:
            self.reportFailure("use_new_random_seed")
    
        
        # travel_time
        try:
            outputPath = Suppress("Output_Path:") + Word(alphanums + r'._\\/:')
            header = Group(Word(alphanums) + Word(alphanums + "/") + Word(alphanums) + Word(alphanums + "_"))
            row = Group(Word(alphanums)("nodeID") + Word(alphanums)("waterbody") + 
                        Word(alphanums)("distance") + Word(alphanums)("stationName"))
            parser = Group(Suppress("Travel_Time_Output") + 
                            outputPath("outputPath") + header("header") + 
                            OneOrMore(row)("rows") + 
                            Suppress("End_Travel_Time_Output"))
            result = parser.search_string(self.behaviorTextNoComment)
        
            self.config["travel_time_output_path"] = result[0][0]["outputPath"][0]
            travelTimeHeader = list(result[0][0]["header"])
            self.config["travel_time_header"] = list(result[0][0]["header"])
            
            travelTime = pd.DataFrame(result[0][0]["rows"], columns=travelTimeHeader)
            for i in [0, 1, 2]:
                travelTime[travelTimeHeader[i]] = travelTime[travelTimeHeader[i]].astype("int")
            
            self.config["travel_time"] = travelTime.values.tolist()
        except:
            for v in ["travel_time_output_path", "travel_time_header", "travel_time"]:
                self.reportFailure(v)
            
        # release_groups
        try:
            releaseLocHeader = Group(Word(alphanums) + Word(alphanums + "/") + Word(alphanums) + Word(alphanums + "_"))
            releaseLoc = Group(Word(nums)("nodeID") + Word(alphanums)("waterbody") + Word(nums)("distance") + Word(alphanums + "_")("stationName"))
            releaseHeader = Group(Word(alphanums + "_") + Word(alphanums + "_") + Word(alphanums + "_") + Word(alphanums + "_"))
            releaseRow = Group(Word(alphanums + "/")("date") + Word(alphanums + ":")("time") + Word(nums)("number") + Word(alphas)("style"))
            releaseGroup = Group(Word(alphanums + "_")("name") + 
                                  releaseLocHeader("release_loc_header") + releaseLoc("release_loc") + 
                                  releaseHeader("releases_header") + OneOrMore(releaseRow)("releases") + 
                                  Regex("End_.+"))
            parser = Group(Suppress("Fish_Release_Inputs") +
                            Suppress("Number_Of_Release_Groups:") + Word(nums) +
                            OneOrMore(releaseGroup)("releaseGroups") + 
                            Suppress("End_Fish_Release_Inputs"))
            result = parser.search_string(self.behaviorTextNoComment)
            
            numRelGroups = len(result[0][0]["releaseGroups"])
            relGroups = list()
            for i in range(numRelGroups):
                thisResult = result[0][0]["releaseGroups"][i]
                releaseLoc = list(thisResult["release_loc"])
                for j in range(3):
                    releaseLoc[j] = int(releaseLoc[j])
                thisRelGroup = {"name":thisResult["name"],
                                "release_loc_header":list(thisResult["release_loc_header"]),
                                "release_loc":releaseLoc,
                                "releases_header":list(thisResult["releases_header"])}
                thisReleases = list()
                for j in range(len(thisResult["releases"])):
                    thisRelease = thisResult["releases"][j]
                    thisReleases.append([thisRelease["date"], thisRelease["time"], int(thisRelease["number"]), thisRelease["style"]])
                thisRelGroup["releases"] = thisReleases        
                relGroups.append(thisRelGroup)
            self.config["release_groups"] = relGroups
        except:
            self.reportFailure("release_groups")
        
        # Swim inputs
        self.parseScalars({"sunrise":"Sunrise", "sunset":"Sunset"}, "string", self.behaviorTextNoComment)
        self.parseScalars({"stst_threshold":"STST_Threshold"}, "float", self.behaviorTextNoComment)
        self.parseScalars({"tidal_cycles_to_calculate_channel_direction":"Tidal_Cycles_to_Calculate_Channel_Direction"}, "int", 
                     self.behaviorTextNoComment)
        self.parseScalars({"confusion_probability_constant":"Constant_Confusion_Probability",
                              "max_confusion_probability":"Maximum_Confusion_Probability",
                              "confusion_probability_slope":"Confusion_Probability_Slope"}, "float", 
                     self.behaviorTextNoComment)
        self.parseScalars({"random_assess":"Random_Access"}, "boolean", self.behaviorTextNoComment)
        self.parseScalars({"assess_probability":"Access_Probability"}, "float", self.behaviorTextNoComment)
        self.parseScalars({"stuck_threshold":"Stuck_Threshold"}, "int", self.behaviorTextNoComment)
        
        # swimming_vel
        try:
            header = Group(Word(alphas + "_")*6)
            row = Group(Word(alphas) + Word(nums + ".")*5)
            parser = Group(Suppress("Swimming_Velocities") + 
                                  header("header") + OneOrMore(row)("rows") + 
                                  Suppress("End_Swimming_Velocities"))
            result = parser.search_string(self.behaviorTextNoComment)
            swimmingVelHeader = list(result[0][0]["header"])
            self.config["swimming_vel_header"] = swimmingVelHeader
            swimmingVel = pd.DataFrame(result[0][0]["rows"], columns=swimmingVelHeader).reset_index(drop=True)
            for col in swimmingVelHeader[1:]:
                swimmingVel[col] = swimmingVel[col].astype("float")
            self.config["swimming_vel"] = np.array(swimmingVel).tolist()
        except:
            for v in ["swimming_vel_header", "swimming_vel"]:
                self.reportFailure(v)
        
        # channel_groups
        try:
            channelGroup = Word(alphanums + "_") + Group(OneOrMore(Word(nums + ","))) + Suppress(Literal("End") + Word(alphas + "_"))
            parser = Suppress("Channel_List") + OneOrMore(channelGroup)
            result = parser.search_string(self.behaviorTextNoComment)
            self.config["channel_groups"] = list()
            for i in np.arange(1, len(result[0]), 2):
                index = int(i)
                thisChannels = list(filter(None, "".join(result[0][index]).split(",")))
                thisChannels = [int(c) for c in thisChannels]
                thisChanGroup = {"name":result[0][index-1],
                                  "channels":thisChannels}
                self.config["channel_groups"].append(thisChanGroup)
        except:
            self.reportFailure("channel_groups")
            
        # Route inputs
        self.parsePaths({"output_path_entrainment":"Output_Path_Entrainment", "trans_probs_path":"TransProbsPath",
                            "output_path_flux":"Output_Path_Flux"}, self.behaviorTextNoComment)
        try:
            # channel_name_lookup
            self.config["channel_name_lookup_header"] = ["Name", "ChanneID"]
            channelName = Group(Suppress("(") + Word(alphas + "_") + Suppress(",") + Word(nums + "_") + Suppress(")") + 
                                Suppress(Optional(",")))
            parser = Group(Suppress("Channel_Name_Look_Up:") + OneOrMore(channelName)("channelNames"))
            result = parser.search_string(self.behaviorTextNoComment)
            self.config["channel_name_lookup"] = list()
            for i in range(len(result[0][0]["channelNames"])):
                thisChanName = list(result[0][0]["channelNames"][i])
                thisChanName[1] = int(thisChanName[1])
                self.config["channel_name_lookup"].append(thisChanName)
        except:
            for v in ["channel_name_lookup_header", "channel_name_lookup"]:
                self.reportFailure(v)
            
        # special_behavior
        try:
            header = Group(Word(alphas + "_")*3)
            row = Group(Word(nums) + Word(alphanums) + Word(nums + "-"))
            parser = Group(Suppress("Special_Behaviors") + 
                            Suppress("Channel_Name_Look_Up:") + OneOrMore(channelName) + 
                            header("header")  + OneOrMore(row)("rows") + 
                            Suppress("End_Special_Behaviors"))
            result = parser.search_string(self.behaviorTextNoComment)
            specialBehaviorHeader = list(result[0][0]["header"])
            self.config["special_behavior_header"] = specialBehaviorHeader
            specialBehaviors = pd.DataFrame(result[0][0]["rows"], columns=specialBehaviorHeader).reset_index(drop=True)
            specialBehaviors[specialBehaviorHeader[0]] = specialBehaviors[specialBehaviorHeader[0]].astype("int")
            specialBehaviors[specialBehaviorHeader[2]] = specialBehaviors[specialBehaviorHeader[2]].astype("int")
            self.config["special_behavior"] = np.array(specialBehaviors).tolist()
        except:
            for v in ["special_behavior_header", "special_behavior"]:
                self.reportFailure(v)
        
        # barriers
        try:
            barrierName = Word(alphanums + "_")
            barrierLocHeader = Group(Word(alphanums) + Word(alphanums + "/") + Word(alphanums))
            barrierLoc = Group(Word(nums) + Word(nums))
            scheduleHeader = Group(Word(alphas) + Word(alphas) + Word(alphas + "/"))
            scheduleRow = Group(Word(alphanums + "/") + Word(nums + ":") + Word(nums))
            barrier = Group(barrierName("name") + barrierLocHeader("barrierLocHeader") + barrierLoc("barrierLoc") +
                            scheduleHeader("scheduleHeader") + OneOrMore(scheduleRow)("rows") + Word(alphanums + "_"))
            parser = Group(Suppress("Barriers") + 
                            Suppress("Number_of_barriers:") + Word(nums) +
                            OneOrMore(barrier)("barriers") +
                            Suppress("End_Barriers"))
            result = parser.search_string(self.behaviorTextNoComment)
            
            numBarriers = len(result[0][0]["barriers"])
            barriers = list()
            for i in range(numBarriers):
                thisResult = result[0][0]["barriers"][i]
                thisBarrier = {"name":thisResult["name"],
                                "nodeID":int(thisResult["barrierLoc"][0]),
                                "waterbodyID":int(thisResult["barrierLoc"][1]),
                                "schedule_header":list(thisResult["scheduleHeader"])}
                thisSchedule = list()
                for j in range(len(thisResult["rows"])):
                    thisRow = thisResult["rows"][j]
                    thisRow[2] = int(thisRow[2])
                    thisSchedule.append(list(thisRow))
                thisBarrier["schedule"] = thisSchedule
                barriers.append(thisBarrier)
            self.config["barriers"] = barriers
        except:
            self.reportFailure("barriers")
                    
        # dicu_filter_efficiency
        try:
            parser = Group(Suppress("DICU_Filter") + 
                            Suppress("Filter_Efficiency:") + Word(nums + ".")("dicu_filter_efficiency") +
                            Suppress("End_DICU_Filter"))
            result = parser.search_string(self.behaviorTextNoComment)
            self.config["dicu_filter_efficiency"] = float(result[0][0]["dicu_filter_efficiency"])
        except:
            self.reportFailure("dicu_filter_efficiency")
        
        # fish_screens
        try:
            header = Group(Word(alphanums) + Word(alphanums + "/"))
            row = Group(Word(nums) + Word(nums))
            parser = Group(Suppress("Fish_Screens") + 
                            header("header") +
                            OneOrMore(row)("rows") + 
                            Suppress("End_Fish_Screens"))
            result = parser.search_string(self.behaviorTextNoComment)
            fishScreensHeader = list(result[0][0]["header"])
            self.config["fish_screens_header"] = fishScreensHeader
            fishScreens = pd.DataFrame(result[0][0]["rows"], columns=fishScreensHeader).reset_index(drop=True)
            fishScreens = fishScreens.astype("int")
            self.config["fish_screens"] = np.array(fishScreens).tolist()
        except:
            for v in ["fish_screens_header", "fish_screens"]:
                self.reportFailure(v)
        
        # survival_groups
        try:
            stationPair = Group(Word("( ") + Word(alphanums + "_")("channel") + Word(" ,") + Word(nums + "-")("distance") + Word(") ,"))
            survOutputPath = Group(Suppress("Output_Path:") + Word(alphanums + r'._\\/:')("path"))
            row = Group(Word(alphanums + "_")("endStation") + Word(nums + ".-")("lambda") + Word(nums + ".-")("omega") + Word(nums + ".-")("x"))
            survGroup = Group(Word(alphanums + "_") + Word("Name:") + Word(alphanums + "_")("name") + 
                                  Suppress("Start_Station:") + stationPair("startStation") + 
                                  Suppress("End_Station:") + OneOrMore(stationPair)("endStations") +
                                  Optional(Suppress("Exchangeable_Start_Station:") + ZeroOrMore(stationPair)("exchangeableStartStations")) +
                                  (Word(alphas + "_")*4)("header") + OneOrMore(row)("rows") +
                                  Word(alphanums + "_"))
            parser = Group(Suppress("Survival_Parameters") + 
                            survOutputPath("survOutputPath") + 
                            Word("Number_of_Survival_Calculation_Group:") + Word(nums) +
                            OneOrMore(survGroup)("survGroups"))
            result = parser.search_string(self.behaviorTextNoComment)
            self.config["survival_output_path"] = result[0][0]["survOutputPath"]["path"]
            
            # Read the expected number of survival groups
            expectedSurvGroups = int(self.extractVar("Number_of_Survival_Calculation_Group", self.behaviorTextNoComment))
            numSurvGroups = len(result[0][0]["survGroups"])
            if numSurvGroups!=expectedSurvGroups:
                print("-"*75)
                print(f"WARNING: number of survival groups extracted ({numSurvGroups}) doesn't match expected number ({expectedSurvGroups})")
                print("Please check the original survival groups for special characters that may not be matched by the parser.")
                print("You may need to edit the converted file to add the missing groups")
                print("-"*75)
            survGroups = list()
            for i in range(numSurvGroups):
                thisResult = result[0][0]["survGroups"][i]
                thisEndStations = list()
                for j in range(len(thisResult["endStations"])):
                    thisEndStations.append([thisResult["endStations"][j]["channel"], int(thisResult["endStations"][j]["distance"])])
                
                thisSurvGroup = {"number":i+1, "name":thisResult["name"],
                                  "start_stations": [[thisResult["startStation"]["channel"], int(thisResult["startStation"]["distance"])]],
                                  "end_stations":thisEndStations}
                if len(thisResult["exchangeableStartStations"])>0:
                    thisExchStartStations = []
                    for j in range(len(thisResult["exchangeableStartStations"])):
                        thisExchStartStations.append([thisResult["exchangeableStartStations"][j]["channel"], 
                                                      int(thisResult["exchangeableStartStations"][j]["distance"])])
                    thisSurvGroup["exchangeable_start_stations"] = thisExchStartStations
                survParamsHeader = list(thisResult["header"])
                thisSurvGroup["survival_params_header"] = survParamsHeader
                survParams = pd.DataFrame(thisResult["rows"], columns=survParamsHeader).reset_index(drop=True)
                for param in survParamsHeader[1:]:
                    survParams[param] = survParams[param].astype("float")
                thisSurvGroup["survival_params"] = np.array(survParams).tolist()
                    
                survGroups.append(thisSurvGroup)
            self.config["survival_groups"] = survGroups
        except:
            self.reportFailure("survival_groups")
        
        # simulation
        self.parseScalars({"simulation_start_date":"Simulation_Start_Date", "simulation_scenario":"Simulation_Scenario"}, "string",
                      self.behaviorTextNoComment)
        
        # particle_flux_header
        try:
            header = Group(Word(alphas + "_")*3)
            row = Group(Word(alphanums + "_")("name") + Word(nums)("fromGroup") + Word(nums)("toGroup"))
            parser = Group(Suppress("Particle_Flux") + 
                            header("header") +
                            ZeroOrMore(row)("rows") +
                            Suppress("End_Particle_Flux"))
            result = parser.search_string(self.behaviorTextNoComment)
            particleFluxHeader = list(result[0][0]["header"])
            self.config["particle_flux_header"] = particleFluxHeader
            if len(result[0][0]["rows"])>0:
                particleFlux = pd.DataFrame(result[0][0]["rows"], columns=particleFluxHeader).reset_index(drop=True)
                for param in particleFluxHeader[1:]:
                    particleFlux[param] = particleFlux[param].astype("int")
                
                self.config["particle_flux"] = np.array(particleFlux).tolist()
        except:
            for v in ["particle_flux_header", "particle_flux"]:
                self.reportFailure(v)
        
        # individual_route_survival
        try:
            header = Group(Word(alphas) + Word(alphas + "_"))
            row = Group(Word(alphanums + "_")("name") + Word(nums + "(), ")("groupList"))
            parser = Group(Suppress("Individual_Route_Survival") +
                            header("header") + 
                            ZeroOrMore(row)("rows") + 
                            Suppress("End_Individual_Route_Survival"))
            result = parser.search_string(self.behaviorTextNoComment)
            indRouteSurvHeader = list(result[0][0]["header"])
            if len(result[0][0]["rows"])>0:
                indRouteSurv = pd.DataFrame(result[0][0]["rows"], columns=indRouteSurvHeader).reset_index(drop=True)
                self.config["individual_route_survival_header"] = indRouteSurvHeader
                self.config["individual_route_survival"] = np.array(indRouteSurv).tolist()
        except:
            for v in ["individual_route_survival_header", "individual_route_survival"]:
                self.reportFailure(v)
        
        # These variables are not present in the *.inp config files
        self.config["show_route_survival_detail"] = False
        
        # route_survival_equations
        try:
            # These equations may include "#" => need to use behaviorTextOrig
            header = Group(Word(alphanums) + Word(alphanums + "_"))
            row = Group(Word(alphanums + "_")("name") + Word(alphanums + "*()[]{}<>/#._+")("equation"))
            parser = Group(Suppress("Route_Survival_Equations") +
                            header("header") +
                            ZeroOrMore(row, stop_on="End_Route_Survival_Equations")("rows"))
            result = parser.search_string(self.behaviorTextOrig)
            routeSurvEqsHeader = list(result[0][0]["header"])
            self.config["route_survival_equations_header"] = routeSurvEqsHeader
            if len(result[0][0]["rows"])>0:
                routeSurvEqs = pd.DataFrame(result[0][0]["rows"], columns=routeSurvEqsHeader).reset_index(drop=True)
                self.config["route_survival_equations"] = np.array(routeSurvEqs).tolist()
        except:
            for v in ["route_survival_equations_header", "route_survival_equations"]:
                self.reportFailure(v)
            
        # individual_reach_survival
        try:
            header = Group(Word(alphanums + "_") + Word(alphanums))
            row = Group(Word(alphanums + "_") + Word(nums + "."))
            parser = Group(Suppress("Individual_Reach_Survival") +
                            header("header") + 
                            ZeroOrMore(row, stop_on="End_Individual_Reach_Survival")("rows"))
            result = parser.search_string(self.behaviorTextNoComment)
            indReachSurvHeader = list(result[0][0]["header"])
            self.config["individual_reach_survival_header"] = indReachSurvHeader
            if len(result[0][0]["rows"])>0:
                indReachSurv = pd.DataFrame(result[0][0]["rows"], columns=indReachSurvHeader).reset_index(drop=True)
                indReachSurv[indReachSurvHeader[1]] = indReachSurv[indReachSurvHeader[1]].astype("float")
                self.config["individual_reach_survival"] = np.array(indReachSurv).tolist()
        except:
            for v in ["individual_reach_survival_header", "individual_reach_survival"]:
                self.reportFailure(v)
        
        # These variables are not present in the *.inp config files
        self.config["exit_stations"] = ["MAL"]
        
        self.parseScalars({"display_simulation_timestep_write_all":"Display_Simulation_Timestep_Write_All",
                              "flux_write_all":"Flux_Write_All",
                              "entrainment_write_all":"Entrainment_Write_All",
                              "survival_write_all":"Survival_Write_All",
                              "route_survival_write_all":"Route_Surv_Write_All",
                              "fates_write_all":"Fates_Write_All"}, "boolean", self.behaviorTextNoComment)

        # survival_detail_write_all is not present in the *.inp config files
        self.config["survival_detail_write_all"] = True
        
        self.parseScalars({"ptm_start_date":"PTM_START_DATE"}, "string", self.DSM2configTextNoComment)
        self.config["ptm_start_time"] = "0000"
        self.parseScalars({"ptm_end_date":"END_DATE", "ptm_end_time":"END_TIME"}, "string", self.DSM2configTextNoComment)
        self.parseScalars({"ptm_time_step":"ptm_time_step",
                              "display_intvl":"display_intvl"}, "string", self.PTMconfigTextNoComment)
        self.parseScalars({"theta":"theta"}, "float", self.PTMconfigTextNoComment)
        
        for param in ["ptm_ivert", "ptm_itrans", "ptm_iey", "ptm_iez"]:
            self.parseScalars({param:param}, "boolean", self.PTMconfigTextNoComment)
        
        # These variables are not present in the *.inp config files
        self.config["ptm_iprof"] = False
        self.config["ptm_igroup"] = False
        
        for param in ["ptm_flux_percent", "ptm_group_percent", "ptm_flux_cumulative"]:
            self.parseScalars({param:param}, "boolean", self.PTMconfigTextNoComment)
        
        self.parseScalars({"ptm_random_seed":"ptm_random_seed"}, "int", self.PTMconfigTextNoComment)
        
        for param in ["ptm_trans_constant", "ptm_vert_constant", "ptm_trans_a_coef", "ptm_trans_b_coef",
                      "ptm_trans_c_coef"]:
            self.parseScalars({param:param}, "float", self.PTMconfigTextNoComment)
            
        self.parseScalars({"ptm_num_animated":"ptm_no_animated"}, "int", self.PTMconfigTextNoComment)    
        
        # particle_group_output
        try:
            header = Group(Word(alphas + "_")*4)
            row = Group(Word(alphanums + "_")("name") + Word(alphanums + "_")("groupName") + Word(alphanums) + Word(alphanums + r"._\\/:${}"))
            parser = Group(Suppress("PARTICLE_GROUP_OUTPUT") + 
                          header("header") +
                          ZeroOrMore(row, stop_on="END")("rows"))
            result = parser.search_string(self.PTMconfigTextNoComment)
            particleGroupHeader = list(result[0][0]["header"])
            if len(result[0][0]["rows"])>0:
                particleGroups = pd.DataFrame(result[0][0]["rows"], columns=particleGroupHeader).reset_index(drop=True)
                particleGroups = particleGroups[particleGroupHeader[0:2]]
                self.config["particle_group_output_header"] = ["name", "groupName"]
                self.config["particle_group_output"] = np.array(particleGroups).tolist()
        except:
            for v in ["particle_group_output_header", "particle_group_output"]:
                self.reportFailure(v)
        
        # particle_flux_output
        try:
            header = Group(Word(alphas + "_")*5)
            row = Group(Word(alphanums + "_")("name") + Word(alphanums + "_:")("from") + Word(alphanums + "_:")("to") + 
                        Word(alphanums) + Word(alphanums + r"._\\/:${}"))
            parser = Group(Suppress("PARTICLE_FLUX_OUTPUT") +
                            header("header") + 
                            ZeroOrMore(row, stop_on="END")("rows"))
            result = parser.search_string(self.PTMconfigTextNoComment)
            particleFluxHeader = list(result[0][0]["header"])
            if len(result[0][0]["rows"])>0:
                particleFlux = pd.DataFrame(result[0][0]["rows"], columns=particleFluxHeader).reset_index(drop=True)
                particleFlux[["from_wb_type", "from_wb"]] = particleFlux["FROM_WB"].str.split(":", expand=True)
                particleFlux[["to_wb_type", "to_wb"]] = particleFlux["TO_WB"].str.split(":", expand=True)
                particleFlux = particleFlux[["NAME", "from_wb", "from_wb_type", "to_wb", "to_wb_type"]]
                self.config["particle_flux_output_header"] = ["name", "from_wb", "from_wb_type", "to_wb", "to_wb_type"]
                self.config["particle_flux_output"] = np.array(particleFlux).tolist()
        except:
            for v in ["particle_flux_output_header", "particle_flux_output"]:
                self.reportFailure(v)
        
        # groups
        try:
            header = Group(Word(alphas + "_")*3)
            row = Group(Word(alphanums + "_")("name") + Word(alphas)("type") + Word(alphanums + "_()|*."))
            parser = Group(Suppress("GROUP_MEMBER") + 
                            header("header") + 
                            ZeroOrMore(row, stop_on="END")("rows"))
            result = parser.search_string(self.PTMconfigTextNoComment)
            groupsHeader = list(result[0][0]["header"])
            if len(result[0][0]["rows"])>0:
                groups = pd.DataFrame(result[0][0]["rows"], columns=["name", "type", "pattern"]).reset_index(drop=True)
                groups["type"] = [t.lower() for t in groups["type"]]
                groups.loc[groups["type"]=="channel", "type"] = "chan"
                groups.loc[groups["type"]=="reservoir", "type"] = "res"
                self.config["groups_header"] = ["name", "type", "pattern"]
                self.config["groups"] = np.array(groups).tolist()
        except:
            for v in ["groups_header", "groups"]:
                self.reportFailure(v)
            
        # io_file
        try:            
            header = Group(Word(alphas)*5)
            row = Group(Word(alphas)("model") + Word(alphas)("type") + Word(alphas)("io") + 
                        Word(alphanums)("interval") + Word(alphanums + '"{}$/._')("file"))
            parser = Group(Suppress("IO_FILE") + 
                            header("header") + 
                            ZeroOrMore(row, stop_on="END")("rows"))
            result = parser.search_string(self.PTMconfigTextNoComment)
            if len(result[0][0]["rows"])>0:
                ioFiles = []
                for i in range(len(result[0][0]["rows"])):
                    # Only use trace file from *.inp config
                    thisType = result[0][0]["rows"][i]["type"]
                    if thisType=="trace":
                        thisFile = {"type":result[0][0]["rows"][i]["type"],
                                    "interval":result[0][0]["rows"][i]["interval"],
                                    "file":result[0][0]["rows"][i]["file"]}
                        ioFiles.append(thisFile)
                # Add new output files with default paths
                ioFiles.append({"type":"flux", "interval":"1hour", "file":"./output/ptm_out.ncd"})
                ioFiles.append({"type":"survival", "interval":"none", "file": "./output/ptm_out.ncd"})
                ioFiles.append({"type":"echoConfig", "interval":"none", "file": "./output/echoConfig.yaml"})
                ioFiles.append({"type":"echoConfigNetCDF", "interval":"none", "file": "./output/ptm_out.ncd"})
                ioFiles.append({"type":"routeSurvival", "interval":"none", "file": "./output/routeSurvival.csv"})
                ioFiles.append({"type":"fates", "interval":"none", "file": "./output/fates.csv"})
                ioFiles.append({"type":"survDetail", "interval":"none", "file": "./output/survivalDetail.csv"})

                self.config["io_file"] = ioFiles     
        except:
            self.reportFailure("io_file")
        
    
    def reportFailure(self, varName):
        """Report a failed extraction and set variable to FAILED TO EXTRACT"""
        print(f"Could not extract {varName}. Skipping.")
        self.config[varName] = "FAILED TO EXTRACT"
        
    def writeOutput(self):
        """Write extracted configuration to output file."""
        try:
            with open(self.outputFile, "w") as fH:
                yaml.dump(self.config, fH, default_flow_style=None, indent=4, sort_keys=False)
                
            print("Finished converting config file.")
            print(f"Output: {self.outputFile}")
        except:
            print(f"Could not write output to:{self.outputFile}. Aborting")
    
if __name__=="__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Utility to convert *.inp configuration files for ECO-PTM to YAML.")
    parser.add_argument("--DSM2configFile", action="store", dest="DSM2configFile", required=True)
    parser.add_argument("--PTMconfigFile", action="store", dest="PTMconfigFile", required=True)
    parser.add_argument("--PTMbehaviorFile", action="store", dest="PTMbehaviorFile", required=True)
    parser.add_argument("--outputFile", action="store", dest="outputFile", required=True)
    
    args = parser.parse_args()
    
    
    
    cC = ConvertConfig(args.DSM2configFile, args.PTMconfigFile, args.PTMbehaviorFile, args.outputFile)
    cC.run()
    