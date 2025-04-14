# Survival Inputs

---

## Overview

Parameters of the XT survival model for each user-specified survival group (reach).

---

## Example

```plaintext
Survival_Inputs
Survival_Parameters
Output_Path: .\output\survival-01-16-2013.csv
Number_of_Survival_Calculation_Group: 10
# group 0 in Russ's report from Sacramento to Freeport
Group_1
Name: Sac to Fpt
# station is defined by (channel number, distance from upstream node)
# the group number for a start station will be used as the key to search for the calculation group 
# Lambda and X in feet, Omega in feet/second
Start_Station: (412,0)	
End_Station: (415,0)		
Exchangeable_Start_Station:	
Lambda		Omega			X
1208005.29	0.558198472		65616.8
End_Group_1	
# group 1 in Russ's report from Freeport to Sutter/Steamboat Sloughs
...
End_Survival_Parameters 
End_Survival_Inputs
```

---

## Field Descriptions

**Output_Path**

**Description:** the path to the survival output file. Note that Windows path separators (\) should be used. To specify paths relative to the working directory from which the ECO-PTM was launched, use a period followed by the relative path.  
**Example:** Output_Path: .\output\survival-01-01-2011.csv

**Number_of_Survival_Calculation_Group**

**Description:** the number of defined groups (reaches) that will be used for survival calculations  
**Example:** Number_of_Survival_Calculation_Group: 10

**Group_<x>**

**Example keyword:** Group_1

**Name**

**Description:** the name of the reach. This is used to label the outputs in the output file.  
**Example:** Name: Sac to Fpt

**Start_Station**

**Description:** location of the station, specified using a channel number and a distance (feet) from the upstream node of the channel. Only a single start station can be specified, and the start stations must be unique for each reach. Only a single station per channel is allowed.  
**Example:** Start_Station: (412,0)

**End_Station**

**Description:** the location(s) of one or more end stations, specified using a channel number and a distance (feet) from the upstream node of the channel. Multiple end stations can be specified, and end stations may be shared with multiple reaches. The end station must be in a different channel from the start station, and only a single station per channel is allowed.  
**Example:** End_Station: (365,0), (423,0), (366,0)

**Exchangeable_Start_Station**

**Description:** to accommodate particles that begin traveling through a reach but then subsequently enter another reach before passing one of the original reach’s end stations, the user can define one or more exchangeable start stations for each reach. If a particle passes one of the exchangeable start stations before passing an end station, its survival will be calculated for the new reach instead of the original reach. However, the travel time clock will not be reset when the exchangeable start station is passed, i.e., the travel time will be calculated from the original reach’s start station.  
**Example:** Exchangeable_Start_Station: (379,951), (383,0)

**Table**

**Description:** specify the parameter values for the XT survival model

**Lambda**

**Description:** the mean free path (feet) between predator encounters  
**Example:** 1208005.29

**Omega**

**Description:** the random encounter velocity (ft/sec)  
**Example:** 0.558198472

**X**

**Description:** the length (feet) of the reach  
**Example:** 65616.8

---

---

> ⚠️ **Warning:**  
>  This block is optional


