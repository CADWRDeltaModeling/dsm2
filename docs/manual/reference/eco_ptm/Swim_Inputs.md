# Swim Inputs

---

## Overview

Parameters that determine the swimming behavior of the fish. Some parameters apply to all locations within the Delta, and others vary by user-defined channel groups.

---

## Example

```plaintext
Swim_Inputs
Sunrise: 6:31
Sunset: 18:39 
# STST holding parameters
STST_Threshold: -10.0
Tidal_Cycles_to_Calculate_Channel_Direction: 2
Constant_Confusion_Probability: 1.04
Maximum_Confusion_Probability: 1.0
Confusion_Probability_Slope: -0.55
Random_Access: True
Access_Probability: 0.01
Channel_Groups
Swimming_Velocities
# rearing holding in hours
Group Name              Constant_Swimming_Velocity		Standard_Deviation_Particles	Standard_Deviation_Times	Rearing_Holding_Mean	Day_time_not_swim_percent
All                     -0.60796								0.53012								0.03448						0.0				0.60081
Riverine    			-0.13495								0.53012								0.03448						0.0				0.50269
Transitional			-0.27595								0.53012								0.03448						0.0				0.72489	
End_Swimming_Velocities 
Channel_List       
Riverine
700,701,702,703,410,412,413,414,415,416,417,
End_Riverine
Transitional
323,329,
330,331,332,333,334,335,336,337,338,339,
340,341,342,343,344,345,346,347,348,
350,351,352,353,354,355,356,357,358,359,
360,361,362,363,364,365,366,367,368,369,
370,371,372,373,374,375,376,377,378,379,
380,381,382,383,384,385,386,387,388,389,
390,391,392,393,394,395,396,397,398,399,
400,401,402,403,404,405,406,407,408,409,
418,419,
420,421,422,423,424,426,427,428,429,430,
549,
550,552,553,555,556,557,558,559,
End_Transitional
End_Channel_List
End_Channel_Groups
End_Swim_Inputs
```

## Field Descriptions

**Sunrise**

**Description:** time of sunrise; used to define daytime for the probability of diurnal swimming  
**Example:** Sunrise: 6:31

**Sunset**

**Description:** time of sunset: used to define daytime for the probability of diurnal swimming  
**Example:** Sunset: 18:39

**STST_Threshold**

**Description:** flow velocity threshold at which selective tidal stream transport (STST) holding will be activated (ft/sec). When flow velocity exceeds this threshold, the fish will hold position. Velocity is defined relative to the downstream direction, so a negative value should be used to specify an upstream flow threshold.  
**Example:** STST_Threshold: -10.0

**Tidal_Cycles_to_Calculate_Channel_Direction**

**Description:** the number of tidal cycles over which the net flow direction should be calculated when determining the apparent direction of flow in a channel  
**Example:** Tidal_Cycles_to_Calculate_Channel_Direction: 2

**Constant_Confusion_Probability**

**Description:** the half-saturation point of the logistic function defining the probability of confusion as a function of the signal to noise ratio of the flow. The half-saturation point is the point at which the function reaches half of its maximum value. Changing this number has the effect of shifting the function left and right.  
**Example:** Constant_Confusion_Probability: 1.04

**Maximum_Confusion_Probability**

**Description:** the maximum value of the logistic function defining the probability of confusion as a function of the signal to noise ratio of the flow  
**Example:** Maximum_Confusion_Probability: 1.0

**Confusion_Probability_Slope**

**Description:** the growth rate, or steepness, of the logistic function defining the probability of confusion as a function of the signal to noise ratio of the flow  
**Example:** Confusion_Probability_Slope: -0.55

**Random_Access**

**Description:** enable or disable random assessment of confusion. Setting this value to FALSE disables the probability of confusion feature.  
**Allowable entries:** Random_Access: TRUE or Random_Access: FALSE

**Access_Probability**

**Description:** probability of assessing confusion in a given 15-minute PTM time step  
**Example:** Access_Probability: 0.01

**Channel_Groups**

**Description:** specifies user-defined channel groups and swimming parameters that vary by channel group. The channel group name “ALL” is a special name that applies to all channels that are not associated with another user-defined channel group.

**Swimming_Velocities**

**Table:** swimming parameters that vary by channel group

**Group_Name**

**Description:** the user-specified name of the channel group  
**Example:** Riverine

**Constant_Swimming_Velocity**

**Description:** mean of the distribution from which each particle’s mean swimming velocity (ft/sec) will be drawn from in the channel group. Each channel group has a distribution of mean velocities associated with it. Each particle draws from this distribution to obtain its own unique mean velocity in the channels associated with this group. The actual velocity of the particle in a given time step is then drawn from a velocity distribution defined by the particle’s mean velocity and the Standard_Deviation_Times parameter.  
**Example:** -0.60796

**Standard_Deviation_Particles**

**Description:** standard deviation of the distribution from which each particle’s mean swimming velocity (ft/sec) will be drawn from in the channel group  
**Example:** 0.53012

**Standard_Deviation_Times**

**Description:** standard deviation of the distribution from which the particle’s swimming speed will be drawn from in each time step  
**Example:** 0.03448

**Rearing_Holding_Mean**

**Description:** mean holding time, in hours. The actual holding time for each particle and channel group will be drawn from an exponential distribution with a mean equal to Rearing_Holding_Mean.  
**Example:** 24.0

**Day_time_not_swim_percent**

**Description:** the probability that the fish will be inactive during any given time step during the daytime. A value of 0.0 means that the fish will never be inactive (hold) during the day. A value of 1.0 means that fish will always hold during the daytime hours.  
**Example:** 0.25

**Channel_List**

**Description:** list of channels associated with each channel group specified in the Swimming_Velocities table. Each channel group will be specified using a separate block. Channels that are not included in a user-defined channel group are automatically included in the ALL group; it is not necessary to explicitly specify the channels in the ALL group.

**<Channel_group>**

**Description:** list of channels associated with a channel group  
**Example keyword:** Riverine  
**Example entry:** 410,412,413,414,415,416,417,

---

STST_Threshold, Constant_Confusion_Probability, Confusion_Probability_Slope, Constant_Swimming_Velocity, Standard_Deviation_Particles, Standard_Deviation_Times, Rearing_Holding_Mean, and Day_time_not_swim_percent are all calibrated from field tag data. They should not be changed unless a new calibration is needed.


---

> ⚠️ **Warning:**  
> This block is required