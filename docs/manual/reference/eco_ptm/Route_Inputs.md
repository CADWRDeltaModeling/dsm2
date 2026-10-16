# Route Inputs
---

## Overview

Parameters related to the configuration and reporting of the routing model.

---

## Example

```plaintext
Route_Inputs
Output_Path_Entrainment: 
Output_Path_Flux: 
Special_Behaviors
Channel_Name_Look_Up: (GS,366),(SACUPGS,422),(SACDOWNGS,423),(DCC,365),(SACUPDCC,421),(SACUPSUT,418),(SACDOWNSUT,419),(SUT,379),(STM,383),(SACDOWNSTM,420)
# Users can modify calculated entrainment rates. 100 means no change, and 150 means calculated_chance * 1.5.
# For STM & SUT, if pct < 100, pct is added (e.g., 10 means calculated_chance + 0.1).
NodeID      Class_Name                      Percent_Increase_Decrease
343         SalmonGSJRouteBehavior          100
339         SalmonSutterJRouteBehavior      100
340         SalmonSTMJRouteBehavior         100
342         SalmonDCCRouteBehavior          100
End_Special_Behaviors

Barriers
Number_of_barriers: 1
Barrier1
NodeID          ChannelID/ReservoirName/Obj2objName   Distance
343             366
Date        Time    On/Off
1/1/1989    0:00     0
12/31/2017  21:20    0
End_Barrier1
End_Barriers

DICU_Filter
Filter_Efficiency: 0.0
End_DICU_Filter
End_Route_Inputs
```

---

## Field Descriptions

**Output_Path_Entrainment**

**Description:** The path to the entrainment output file that records flows and entrainment probabilities for all particles that pass through the key junctions (Steamboat Slough, Sutter Slough, DCC, and Georgiana Slough). Note that Windows path separators (\) should be used. To specify paths relative to the working directory from which the ECO-PTM was launched, use a period followed by the relative path.  
**Example:** `Output_Path_Entrainment: .\output\entrainment-01-01-2011.csv`

**Special_Behaviors**

**Channel_Name_Look_Up**

**Description:** Used to specify a lookup table so the routing model can use channel names in place of channel numbers. The lookup is specified using a comma-separated list of (name, channel number) pairs within parentheses.  
**Example:** `Channel_Name_Look_Up: (GS,366),(SACUPGS,422),(SACDOWNGS,423),(DCC,365),(SACUPDCC,421),(SACUPSUT,418),(SACDOWNSUT,419),(SUT,379),(STM,383),(SACDOWNSTM,420)`

**Table: Specify an amount by which to increase or decrease the calculated entrainment probability at a given node**

**NodeID**  
**Description:** The node at which to increase or decrease entrainment.  
**Example:** `343`

**Class_Name**  
**Description:** The class name that the adjustment will be applied to. This must exactly match one of the existing route behavior classes in the ECO-PTM package.  
**Allowable entries (case-sensitive):** `SalmonGSJRouteBehavior`, `SalmonSutterJRouteBehavior`, `SalmonSTMJRouteBehavior`, or `SalmonDCCRouteBehavior`

**Percent_Increase_Decrease**  
**Description:** A multiplier that adjusts the calculated entrainment probability, as follows: If this number is less than 100, the adjusted probability is equal to calculated*(Percent_Increase_Decrease/100). If this number is greater than 100, the meaning depends on the junction. In the Georgiana Slough and DCC junctions, adjusted = calculated*(Percent_Increase_Decrease/100). In the Steamboat Slough and Sutter Slough junctions, adjusted = calculated+(Percent_Increase_Decrease/100-1). In other words, a value of 150 would result in a relative increase of 50% at Georgiana Slough and DCC but an absolute increase of 50% at Steamboat Slough and Sutter Slough.  
**Example:** `150`

**Barriers**

**Description:** Define barriers and a schedule during which the barriers will be active. Barriers are used to alter the coefficients of the critical streakline model, which was originally derived in order to capture the effects of a bio-acoustic fish fence (BAFF). Currently, the barrier is only used for the Georgiana Slough routing model.

**Number of barriers**

**Description:** The number of barriers that will be defined.  
**Example:** `Number_of_barriers: 1`

**Barrier<x>**

**Example keyword:** `Barrier1`

**Table: Location of the barrier**

**NodeID**  
**Description:** The upstream node of the channel in which the barrier is located.  
**Example:** `343`

**ChannelID/ReservoirName/Obj2objName**  
**Description:** The channel within which the barrier is located.  
**Example:** `366`

**Distance**  
**Description:** Not used.

**Table: Barrier schedule**

**Date**  
**Description:** The date at which the barrier will change from its previous state to the new state. The range between the first and last dates in the schedule must encompass the complete ECO-PTM scenario.  
**Example:** `1/1/1989`

**Time**  
**Description:** The time at which the barrier will change from its previous state to the new state.  
**Example:** `21:20`

**On/Off**  
**Description:** The new state. 0 = off, 1 = on.  
**Allowable entries:** `0` or `1`

**DICU_Filter**

**Description:** Specify whether particles should be allowed to enter Delta Island Consumptive Use (DICU) boundaries (agricultural diversions).

**Filter_Efficiency**

**Description:** Probability that a particle will enter a DICU boundary if that route is chosen. 0.0 means no particles will enter the DICU and 1.0 means there is no restriction on particles entering an agricultural diversion.  
**Example:** `Filter_Efficiency: 0.0`


> ⚠️ **Warning:**  
> **This block is optional**


