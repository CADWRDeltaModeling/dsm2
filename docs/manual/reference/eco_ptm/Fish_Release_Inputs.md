# Fish Release Inputs

---

## Overview

This parameter provides particle release information. It defines where, when, and how many particles will be released in a simulation.

---

## Example

```plaintext
Fish_Release_Inputs
Number_Of_Release_Groups:1
Group_1
NodeID  ChannelID/ReservoirName/Obj2objName   Distance       Station_Name  
332     412                                   0              Sacramento
Release_Date    Release_Time    Particle_Number Release_Style
03/18/2012      1:00            100             Random
03/18/2012      1:15            100             Random
03/18/2012      1:30            100             Random
End_Group_1
End_Fish_Release_Inputs
```

---

## Field Descriptions

**Number_Of_Release_Groups**

**Description:** The number of defined release groups. Fish within a release group are all released from the same location, but they can be released at different times.

**Example:** `Number_Of_Release_Groups: 2`

**Group_<x>**

**Description:** Defines the release location, timing, and number of fish within release group x

**Example keyword:** `Group_1`

**Table: Release location (one entry)**

**NodeID**

**Description:** The upstream node of the channel where the fish will be released

**Example:** `332`

**ChannelID/ReservoirName/Obj2objName**

**Description:** The channel where the fish will be released

**Example:** `412`

**Distance**

**Description:** Distance (feet) of the station from the upstream node of the channel where the fish will be released

**Example:** `20`

**Station_Name**

**Description:** Unique name used to identify the release location

**Example:** `Sacramento`

**Table: List of release times and numbers of fish (multiple entries)**

**Release_Date**

**Description:** The date to release the fish. This must be after the run_start_date defined in the ECO-PTM configuration file

**Example:** `03/18/2012`

**Release_Time**

**Description:** The time at which to release the fish

**Example:** `1:00`

**Particle_Number**

**Description:** The number of fish to release at this date and time

**Example:** `100`

**Release_Style**

**Description:** How the fish should be distributed when they are released

**Allowable entries:** `RANDOM`

--- 

> ⚠️ **Warning:**  
> `Fish_Release_Inputs` is only needed for `Particle_Type_Inputs` set to `salmon_particle`.