# Travel Time Output

**Owned by:** Xiao Wang  
**Last updated:** May 19, 2021  

---

## Overview

The `Travel_Time_Output` parameter provides options for users who need to output travel time information from a specified start location to an end location.  

- Users provide a path to the travel time output file.  
- Note that Windows path separators (`\`) should be used.  
- To specify paths relative to the working directory from which the ECO-PTM was launched, use a period followed by the relative path.

---

## Example

```plaintext
Travel_Time_Output
Output_Path: .\output\travel_time_in_min-03-18-2012.csv 
NodeID          ChannelID/ReservoirName/Obj2objName   Distance          Station_Name
339             379                                     951             Sutter
End_Travel_Time_Output
```

---

## Field Descriptions

**Table**

List of downstream locations, or stations, at which to record the travel time.

**NodeID**

**Description:** Upstream node of the channel containing the station  
**Example:** 339

**ChannelID/ReservoirName/Obj2objName**

**Description:** Channel within which the station is located  
**Example:** 379

**Distance**

**Description:** Distance (feet) of the station from the upstream node of the channel  
**Example:** 951

**Station_Name**

**Description:** Unique name used to identify the station in the travel time output file. These stations will be listed under the “Detect_Sta” header in the output file.

---

---

> ⚠️ **Warning:**  
> Only one travel time output location can be specified


