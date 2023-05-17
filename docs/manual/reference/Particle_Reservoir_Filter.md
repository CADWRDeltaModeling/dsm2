# Particle Reservoir Filter

## Overview

**Particle Reservoir Filter* is a section in the PTM input where you set
up particle-filters. Particle Reservoir Filter is designed to modify the
particle flux at reservoir, without changing hydrodynamic condition, by
keeping particles from entering the specified waterbody.*

## Tables

**Example**

``` text
PARTICLE_RES_FILTER 
NAME          RES_NAME      WATERBODY          FILLIN FILE           PATH 
clfc_div_bbid clifton_court qext:dicu_div_bbid last   ./filterOp.dss /HIST+FILTER/CLFC_DIV/FILTER_OP//IR-DECADE/DWR-BDO/  
END
```

  

This is a special filter, which locates at a reservoir directly
connecting to a source flow. The PARTICLE_RES_FILTER table defines
particle filters by giving them names, associating them to a reservoir
and one of its directly connecting waterbody, and setting up the passing
efficiency. (which could be constant value, or time-varying data in
DSS).

#### Field Descriptions

##### NAME

Name assigned to the particle filter. This is the identifier of the
filter used elsewhere to refer to the filter.

##### RES_NAME

The name of the reservoir to which the filter is applied.

##### AT_WB

The type and ID of the waterbody to which the filter is attached .

##### FILLIN

Method for filling in data if the time step of the assigned series is
coarser than the time step of the model. See fillin types.

##### FILE

DSS or text file in which data are stored. Use consistent case when
referring to the same file. You may also enter the word *constant *if
you would like to assign a constant value to the input (the value will
be entered in the next column).**

##### PATH

The path within the text or DSS file of the time series data. If you
used the constant keyword in the Input File column, enter the value
here. The stored variable is particle passing efficiency, a float value
between 0 ~ 1: 0-block; 1-totally pass. **

  

Similar as Particle Filter

  

  

  
