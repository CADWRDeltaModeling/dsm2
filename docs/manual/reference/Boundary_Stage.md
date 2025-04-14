# Boundary Stage

## Overview

Stage Boundaries are locations where water levels are known. They are often used to represent the tidal boundary of an estuary. This view defines the tidal boundary and assigns a time series to water levels at that boundary.

## Tables

### Example

```text
# Description:
# Historical stage at Martinez
BOUNDARY_STAGE
NAME  NODE  FILLIN  FILE           PATH                                                
mtz   361   linear  ${BNDRYINPUT}  /FILL+CHAN/RSAC054/STAGE//15MIN/${HISTSTAGEVERSION}_NAVD/ 
END
```

### Stage Boundary Table

The Stage Boundary table defines the stage boundary by giving it a name and associating it with a node. The table also assigns a time series to the boundary. Stage Boundary is a top-level layered table.

#### Field Descriptions

- **NAME**: Name assigned to the source. This is the identifier of the boundary and is referred to elsewhere in the input system. If you assign water quality, you will use the same name to match concentration to flow.
- **NODE**: Node number at which the source is applied.
- **FILLIN**: Method for filling in data if the time step of the assigned series is coarser than the time step of the model. See fillin types.
- **FILE**: DSS or text file in which data are stored. Use consistent case when referring to the same file. You may also enter the word `constant` if you would like to assign a constant value to the input (the value will be entered in the next column).
- **PATH**: The path within the text or DSS file of the time series data. If you used the `constant` keyword in the Input File column, enter the value (e.g., `4.22`) here.

#### Table Info

- **Identifier**: NAME
- **Include Block**: HYDRO_TIME_SERIES

> Only one boundary (flow or stage) should be assigned at a node. HYDRO is able to accept sources and sinks at boundary nodes, but this is not good modeling practice.
