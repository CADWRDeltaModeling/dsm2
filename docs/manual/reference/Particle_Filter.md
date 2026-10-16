# Particle Filter

## Overview

**Particle Filter** is a section in the PTM input where you set up particle filters. It is designed to modify the particle flux at a node without changing hydrodynamic conditions by keeping particles from entering the specified waterbody.

## Tables

### Example

```text
PARTICLE_FILTER
NAME NODE WATERBODY FILLIN FILE PATH
filter_hor 8 chan:54 last constant 0
filter_nf 280 chan:357 last ./Filter_OP_NF.dss /HIST+FILTER/FILTER_NF/FILTER_OP//IR-DECADE/DWR-BDO/
END
```

The PARTICLE_FILTER table defines particle filters by giving them names, associating them to a node and a waterbody, and setting up the passing efficiency (which could be a constant value or time-varying data in DSS).

### Field Descriptions

- **NAME**: Name assigned to the particle filter. This is the identifier of the filter used elsewhere to refer to the filter.
- **NODE**: The ID of the node to which the filter is attached.
- **WATERBODY**: The type and ID of the waterbody to which the filter is attached.
- **FILLIN**: Method for filling in data if the time step of the assigned series is coarser than the time step of the model. See fillin types.
- **FILE**: DSS or text file in which data are stored. Use consistent case when referring to the same file. You may also enter the word `constant` if you would like to assign a constant value to the input (the value will be entered in the next column).
- **PATH**: The path within the text or DSS file of the time series data. If you used the `constant` keyword in the Input File column, enter the value here. The stored variable is particle passing efficiency, a float value between 0 (block) and 1 (totally pass).

> Filters are 2-directional and function differently for each direction:
> - Waterbody → Node: Filter serves as a total block with passing efficiency 0.
> - Node → Waterbody: Filter adjusts particle decision-making with passing efficiency as a re-adjusting factor.






