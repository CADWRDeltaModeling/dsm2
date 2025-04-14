# Source Flow

## Overview

Source flows represent inflows and outflows in the interior of the model domain at nodes. An entry here creates a source and assigns a time series of in/outflows to it.

## Tables

### Example

```text
# Description:
# Historical source flow at Tracy Pump
SOURCE_FLOW
NAME NODE SIGN FILLIN FILE PATH
cvp 181 -1 last ${BNDRYINPUT} /FILL+CHAN/CHDMC004/FLOW-EXPORT//1DAY/${HISTFLOWVERSION}/
END
```

The node SOURCE_FLOW table defines sources and sinks by giving them names and associating them to a node. The table also assigns a time series to the source.

### Field Descriptions

- **NAME**: Name assigned to the source. This is the identifier of the boundary and is referred to elsewhere in the input system. If you assign water quality, you will use the same name to match concentration to flow.
- **NODE**: Node number at which the source is applied.
- **SIGN**: Forces the time series to be a source or a sink. Positive values are normally associated with a source, but the data (especially sinks such as agricultural diversions) are sometimes measured in absolute flow. Use `1` to force the value to be a positive source or `-1` to interpret values as a sink.
- **FILLIN**: Method for filling in data if the time step of the assigned series is coarser than the time step of the model. See fillin types.
- **FILE**: DSS or text file in which data are stored. Use consistent case when referring to the same file. You may also enter the word `constant` if you would like to assign a constant value to the input (the value will be entered in the next column).
- **PATH**: The path within the text or DSS file of the time series data. If you used the `constant` keyword in the Input File column, enter the value (e.g., `4.22`) here.

### Table Info

- **Identifier**: NAME

> Multiple sources and sinks can be assigned to a node. They are usually kept separate in order to assign different concentrations to them.
> HYDRO is able to accept sources and sinks at boundary nodes, but this is not good modeling practice. Use them on the interior.
