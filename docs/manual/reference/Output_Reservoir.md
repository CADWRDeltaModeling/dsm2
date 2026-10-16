# Output Reservoir

## Overview

The OUTPUT_RESERVOIR table is used by both HYDRO and QUAL to specify output requests inside a reservoir. Output is in HEC-DSS or text format. The variables that can be requested vary by model.

## Tables

### OUTPUT_RESERVOIR

The table specifies the name for the output request, as well as the location, variable being output, time aggregation, and destination file.

### Field Descriptions

- **NAME**: Name of the output request. This is part of the identifier of the table and will be used in the B_PART of the output if it is in DSS format. Avoid using VARIABLE inside this name to prevent redundancy in the output DSS path.
- **RES_NAME**: Name of the reservoir in which output is requested.
- **NODE**: Node number, if the request is for a flow to a particular connected node.
- **VARIABLE**: Model variable to be output. In HYDRO, you can request `stage`, `flow`, or `vel`. In QUAL, you can request `stage`, `flow`, or the name of any constituent in the model.
- **INTERVAL**: Time interval of the output. Can be any DSS-compliant interval with a unit that is not calendar-dependent (e.g., MIN, HOUR, DAY).
- **PERIOD_OP**: Period aggregation performed to convert the model time step into the time interval of the output. May be `INST` or `AVE`.
- **FILE**: Name of the output file where the data will be stored. If the extension is `.txt`, the output is in text format. If a `.dss` extension is used, output is in HEC-DSS format.

### Table Info

- **Identifier**: NAME, VARIABLE
- **Include Block**: OUTPUT_TIME_SERIES

### OUTPUT_RESERVOIR_SOURCE_TRACK

This table is identical to OUTPUT_RESERVOIR except it is only used in QUAL and contains one additional field for tracking constituent sources.

### Field Descriptions

- **NAME**: Name of the output request. This is part of the identifier of the table and will be used in the B_PART of the output if it is in DSS format. Avoid using SOURCE_NAME in the output name.
- **RES_NAME**: Name of the reservoir in which output is requested.
- **NODE**: Node number, if the request is for a flow to a particular connected node. Otherwise, use `none`.
- **VARIABLE**: Model variable to be output. In HYDRO, you can request `stage`, `flow`, or `vel`. In QUAL, you can request `stage`, `flow`, or the name of any constituent in the model.
- **SOURCE_GROUP**: Name of the source group that is being tracked in this output request. The group used must consist entirely of boundary or source locations—not water bodies.
- **INTERVAL**: Time interval of the output. Can be any DSS-compliant interval with a unit that is not calendar-dependent (e.g., MIN, HOUR, DAY).
- **PERIOD_OP**: Period aggregation performed to convert the model time step into the time interval of the output. May be `INST` or `AVE`.
- **FILE**: Name of the output file where the data will be stored. If the extension is `.txt`, the output is in text format. If a `.dss` extension is used, output is in HEC-DSS format.

### Table Info

- **Identifier**: NAME, VARIABLE, SOURCE_GROUP
- **Include Block**: OUTPUT_TIME_SERIES

> Finer output is preferred to daily. You can easily average to daily later outside the model using a script or time series application. Tidal data are poorly represented by daily aggregations, and numerous incorrect conclusions have arisen from aliasing when a 24-hour daily averaging operation is imposed on a naturally 25-hour phenomenon.



