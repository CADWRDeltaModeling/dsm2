# Node Concentration

## Overview:

Node concentration represents the concentration of constituents attached
to boundary inflows and sources.

## Tables:

### NODE_CONCENTRATION

The NODE_CONCENTRATION table attaches concentrations to boundary and
source flows defined in HYDRO. The table also assigns a time series to
the source.

#### Field Descriptions

##### NAME

Name assigned to the source. An entry here must have the same name as an
entry in the BOUNDARY_STAGE, BOUNDARY_FLOW or SOURCE_FLOW tables -- by
matching names you will attach concentrations to the flow.

##### NODE_NO

Node number where the flow is applied. This must match the node number
given in the original flow table (it is a bit redundant, but easier to
look things up).

##### VARIABLE

Constituent name. If no output is requested for the constituent
currently it will be ignored.

##### FILLIN

Method for filling in data if the time step of the assigned series is
coarser than the time step of the model.

##### FILE

DSS or text file in which data are stored. Use consistent case when
referring to the same file. You may also enter the word constant if you
would like to assign a constant value to the input (the value will be
entered in the next column).

##### PATH

The path within the text or DSS file of the time series data. If you
used the constant keyword in the Input File column, enter the value
(e.g. 4.22) here.

#### Table Info

##### Identifier:

NAME

##### Include Block:

QUAL_TIME_SERIES

------------------------------------------------------------------------

Multiple sources and sinks can be assigned to a node. They are often
kept separate in order to assign different concentrations to them

  
