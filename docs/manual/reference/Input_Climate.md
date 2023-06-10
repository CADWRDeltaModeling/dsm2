# Input Climate

## Overview:

Climate inputs are time series assignments to climate variables used in
non-conservative constituent runs.

## Tables:

-   -   INPUT_CLIMATE

### INPUT_CLIMATE

Climate input assigns time varying properties to [gate](Gate.md) parameters, The table assigns a time series data source.

#### Field Descriptions

##### NAME

Name of the input, used for layering.

##### VARIABLE

The variable that is set by this assignment.

##### FILLIN

Method for filling in data if the time step of the assigned series is
coarser than the time step of the model. See fillin types

##### FILE

DSS or text file in which data are stored. Use consistent case when
referring to the same file. You may also enter the word constant if you
would like to assign a constant value to the input (the value will be
entered in the next column).

PATHThe path within the text or DSS file of the time series data. If you
used the constant keyword in the Input File column, enter the value
(e.g. 4.22) here.

#### Table Info

##### Identifier:

NAME

##### Include Block:

QUAL_TIME_SERIES
