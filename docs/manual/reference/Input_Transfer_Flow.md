# Input Transfer Flow

## Overview:

Transfer Flows are flow time series assignments to pre-defined mass
transfers.

  

## Tables:

  

### INPUT_TRANSFER_FLOW

The transfer flow table assigns time series flows to transfers, The
table assigns a time series data source to the boundary condition.

#### Field Descriptions

##### TRANSFER_NAME

This must be the same as the name of the transfer.

##### FILLIN

Method for filling in data if the time step of the assigned series is
coarser than the time step of the model. See fillin types

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

HYDRO_TIME_SERIES

------------------------------------------------------------------------

  

Only one flow (and no concentration) can be assigned to a transfer.
