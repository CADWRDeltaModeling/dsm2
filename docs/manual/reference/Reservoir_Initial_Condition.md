# Reservoir Initial Condition

## Overview

HYDRO requires a water surface initial condition at reservoirs. The
Reservoir IC view allows the user to specify default initial conditions.
The default initial condition is required, but will be overridden if a
restart file is used.

## Tables

**Example**

``` text
# Description:
# Initial Condition of Clifton Court Forebay
RESERVOIR_IC 
RES_NAME          STAGE   
clifton_court     5.000          
END
```

  

The RESERVOIR_CONCENTRATION table attaches concentrations to boundary
and source flows defined in QUAL. The table also assigns a time series
to the source.

#### Field Descriptions

##### RES_NAME

Name of reservoir where initial condition is to be applied.

##### STAGE

Initial water surface elevation.

#### Table Info

##### Identifier:

NAME

##### Include Block:

INITIAL_CONDITION

Default initial values are replaced if a restart file is used.

Currently, QUAL cannot take a spatially distributed default initial
condition for constituent concentrations. It has only a single scalar.

Consistency should be maintained between initial reservoir-channel stage
differences and flows at nodes. If the reservoir is assigned a different
initial stage than surrounding channels, the head difference implies a
flow described by the reservoir equations (see Reservoir View). Unless
you are very careful to balance the implied flow with other channel
flows, the initial time step will have a mass imbalance. If what you
want is an easy initial condition, try making stage in the reservoir
equal to the stage in all the surrounding channels and making the
initial flow zero. Alternatively, you can put up with the imbalance --
just run HYDRO for an hour or so extra before the start of any QUAL run
you want do.

  

  

  
