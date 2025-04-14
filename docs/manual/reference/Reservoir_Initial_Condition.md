# Reservoir Initial Condition

## Overview

HYDRO requires a water surface initial condition at reservoirs. The Reservoir IC view allows the user to specify default initial conditions. The default initial condition is required but will be overridden if a restart file is used.

## Tables

### Example

```text
# Description:
# Initial Condition of Clifton Court Forebay
RESERVOIR_IC
RES_NAME STAGE
clifton_court 5.000
END
```

The RESERVOIR_IC table specifies initial water surface elevations for reservoirs.

### Field Descriptions

- **RES_NAME**: Name of the reservoir where the initial condition is to be applied.
- **STAGE**: Initial water surface elevation.

### Table Info

- **Identifier**: NAME
- **Include Block**: INITIAL_CONDITION

> Default initial values are replaced if a restart file is used.
> Consistency should be maintained between initial reservoir-channel stage differences and flows at nodes. If the reservoir is assigned a different initial stage than surrounding channels, the head difference implies a flow described by the reservoir equations. Unless carefully balanced, the initial time step may have a mass imbalance. To avoid this, set the reservoir stage equal to surrounding channels and make the initial flow zero.






