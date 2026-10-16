
# Channel Initial Condition

## Overview:

HYDRO requires water surface and flow initial condition. This view
allows the user to specify default initial conditions. The default
initial condition is required. The default will be overridden if a
restart file is used.

## Tables:

-   -   <a
        href="file:///U:/dsm2/doc/html/reference/channel_ic.html#channel_ic_table"
        rel="nofollow">Channel Initial Conditions</a>

### CHANNEL_IC

The table pairs channel locations with default initial values.
Interpolation is used between locations. Water surface (stage) and flow
must be specified at the upstream and downstream ends of the channel.

#### Field Descriptions

##### CHAN_NO

Channel number of channel where initial condition is to be applied.

##### DISTANCE

Distance along channel where initial condition is to be applied. This
may be a numerical distance or the keyword "length" to indicate the end
of the channel. If you edit an entry that says "length", you may see a
complicated coded value, which is only for internal use.

##### STAGE

Initial water surface elevation.

##### FLOW

Initial flow (cfs).

#### Table Info

##### Identifier:

CHAN_NO, DISTANCE

##### Parent Table:

Table is parent

##### Include Block:

INITIAL_CONDITION

------------------------------------------------------------------------

-   Default initial values specified in the GUI are replaced if a
    restart file is used.
-   Reservoir initial surfaces should be matched to the surrounding
    channels. Differences imply a flow, and if you haven't accounted for
    the flow in your other initial conditions you will have a flow
    imbalance or even instability on the first step.

  
