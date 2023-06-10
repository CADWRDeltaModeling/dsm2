# Reservoir

## Overview

*Reservoirs* are open bodies of water that store flow and are connected
to nodes by means of an energy-based equation. Reservoirs are considered
instantly well-mixed.

-   The Reservoirs Table specifies the identity and physical properties
    of the reservoir.
-   Connections to nodes are specified in the Reservoir Connections
    table. 
-   Reservoir area as a function of elevation is specified in Reservoir
    volume table, while volume is calculated in code (since 8.2)

  

## RESERVOIR Table

  

  

 A sample is given below

**Example**

``` text
# Description:
# Setting of Clifton Court Forebay
RESERVOIR
NAME  AREA  BOT_ELEV   
clifton_court       91.868000   -7.748      
END
```

  

The RESERVOIR table defines the name and physical properties of the
reservoir. In the case of a "tank" like reservoir the area and volume
are simply defined by the constant area and bottom elevation times the
constant area, respectively.

#### Field Descriptions

##### NAME

Name of the reservoir. This is the identifier of the reservoir used in
other tables.

##### AREA

Surface area (in units of million sq ft) of the reservoir at typical
depth. This area is used to calculate volume changes.

##### BOT_ELEV

Elevation (ft) of the bottom of the reservoir.

#### Table Info

##### Identifier:

NAME

##### Include Block:

GRID

##  RESERVOIR_CONNECTION Table

See sample below

**Example**

``` text
# Description:
# Setting of Frank Tract Connections
RESERVOIR_CONNECTION

RES_NAME  NODE  COEF_IN  COEF_OUT   
franks_tract        103   2250.000  2250.000     
franks_tract        216   1500.000  1500.000    
END
```

  

The RESERVOIR_CONNECTION table lists reservoir connections to
neighboring nodes. Flow through reservoir connections is calculated
using the following formula

Q = C<sub>to</sub> sqrt\[ 2g(z<sub>node</sub> - z<sub>res</sub>) \] ...
z<sub>res</sub> \< z<sub>node</sub>

Q = C<sub>from</sub> sqrt\[ 2g(z<sub>res</sub> - z<sub>node</sub>) \]
... z<sub>res</sub> \> z<sub>node</sub>

Where:

-   C<sub>to</sub> and C<sub>from</sub> are coefficients representing
    the hydraulic efficiency of the reservoir connection and the nominal
    Area perpendicular to flow.
-   g is gravity and
-   z<sub>res</sub> and z<sub>node</sub> are the water surface
    elevations at the reservoir and node (node surface is assessed by
    means of a reference channel that has no reservoirs attached to it).

#### Field Descriptions

##### RES_NAME

Name of reservoir at which connection is specified.

##### NODE

Number identifying the node at which connection is specified.

##### COEF_IN

Coefficient from node to reservoir, greater than zero. If you compare
the reservoir equation to the gate or other orifice equation you will
find that the reservoir coefficient actually folds several quantities
into one parameter: a flow efficiency (between zero and one) and a area
of flow. If you have an observation of the area normal to flow, the
coefficient should be some fraction of this aperture.

##### COEF_OUT

Flow direction out of the reservoir.

  

#### Table Info

##### Identifier:

RES_NAME, NODE

##### Parent Table:

RESERVOIR

##### Parent Identifier:

RES_NAME

##### Include Block:

GRID

A node may not have more than three reservoir connections and must have
at least one ungated channel connection.

## RESERVOIR_VOL Table

See sample below

``` python
RESERVOIR_VOL
RES_NAME                 ELEV          AREA
liberty                -61.975          0.000
liberty                -32.808          2.478
liberty                -16.404         16.220
liberty                 -3.281        272.328
liberty                 -1.640       1017.270
liberty                  0.000       1999.522
liberty                  1.640       3031.999
liberty                  3.281       4209.851
liberty                  4.921       4584.028
liberty                  6.562       5190.456
liberty                  8.202       6359.679
liberty                  9.843       6636.050
liberty                 13.123       6731.118
liberty                 16.404       6830.894
liberty                 19.685       6876.916
liberty                 22.966       6890.138
END
```

  

Since version 8.2, reservoirs can also have variable area and volume
defined as a function of elevation. This table still requires the
reservoir to be defined in the RESERVOIR table even though the elevation
area specified in the RESERVOIR table will be ignored if it is specified
here.

#### Field Descriptions

##### RES_NAME

Name of the reservoir. This is the identifier of the reservoir should
have been specified in the RESERVOIR table (Elevation and area from that
table are ignored)

##### ELEV 

Elevation (ft) of the reservoir at which the area and volume are
specified. This elevation is to the datum of the rest of model
(currently NAVD88)

##### AREA

Surface area (in acres) of the reservoir at specified elevation. The
area is interpolated between elevations based on the current elevation
of the water level

(Reference: Annual Report 2015, Chapter 2). See Figure below for a
snippet

##### VOLUME

Volume (in units of acre-ft) of the reservoir at specified elevation.
The volume is calculated as explained [here](Reservoir_Volume_Calculation.md).

#### Table Info

##### Identifier:

NAME

##### Include Block:

GRID

