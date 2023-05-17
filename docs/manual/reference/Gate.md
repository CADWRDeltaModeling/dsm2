# Gate

## Overview:

*Gates* are sites that present a barrier or control on flow. A gate may
have an arbitrary number of associated hydraulic devices (pipes and
weirs), each of which may be operated independently to control flow.

The Gates View is primarily for specifying the physical properties of
the gate and some simple operating modes. Gates that are operated simply
can be completely specified in this table. Much more elaborate controls
are possible using Gate Time Series and Operating Rules, and in addition
to manipulating the hydraulic devices you can completely uninstall the
gate.

  

## Tables:

-   [Gates](#Gate-gate)
-   [Gate Weir Devices](#Gate-gate_weir_device)
-   [Gate Pipe Devices](#Gate-gate_pipe_device)

  

### GATE

The Gate table defines the name and connectivity of the gate. Gates are
a top-level layered table.

#### Field Descriptions

##### NAME

Name of the gate. This is the identifier of the gate used elsewhere to
refer to the gate.

##### FROM_OBJ

Type (channel/reservoir) of the water body to which the gate is
attached. Gates are always connected from a water body to a node. This
column is a picklist that is also connected to the Name/no. column.

##### FROM_IDENTIFIER

Identifier (channel number or reservoir name) of the water body to which
the gate is attached.

##### TO_NODE

Node to which gate is attached.

#### Table Info

Identifier:

##### NAME

Parent Table:

##### GATE

Include Block:

##### GRID

------------------------------------------------------------------------

  

### GATE_WEIR_DEVICE

This table lists hydraulic structures that exist at the gate site to
control flow that resemble weirs or rectangular conduits. In this table,
the user specifies physical properties of the device as well as default
operations. Both employ the following formulas depending on whether the
water surface is higher on the water body or node side of the gate:

Q = nC<sub>op_to</sub>C<sub>to</sub>A(z<sub>wb</sub>, p)* sqrt*\[
2g(z<sub>wb</sub> - z<sub>node</sub>) \] ... z<sub>wb</sub> \>
z<sub>node</sub>

Q = nC<sub>op_from</sub>C<sub>from</sub>A(z<sub>node</sub>, p)* sqrt*\[
2g(z<sub>node</sub> - z<sub>wb</sub>) \] ... z<sub>wb</sub> \<
z<sub>node</sub>

Where:

-   n is the number of duplicate devices\>
-   C<sub>op_to</sub> and C<sub>op_to</sub> are operating coefficient
    representing controls such as flap gates
-   C<sub>to</sub> and C<sub>from</sub> are coefficients representing
    the hydraulic efficiency of the gate
-   A is the area of flow depending on higher water surface and position
    p
-   g is gravity and
-   z<sub>wb</sub> and z<sub>node</sub> are the water surface elevations
    at the water body and node (node surface is assessed by means of a
    reference channel that has no gates attached to it).

Please see usage notes below

  

#### Field Descriptions

##### GATE_NAME

Name of the gate this device in which the device is located.

##### DEVICE

Name of the device.

##### NDUPLICATE

Number of exact duplicates, such as a number of similar pipes in
parallel. Parameters such as width apply to a single one of the
duplicates.

##### WIDTH

Maximum width of the device (radius of a pipe, width of a weir).

##### ELEV

Invert elevation or weir crest.

##### HEIGHT

Height of the device from the invert elevation. This can be used to
represent the height of rectangular flashboards or of a radial gate. If
the surface goes above this height, flow will be submerged. Use NA for
an open top. If you click in an NA column, you will see that it is
encoded using a large number, but you should only use 'NA' or a real
height.

##### CF_FROM_NODE

Flow coefficient of the gate (0 \< Cto \<= 1.0) describing the
efficiency of the gate from node to water body. This parameter is the
physical coefficient of flow. It should never be zero and should not be
used to describe a control structure or operation such as flap gates or
gate openings.

##### CF_TO_NODE

Same as CF_FROM_NODE, but for the direction from water body to node.

##### DEFAULT_OP

Default operation mode. The gate operation is a "magic" parameter
between 0.0 and 1.0 that modulates gate flow. Operating coefficients can
be used to represent flap gates, fractions of duplicates operating or
other physical controls. The default ops are simple on this table are
like initial conditions -- if you want more sophisticated control you
will need to use a Gate Time Series or Operating Rule. Nevertheless, the
defaults are enough to represent structures that are fully open or
closed or operated unidirectionally. Here is how the default operation
mode will affect the operating coefficient:

##### gate_open

C<sub>op_to</sub>=1.0; C<sub>op_from</sub>=1.0;

##### gate_close

C<sub>op_to</sub>=0.0; C<sub>op_from</sub>=0.0;

##### unidir_to

C<sub>op_to</sub>=1.0; C<sub>op_from</sub>=0.0;

##### unidir_from

C<sub>op_to</sub>=0.0; C<sub>op_from</sub>=1.0;

#### Table Info

##### Identifier:

GATE_NAME, DEVICE

##### Parent Table:

GATE

##### Parent Identifier:

GATE_NAME

##### Include Block:

GRID

  

### GATE_PIPE_DEVICE

This table lists pipes at the gate site. In this table, the user
specifies physical properties of the device as well as default
operations.

#### Field Descriptions

##### GATE_NAME

Name of the gate this device in which the device is located;

##### DEVICE

Name of the device.

##### NDUPLICATE

Number of exact duplicates, such as a number of similar pipes in
parallel. Parameters such as width apply to a single one of the
duplicates.

##### RADIUS

Maximum width of the device (radius of a pipe, width of a weir).

##### ELEV

Invert elevation or weir crest.

##### CF_FROM_NODE

Flow coefficient of the gate (0 \< Cto \<= 1.0) describing the
efficiency of the gate from node to water body. This parameter is the
physical coefficient of flow. It should never be zero and should not be
used to describe a control structure or operation such as flap gates or
gate openings.

##### CF_TO_NODE

Same as CF_FROM_NODE, but for the direction from water body to node.

##### DEFAULT_OP

Default operation mode. The gate operation is a "magic" parameter
between 0.0 and 1.0 that modulates gate flow. Operating coefficients can
be used to represent flap gates, fractions of duplicates operating or
other physical controls. The default ops are simple on this table are
like initial conditions -- if you want more sophisticated control you
will need to use a Gate Time Series or Operating Rule. Nevertheless, the
defaults are enough to represent structures that are fully open or
closed or operated unidirectionally. Here is how the default operation
mode will affect the operating coefficient:

##### gate_open

C<sub>op_to</sub>=1.0; C<sub>op_from</sub>=1.0;

##### gate_close

C<sub>op_to</sub>=0.0; C<sub>op_from</sub>=0.0;

##### unidir_to

C<sub>op_to</sub>=1.0; C<sub>op_from</sub>=0.0;

##### unidir_from

C<sub>op_to</sub>=0.0; C<sub>op_from</sub>=1.0;

#### Table Info

##### Identifier:

GATE_NAME, DEVICE

##### Parent Table:

GATE

##### Parent Identifier:

GATE_NAME

##### Include Block:

GRID

------------------------------------------------------------------------

  

## Several types of time series and operational controls can be placed on gates

-   At least one channel at every node must be ungated.

-   Gates can be removed using an operation rule that sets the
    gates *install* variable  to zero. Gates that are uninstalled behave
    like normal nodes with equal water surface constraints between them.
    Operations and time series that manipulate the device operating
    coefficients and positions will be applied, but the devices will be
    totally ignored in computations to determine flow. flow.

-   Gates can be controlled by a number of variables that are
    time-varying and controlled by time series or operating rules:

    *install*

    -   Install applies to the whole gate, not individual devices. When
        the gate is uninstalled (install=0) the gate ceases to exist,
        none of its devices are applied (although the continue to exist
        in the background). The gate is totally replaced by an
        equal-stage compatibility condition.

-   *op_to_node*

    -   Operating coefficient in the direction from water body to node.

-   *op_from_node*

    -   Operating coefficient in the direction from node to water body.

-   *op*

    -   Operating coefficient in both directions. This is just a
        convenience combo of the individual to/from node versions. It is
        write-only in operating rules, because it combines two variables
        and there is no single value that can be read.

-   *position*

    -   Physical operating position whose interpretation depends on the
        Position Control setting of the gate device. This is now
        deprecated in favor of more direct manipulation of things like
        gate elevation.

-   *elev*

    -   Weir crest or pipe invert elevation. This can represent
        evolution over time or a bottom-operating structure.

-   *width*

    -   Weir width or pipe radius. This usually represents evolution
        over time.

-   *height*

    -   Weir gate height, width of a flashboard. This can represent
        evolution over time or a top-operating structure like a radial
        gate.
