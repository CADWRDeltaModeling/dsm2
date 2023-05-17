# Output Gate

## Overview:

The OUTPUT_GATE table is used by both HYDRO to specify output requests
at a gate. Output is HEC-DSS or text format.

  

## Tables:

-   [OUTPUT_GATE](#OutputGate-output_gate)

  

### OUTPUT_GATE

The table specifies the name for output request, as well as the
location, variable being output, time aggregation and destination file.

#### Field Descriptions

##### NAME

Name of the output request. This is the identifier of the table and will
be used in the B_PART of the output if it is in DSS format. It can be
the same as the gate name but it doesn't have to be. Avoid using
VARIABLE inside this name -- this causes redundancy in the output and
the layering won't work correctly.

##### GATE_NAME

Name of the gate at which output is requested.

##### DEVICE

Name of the gate device, if applicable. You can request operational or
physical data from a device as well as flow. You can also request some
gate output (install,flow) that is not linked to a particular device. In
this case, the field should be set to none

##### VARIABLE

Model variable to be output. From a device you can request some physical
data (width, height, elev), operational data
(op_to_node, op_from_node, position ) or flow oriented from water body
to node. From a gate with device=none you can request the
variables install, or total flow oriented from water body to node

##### INTERVAL

Time Interval of the output. Can be any DSS-compliant interval with a
unit that is not calendar dependent (MIN, HOUR, DAY). This is a
departure from previous versions of DSM2, which offered monthly output.

##### PERIOD_OP

Period aggregation performed to convert the model time step into the
time interval of the output. May be INST or AVE, but AVE can be
meaningless for a lot of gate variables.

##### FILE

Name of the output file where the data will be stored. If the extension
.txt is given, the output is automatically in text format. If a .dss
extension is used, output is in HEC-DSS format.

#### Table Info

##### Identifier:

NAME, VARIABLE

##### Parent Table:

Table is parent

##### Include Block:

OUTPUT_TIME_SERIES

  

------------------------------------------------------------------------

  

## Examples:

output_gate_example.inp   
  

------------------------------------------------------------------------

  

-   Initially, the thing that is hard to get about gate output is the
    flow orientation. The output for the gate is oriented with the gate,
    which may or may not be in the upstream-downstream direction
