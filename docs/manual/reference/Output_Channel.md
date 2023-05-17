# Output Channel

## Overview:

The OUTPUT_CHANNEL table is used by both HYDRO and QUAL to specify
output requests inside of the channel. Output is HEC-DSS or text format.
The variables that can be requested vary by model.

## Tables:

-   [OUTPUT_CHANNEL](http://msb-confluence/display/DSM2/Output+Channel#OutputChannel-output_channel)
-   [OUTPUT_CHANNEL_SOURCE_TRACK](http://msb-confluence/display/DSM2/Output+Channel#OutputChannel-output_channel_source_track)

### OUTPUT_CHANNEL

The table specifies the name for output request, as well as the
location, variable being output, time aggregation and destination file.

#### Field Descriptions

##### NAME

Name of the output request. This is part of the identifier of the table
and will be used in the B_PART of the output if it is in DSS format.
Generally, non-modelers will have an easier time understanding your
output if this is a station name that is geographically fixed (e.g.
"vernalis" or "RSAC075") than if it is a modeling construct ("ch101").
Similarly, avoid using avoid using VARIABLE inside this name -- this
causes redundancy in the output DSS path and the layering won't work as
well.

##### CHAN_NO

Channel number in which output is requested.

##### DISTANCE

Distance along channel (from upstream node to downstream), typically in
feet. Results will be interpolated between Eulerian (HYDRO) or
Lagrangian (QUAL) computational points.

##### VARIABLE

Model variable to be output. In HYDRO, you can request stage,flow,vel.
In QUAL you can request stage,flow or the name of any constituent in the
model. When no output request is made for a constituent that is not
required for reaction kinetics, it is not calculted.

##### INTERVAL

Time Interval of the output. Can be any DSS-compliant interval with a
unit that is not calendar dependent (MIN, HOUR, DAY). This is a
departure from previous versions of DSM2, which offered monthly output.

##### PERIOD_OP

Period aggregation performed to convert the model time step into the
time interval of the output. May be INST or AVE, which produce
instantaneous

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

  

### OUTPUT_CHANNEL_SOURCE_TRACK

This table is identical to OUTPUT_CHANNEL except it is only used in QUAL
and it contains one additional field for tracking constituent sources.

#### Field Descriptions

##### NAME

Name of the output request. See comments above, and note that in this
case you should also avoid using the SOURCE_NAME in the output name.

##### CHAN_NO

Channel number in which output is requested.DISTANCEDistance along
channel (from upstream node to downstream), typically in feet. Results
will be interpolated between Eulerian (HYDRO) or Lagrangian (QUAL)
computational points.

##### VARIABLE

Model variable to be output. In HYDRO, you can request stage,flow,vel.
In QUAL you can request stage,flow or the name of any constituent in the
model. When no output request is made for a constituent that is not
required for reaction kinetics, it is not calculted.

##### SOURCE_GROUP

Name of the source group that is being tracked in this output request.
To learn how to define a group, see group reference. The group used must
consist entirely of boundary or source locations -- not water bodies.

##### INTERVAL

Time Interval of the output. Can be any DSS-compliant interval with a
unit that is not calendar dependent (MIN, HOUR, DAY). This is a
departure from previous versions of DSM2, which offered monthly output.

##### PERIOD_OP

Period aggregation performed to convert the model time step into the
time interval of the output. May be INST or AVE, which produce
instantaneous

##### FILE

Name of the output file where the data will be stored. If the extension
.txt is given, the output is automatically in text format. If a .dss
extension is used, output is in HEC-DSS format.

#### Table Info

##### Identifier:

NAME, VARIABLE, SOURCE_GROUP

##### Parent Table:

Table is parent

##### Include Block:

OUTPUT_TIME_SERIES

-   Finer output is preferred to daily. You can easily average to daily
    later outside the model using a script or time series application.
    Tidal data are poorly represented by daily aggregations, and
    numerous incorrect conclusions have arisen from aliasing
    (fluctuations over two weeks) when a 24 hour daily averaging
    operation is imposed on a naturally 25-hour phenomenon. Monthly
    output is no longer allowed.

  
  
