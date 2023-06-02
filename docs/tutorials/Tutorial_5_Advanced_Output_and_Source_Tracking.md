# Tutorial 5: Advanced Output and Source Tracking

**Task**

-   Create boundary and source groups
-   Request output for constituent source tracking

  
**Skills Gained**  
Learn how to use advanced output options in DSM2 including source
tracking

  
  
The purpose of this tutorial is to provide instruction on advanced
output options in DSM2. Basic outputs include flow, stage and
constituent concentrations at nodes and channel locations. Advanced
outputs include creating output groups and source tracking.  
The first part of this tutorial involves modifications to the text input
file, *hydro.inp*. We will add some outputs and also take a look at how
data in *hydro.inp* is prioritized. The second part introduces the use
of *groups* for source tracking. This tutorial uses the simple channel
network shown in Figure 1.  
![figure 1](../attachments/87228777/87228779.png)
**dummy_res**  
**Figure 1:** **Simple channel with a reservoir, gate, flow transfer and
dummy reservoir.**  
  
  
  
  
  

1.  **Add Output Paths to *hydro.inp*:**

In this step of the tutorial, we will request output upstream and
downstream of the gate and reservoir 1.

1.  1.  In Windows Explorer, navigate to the directory,
        *\\{DSM2_home}\tutorial\simple\t5_output*.
    2.  Open the file *addin.inp* and note the new output paths for the
        channels and reservoir.
    3.  Copy the entire file contents to the clipboard.
    4.  Open the file *hydro.inp*.
    5.  Navigate to the bottom of the file and paste the information.
        Note that there are now two output requests for a location named
        bnd_1. In *hydro.inp* bnd_1 is defined as channel1 location 0
        and in *output_hydro_tutorial.inp* it has been defined as
        channel 1 location 100.

<img src=../../attachments/87228777/87228778.png width="29" height="29"/> For
flow data at bnd_1, will the output be written at the upstream end of
the channel (location 0) or 100ft downstream?  
**Answer:** The output will be for 100ft downstream because the output
request in the launch file (e.g. *hydro.inp* or *qual.inp*) supersedes
all other output requests that have the same identifier. In this case
the identifier is the NAME and VARIABLE combination (e.g. bnd_1 and
flow).  
<img src="../../attachments/87228777/87228778.png" width="29" height="29" />
 How
would you get output at channel 1 and both location 0 and location
100?  
**Answer:** Give each location a unique identifier, eg. bnd_1 and
bnd_100.

  
  

1.  **Add *Boundary* and *Source Groups*:**

GROUPS are user-defined groups of model objects, for instance groups of
water bodies or groups of boundary inputs. Groups are used a number of
places in DSM2, including: tracking of constituents originated from
grouped sources, tracking of particles as they reside or move between
groups of water bodies and/or boundaries, and assignment of rate
coefficients in QUAL to groups of water bodies. In the output
specifications, groups are used to define aggregate sources for source
tracking. For example, output groups could be used to track mass
originating from all the boundaries, or from all Delta Island
Consumptive Use (DICU) diversions, etc. In this section, we will create
two output groups: boundary locations and water quality constituent
source locations.

1.  In the study directory, create a file called
        *group_tutorial.inp*.
2.  In the *group_tutorial.inp* file, add a group table. Note that
        this is a parent table for overwriting/layering purposes. Define
        a boundary and a sources group:

GROUP  
NAME  
boundary  
sources  
END

1.  Now define the group members. Create the GROUP_MEMBER table
        below the GROUP table:

GROUP_MEMBER  
GROUP_NAME MEMBER_TYPE PATTERN  
END

1.  In the *Group Members* *table*:<BR>
        1.  Enter a row with the following values in the appropriate
            fields:<BR>
            1.  GROUP_NAME: *boundary*<BR>
            2.  MEMBER_TYPE: *stage*<BR>
            3.  PATTERN: *.**stream.***<BR>
            4.  Note that the dot-star .\* in the above pattern is a
                "regular expression" wildcard. You can use any standard
                Perl-style regular expression in groups, but the html
                documentation for GROUPS describes most of the patterns
                you can put in a GROUP_MEMBER that are really useful.<BR>

<img src="../../attachments/87228777/87228778.png" width="29" height="29" />
Look in the *input_boundary_hydro_tutorial.inp* file and determine what
boundary conditions are part of the boundary group based on the member
type "stage" and the pattern ".**stream.**".

1.  Enter another row with the following values in the
        appropriate fields:<BR>
        1.  GROUP_NAME: *boundary*<BR>
        2.  MEMBER_TYPE: *flow_boundary*<BR>
        3.  PATTERN: *.**stream.***<BR>

<img src="../../attachments/87228777/87228778.png" width="29" height="29" />
Look in the *input_boundary_hydro_tutorial.inp* file and determine what
boundary conditions are part of the boundary group based on the member
type "flow_boundary" and the pattern ".**stream.**".

1.  In the *Group Members* *table* insert another row with the
        following values in the appropriate fields:<BR>
        1.  GROUP_NAME: *sources*<BR>
        2.  MEMBER_TYPE: *source_sink*<BR>
        3.  PATTERN: *source1*<BR>

<img src="../../attachments/87228777/87228778.png" width="29" height="29" />
Look in the various qual input files and determine which inputs will
make up the sources group defined above.

1.  In the *qual.inp* file, create the GROUPS (note the plural)
        include block that will reference this file:

GROUPS  
group_tutorial.inp  
END

1.  Save the current settings.

  

1.  **Source Tracking:**

Source tracking (aka fingerprinting) determines the amount of water or
of a constituent at one location that originated from a specified
location. For constituent fingerprinting, 1) define a source group (e.g.
boundaries or DICU locations), and then 2) request output for that
group. For volumetric fingerprinting that indicates the percentage of
flow that originated from each boundary location, 1) create a
fingerprinting constituent and set its value equal to 100 at all
boundaries, 2) define a source group for all boundaries, and 3) request
output from that source group.  
  

1.  **Add Source Tracking Output for *Channel 5*:**

To demonstrate source tracking, this part of the tutorial examines how
much of the EC in channel 5 (see Figure 1) came from the boundaries and
from other sources. For comparison purposes, the EC from all sources
will also be output.  
Create a new file called output_qual_sourcetrack.inp.

1.  1.  In this file, create an OUTPUT_CHANNEL_SOURCE_TRACK table. Refer
        to the documentation to create the header.
    2.  In the *Channel Output* table create 3 rows:
        1.  For the first new row, enter the following values into the
            appropriate fields:
            1.  Name: *ch5*
            2.  Channel: *5*
            3.  Distance: *5000*
            4.  Variable: *ec*
            5.  Source Group: *all* (this will track ec from all
                sources)
            6.  Output File: *${QUALOUTDSSFILE}*
            7.  Time Interval: *15min*
            8.  Period Op: *inst*
        2.  For the second new row, enter the following values into the
            appropriate fields:
            1.  Name: *ch5*
            2.  Channel: *5*
            3.  Distance: *5000*
            4.  Variable: *ec*
            5.  Source Group: *boundary*
            6.  Output File: *${QUALOUTDSSFILE}*
            7.  Time Interval: *15min*
            8.  Period Op: *inst*
        3.  For the third new row, enter the following values into the
            appropriate fields:
            1.  Name: *ch5*
            2.  Channel: *5*
            3.  Distance: *5000*
            4.  Variable: *ec*
            5.  Source Group: *sources*
            6.  Output File: *${QUALOUTDSSFILE}*
            7.  Time Interval: *15min*
            8.  Period Op: *inst*
    3.  Save the current settings.

  

1.  **Running HYDRO and QUAL**
    1.  Open a command window for the *t5_output* directory.
    2.  In the command window, type: *hydro hydro.inp*.
    3.  In the command window, type: *qual qual.inp*.
    4.  Open the hydro echo file output_tutorial_hydro_echo.inp. Which
        version of bnd_1 got picked up by the model, the one in
        hydro.inp or the one in output_hydro_tutorial.inp.
    5.  Open the *output.dss* file in the *t5_output* directory, and
        examine the results. Do a mass balance to make sure the source
        tracking adds up.

<img src="../../attachments/87228777/87228776.png" width="82" height="96" />

1.  **Brain Teaser**

How would you set up a source tracking simulation to determine what
percentage of water/flow at a given location originated from a specified
boundary?

## Attachments:

<img src="../../images/icons/bullet_blue.gif" width="8" height="8" />
[worddavfc12b6cdb26e67dfc2a101ee2af340bd.png](../../attachments/87228777/87228776.png)
(image/png)  
<img src="../../images/icons/bullet_blue.gif" width="8" height="8" />
[worddavf9e3aa73957a24bdb115e43494948841.png](../../attachments/87228777/87228778.png)
(image/png)  
<img src="../../images/icons/bullet_blue.gif" width="8" height="8" />
[worddavd56eb5f63f4c9181cb2a8632c8c6c562.png](../../attachments/87228777/87228779.png)
(image/png)  
