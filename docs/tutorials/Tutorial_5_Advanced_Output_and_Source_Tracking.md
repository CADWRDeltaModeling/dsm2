# Tutorial 5: Advanced Output and Source Tracking

## Task

- Create boundary and source groups.
- Request output for constituent source tracking.

## Skills Gained

Learn how to use advanced output options in DSM2, including source tracking.

The purpose of this tutorial is to provide instruction on advanced output options in DSM2. Basic outputs include flow, stage, and constituent concentrations at nodes and channel locations. Advanced outputs include creating output groups and source tracking.

### Add Output Paths to `hydro.inp`

1. Navigate to the directory: `{DSM2_home}\tutorial\simple\t5_output`.
2. Open the file `addin.inp` and note the new output paths for the channels and reservoir.
3. Copy the entire file contents to the clipboard.
4. Open the file `hydro.inp`.
5. Navigate to the bottom of the file and paste the information.

> **Note**: The output request in the launch file (e.g., `hydro.inp` or `qual.inp`) supersedes all other output requests that have the same identifier. To ensure outputs at both locations, assign unique identifiers to each location, e.g., `bnd_1` for location 0 and `bnd_100` for location 100.

### Add Boundary and Source Groups

GROUPS are user-defined groups of model objects, for instance groups of water bodies or groups of boundary inputs. Groups are used a number of places in DSM2, including: tracking of constituents originated from grouped sources, tracking of particles as they reside or move between groups of water bodies and/or boundaries, and assignment of rate coefficients in QUAL to groups of water bodies. In the output specifications, groups are used to define aggregate sources for source tracking. For example, output groups could be used to track mass originating from all the boundaries, or from all Delta Island Consumptive Use (DICU) diversions, etc. In this section, we will create two output groups: boundary locations and water quality constituent source locations.

1. In the study directory, create a file called `group_tutorial.inp`.
2. In the `group_tutorial.inp` file, add a group table. Note that this is a parent table for overwriting/layering purposes. Define a boundary and a sources group:

    ```
    GROUP
    NAME
    boundary
    sources
    END
    ```

3. Now define the group members. Create the GROUP_MEMBER table below the GROUP table:

    ```
    GROUP_MEMBER
    GROUP_NAME MEMBER_TYPE PATTERN
    END
    ```

4. In the Group Members table:
    1. Enter a row with the following values in the appropriate fields:
        - GROUP_NAME: `boundary`
        - MEMBER_TYPE: `stage`
        - PATTERN: `.*stream.*`
        
        > **Note**: The dot-star `.*` in the above pattern is a "regular expression" wildcard. You can use any standard Perl-style regular expression in groups, but the html documentation for GROUPS describes most of the patterns you can put in a GROUP_MEMBER that are really useful.

5. Enter another row with the following values in the appropriate fields:
    - GROUP_NAME: `boundary`
    - MEMBER_TYPE: `flow_boundary`
    - PATTERN: `.*stream.*`

6. In the Group Members table insert another row with the following values in the appropriate fields:
    - GROUP_NAME: `sources`
    - MEMBER_TYPE: `source_sink`
    - PATTERN: `source1`

7. In the `qual.inp` file, create the GROUPS (note the plural) include block that will reference this file:

    ```
    GROUPS
    group_tutorial.inp
    END
    ```

8. Save the current settings.

### Source Tracking

Source tracking (aka fingerprinting) determines the amount of water or of a constituent at one location that originated from a specified location. For constituent fingerprinting, 1) define a source group (e.g. boundaries or DICU locations), and then 2) request output for that group. For volumetric fingerprinting that indicates the percentage of flow that originated from each boundary location, 1) create a fingerprinting constituent and set its value equal to 100 at all boundaries, 2) define a source group for all boundaries, and 3) request output from that source group.

### Add Source Tracking Output for Channel 5

To demonstrate source tracking, this part of the tutorial examines how much of the EC in channel 5 (see Figure 1) came from the boundaries and from other sources. For comparison purposes, the EC from all sources will also be output.  
Create a new file called `output_qual_sourcetrack.inp`.

1. In this file, create an OUTPUT_CHANNEL_SOURCE_TRACK table. Refer to the documentation to create the header.
2. In the Channel Output table create 3 rows:
    1. For the first new row, enter the following values into the appropriate fields:
        - Name: `ch5`
        - Channel: `5`
        - Distance: `5000`
        - Variable: `ec`
        - Source Group: `all` (this will track ec from all sources)
        - Output File: `${QUALOUTDSSFILE}`
        - Time Interval: `15min`
        - Period Op: `inst`
    2. For the second new row, enter the following values into the appropriate fields:
        - Name: `ch5`
        - Channel: `5`
        - Distance: `5000`
        - Variable: `ec`
        - Source Group: `boundary`
        - Output File: `${QUALOUTDSSFILE}`
        - Time Interval: `15min`
        - Period Op: `inst`
    3. For the third new row, enter the following values into the appropriate fields:
        - Name: `ch5`
        - Channel: `5`
        - Distance: `5000`
        - Variable: `ec`
        - Source Group: `sources`
        - Output File: `${QUALOUTDSSFILE}`
        - Time Interval: `15min`
        - Period Op: `inst`
3. Save the current settings.

### Running HYDRO and QUAL

1. Open a command window for the `t5_output` directory.
2. In the command window, type: `hydro hydro.inp`.
3. In the command window, type: `qual qual.inp`.
4. Open the hydro echo file `output_tutorial_hydro_echo.inp`. Which version of `bnd_1` got picked up by the model, the one in `hydro.inp` or the one in `output_hydro_tutorial.inp`.
5. Open the `output.dss` file in the `t5_output` directory, and examine the results. Do a mass balance to make sure the source tracking adds up.

## Brain Teaser

How would you set up a source tracking simulation to determine what percentage of water/flow at a given location originated from a specified boundary?
