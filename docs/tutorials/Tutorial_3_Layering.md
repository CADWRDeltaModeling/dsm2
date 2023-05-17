# Tutorial 3: Layering

**Task**

-   Separate DSM2 input data into multiple input files
-   Use layers in DSM2 to group related items

  
**Skills Gained**   
Learn how to use layering in DSM2 to add, change and delete features in
a DSM2 simulation, for example including a new reservoir in a simulation

The purpose of this tutorial is to demonstrate the use of layering to
structure your project. Layers are part of the DSM2 data management
system. They allow input items to be grouped in logical bundles, and
allow changes to be brought into an old simulation without erasing or
altering archived items. At the same time we will neaten up our input by
dividing it into several files that are "included" from a fairly sparse
primary file. The layering concept will be demonstrated by adding a
"dummy" reservoir connected to nodes 5 and 6 (Figure 1) that will be
"turned on" or "turned off" in a simulation. We will also use
DSM2MODIFIER to differentiate between alternative simulations.  
**dummy_res**  
**Figure 1:** **Simple channel with a reservoir, gate, flow transfer and
dummy reservoir.**

1.  **Convert the previous hydro.inp GRID items to external files**

In order to use layers, the input tables have to be gathered into
individual input files.   
 Key points about layering:

-   Each file represents a layer
-   Information in the launch file (*hydro.inp* or *qual.inp*)
    supercedes all other input information.
-   ***For include blocks, files that are read later replace files that
    are read earlier***, in other words, if the same type of input
    information exists in more than one file, the last information read
    will overwrite the previously read values.
-   Overriding values is based on an identifier (e.g.NAME or NAME and
    VARIABLE—identifiers are listed in table reference documentation)
-   Parent and child tables (e.g. channel and xsect) must be grouped in
    the same file.
-   If a parent item is overridden, all of the child items associated
    with the overridden parent item are ignored.
    1.  Move the channel and reservoir data:
        1.  Navigate to the t3_layering directory.
        2.  Create a new file in Notepad++
            called *grid_tutorial_base.inp*
        3.  Open *hydro.inp*.
        4.  Locate the CHANNEL and XSECT_LAYER tables
            in *hydro.inp*. **Cut** them and paste them
            into *grid_tutorial_base.inp.*
        5.  Locate the RESERVOIR and RESERVOIR_CONNECTION tables
            in *hydro.inp*. **Cut** them and paste them
            into *grid_tutorial_base.inp* and save the file. Note: leave
            the RESERVOIR_IC in the *hydro.inp* file.
        6.  Similarly move the TRANSFER and GATE information
            from *hydro.inp* to *grid_tutorial_base.inp*.  Be sure to
            move the GATE child tables too. Leave the
            INPUT_TRANSFER_FLOW table in the *hydro.inp* file.
        7.  Make sure the data tables listed above have been removed
            from *hydro.inp*.
        8.  Now add these lines to *hydro.inp* that will tell DSM2 you
            want to include data from other files and that these files
            will contain GRID (channel, reservoir, transfer and gate)
            tables and their child tables. Add the GRID table after the
            IO_FILE block and before any of the initial condition
            blocks.

GRID  
grid_tutorial_base.inp  
END  
 Be sure that there is a carriage return at the end of each \*.inp
file. 

1.  **Running HYDRO and QUAL with grid information in separate files**

This simulation will serve as the base case for comparison for the other
simulations run in this tutorial. We will use the DSM2MODIFIER to
differentiate between the various simulations. DSM2MODIFIER is a special
ENVVAR definition that is automatically used by DSM2 to mark output (the
F Part of the DSS Path).

1.  1.  In the ENVVAR section of *hydro.inp* and *qual.inp*, change
        DSM2MODIFIER to layers_base and save the files.

    2.  In Windows Explorer, navigate to the directory: \_  
        Unknown macro: {DSM2_home}tutorialsimple{\_}

        .

        1.  1.  Right-click on the directory, *t3_layering*, and
                select *Open Command Window Here*. Note: for computers
                running Vista, use a shift+right click on the directory
                name to get the Open Command Window.
            2.  In the command window, type: *hydro hydro.inp*.
            3.  In the command window, type: *qual qual.inp*.
            4.  Note that many of output files all use the DSM2MODIFIER
                in their name, e.g. *layers_base.out*.
                The *output.dss* file distinguishes between scenarios by
                using the DSM2MODIFIER in the F-Part. Open
                the *output.dss* file in the *t3_layering* directory,
                and examine the results.

          

        1.  **Creating a new reservoir:**

        In this section, we will learn how to add a feature by adding a
        new reservoir. We don't want to mess too much with what we have
        already, so we are going to add a dummy reservoir in our
        grid_tutorial_base layer. Later in this tutorial, we will learn
        how to use layers to disable this feature as well.   
         The ability to mask and delete features such as reservoirs and
        gates in DSM2 is often used in planning runs to "turn on" and
        "turn off" features when studying planning alternatives. 

        1.  1.  1.  Create a new Reservoir in *grid_tutorial_base*
                    1.  In tutorial_grid_base.inp, enter data for the
                        new reservoir below the data for res_1
                    2.  Name: *dummy_res*
                    3.  Area (million sq ft): *60*
                    4.  Bottom elev (ft): -30

            2.  In the *Reservoir Connection* table:
                1.  Enter the following values into the appropriate
                    fields:
                    1.  Reservoir name: dummy_res
                    2.  Node: *5*
                    3.  Res Coef (in): *220*
                    4.  Res Coef (out): *220*

            3.  Again, in the *Reservoir Connection* table:
                1.  Enter the following values into the appropriate
                    fields:
                    1.  Reservoir name: dummy_res
                    2.  Node: *6*
                    3.  Res Coef (in): *220*
                    4.  Res Coef (out): *220*

            4.  Save the current settings.

          

        1.  **Running HYDRO and QUAL with the new reservoir**

        This simulation is our first alternative which adds a reservoir.
        We will use the DSM2MODIFIER to differentiate this simulation
        from the base simulation.

        1.  1.  In the ENVVAR section of *hydro.inp* and *qual.inp*,
                change DSM2MODIFIER to layers_dummyres and save the
                files.
            2.  In Windows Explorer, navigate to the directory: \_

        .

    3.  Right-click on the directory, *t3_layering*, and select *Open
        Command Window Here*. For Vista shift+right click on directory
        to get the Open Command Window.

    4.  In the command window, type: *hydro hydro.inp*.

    5.  In the command window, type: *qual qual.inp*.

    6.  Note that many of output files all use the DSM2MODIFIER in their
        name, e.g. *layers_dummyres.out*. Compare
        the *layer_base.out* and the *layer_dummyres.out*echoed input
        files to make sure that the dummy reservoir was included in the
        simulation. The *output.dss* file distinguishes between
        scenarios by using the DSM2MODIFIER in the F-Part. Open
        the *output.dss* file in the *t3_layering* directory, and look
        for results from the base run and from the new dummyres
        simulation.

2.  **Disabling a reservoir using a revision layer**

In this step of the tutorial, we will disable (remove) the dummy
reservoir from the simulation using a revision layer. Revision layers
allow the user to add or remove features for alternatives without
altering the base input files.

1.  1.  Create a Reservoir Revision Layer:
        1.  Create a file called *grid_tutorial_revision.inp.* Add this
            file to your GRID include-file section in *hydro.inp*, which
            will now look like this:

GRID  
grid_tutor*ial_base.inp*  
grid_tutor*ial_revision.inp*  
*END*  
 The include files will be prioritized in the order they are read, later
files replacing earlier ones. In this example, the information
in *grid_tutorial_revision.inp* has priority
over *grid_tutorial_base.inp*, thus any duplicate information
in *grid_tutorial_revision.inp* will override the information
in *grid_tutorial_base.inp*. When a parent table identifier (usually a
channel/node number or a "name") is overridden by a later file, its
original data (including child tables) will be ignored. Everything will
come from the higher priority layer.

1.  1.  1.  Copy the reservoir table header and dummy reservoir data
            from *grid_tutorial_base.inp* to *grid_tutorial_revision.inp*.
            It is important to copy both the parent (RESERVOIR) and the
            child (RESERVOIR_CONNECTION) tables into the revision layer.
        2.  Add a carat (^ shift and 6 key) before the reservoir name in
            the parent table. Your entry should look like this:

RESERVOIR  
NAME AREA BOT_ELEV  
^dummy_res 60.0 -30.0  
END   
RESERVOIR_CONNECTION  
RES_NAME NODE COEF_IN COEF_OUT  
dummy_res 5 220.0 220.0  
dummy_res 6 220.0 220.0  
END

1.  1.  By overriding the name "dummy_res" and also marking it unused,
        you have now effectively removed dummy_res from the
        calculations. The child table is automatically ignored as well
        (so in a sense the entries there are unnecessary).  What is the
        difference between commenting out "dummy_res" in the revision
        layer and using a carat (^) in the revision layer?  
        **Answer:** Commenting out the reservoir in the revision layer
        will be like the revision never existed and the information from
        the original grid layer will be used in the simulation.  
        Using the carat (^) will "turn off" that reservoir for the
        simulation. Neither the information in the original grid layer
        or in the revision layer will be used in that simulation. Thus
        using the carat is a way to "turn on or off" alternative
        components.
    2.  Save the current settings.

  

1.  **Running HYDRO and QUAL disabling the new reservoir**
    1.  In the ENVVAR section of *hydro.inp* and *qual.inp*, change
        DSM2MODIFIER to layers_nodummyres and save the files.

    2.  In Windows Explorer, navigate to the directory: \_  
        Unknown macro: {DSM2_home}tutorialsimple{\_}

        .

        1.  1.  Right-click on the directory, *t3_layering*, and
                select *Open Command Window Here*. For Vista shift+right
                click on directory to get the Open Command Window.
            2.  In the command window, type: *hydro hydro.inp*.
            3.  In the command window, type: *qual qual.inp*.
            4.  Compare the *layer_base.out*, *layer_dummyres.out, and
                layer_nodummyres.out* echoed input files and
                the *output.dss* file. Are the results the same for the
                base simulation and the no dummy reservoir simulation?

          

        1.  **Changing the properties of a reservoir**

        This part of the tutorial demonstrates how a revision layer can
        be used to change the properties of a simulation. In this case
        the area of reservoir 1 is increased.

        1.  1.  Altering the Properties of the Original Reservoir res_1:
                1.  In the *Reservoirs* table of
                    grid_tutorial_revision.inp, change the *Area
                    (million sq ft)* field of res_1 from *40* to *50*.
                2.  Copy the RESERVOIR_CONNECTION entries for res_1 from
                    grid_tutorial_base to grid_tutorial_revision. The
                    revision layer should look similar to the one below.

        RESERVOIR  
        NAME AREA BOT_ELEV   
        res_1 50.0 -24.0   
        ^dummy_res 60.0 -30.0  
        END   
        RESERVOIR_CONNECTION  
        RES_NAME NODE COEF_IN COEF_OUT  
        res_1 3 200.0 200.0   
        res_1 4 200.0 200.0  
        dummy_res 5 220.0 220.0   
        dummy_res 6 220.0 220.0   
        END  Why is it necessary to copy the reservoir connection
        entries to the revision file?  
        **Answer:** When you override a layer (file) with another entry
        in a parent table that has the same identifier, you COMPLETELY
        replace that item in the new layer including child items. In
        other words, if the child table-RESERVOIR_CONNECTIONS in this
        case-is not included in the revision layer, the reservoir will
        have no connections. The values in the original grid layer will
        not be read.

        1.  1.  Save the current settings.

        2.  **Running HYDRO and QUAL** **with increased area for
            reservoir 1**
            1.  In the ENVVAR section of *hydro.inp* and *qual.inp*,
                change DSM2MODIFIER to layers_larger_res1 and save the
                file.
            2.  In Windows Explorer, navigate to the directory: \_

        .

    3.  Right-click on the directory, *t3_layering*, and select *Open
        Command Window Here*. For Vista shift+right click on directory
        to get the Open Command Window.

    4.  In the command window, type: *hydro hydro.inp*.

    5.  In the command window, type: *qual qual.inp*.

    6.  Compare the output to the earlier simulations.

  

1.  **Changing the name of Channel 2004:**

In this step, we will replace the channel number of Channel 2004. In
this case, what we are changing is the identifier itself, rather than
the parameters and data. So what we will do is delete Channel 2004 and
put in a Channel 4 that is identical. In the process, we will ignore
this change in other parts of the input and see what happens to initial
conditions and output requests that reference a non-existent channel.

1.  1.  Keep the *grid_tutorial_revision* file open.
    2.  Copy the channel and xsect data from *grid_tutorial_base.inp* to
        the beginning of *grid_tutorial_revision.inp*. Keep only channel
        2004.
    3.  In *grid_tutorial_revision.inp* in the CHANNEL and XSECT tables,
        copy the data for Channel 2004 and paste another copy into those
        tables.
    4.  In one of your two copies of channel 2004, change the channel
        number in both tables to 4.
    5.  Eliminate channel 2004 by prepending a carat in the CHANNEL
        table. Your revision should look like this:

CHANNEL  
CHAN_NO LENGTH MANNING DISPERSION UPNODE DOWNNODE  
4 15000 0.035 0.3 4 5   
^2004 15000 0.035 0.3 4 5   
END   
  
  
XSECT_LAYER  
CHAN_NO DIST ELEV AREA WIDTH WET_PERIM  
4 0.5 -24.0 0.0 40.0 40.0   
4 0.5 0.0 960.0 80.0 91.22   
4 0.5 20.0 2640.0 160.0 133.6   
2004 0.5 -24.0 0.0 40.0 40.0   
2004 0.5 0.0 960.0 80.0 91.22   
2004 0.5 20.0 2640.0 160.0 133.6   
END 

1.  1.  Save your work. Note that the entries in XSECT_LAYER for channel
        2004 in the *grid_tutorial_revision.inp* are redundant since the
        channel was disabled. However it is good practice to always
        include full parent/child table groups in the revision layer so
        that choices can be turned "on" or "off."

  

1.  **Add Initial Conditions for the New *Channel 4*:**

Since there is no default initial condition for channel 4, we will have
to add one. Similar to the other channels, we will use a zero flow
initial condition.

1.  1.  Create a file called *channel_ic_revision.inp*.
    2.  Copy the CHANNEL_IC table headers from *hydro.inp* to the new
        file.
    3.  Create two rows of data for channel 4:

CHANNEL_IC  
CHAN_NO DISTANCE STAGE FLOW  
4 0 0.0 0.0  
4 length 0.0 0.0  
END 

1.  1.  In the *hydro.inp* file create an INITIAL_CONDITION include
        block underneath the GRID include block:

INITIAL_CONDITION  
channel_ic_revision*.inp*  
*END*

1.  1.  Now every channel has an initial condition. Do you need to do
        something about the "extra" initial condition for Channel 2004?
        Try and see.

  

1.  **Running HYDRO and QUAL**
    1.  In the ENVVAR section of *hydro.inp* and *qual.inp*, change
        DSM2MODIFIER to layers_ch2004_to_ch4 and save the files.

    2.  In Windows Explorer, navigate to the directory: \_  
        Unknown macro: {DSM2_home}tutorialsimple{\_}

        .

        1.  1.  Right-click on the directory, *t3_layering*, and
                select *Open Command Window Here*.
            2.  In the command window, type: *hydro hydro.inp*.
            3.  In the command window, type: *qual qual.inp*.
            4.  Open the *output.dss* file in
                the *t3_layering* directory, and examine the results.
            5.  Open *layers_ch2004_to_ch4_hydro_echo.inp*. This is an
                "echoed input" that replicates your input verbatim,
                except ENVVAR replacements have been made and all the
                channel xsects are in the one-file format. You should be
                able to run the model using this file as easily as with
                the original hydro.inp. Take a look and see:
                1.  Did channel 4 get in the input?
                2.  Did channel 2004? What does this mean?
            6.  Look at the output.dss file. Did the output for channel
                4 get included in the output file? If not, what would
                you change to get output for channel 4?

         Only output specified in the input files is written to the
        output.dss file. However, output for all locations is recorded
        in the hdf5 \*.h5 output file. 

        1.  **Converting hydro.inp to input blocks**

        Now let's convert hydro.inp completely to include files except
        for the SCALAR and IO_FILE sections. In future tutorials, hydro
        and qual simulations will be organized this way. The file
        hydro.inp or qual.inp is usually reserved for scalar or
        input/output file designations.

        1.  1.  In the previous section of this tutorial, an
                INITIAL_CONDITION include block was created underneath
                the GRID include block. We will create an initial
                condition input file for the original initial conditions
                and include that file here. Add the file ic_tutorial.inp
                as the first line of the INITIAL_CONDITION include
                block. The *channel_ic_revision.inp* file was already
                included in this block in the previous section of this
                tutorial.

        INITIAL_CONDITION  
        ic_tutor*ial.inp*  
        channel_ic_revision*.inp*  
        *END*

        1.  1.  Create a file called ic_tutorial.inp
            2.  Cut (not copy) the CHANNEL_IC and RESERVOIR_IC data
                from *hydro.inp* and paste it into this file.
            3.  Create an include block called HYDRO_TIME_SERIES as
                follows, in *hydro.inp*.

        HYDRO_TIME_SERIES  
        input_boundary_hydro_tutorial.inp  
        input_transfer_flow_tutorial.inp  
        END

        1.  1.  Create a file
                called *input_boundary_hydro_tutorial.inp*. Cut (not
                copy) the BOUNDARY_STAGE and BOUNDARY_FLOW input
                from *hydro.inp* to *input_boundary_hydro_tutorial.inp*.
            2.  Similarly, create a file called
                input_transfer_flow_tutorial.inp. Cut and paste the
                INPUT_TRANSFER_FLOW data into this file.
            3.  Create an include block called OUTPUT_TIME_SERIES.

        OUTPUT_TIME_SERIES  
        output_hydro_tutorial.inp  
        END

        1.  1.  Similarly, create the file called
                output_hydro_tutorial.inp. Cut and paste the
                OUTPUT_CHANNEL data into this file.
            2.  The remaining tutorials will use include blocks
                extensively for both hydro and qual.
            3.  Save all of the files.

          

        1.  **Running HYDRO and QUAL with all include files**
            1.  In the ENVVAR section of *hydro.inp* and *qual.inp*,
                change DSM2MODIFIER to layers_include_block
            2.  In Windows Explorer, navigate to the directory: \_

        .

    3.  Right-click on the directory, *t3_layering*, and select *Open
        Command Window Here*.

    4.  In the command window, type: *hydro hydro.inp*.

    5.  In the command window, type: *qual qual.inp*.

    6.  Open the *output.dss* file in the *t3_layering* directory, and
        examine the results, comparing it to the last run. Did putting
        things in input blocks change anything?
2.  **Learning more**

Overriding is easy to understand. The main things you will need to keep
in mind are

1.  Understanding how child table replacement works:
    1.  You can't replace the child element without replacing the
        parent.
    2.  The children of an overridden parent element are never used.
2.  What is the unique identifier for each row in a table? In most cases
    this is the first field and it is usually a name or a map number (it
    is a label rather than a piece of hard data). In some cases (e.g.
    output), the unique identifier may be two fields such as NAME and
    VARIABLE for output. Overriding only occurs when the identifier for
    the row is duplicated. This information is available in the table
    reference documentation in the "documentation" folder.
3.  Which data can be included in which blocks. For instance, GRID can
    contain CHANNEL, GATE, RESERVOIR and TRANSFER data. This information
    is given in Table 1 on the next page.

<!-- -->

1.  **Brain Teaser**
    1.  For the same change in elevation between the reservoir and
        connecting node, which reservoir would have a higher flow, res_1
        or dummy_res?

**Table 1: Include Blocks for DSM2 Input Files**

<table class="confluenceTable">
<tbody>
<tr class="odd">
<td class="confluenceTd" style="text-align: left;"><p>Include
Block</p></td>
<td class="confluenceTd" style="text-align: left;"><p>Sections</p></td>
</tr>
<tr class="even">
<td class="confluenceTd"
style="text-align: left;"><p>CONFIGURATION </p></td>
<td class="confluenceTd" style="text-align: left;"><p>ENVVAR <br />
SCALAR</p></td>
</tr>
<tr class="odd">
<td class="confluenceTd" style="text-align: left;"><p>GRID</p></td>
<td class="confluenceTd" style="text-align: left;"><p>CHANNEL <br />
XSECT (child) <br />
XSECT_LAYER (child) <br />
RESERVOIR <br />
RESERVOIR_CONNECTION (child) <br />
GATE <br />
GATE_WEIR_DEVICE (child) <br />
GATE_PIPE_DEVICE (child) <br />
TRANSFER</p></td>
</tr>
<tr class="even">
<td class="confluenceTd" style="text-align: left;"><p>GROUPS</p></td>
<td class="confluenceTd" style="text-align: left;"><p>GROUP <br />
GROUP_MEMBER (child)</p></td>
</tr>
<tr class="odd">
<td class="confluenceTd"
style="text-align: left;"><p>HYDRO_TIME_SERIES</p></td>
<td class="confluenceTd"
style="text-align: left;"><p>INPUT_TRANSFER_FLOW <br />
INPUT_GATE <br />
BOUNDARY_STAGE <br />
BOUNDARY_FLOW <br />
SOURCE_FLOW <br />
SOURCE_FLOW_RESERVOIR</p></td>
</tr>
<tr class="even">
<td class="confluenceTd"
style="text-align: left;"><p>INITIAL_CONDITION</p></td>
<td class="confluenceTd" style="text-align: left;"><p>CHANNEL_IC <br />
RESERVOIR_IC </p></td>
</tr>
<tr class="odd">
<td class="confluenceTd" style="text-align: left;"><p>OPERATION</p></td>
<td class="confluenceTd"
style="text-align: left;"><p>OPERATING_RULE <br />
<br />
OPRULE_EXPRESSION <br />
OPRULE_TIME_SERIES</p></td>
</tr>
<tr class="even">
<td class="confluenceTd"
style="text-align: left;"><p>OUTPUT_TIME_SERIES</p></td>
<td class="confluenceTd"
style="text-align: left;"><p>OUTPUT_CHANNEL <br />
OUTPUT_RESERVOIR <br />
OUTPUT_CHANNEL_SOURCE_TRACK <br />
OUTPUT_RESERVOIR_SOURCE_TRACK <br />
OUTPUT_GATE</p></td>
</tr>
<tr class="odd">
<td class="confluenceTd" style="text-align: left;"><p>PARTICLE</p></td>
<td class="confluenceTd"
style="text-align: left;"><p>PARTICLE_INSERTION <br />
PARTICLE_FLUX_OUTPUT <br />
PARTICLE_GROUP_OUTPUT</p></td>
</tr>
<tr class="even">
<td class="confluenceTd"
style="text-align: left;"><p>QUAL_SPATIAL</p></td>
<td class="confluenceTd"
style="text-align: left;"><p>RATE_COEFFICIENT</p></td>
</tr>
<tr class="odd">
<td class="confluenceTd"
style="text-align: left;"><p>QUAL_TIME_SERIES</p></td>
<td class="confluenceTd"
style="text-align: left;"><p>INPUT_CLIMATE <br />
NODE_CONCENTRATION <br />
RESERVOIR_CONCENTRATION</p></td>
</tr>
</tbody>
</table>

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavfc12b6cdb26e67dfc2a101ee2af340bd.png](attachments/87228767/87228766.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavf9e3aa73957a24bdb115e43494948841.png](attachments/87228767/87228768.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddave8e1df4e853bb46c4ee6f68afece040d.png](attachments/87228767/87228769.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavd56eb5f63f4c9181cb2a8632c8c6c562.png](attachments/87228767/87228770.png)
(image/png)  
