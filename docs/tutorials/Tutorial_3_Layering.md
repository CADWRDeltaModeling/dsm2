# Tutorial 3: Layering

**Task**

- Separate DSM2 input data into multiple input files.
- Use layers in DSM2 to group related items.

**Skills Gained**  
Learn how to use layering in DSM2 to add, change, and delete features in a DSM2 simulation, for example, including a new reservoir in a simulation.

The purpose of this tutorial is to demonstrate the use of layering to structure your project. Layers are part of the DSM2 data management system. They allow input items to be grouped in logical bundles and allow changes to be brought into an old simulation without erasing or altering archived items. At the same time, we will neaten up our input by dividing it into several files that are "included" from a fairly sparse primary file. The layering concept will be demonstrated by adding a "dummy" reservoir connected to nodes 5 and 6 (Figure 1) that will be "turned on" or "turned off" in a simulation. We will also use `DSM2MODIFIER` to differentiate between alternative simulations.  

![Simple channel with a reservoir, gate, flow transfer, and dummy reservoir](../../images/fig_res_conn_w_trans.png)  
**Figure 1:** Simple channel with a reservoir, gate, flow transfer, and dummy reservoir.

---

### 1. Convert the Previous `hydro.inp` GRID Items to External Files

To use layers, the input tables must be gathered into individual input files.  

> **Key Points about Layering:**
> - Each file represents a layer.
> - Information in the launch file (`hydro.inp` or `qual.inp`) supersedes all other input information.
> - **For include blocks, files read later replace files read earlier.** If the same type of input information exists in more than one file, the last information read will overwrite the previously read values.
> - Overriding values is based on an identifier (e.g., `NAME` or `NAME` and `VARIABLE`—identifiers are listed in table reference documentation).
> - Parent and child tables (e.g., `CHANNEL` and `XSECT`) must be grouped in the same file.
> - If a parent item is overridden, all the child items associated with the overridden parent item are ignored.

#### Steps:
1. Navigate to the `t3_layering` directory.
2. Create a new file called `grid_tutorial_base.inp`.
3. Open `hydro.inp` and:
   - Locate the `CHANNEL` and `XSECT_LAYER` tables. **Cut** them and paste them into `grid_tutorial_base.inp`.
   - Locate the `RESERVOIR` and `RESERVOIR_CONNECTION` tables. **Cut** them and paste them into `grid_tutorial_base.inp`. Leave the `RESERVOIR_IC` table in `hydro.inp`.
   - Move the `TRANSFER` and `GATE` information to `grid_tutorial_base.inp`. Ensure the `GATE` child tables are also moved. Leave the `INPUT_TRANSFER_FLOW` table in `hydro.inp`.
4. Ensure the data tables listed above have been removed from `hydro.inp`.
5. Add the following lines to `hydro.inp` to include the external file:

```
GRID
grid_tutorial_base.inp
END
```

   > **Note:** Ensure there is a carriage return at the end of each `.inp` file.

---

### 2. Running HYDRO and QUAL with Grid Information in Separate Files

This simulation will serve as the base case for comparison with other simulations. Use `DSM2MODIFIER` to differentiate between the various simulations. `DSM2MODIFIER` is a special `ENVVAR` definition automatically used by DSM2 to mark output (the F-Part of the DSS Path).

#### Steps:
1. In the `ENVVAR` section of `hydro.inp` and `qual.inp`, change `DSM2MODIFIER` to `layers_base` and save the files.
2. Navigate to the `t3_layering` directory.
3. Open a command window in the directory.
4. Run the following commands:
```
hydro hydro.inp
qual qual.inp
```
5. Open the `output.dss` file in the `t3_layering` directory and examine the results.

---

### 3. Creating a New Reservoir

To add a feature, such as a new reservoir, follow these steps:

#### Steps:
1. In `grid_tutorial_base.inp`, add the following data for the new reservoir below the data for `res_1`:

```
RESERVOIR
NAME AREA BOT_ELEV
dummy_res 60.0 -30.0
END

RESERVOIR_CONNECTION
RES_NAME NODE COEF_IN COEF_OUT
dummy_res 5 220.0 220.0
dummy_res 6 220.0 220.0
END
```

2. Save the file.

---

### 4. Running HYDRO and QUAL with the New Reservoir

This simulation is the first alternative, which adds a reservoir. Use `DSM2MODIFIER` to differentiate this simulation from the base simulation.

#### Steps:
1. In the `ENVVAR` section of `hydro.inp` and `qual.inp`, change `DSM2MODIFIER` to `layers_dummyres` and save the files.
2. Navigate to the `t3_layering` directory.
3. Open a command window in the directory.
4. Run the following commands:
```
hydro hydro.inp
qual qual.inp
```
5. Compare the `layers_base.out` and `layers_dummyres.out` echoed input files to ensure the dummy reservoir was included in the simulation. Open the `output.dss` file and compare results from the base run and the new `dummyres` simulation.

---

### 5. Disabling a Reservoir Using a Revision Layer

To disable the dummy reservoir, use a revision layer.

#### Steps:
1. Create a file called `grid_tutorial_revision.inp`. Add this file to the `GRID` include-file section in `hydro.inp`:

```
GRID
grid_tutorial_base.inp
grid_tutorial_revision.inp
END
```

2. Copy the `RESERVOIR` and `RESERVOIR_CONNECTION` tables for `dummy_res` from `grid_tutorial_base.inp` to `grid_tutorial_revision.inp`. Add a caret (`^`) before the reservoir name in the parent table:

```
RESERVOIR
NAME AREA BOT_ELEV
^dummy_res 60.0 -30.0
END

RESERVOIR_CONNECTION
RES_NAME NODE COEF_IN COEF_OUT
dummy_res 5 220.0 220.0
dummy_res 6 220.0 220.0
END
```

3. Save the file.

---

### 6. Running HYDRO and QUAL Disabling the New Reservoir

#### Steps:
1. In the `ENVVAR` section of `hydro.inp` and `qual.inp`, change `DSM2MODIFIER` to `layers_nodummyres` and save the files.
2. Navigate to the `t3_layering` directory.
3. Open a command window in the directory.
4. Run the following commands:
```
hydro hydro.inp
qual qual.inp
```
5. Compare the `layers_base.out`, `layers_dummyres.out`, and `layers_nodummyres.out` echoed input files and the `output.dss` file. Are the results the same for the base simulation and the no-dummy-reservoir simulation?

---

### 7. Changing the Properties of a Reservoir

To change the properties of `res_1`, such as increasing its area:

#### Steps:
1. In `grid_tutorial_revision.inp`, update the `RESERVOIR` table:

```
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
END
```

2. Save the file.

---

### 8. Running HYDRO and QUAL with Increased Area for `res_1`

#### Steps:
1. In the `ENVVAR` section of `hydro.inp` and `qual.inp`, change `DSM2MODIFIER` to `layers_larger_res1` and save the files.
2. Navigate to the `t3_layering` directory.
3. Open a command window in the directory.
4. Run the following commands:
```
hydro hydro.inp
qual qual.inp
```
5. Compare the output to earlier simulations.

---

### 9. Changing the Name of Channel 2004

To replace the channel number of Channel 2004 with Channel 4:

#### Steps:
1. In `grid_tutorial_revision.inp`, copy the `CHANNEL` and `XSECT_LAYER` data for Channel 2004. Update the channel number to 4 and disable Channel 2004 using a caret (`^`):

```
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
```

2. Save the file.

---

### 10. Add Initial Conditions for Channel 4

#### Steps:
1. Create a file called `channel_ic_revision.inp` and add the following:

```
CHANNEL_IC
CHAN_NO DISTANCE STAGE FLOW
4 0 0.0 0.0
4 length 0.0 0.0
END
```

2. Update `hydro.inp` to include the file:

```
INITIAL_CONDITION
channel_ic_revision.inp
END
```

3. Save the files.

---

### 11. Running HYDRO and QUAL with Channel 4

#### Steps:
1. In the `ENVVAR` section of `hydro.inp` and `qual.inp`, change `DSM2MODIFIER` to `layers_ch2004_to_ch4` and save the files.
2. Navigate to the `t3_layering` directory.
3. Open a command window in the directory.
4. Run the following commands:
```
hydro hydro.inp
qual qual.inp
```
5. Examine the results in the `output.dss` file and the echoed input file.

---

### Brain Teaser

For the same change in elevation between the reservoir and connecting node, which reservoir would have a higher flow, `res_1` or `dummy_res`?

**Table 1: Include Blocks for DSM2 Input Files**

| Include Block         | Sections                                                                 |
|-----------------------|--------------------------------------------------------------------------|
| CONFIGURATION         | ENVVAR, SCALAR                                                           |
| GRID                  | CHANNEL, XSECT (child), XSECT_LAYER (child), RESERVOIR, RESERVOIR_CONNECTION (child), GATE, GATE_WEIR_DEVICE (child), GATE_PIPE_DEVICE (child), TRANSFER |
| GROUPS                | GROUP, GROUP_MEMBER (child)                                              |
| HYDRO_TIME_SERIES     | INPUT_TRANSFER_FLOW, INPUT_GATE, BOUNDARY_STAGE, BOUNDARY_FLOW, SOURCE_FLOW, SOURCE_FLOW_RESERVOIR |
| INITIAL_CONDITION     | CHANNEL_IC, RESERVOIR_IC                                                 |
| OPERATION             | OPERATING_RULE, OPRULE_EXPRESSION, OPRULE_TIME_SERIES                    |
| OUTPUT_TIME_SERIES    | OUTPUT_CHANNEL, OUTPUT_RESERVOIR, OUTPUT_CHANNEL_SOURCE_TRACK, OUTPUT_RESERVOIR_SOURCE_TRACK, OUTPUT_GATE |
| PARTICLE              | PARTICLE_INSERTION, PARTICLE_FLUX_OUTPUT, PARTICLE_GROUP_OUTPUT          |
| QUAL_SPATIAL          | RATE_COEFFICIENT                                                         |
| QUAL_TIME_SERIES      | INPUT_CLIMATE, NODE_CONCENTRATION, RESERVOIR_CONCENTRATION               |

---

### Learning More

Overriding is easy to understand. The main things you will need to keep in mind are:

1. Understanding how child table replacement works:
   - You can't replace the child element without replacing the parent.
   - The children of an overridden parent element are never used.
2. What is the unique identifier for each row in a table? In most cases, this is the first field and it is usually a name or a map number (it is a label rather than a piece of hard data). In some cases (e.g., output), the unique identifier may be two fields such as `NAME` and `VARIABLE` for output. Overriding only occurs when the identifier for the row is duplicated. This information is available in the table reference documentation in the "documentation" folder.
3. Which data can be included in which blocks. For instance, `GRID` can contain `CHANNEL`, `GATE`, `RESERVOIR`, and `TRANSFER` data. This information is given in Table 1.

---

### Brain Teaser

For the same change in elevation between the reservoir and connecting node, which reservoir would have a higher flow, `res_1` or `dummy_res`?

