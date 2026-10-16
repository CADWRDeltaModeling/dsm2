# Tutorial 4: Time-Varying Data

## Task

Convert the boundary conditions and gate operations from constants to time-varying input data.

## Skills Gained

- Learn about HEC-DSS as a time series data storage system.
- Learn how HEC-DSS path names are used to reference time series in DSM2 input files.

The purpose of this tutorial is to incorporate time-varying information into the model. In the previous sections, all boundary conditions and gate timings were set as constant, and no input files were needed. In this section, the model is set to read time-varying information stored in HEC-DSS files.

![USACE Logo](../images/fig_usace_logo.png)  
The U.S. Army Corps of Engineers' Hydrologic Engineering Center Data Storage System, or HEC-DSS, is a database system designed to efficiently store and retrieve scientific data that is typically sequential. Such data types include, but are not limited to, time series data, curve data, spatial-oriented gridded data, and others. The system was designed to make it easy for users and application programs to retrieve and store data.

Data in HEC-DSS format can be viewed using special software including VISTA (DWR) or HEC-DSSVue. Each time series is described in the database using DSS Pathnames. For DSM2, the pathnames are typically used as follows:

![Calsim DSS Paths](../images/fig_calsim_dss_paths.png)

- **A-Part**: Data Source  
- **B-Part**: Location  
- **C-Part**: Variable  
- **D-Part**: Date range  
- **E-Part**: Data frequency  
- **F-Part**: Description (e.g., CalSim run identifier).

For more information, see the [HEC-DSS website](https://www.hec.usace.army.mil/software/hec-dssvue/).

---

### Steps to Incorporate Time-Varying Data

#### 1. Change the Transfer Flows to HEC-DSS Input

1. Create a new file called `input_hydro_ts_tutorial.inp`.
2. In the new file, create the `INPUT_TRANSFER_FLOW` table:

```text
INPUT_TRANSFER_FLOW
TRANSFER_NAME FILLIN FILE PATH
transfer_1 linear ${TUTORIALINPUT} /TUTORIAL/TRANSFER/FLOW//15MIN/CONSTANT/
END
```

> **Note**: HEC-DSS pathnames use forward slashes: `/A-Part/B-Part/C-Part/D-Part/E-Part/F-Part/`. In the example above, the A-Part is `TUTORIAL`, the B-Part is `TRANSFER`, etc.

3. Open `hydro.inp` and add the following `ENVVAR` definition:

```text
ENVVAR
NAME VALUE
HYDROOUTDSSFILE output.dss
DSM2MODIFIER timevar_1
TUTORIALINPUT ../timeseries/tutorial.dss
END
```

4. Update the `HYDRO_TIME_SERIES` block in `hydro.inp` to include the new file:

```text
HYDRO_TIME_SERIES
input_boundary_hydro_tutorial.inp
input_transfer_flow_tutorial.inp
input_hydro_ts_tutorial.inp
END
```

5. Save the files.
6. Open `qual.inp` and set `DSM2MODIFIER` to `timevar_1` to match `hydro.inp`.

---

#### 2. Running HYDRO and QUAL

1. Navigate to the directory: `${DSM2_home}\tutorial\simple\t4_timevar`.
2. Open a command window in the directory.
3. Run the following commands:

```text
hydro hydro.inp
qual qual.inp
```

4. Open the `output.dss` file in the `t4_timevar` directory and verify the results.

---

#### 3. Adjust DSM2MODIFIER for a Variant Scenario

1. Open `hydro.inp` and change `DSM2MODIFIER` to `timevar_2` in the `ENVVAR` section.
2. Open `qual.inp` and make the same change to `DSM2MODIFIER`.

---

#### 4. Add Source Information into HYDRO

1. In `input_hydro_ts_tutorial.inp`, add the `SOURCE_FLOW` table:

```text
SOURCE_FLOW
NAME NODE SIGN FILLIN FILE PATH
source1 5 1 linear ${TUTORIALINPUT} /TUTORIAL/SOURCE/FLOW//15MIN/CONSTANT/
END
```

2. Save the file.

---

#### 5. Add Corresponding Source Information into QUAL

1. Create a new file called `input_qual_ts_tutorial.inp`.
2. Add the `NODE_CONCENTRATION` table:

```text
NODE_CONCENTRATION
NAME NODE_NO VARIABLE FILLIN FILE PATH
source1 5 ec last ${TUTORIALINPUT} /TUTORIAL/SOURCE/EC//15MIN/CONSTANT/
END
```

3. Add the `TUTORIALINPUT` definition to `qual.inp`:

```text
ENVVAR
NAME VALUE
TUTORIALINPUT ../timeseries/tutorial.dss
END
```

4. Update the `QUAL_TIME_SERIES` block in `qual.inp`:

```text
QUAL_TIME_SERIES
input_node_conc_tutorial.inp
input_qual_ts_tutorial.inp
END
```

5. Save the files.

---

#### 6. Add Time-Varying Tide Information for Downstream Boundary in HYDRO

1. In `input_hydro_ts_tutorial.inp`, add the `BOUNDARY_STAGE` table:

```text
BOUNDARY_STAGE
NAME NODE FILLIN FILE PATH
downstream_stage 7 linear ${TUTORIALINPUT} /TUTORIAL/DOWNSTREAM/STAGE//15MIN/REALISTIC/
END
```

---

#### 7. Add Downstream Boundary in QUAL

1. In `input_qual_ts_tutorial.inp`, add the following to the `NODE_CONCENTRATION` table:

```text
NODE_CONCENTRATION
NAME NODE_NO VARIABLE FILLIN FILE PATH
downstream_stage 7 ec last ${TUTORIALINPUT} /TUTORIAL/DOWNSTREAM/EC//15MIN/REALISTIC/
END
```

---

#### 8. Add a Gate Time Series to HYDRO

1. Create a new file called `input_gate_tutorial.inp`.
2. Add the `INPUT_GATE` table:

```text
INPUT_GATE
GATE DEVICE VARIABLE FILLIN FILE PATH
gate_1 weir op_from_node none ${TUTORIALINPUT} /TUTORIAL/GATE/FLAP_OP//IR-YEAR/TIMEVAR/
END
```

3. Update the `HYDRO_TIME_SERIES` block in `hydro.inp`:

```text
HYDRO_TIME_SERIES
input_boundary_hydro_tutorial.inp
input_transfer_flow_tutorial.inp
input_hydro_ts_tutorial.inp
input_gate_tutorial.inp
END
```

4. Save the files.

---

#### 9. Running HYDRO and QUAL

1. Navigate to the directory: `${DSM2_home}\tutorial\simple\t4_timevar`.
2. Open a command window in the directory.
3. Run the following commands:

```text
hydro hydro.inp
qual qual.inp
```

4. Open the `output.dss` file in the `t4_timevar` directory and examine the results.
