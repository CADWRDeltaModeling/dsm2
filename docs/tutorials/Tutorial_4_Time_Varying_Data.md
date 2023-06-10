# Tutorial 4: Time Varying Data

**Task**  
Convert the boundary conditions and gate operations from constants to
time varying input data.   
**Skills Gained**

-   Learn about HEC-DSS as a time series data storage system
-   Learn how HEC-DSS path names are used to reference time series in
    DSM2 input files   
    The purpose of this tutorial is to incorporate time-varying
    information into the model. In the previous sections, all boundary
    conditions and gate timings were set as constant, and no input files
    were needed. In this section, the model is set to read time-varying
    information stored in HEC-DSS files.   
    <img src="../images/fig_usace_logo.png"/> The U.S. Army Corps of Engineers' Hydrologic Engineering Center
    Data Storage System, or HEC-DSS, is a database system designed to
    efficiently store and retrieve scientific data that is typically
    sequential. Such data types include, but are not limited to, time
    series data, curve data, spatial-oriented gridded data, and others.
    The system was designed to make it easy for users and application
    programs to retrieve and store data.   
    Data in HEC-DSS format can be viewed using special software
    including VISTA (DWR), or HEC-DSSVue. Each time series is described
    in the database using DSS Pathnames (see column headings in figure).
    For DSM2 the pathnames are typically used as follows:  
![Calsim DSS Paths](../images/fig_calsim_dss_paths.png)

    A-Part: Data Source  
    B-Part: Location  
    C-Part: Variable  
    D-Part: Date range  
    E-Part: Data frequency   
    F-Part: Description (in the sample shown the F-Part is the CalSim
    run identifier.  
    For more information see the [HEC-DSS
    website](https://www.hec.usace.army.mil/software/hec-dssvue/).<BR>
    **dummy_res**  
    ![reservoir connections with ](../images/fig_res_conn_w_trans.png)
    **Figure 1:** **Simple channel with a reservoir, gate, flow transfer
    and dummy reservoir.**   
      

1.  **Change the Transfer Flows to HEC-DSS input:**

The constant transfer flow from the previous tutorials will be changed
to a time series.

1.  Create a new file in Notepad++ or another text editor called
        input_hydro_ts_tutorial.inp
2.  In the new file, create the *TRANSFER_TIME_SERIES* *table*:

INPUT_TRANSFER_FLOW  
TRANSFER_NAME FILLIN FILE PATH  
END

1.  Enter the following values into the appropriate fields:
        1.  Input Name: *transfer_1*
        2.  Fillin: *linear*
        3.  Input File: *${TUTORIALINPUT}*
        4.  Path/Value: */TUTORIAL/TRANSFER/FLOW//15MIN/CONSTANT/*

<img src="../../images/icon_warning.png" width=29 height=29/> The HEC-DSS pathnames are referred to using forward slashes  
/A-Part/B-Part/C-Part/D-Part/E-Part/F-Part/  
In the example above, the A-Part is Tutorial, the B-Part is TRANSFER,
etc. and the D-Part isn't specified. 

1.  Open hydro.inp. The input file uses an ENVVAR reference as the
        filename, so add the definition of TUTORIALINPUT. At the same
        time, set DSM2MODIFIER to timevar_1:

ENVVAR  
NAME VALUE   
HYDROOUTDSSFILE output.dss   
DSM2MODIFIER timevar_1   
TUTORIALINPUT ../timeseries/tutorial.dss   
END 

1.  We are going to replace the existing time series with the new
        file, so make sure it is listed below the other files as
        follows.

HYDRO_TIME_SERIES  
input_boundary_hydro_tutorial.inp  
input_transfer_flow_tutorial.inp  
input_hydro_ts_tutorial.inp  
END

1.  Save the files.
2.  Open qual.inp and set DSM2MODIFIER to timevar_1 as well
        (hydro.inp and qual.inp must agree or the tidefile won't be
        found).

  

1.  **Running HYDRO and QUAL**
    1.  In Windows Explorer, navigate to the directory: \_  
        Unknown macro: {DSM2_home}tutorialsimple{\_}

        .

        1.  Right-click on the directory, *t4_timevar*, and
                select *Open Command Window Here*.
        2.  In the command window, type: *hydro hydro.inp*. Examine
                timebar_1_hydro_echo.inp. Did the time series assignment
                get used?
        3.  In the command window, type: *qual qual.inp*.
        4.  Open the *output.dss* file in
                the *t4_timevar* directory, and verify that the results
                are identical to the results from the previous tutorial
                (located in the *t3_layering* directory). Why is this?

          
        **Adjust DSM2MODIFIER to represent a variant scenario:**

        1.  In Windows Explorer, navigate to the
                directory: *\\{DSM2_home}\tutorial\simple\t4_timevar*
        2.  Open *hydro.inp* for editing.
        3.  In the *ENVVAR* section, change
                the *DSM2MODIFIER* environment variable
                from *timevar_1* to *timevar_2*.
        4.  Open *qual.inp* for editing.
        5.  In the *ENVVAR* section, change
                the *DSM2MODIFIER* environment variable
                from *timevar_1* to *timevar_2*.

          

        1.  **Add Source information into HYDRO:**
            1.  In *input_hydro_ts_tutorial.inp*, create the table for
                node sources:

        SOURCE_FLOW  
        NAME NODE SIGN FILLIN FILE PATH  
        END

        1.  Enter the following values into the appropriate fields:
                1.  Name: *source1*
                2.  Node: *5*
                3.  Input File: *${TUTORIALINPUT}*
                4.  Path/Value: */TUTORIAL/SOURCE/FLOW//15MIN/CONSTANT/*
                5.  Sign: *1*
                6.  Fillin: *linear*
        2.  Save the current settings.

          

        1.  **Add Corresponding Source information into QUAL:**
            Create a file called *input_qual_ts_tutorial.inp*.
            1.  In input_qual_ts_tutorial.inp, create the
                    NODE_CONCENTRATION table

        NODE_CONCENTRATION  
        NAME NODE_NO VARIABLE FILLIN FILE PATH  
        END 

        1.  Enter the following values into the appropriate
                fields:
                1.  Input Name: *source1*
                2.  Node: *5*
                3.  Variable: *ec*
                4.  Input File: *${TUTORIALINPUT}*
                5.  Path/Value: */TUTORIAL/SOURCE/EC//15MIN/CONSTANT/*
                6.  Fillin: *last*
        2.  Add the ENVVAR definition for TUTORIALINPUT in
                qual.inp

        TUTORIALINPUT ../timeseries/tutorial.dss

        1.  In qual.inp, make sure that the file gets used:

        QUAL_TIME_SERIES  
        input_node_conc_tutorial.inp  
        input_qual_ts_tutorial.inp  
        END 

        1.  **Add Time-varying Tide Information for Downstream Boundary
            in HYDRO:**
            1.  Reopen i*nput_hydro_ts_tutorial.inp*
            2.  Create the *BOUNDARY_STAGE* table.

          
        BOUNDARY_STAGE  
        NAME NODE FILLIN FILE PATH  
        END

        1.  In the *Boundary Stage* *table* enter the following
                values into the appropriate fields:
                1.  Input Name: *downstream_stage*
                2.  Node: *7*
                3.  Input File: *${TUTORIALINPUT}*
                4.  Path/Value: */TUTORIAL/DOWNSTREAM/STAGE//15MIN/REALISTIC/*
                5.  Fillin: *linear*

        2.  **Add Downstream Boundary in QUAL:**
            1.  Re-open *input_qual_ts_tutorial.inp*.
            2.  In the *Node Concentration* *table*:
                1.  Enter the following values into the appropriate
                    fields:
                    1.  Input Name: *downstream_stage*
                    2.  Node: *7*
                    3.  *Variable: ec*
                    4.  Input File: *${TUTORIALINPUT}*
                    5.  Path/Value: */TUTORIAL/DOWNSTREAM/EC//15MIN/REALISTIC/*
                    6.  Fillin: *last*

          

        1.  **Add a Gate Time Series to HYDRO:**

        This gate time series will control the weir. The pipe is to be
        left open all the time (its default).

        1.  Create a file for the gate input
            called *input* gate_tutorial.inp\_
        2.  Create the *gate time series table* INPUT_GATE:
        3.  In the table enter the following values into the
            appropriate fields:
            1.  Gate: *gate_1*
            2.  Device: *weir*
            3.  Variable: *op_from_node*
            4.  Input File: *${TUTORIALINPUT}*
            5.  Path/Value: */TUTORIAL/GATE/FLAP_OP//IR-YEAR/TIMEVAR/*
            6.  Fillin: *none* (Can you tell why fillin is
                    "none" for this time series?)
        4.  Add the include file to hydro.inp. The time series block
            should look as follows:

        HYDRO_TIME_SERIES  
        input_boundary_hydro_tutorial.inp  
        input_transfer_flow_tutorial.inp  
        input_hydro_ts_tutorial.inp  
        input_gate_tutorial.inp  
        END

        1.  Save the current settings.

        **Running HYDRO and QUAL**

        1.  In Windows Explorer, navigate to the directory: \_

        .

    2.  Right-click on the directory, *t4_timevar*, and select *Open
        Command Window Here*.

    3.  In the command window, type: *hydro hydro.inp*.

    4.  In the command window, type: *qual qual.inp*.

    5.  Open the *output.dss* file in the *t4_timevar* directory, and
        examine the results.
