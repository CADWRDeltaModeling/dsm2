# DSM2 Bay-Delta Tutorial: SDIP Permanent Gate Simulation

## Purpose: ***The goal of this tutorial is to learn*** ***how the gate operating rules are implemented in DSM2v8 and how they work.***

## ***The Planning study*** ***we will use is "ocap_sdip" provided in the study_templates directory. We will prepare and launch the run using permanent barriers configurations (SDIP: South Delta Improvements Program).***

## Preparation

We will begin by creating a study space to house the planning study.

1.  **Copy the study template:**
    1.  In windows, navigate to *\\{DSM2_home}\study_templates.* Copy
        the *ocap_sdip* folder to *\\{DSM2_home}\tutorial*. Rename
        *ocap_sdip* to *ocap_sdip_oprule*.
    2.  In each new study folder, create a directory called "output" if
        there is not such a folder there already.
    3.  Copy the file *ocap_2005A01A_EWA2_71_novamp_DV.dss* (CALSIM
        output file used for planning runs) from
        *\\{DSM2_home}\timeseries* to
        *\\{DSM2_home}\tutorial\data\calsim.* Note that we just put this
        file in timeseries as a sample – in practice CalSim output will
        be exterior to the DSM2 distribution (or will be in the study
        folder).

  

1.  **Preprocess for SDIP barriers:**
    1.  Rename *config_sdip_ocap_71.inp* to
        *config_sdip_ocap_oprule.inp* and open
        *config_sdip_ocap_oprule.inp*.
    2.  Make sure that the run dates are set to the full 1974-1991
        (01OCT1974 0000 – 01OCT1991 0000) sixteen year planning period.
        It is a good idea to preprocess the full period even if you want
        to run a subset of these dates.
    3.  Set the DSM2MODIFIER to ocap_sdip_oprule.
    4.  Make sure that the DICU version in the configuration file is
        2005, representing a future (2005) level of development.
    5.  Makes sure the STAGE_VERSION in the configuration file is
        PLANNING-2-SL.
    6.  Make sure the configuration file is pointing to the right data.
        Open *config_sdip_ocap_oprule.inp*. Make sure that it is
        pointing to the right directory, file and DSS path to find the
        CalSim results. In this case, set:
        1.  CALSIMNAME to ocap_2005A01A_EWA2_71_novamp_DV (CalSim output
            file without the ".dss" extension)
        2.  CALSIMSTUDY_ORIGINAL to 2005A01A
        3.  CALSIMDIR to ../data/calsim
    7.  Save your data
    8.  Launch the preprocessing system. Obtain a command prompt and
        type:

\> prepro config_sdip_ocap_oprule.inp

1.  **Run DSM2:**
    1.  In Windows Explorer, navigate to the directory,
        *\\{DSM2_home}\tutorial\ocap_sdip_oprule*
    2.  Open the configuration files *config_sdip_ocap_oprule.inp.*
    3.  Set the dates to a shorter period; say 1975-1976 (01OCT1975 0000
        – 01OCT1976 0000), so that the run will take reasonable time for
        the tutorial. Note that we always preprocess the full period
        even when we attempt to shorten the run.
    4.  Open *hydro.inp* file and change the configuration file name to
        config_sdip_ocap_oprule.inp and save it.
    5.  Run the sdip simulation for hydro by typing:

\> hydro hydro.inp

1.  **Examine the output:**

The permanent barriers protect water levels in the South Delta in very
different ways. Compare the output at the downstream location of all
four gates with the flow and EC in San Joaquin River at Vernalis to see
how the operating rules are working. Check the Trigger and Action in the
plots.  
  

# DSM2 Bay-Delta Tutorial: Changing Gate Operation Rules

## Purpose: ***The goal of this tutorial is to learn how to change the gate operating rules and check the corresponding impact in the vicinity of gate structures.***

## ***The Planning study we will use is "*ocap_sdip_oprule*". We will prepare and launch the run using permanent barriers configurations (SDIP: South Delta Improvements Program).***

## Preparation

We will begin by changing operating rules in DSM2 model input setups.  
**1.** **Preprocess SDIP with change in Operating Rules:**

1.  1.  *Copy* *oprule_sdip_20090917.inp* file from *common_input* and
        paste it under *\\{DSM2_home}\tutorial\ocap_sdip_oprule*
        directory. Rename the file as
        *oprule_sdip_20090917_tutorial.inp.*
    2.  Open *Oprule_sdip_20090917_tutorial.inp* and browse to the line
        started with "orh_open" in Operating_Rule block. In the
        expression of Action; change RAMP 60MIN to RAMP 30MIN and save
        it.
    3.  Go to *\\{DSM2_home}\tutorial\ocap_sdip_oprule* directory. Make
        sure the configuration file is pointing to the right data. Open
        *config_sdip_ocap_oprule.inp*. Make sure that it is pointing to
        the right directory, file and DSS path to find the CalSim
        results. In this case, set:
        1.  CALSIMNAME to ocap_2005A01A_EWA2_71_novamp_DV (CalSim output
            file without the ".dss" extension)
        2.  CALSIMSTUDY_ORIGINAL to 2005A01A
        3.  CALSIMDIR to ../data/calsim
        4.  Set the DSM2MODIFIER to ocap_sdip_oprule_r30.
        5.  Set simulation time from 01OCT1974 0000 to 01OCT1991 0000.
    4.  Save your data
    5.  Launch the preprocessing system. Obtain a command prompt and
        type:

\> prepro config_sdip_ocap_oprule.inp  
  
  

1.  **Run DSM2:**
    1.  In Windows Explorer, navigate to the directory,
        *\\{DSM2_home}\tutorial\ocap_sdip_oprule*
    2.  Open the configuration files *config_sdip_ocap_oprule.inp.*
    3.  Set the dates to a shorter period; say 1975-1976 (01OCT1975 0000
        – 01OCT1976 0000), so that the run will take reasonable time for
        the tutorial. Note that we always preprocess the full period
        even when we attempt to shorten the run.
    4.  Open *hydro.inp* file. Add a line before the END command in the
        OPERATION block:

OPERATION  
……  
……  
……  
oprule_sdip_20090917_tutorial.inp  
END

1.  1.  Save your data
    2.  Run the sdip simulation for hydro by typing:

\> hydro hydro.inp

1.  **Examine the output:**

The permanent barriers protect water levels in the South Delta in very
different ways. Compare the flow at downstream location of Head of Old
River Gate between OPEN RAMP 60 MIN (*ocap_sdip_oprule .dss*) and OPEN
RAMP 30 MIN (*ocap_sdip_oprule_r30.dss*) runs to see how the changes in
operating rules are working.  
  
  

# DSM2 Bay-Delta Tutorial: Remove SDIP Operable Gates

## Purpose: ***The goal of this tutorial is to learn how to remove the gates from the study.***

## ***The Planning study we will use is "*ocap_sdip_oprule*". We will prepare and launch the run using permanent barriers configurations (SDIP: South Delta Improvements Program).***

## Preparation

We will begin by changing operating rules in DSM2 model input setups.  
**1.** **Preprocess SDIP with change in Operating Rules:**  
a. Go to *\\{DSM2_home}\tutorial\ocap_sdip_oprule* directory.  
b. Create a duplicate copy of *config_sdip_ocap_oprule.inp* file and
rename it *config_sdip_ocap_nogates.inp.*  
c. Open *config_sdip_ocap_nogates.inp* and browse to the line started
with "USE_BARRIERS". Assign the "USE_BARRIERS" environmental variable to
**FALSE** and save it. Make sure that it is pointing to the right
directory, file and DSS path to find the CalSim results. In this case,
set:

1.  1.  1.  CALSIMNAME to ocap_2005A01A_EWA2_71_novamp_DV (CalSim output
            file without the ".dss" extension)
        2.  CALSIMSTUDY_ORIGINAL to 2005A01A
        3.  CALSIMDIR to ../data/calsim
        4.  Set the DSM2MODIFIER to ocap_sdip_nogates.
        5.  Set simulation time from 01OCT1974 0000 to 01OCT1991 0000.

    2.  Save your data

    3.  Launch the preprocessing system. Obtain a command prompt and
        type:

\> prepro config_sdip_ocap_nogates.inp

1.  **Run DSM2:**
    1.  In Windows Explorer, navigate to the directory,
        *\\{DSM2_home}\tutorial\ocap_sdip_oprule*
    2.  Open the configuration files config_sdip_ocap_nogates*.inp.*
    3.  Set the dates to a shorter period; say 1975-1976 (01OCT1975 0000
        – 01DEC1975 0000), so that the run will take reasonable time for
        the tutorial. Note that we always preprocess the full period
        even when we attempt to shorten the run.
    4.  Run the simulation for hydro by typing:

\> hydro hydro.inp

1.  **Examine the output:**

Compare the output at upstream and downstream of all four gate locations
to verify the non-existence of the gates. Remember, the gate operations
will still appear to exist. You can check the gates are ignored by
checking the "install" output for the gates. The effect on flow can also
be verified: stage upstream and downstream of the gates location should
be identical and the flow should be non-zero (and identical possibly
with the exception of a small DICU flow).
