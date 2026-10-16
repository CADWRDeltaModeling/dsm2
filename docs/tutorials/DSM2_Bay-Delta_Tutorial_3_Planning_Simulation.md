# DSM2 Bay-Delta Tutorial 3: Planning Simulation

## Purpose: ***The goal of this tutorial is to learn to preprocess and launch a Bay-Delta planning simulation using CalSim Output as the basis for flow inputs.***

## ***The CalSim study we will use is the ocap_2005A01A_EWA2_71_novamp_DV.dss provided in the tutorials/data directory. We will prepare and launch the run using both temporary barriers and permanent barriers configurations (SDIP: South Delta Improvements Program).***

## Preparation

We will begin by creating a study space to house the planning study.

1.  **Copy the study template:**
    1.  In windows, navigate to *\\{DSM2_home}\study_templates.* Copy
        the *ocap_sdip* template to *\\{DSM2_home}\tutorial\ocap_sdip*.
        Copy the *ocap_temp_barrier* template to
        *\\{DSM2_home}\tutorial\ocap_temp_barrier*
    2.  In each new study folder, create a directory called "output" if
        there is not such a folder there already.
    3.  Copy the file *ocap_2005A01A_EWA2_71_novamp_DV.dss* from
        *\\{DSM2_home}\timeseries* to
        *\\{DSM2_home}\tutorial\data\calsim.* Note that we just put this
        file in timeseries as a sample – in practice CalSim output will
        be exterior to the DSM2 distribution (or will be in the study
        folder).

  

1.  **Preprocess for sdip and temp_barriers:**
    1.  Navigate to the ocap_sdip study directory and open
        *config_sdip_ocap_71.inp*.
    2.  Make sure that the run dates are set to the full 1974-1991
        sixteen year planning period. It is a good idea to preprocess
        the full period even if you want to run a subset of these dates.
    3.  Set the DSM2MODIFIER to ocap_sdip_tutorial.
    4.  Make sure that the DICU version in the configuration file is
        2005, representing a 2005 level of development.
    5.  Makes sure the STAGE_VERSION in the configuration file is
        PLANNING-2-SL.  
    6.  Make sure the configuration file is pointing to the right data,
        which means using the right directory, file and DSS path to find
        the CalSim results. In this case, set:
        1.  CALSIMNAME to ocap_2005A01A_EWA2_71_novamp_DV (CalSim output
            file without the ".dss" extension)
        2.  CALSIMSTUDY_ORIGINAL to 2005A01A
        3.  ~~ CALSIMDIR to ../data/calsim ~~
    7.  Save your data
    8.  Launch the preprocessing system. Obtain a command prompt and
        type:

\> prepro config_sdip_ocap_71.inp

1.  1.  Repeat the steps above for the temporary barriers directory and
        the configuration file *config_ocap_temp_barriers.inp*. Make
        sure that the dates span the full 1974-1991 period and repeat
        the checks (d) and (e) for the temporary barrier configuration
        file.
    2.  Set the DSM2MODIFIER to ocap_temp_barrier_tutorial.
    3.  Launch the preprocessor with the command:

\> prepro config_ocap_temp_barriers.inp

1.  **Run DSM2:**
    1.  In Windows Explorer, navigate to the directory,
        *\\{DSM2_home}\tutorial\ocap_sdip*
    2.  Open the launch files hydro.inp and qual_ec.inp.
    3.  Set the dates to a shorter period, 1974-1976, so that the run
        will take reasonable time for the tutorial. Note that we always
        preprocess the full period even when we attempt to shorten the
        run.
    4.  Run the sdip simulation, for hydro and qual by typing:

\> hydro hydro.inp,~~ocap_sdip_tutorial.dss  ~~
\> qual qual_ec.inp

1.  1.  Uncomment and Repeat these steps (a-c) and run hydro and qual
        for the temporary barrier simulation.

  

1.  **Examine the output:**
    1.  The temporary barriers and permanent barriers protect water
        levels in the South Delta in very different ways. Compare the
        output at ROLD059, Old River at Tracy Blvd for your two runs to
        see the differences.
