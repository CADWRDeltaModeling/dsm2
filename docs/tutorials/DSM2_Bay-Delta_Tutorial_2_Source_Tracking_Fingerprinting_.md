# DSM2 Bay-Delta Tutorial 2: Source Tracking (Fingerprinting)

## Purpose: ***The purpose of this tutorial is to use the source tracking capabilities of the model to create a fingerprinting study. We will set up both volumetric and concentration-based fingerprinting and visualize the results.***

  

1.  **Reopen the historical tutorial**
    1.  In windows, navigate to *\\{DSM2_home}\tutorial\historical*.
        (folders and files are copied as described in the Delta tutorial
        1)

  

1.  **Create a model for source tracking:**

In the background, source tracking imposes a computational cost on QUAL
that is the same as one additional constituent per source. For this
reason, it is useful to comment out source tracking as a standard course
of running DSM2. But when you desire source tracking, you can uncomment
it as follows:

1.  1.  In *historical_qual_ec.inp*, locate the GROUPS include section.
    2.  Uncomment the group definitions for source tracking (delete the
        \# sign at the start of the line). You may wish to review the
        referenced file to see how the groups are identified.
    3.  Similarly uncomment the two fingerprinting files – the ones that
        have "source_track" in their names.

  

1.  **Define volumetric inputs**
    1.  Create the QUAL volumetric input file*.* Copy
        *historical_qual_ec.inp* and rename as
        *historical_qual_vol.inp*.
    2.  Modify the concentration blocks. Go through each of the node and
        reservoir concentration files for QUAL ec. Modify the
        constituent (variable) to *unit*, value (FILE) to constant,
        (PATH) to 100. This step is conceptually simple, but will
        produce a large file – feel free to break it into several files
        if you prefer. If you are using Notepad++, you may want to use
        its column delete/copying features (press alt while you make
        your selection).
    3.  Compare what you produced to the existing files in common_input
        that have "volumetric" in their names (node and reservoir
        concentration). Are they the same input? How could you test this
        using the echoed output?

  

1.  **Define the fingerprinting output**
    1.  Specify Clifton Court concentration output for each of the
        source groups defined in the previous step, for both
        constituents: *ec* and *unit*, in block
        OUTPUT_RESERVOIR_SOURCE_TRACK. The name should be clifton_court,
        the concentration (variable) should be ec or volume and the
        interval should be 1day. Avoid redundancy or use of the source
        in the output name: i.e. use "clifton_court" for the name, not
        "clifton_ag" or "clifton_ec" . Because the source information is
        recorded in the F part of output dss file.
    2.  Similar specification could be defined for channel source track
        in block OUTPUT_CHANNEL_SOURCE_TRACK. Pick any channel you are
        interested and do the definition.

  

1.  **Run HYDRO and QUAL for One Year**
    1.  Using *historical_hydro.inp,* *historical_qual_ec.inp,*
        *historical_qual_vol.inp* as the launch files, run HYDRO and
        QUAL for one year in 2002. Start QUAL a day later to avoid mass
        conservation errors in the first hour. Make sure the init_conc
        variable (in SCALAR block) is set to zero so that there will be
        no initial condition contribution for any variables (note: for a
        volumetric fingerprint, it may be useful to make this
        concentration 100 if you want to include initial conditions in
        the fingerprint analysis).
    2.  Open the output file (*historical.dss*), and examine the
        results.

  

1.  **Process the output**
    1.  Use VISTA or HEC-DSSVUE to open up the output file. Copy
        May-September concentrations source track of Clifton Court for
        each location. Paste the output into a new sheet in the Excel
        provided called excel_fingerprint.xls, which you can use as a
        reference. Use the "stacked area plot" in Excel (one of the
        standard Excel plot types) to plot up the fingerprint results.

  
