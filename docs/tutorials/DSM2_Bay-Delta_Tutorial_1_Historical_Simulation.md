# DSM2 Bay-Delta Tutorial 1: Historical Simulation

# Purpose:

This tutorial will demonstrate how to launch a basic run of the
historical HYDRO and QUAL simulations.You will also get practice using
the study templates that are distributed with DSM2, see how the
configuration file is used, make some changes in the output and learn
about the post-processing "transfer" script for averaging your output.

Except as part of a re-calibration, it is rare to make big changes in
the historical simulation. More commonly, you will want to add a few
output locations or scalars. Large scale policy or physical changes are
usually analyzed within a Planning simulation framework, covered in a
later tutorial.

## HYDRO and QUAL

1.  **Copy the historical template:**
    1.  In windows, copy the
        folder *\\{DSM2_home}\study_template\historical* to the tutorial
        directory, after creating *\\{DSM2_home}\tutorials\historical*.
        If there is already a historical folder, just copy the contents.
    2.  Open *historical_hydro.inp* and *historical_qual_ec.inp.* Note
        the CONFIGURATION sections of both reference a
        file *configuration_historical.inp*. By containing variables
        such as run dates in this file, you can more easily synchronize
        the models.
    3.  Examine the *common_input* directory. By looking
        at *historical_hydro.inp, configuration_historical.inp* and the
        other main input files, you will see that many of the included
        files for the models are in the directory ${DSM2INPUTDIR}. In
        this distribution, this variable points
        to */dsm2/common_input* – a repository in which all the
        distributed DSM2 input files are housed. Later, you may want to
        copy the input files locally and repoint ${DSM2INPUTDIR} to this
        local directory. In fact, there are tools to help with this.
        Regardless of whether you copy them, please resist changing the
        files directly – it is much easier to diagnose problems if you
        make your changes in the main file (*historical_hydro.inp,
        historical_qual_ec.inp*…) or in a new file of your own making.
2.  **Modify the Run Times in the Configuration File:**

In the configuration file, set the runtime definitions as follows.

#runtime  
START_DATE 01JUL1996  
START_TIME 0000  
QUAL_START_DATE 02JUL1996  
PTM_START_DATE ${QUAL_START_DATE}  
END_DATE 01SEP1996  
END_TIME 0000 

1.  **Note the Output Step in HYDRO:**

If you look in the channel output files
(e.g. *output_channel_std_hydro_rki_20090715.inp)*, you will find that
the time step of the output is itself an ENVVAR definition called
${FINE_OUT}. This is usually defined as 15 minutes in configuration
file. Although DSM2 v8 will perform daily averages, it is recommended
that you use the finer output and aggregate as a postprocessing step (we
will cover this shortly).

1.  **Add some Output**

In historical_hydro.inp, add a block containing an extra flow output for
Old River at Head. Notice that the name in this case is a "practical"
name. Although you may sometimes add input with names like "ch56_0",
such a name is redundant with the other information in the line, is
difficult for non-modelers to understand and causes confusion if the
grid numbering changes.  
  
OUTPUT_CHANNEL  
NAME CHAN_NO DISTANCE VARIABLE INTERVAL PERIOD_OP FILE   
oldr_head 56 0 flow ${FINE_OUT} inst ${HYDROOUTDSSFILE}   
END 

1.  **Run HYDRO and QUAL:**
    1.  In Windows Explorer, navigate to the directory,
        \_\\{DSM2_home}\tutorial\_
    2.  Right-click on the *historical* directory, and select, *Open
        Command Window Here.*
    3.  In the command window, type: *hydro historical_hydro.inp*
    4.  Wait for HYDRO to complete its runs.
    5.  Now type: *qual* *historical_qual_ec.inp*

  

1.  **Aggregate the Output**

Above we recommended that you use post-processing to aggregate your
output. Let's see how this works. At a command prompt in the
${study}/output directory, type:  
\> transfer -–help  
This command should give you the options for the "transfer.py" script
that will help you aggregate your output.   
For instance, if you want to create a daily average of all your flow
output, type (this is all one line):   
\>transfer --out=postpro.dss --selection=///FLOW////   
--transform=period_ave --interval=1DAY historical.dss   
As another example, you may want to take a Godin average of all the
stage output and put it in the same file:   
\>transfer --out=postpro.dss --selection=///STAGE////   
--transform=godin historical.dss   
You can similarly do monthly averages by making the interval 1MON and
you can "slice" in time by specifying a time window (the syntax is given
by the help command:  
\> transfer -–help 

1.  **Running QUAL with Volumetric fingerpringting:**
    1.  In the command window, type: *qual historical_qual_vol.inp*.
    2.  Open the qual echo file qual_vol_echo_historical.inp in the
        output subfolder.
    3.  Open the results file in the output subfolder, and examine the
        results.

  

1.  **Running QUAL with Nonconservative Constituents fingerpringting:**
    1.  In Windows Explorer, navigate to the
        directory, *\\{DSM2_home}\study_template\_
        \_historical_qual_do*\\ Conduct a similar study as EC and VOL.
    2.  Notice that the running time period is 1996-2000, since Stockton
        effluent is not using '*constant'* but detailed
        timeseries: *effluentflow96-2000.dss*

ENVVAR  
NAME VALUE   
STOCKTON_FLOW_FILE ${TSINPUTDIR}/effluentflow96-2000.dss \# needed for
DO runs, if not available use constant  
END

1.  1.  Open the results file in the output subfolder, and examine the
        results.

  

## Particle Tracking Modeling (PTM)

  

1.  **Run PTM in Delta Grid under Historical Condition**
    1.  In Windows Explorer, navigate to the directory, \_  
          
        Unknown macro: {DSM2_home}{\_}tutorial\\ in the command window,
        type: ptm historical_ptm.inp. \*If necessary, reduce the running
        time period by
        modifying *END_DATE* in *configuration_historical.inp*.
    2.  Open the ptm echo file ptm_echo_historical.inp in the output
        subfolder and examine the contents.
    3.  Open the ptmout.dss file in the output subfolder, and examine
        the results. Do a little mass balance to see if the particle
        fluxes add up.

  

1.  **Repeat with Particle Filter on Channel Turned on:**

Set particle filter at Head of Old River 

1.  1.  In historical_ptm.inp, create the table for particle filter,
        with constant closing operation.

  
PARTICLE_FILTER  
NAME NODE AT_WB FILLIN FILE PATH  
Filter_HOR 8 chan:54 last constant 0  
END 

1.  1.  Add the related output, like

  
PARTICLE_FLUX_OUTPUT  
NAME FROM_WB TO_WB INTERVAL FILE  
SJR-OLD chan:7 chan:54 15min ${PTMOUTPUTFILE}  
END 

1.  1.  Open the ptmout.dss file in the output subfolder, and examine
        the results

  
  

1.  **Repeat with Particle Filter on Reservoir Turned on:**

With particle filter installed at Clifton Court Forebay (this is a
special version of filter dealing with source flows directly connecting
to reservoir) 

1.  1.  In historical_ptm.inp, create the table for particle filter,
        with time-varying operation control, specified in DSS file.

  
PARTICLE_RES_FILTER  
NAME RES_NAME AT_WB FILLIN FILE PATH  
clfc_div_bbid clifton_court qext:dicu_div_bbid last ./filterOp.dss
/HIST+FILTER/CLFC_DIV/FILTER_OP//IR-DECADE/DWR-BDO/  
END 

1.  1.  Add the related output, like

  
PARTICLE_FLUX_OUTPUT  
NAME FROM_WB TO_WB INTERVAL FILE  
SWP-AG res:clifton_court group:bbid 15min ${PTMOUTPUTFILE}   
END 

1.  1.  Open the ptmout.dss file in the output subfolder, and examine
        the results

  
  

1.  **Repeat with Particle Filter on Source Flow Turned on:**

Agriculture source flow (diversions and seepages) could be required to
restrict particles from entering in simulations. It is one application
for particle filter. 

1.  1.  In Windows Explorer, navigate to the directory,
        \\{DSM2_home}\tutorial\\ Open the
        file *delta_dicu_filter_closed.txt*. Copy the content into
        historical_ptm.inp

  

1.  1.  Open the ptmout.dss file in the output subfolder, and examine
        the results

  
  

## Making animation of Particle Tracking Modeling (PTM)

  

1.  **Modify the PTM input file to make text output and to turn on the
    dispersion parameters:**
    1.  In Windows Explorer, copy the folder *ptm_animate* (with
        subfolders) from *\\{DSM2_home}\study_templates\ptm_animate*

to the study directory, creating:  
*\\{DSM2_home}\tutorials\historical\ptm_animate*

1.  1.  With the PTM, it is useful to be able to switch easily between
        text and dss output formats – note that the animator requires
        text files. The *configuration_historical.inp* file is
        structured so that we can swap the environmental
        variable *PTMOUTPUTFILE*. We are going to
        point *PTMOUTPUTFILE* to txt format so we can use the animator.
        1.  1.  Locate the *PTMOUTPUTFILE* at the end of the file, and
                modify as:

PTMOUTPUTFILE ptmout.txt

1.  1.  Open the file, *historical_ptm.inp*.
        1.  Locate the SCALARS section. Check all of the dispersion
            parameters to be *t*.

ptm_ivert *t* # Use Vertical velocity profile  
ptm_itrans *t* # Use Transverse velocity profile  
ptm_iey *t* # Use transverse mixing  
ptm_iez *t* # Use vertical mixing

1.  1.  1.  Make sure the *anim_db.bin* line is turned on (this is
            usually commented out to save much running time)

ptm anim out 15min ${DSM2OUTPUTDIR}/anim_db.bin   
  

1.  **Run PTM:**
    1.  In the command window, type: *ptm historical_ptm.inp*.
    2.  In Windows Explorer:
        1.  Navigate to the directory,

*\\{DSM2_home}\tutorials\historical\output*

1.  1.  1.  Examine the output in the *ptmout.txt* file.
        2.  Copy the files, *anim_db.bin* and *ptmout.txt*.
        3.  Navigate to the directory,

*\\{DSM2_home}\tutorials\historical\ptm-animate\dual\left_panel*

1.  1.  1.  Paste the files in the *left_panel* directory.

  

1.  **Repeat with** **Dispersions Parameters Turned Off:**
    1.  In Windows Explorer, navigate to the directory,
        \_\\{DSM2_home}\tutorials\historical\_
    2.  Open the file, *historical_ptm.inp*.
        1.  Locate the SCALARS section.
        2.  Change all of the dispersion parameters from *t* to *f*.

ptm_ivert *f* # Use Vertical velocity profile  
ptm_itrans *f* # Use Transverse velocity profile  
ptm_iey *f* # Use transverse mixing  
ptm_iez *f* # Use vertical mixing

1.  1.  In the command window, type: *ptm historical_ptm.inp*.
    2.  In Windows Explorer:
        1.  Navigate to the directory,

*\\{DSM2_home}\tutorials\historical\output*

1.  1.  1.  Copy the files, *anim_db.bin* and *ptmout.txt*.
        2.  Navigate to the directory,

*\\{DSM2_home}\tutorials\historical\ptm-animate\dual\right_panel*

1.  1.  1.  Paste the files in the *right_panel* directory.
        2.  Navigate to the directory,

*\\{DSM2_home}\tutorials\historical\ptm-animate*

1.  1.  1.  Double-click on *dual.bat* to open the animator.
        2.  Press start to start the animator and use the controls to
            adjust the speed.

  

1.  **Modifying the Animator Display:**
    1.  The *left_panel* and *right_panel* directories contain files
        needed for operation:
        1.  Modify the data path names: *fluxInfoDB.data* stores file
            and path information for the PTM output (the flux output in
            the text file is labeled with DSS-like path names). The
            listings in this file will be turned into the small flux bar
            graphs you see in the animator. The integer you see above
            the file name is an internal node ID, which is how you
            assign locations in the animator (also
            see *network.dat* below). Also, an output file of the PTM
            version 8 contains a minor version number. So the user may
            need to modify the data path names in
            the *fluxInfoDB.data* according to corresponding path names
            in an output file, *ptmout.txt* in this example.
        2.  *labelsDB.data* stores label information. You list labels
            and their location (using nodes, see *network.dat* below)
        3.  *network.dat* stores internal *x-* and *y-*locations for
            nodes and channels. Pseudo-nodes are used for labels and
            other annotations as noted above. Please note that the nodes
            that are used in *network.dat* are internal node numbers,
            not external. (This makes the file very hard to edit, a
            point that will probably be addressed in the future). If you
            want a mapping of external-to-internal numbers, look at your
            echoed hydro output file (\*.out or \*.hof).
    2.  Examine these files and the labels in them. Change the labels to
        something creative and reopen the animator.
