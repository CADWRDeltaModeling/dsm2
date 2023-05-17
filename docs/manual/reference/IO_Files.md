# IO Files

## Overview:

The *IO_FILES* table is where you declare most of the non-dss output
from a simulation, including echoed text output files, restart files and
output tidefiles from HYDRO(input tidefiles are specified for QUAL and
PTM in the <a
href="file:///D:/delta/dsm2_v812_2309_fresh/documentation/html/reference/tidefile.html"
rel="nofollow">TIDEFILE</a>section). IO_FILES can only be specified in
the main text input file (hydro.inp, qual.inp, ptm.inp).

## Tables:

### IO_FILE

#### Keyword Descriptions

##### MODEL

Model generating the file. For a restart file this should be the model
(hydro\|qual) that is being restarted. For echoed output use the keyword
"output".

##### TYPE

Type of file: hdf5, restart,  or "none" for echoed output.

##### IO

Type of file "in" "out" or "none" for echoed output.

##### INTERVAL

Interval, for hdf5 tidefile output.

##### FILENAME

Name of file. Should have a suitable extension: \*.hrf for hydro restart
file, \*.qrffor qual restart file, \*.h5 for hdf5 tidefile or \*.out for
echoed output.

#### Table Info

##### Identifier:

none: no layering

##### Include Block:

none: launch file only

  

## Examples:

### HYDRO example:

This example includes standard hydro runtime output file, a restart
output file that is regenerated every model day (overwriting the
previous day's file), an hdf5 tidefile for passing information to QUAL
and an echo file (replicate of input). All of the file names use text
substitution -- the value would come from an environmental variable,
ENVVARS section in the input file or ENVVARS section of a config file.

    IO_FILES      
    MODEL  TYPE     IO    INTERVAL FILENAME  
    output none     none  none     ${HYDROOUTFILE}  
    hydro  restart  out   1day     ${QUALRSTFILE}  
    hydro  hdf5     out   1hour    ${HYDROHDF5FILE}  
    hydro  echo     out   none     ${DSM2MODIFIER}_hydro_echo.inp  
    END

### QUAL example:

This example includes a general qual runtime output file, a restart
output file that is regenerated every model day (overwriting the
previous day's file), a restart file that will be used to generate the
initial condition for the run, and an hdf5 tidefile for passing
information to QUAL and an echo file (exact replicate of input).

    IO_FILES      
    MODEL  TYPE    IO   INTERVAL FILENAME  
    output none    none none     ${QUALOUTFILE}  
    qual   restart out  1day     ${QUALRESTART}  
    qual   restart in   none     qualinit_30SEP1999.qrf  
    qual   hdf5    out  1hour    ${QUALHDF5FILE} 
    qual   echo    out  none     ${DSM2MODIFIER}_qual_echo.inp  
    END

  

### PTM example:

This example includes a PTM trace file (which is required to produce
flux DSS output) and an animation file (which is required for animated
output).

    IO_FILES      
    MODEL TYPE  IO  INTERVAL FILENAME  
    ptm   trace out none     ${DSM2OUTPUTDIR}/trace.out  
    ptm   anim  out 15min    ${DSM2OUTPUTDIR}/anim.out
    ptm   echo  out none     ${DSM2MODIFIER}_ptm_echo.inp  
    END

  

The runtime output file from HYDRO is used in the preparation of PTM
visualization tools.

  
