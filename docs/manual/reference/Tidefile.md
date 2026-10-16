# Tidefile

## Overview

The tidefile is the HDF5-formatted binary file used to pass flow and geometry data from HYDRO to QUAL and PTM. The tidefile is specified as output by HYDRO in the [IO_FILE](https://dwrnpmsweb0110/io_file.html) table. It is specified as input to QUAL and PTM in the TIDEFILE section. Input tidefiles can be specified only in text.

Tidefiles can be stacked if desired, but this is an old feature that is now deprecated. Stacking means that the flow simulation can be divided temporally among several HYDRO runs and then the resulting tidefiles used sequentially in QUAL or PTM.

## Tables

### Example

```text
TIDEFILE     
START_DATE END_DATE FILENAME   
runtime    length   ${HYDROTIDEFILE} # begin run to 20JUL  
END 
```

The following example uses one tidefile with an environmental variable for the file name. This is the most common treatment.

### Example

```text
TIDEFILE     
START_DATE  END_DATE   FILENAME   
runtime     20JUL1996  hist1.h5      # beginning of run to 20JUL  
20JUL1996   24JUL1996  hist2.h5   
last        length     hist3.h5      # end of previous to end of run  
01SEP1996   length     ficticious.h5 # no error: will never be opened   
END 
```

This example uses several tidefiles tiled together to cover a longer period. Please let us know if you need this functionality, as it is a holdover from the old "repeating tide" days and will probably be deprecated.

#### Field Descriptions

- **START_DATE**: When to start using the tidefile. Tidefiles must be listed in temporal order. The START_DATE of the first tidefile must fall on or before the start of the run. The START_DATE of subsequent tidefiles must exactly coincide with the END_DATES of preceding tidefiles. There is no associated "TIME" part—tidefiles must be matched on calendar days. If a START_DATE is not given or is listed as "none", the timestamp in the tidefile will be used for the start. Special keywords:
  - `runtime`: start time in tidefile
  - `last`: use this tidefile when the previous tidefile ends
  - `none`: use default.
- **END_DATE**: When to stop using the tidefile. If not given, the tidefile is used until it ends. The END_DATE of the last tidefile must overlap the runtime of the simulation. Note that this can be tricky because the ending time is the time `0000` of the END_DATE, so you may need another day. Avoid this problem by specifying run dates with standard times (`0000` instead of military `2400`). Special keywords:
  - `length`: use all of the tidefile, up until its end
  - `none`: use default.
- **FILENAME**: Name of the file. Use upper/lower case consistently because filenames are case-sensitive.

#### Table Info

- **Identifier**: FILENAME

> ENVVARs are often used for names of files, DSS paths, and parameters that are varied over a study—the substitution will occur at runtime.
