# Coupled Hydro-GTM

## Overview

Coupled Hydro-GTM allows the two individual modules, hydro and GTM, to compuate at the same time step, making it possible to integrate any water quality effects, including water quality based operating rule ([EC based operating rule](EC_based_Operating_Rule.md)), into hydrodynamics computation.

One of the most important water quality effects on hydrodynamics in the Delta is salinity, which changes water density and therefore altering the hydrodynamics conditions. Coupled Hydro-GTM incorporates density effects from salinity into hydrodynamics computation and it simulates more realistic hydrodynamics conditions in the Delta than stand-alone hydrodynmaics computation.

## Tables of Contents:

-   [USE OF HYDRO-GTM](#use_of_Hydro-GTM)
-   [USE OF HYDRO-GTM WITH SALINITY DENSITY EFFECTS](#use_of_Hydro-GTM_with_salinity_density_effects)


### Use of Hydro-GTM

To run coupled HYDRO/GTM, provide the hydro input file and gtm input file as arguments. For instance, the command to run coupled Hydro/GTM with hydro input <span style="background-color: lightgray;">hydro.inp</span> and gtm input <span style="background-color: lightgray;">gtm.inp</span> is:

    hydro_gtm.exe hydro.inp gtm.inp

------------------------------------------------------------------------

### Use of Hydro-GTM with salinity density effects

To turn on coupled HYDRO/GTM with density effects from salinity, users need to add the ensure the two following parameters exist in **SCALER** section in <span style="background-color: lightgray;">hydro.inp</span> with vardensity specified to be **true**:

    vardensity       true
    terms            dynamicwave

To turn off the density effects from salinity in coupled Hydro-GTM, change vardensity from **true** to **false**.





