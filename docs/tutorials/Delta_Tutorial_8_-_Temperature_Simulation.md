# Delta Tutorial 8 - Temperature Simulation

DSM2 can be used to simulate water temperature and transport of this
property. It also is influenced with suspended particles and bio matter
in the water and is provided as a module in DSM2. Water temperature can be simulated using either QUAL or GTM. 

-   Hari Could you help outline the steps for a tutorial in temperature
    simulation ?

## Step-by-step guide

1.  
2.  

## Water temperature modeling in GTM

Water temperature modeling in GTM share the same inputs as QUAL with the following differences:

1. Users can select whether to use measured solar raditaion for heat budget calculation in GTM, whereares QUAL can only use solar radiation calculated from empirical equations.
    - The switch to use measured solar radiation or not is parameter **use_meas_solar** in ``scalar_qual_temp_param.inp``, with **true** being use measured solar radiation and **false** being use calculated solar radiations from empirical equations.
        - When use measured solar radiation, users need to provide the time series and specify the dss data path names in ``input_climate_delta_hist.inp``.

2. When use GTM for temperature modeling, users need to use gtm specific input file ``gtm.inp`` as there are unique parameters in ``gtm.inp`` such as ``gtm_dx`` and ``gtm_time_step``, whereas when use QUAL for temperature modeling, users need to use QUAL specific input file ``qual.inp``.


## Related articles

-   Page:

    [Data Requirement](/display/DSM2/Data+Requirement)

-   Page:

    [Delta Tutorial 8 - Temperature
    Simulation](/display/DSM2/Delta+Tutorial+8+-+Temperature+Simulation)

  

  
