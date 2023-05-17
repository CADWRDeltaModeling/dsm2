# DSM2 Tutorial Overview

## DSM2 Overview

**DSM2 Modules**

-   **HYDRO**

-   **QUAL**

-   \*PTM\*The Delta Simulation Model II (DSM2) is a one-dimensional
    mathematical model for dynamic simulation of one-dimensional
    hydrodynamics, water quality and particle tracking in a network of
    riverine or estuarine channels. DSM2 can calculate stages, flows,
    velocities, mass transport processes for conservative and
    non-conservative constituents including salts, water temperature,
    dissolved oxygen, and trihalomethane formation potential, and
    transport of individual particles. DSM2 thus provides a powerful
    simulation package for analysis of complex hydrodynamic, water
    quality, and ecological conditions in riverine and estuarine
    systems.  
    DSM2 currently consists of three modules: HYDRO, QUAL, and PTM. The
    relationship between HYDRO, QUAL and PTM is shown in Figure 1. HYDRO
    simulates one-dimensional hydrodynamics including flows, velocities,
    depth, and water surface elevations. HYDRO provides the flow input
    for QUAL and PTM. QUAL simulates one-dimensional fate and transport
    of conservative and non-conservative water quality constituents
    given a flow field simulated by HYDRO. PTM simulates pseudo 3-D
    transport of neutrally buoyant particles based on the flow field
    simulated by HYDRO. PTM has multiple applications ranging from
    visualization of flow patterns to simulation of discrete organisms
    such as fish eggs and larvae. A fourth module for sediment transport
    is currently being developed. Further information is available on
    the web at <a
    href="http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/models/dsm2/dsm2.cfm"
    rel="nofollow">http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/models/dsm2/dsm2.cfm</a>  
    **HYDRO**  
    1-D flow, velocity, depth, and water surface elevations\*QUAL\*  
    1-D fate and transport of conservative and non-conservative
    constituents\*PTM\*  
    Pseudo 3-D transport of neutrally buoyant particles  
    **Figure 1:** **Schematic of DSM2 Modules**  

    ## **Forecast**

    \*Future Conditions\*DSM2 Study Types  
    **DSM2**  
    **Study Types**

-   **Historical**

-   **Forecasting**

-   \*Planning\*DSM2 is usually used for three kinds of Delta
    simulations: historic conditions, forecasting future conditions
    (real-time), and planning studies (Figure 2 and Table 1). Each type
    of DSM2 study is briefly described below  
    \*Recreate Historic Conditions\*Historical simulations replicate
    past operations, hydrologic conditions, water quality and Delta
    configurations. These historical simulations enable calibration and
    validation of the model by comparison of simulation results and
    field data. Historical simulations also augment available field data
    to provide a more spatially and temporally complete representation
    of the hydrodynamic and water quality conditions for that time
    period.  
    Forecasting simulations, also known as real-time simulations, use
    recent field data and forecast data to project Delta conditions into
    the near future (typically one to ten weeks). Recently collected
    historical data provide current conditions for the Delta. Recent
    tidal elevations at Martinez are used with an astronomical tide
    forecast to project the Martinez tide into the near future.
    Corresponding hydrodynamic and water quality conditions in the Delta
    are then simulated. Forecasting simulations can assist State Water
    Project operations decisions.  
    \*Planning Studies of Hypothetical Conditions\*Delta planning
    studies evaluate how hypothetical changes to factors such as
    hydrologic regimes, water quality standards, system operations, and
    Delta configurations may impact Delta conditions. To explore the
    impacts of a given scenario under various hydrologic conditions,
    DSM2 planning studies are typically run under a 16-year sequence of
    Delta inflows and exports derived from statewide water transfer and
    storage simulations using CalSim-II More information on CalSim-II
    can be found on the web at <a
    href="http://baydeltaoffice.water.ca.gov/modeling/hydrology/CalSim/index.cfm"
    rel="nofollow"><strong>http://baydeltaoffice.water.ca.gov/modeling/hydrology/CalSim/index.cfm</strong></a>
    .. Planning simulations can use historical or astronomical tidal
    data which incorporate influences of the spring-neap tidal cycle or
    simulations can use an average repeating tide (typically the 19-year
    mean tide). Planning simulations typically assess impacts of
    proposed changes to Delta operations or configuration such as
    modified reservoir releases or dredging of channels. Planning study
    may also investigate impacts of hypothesized changes in the natural
    environment such as sea level rise.  
    **Historical**  
    Replicate historical conditions\*Forecasting\*  
    Project conditions for the near future\*Planning\*  
    Hypothetical Delta changes\*DSM2\*  
    **Modes of Operation**  
      
      
      
      
      
      
      
      
      
      
      
    **Figure 2: DSM2 Modes of Operation**  
    **Table 1: Parameter Descriptions for Three Modes of DSM2
    Application**

    <table class="wrapped confluenceTable">
    <colgroup>
    <col style="width: 25%" />
    <col style="width: 25%" />
    <col style="width: 25%" />
    <col style="width: 25%" />
    </colgroup>
    <tbody>
    <tr class="odd">
    <td class="confluenceTd"><p><strong>Simulation
    Parameter</strong></p></td>
    <td class="confluenceTd"><p><strong>Replicate Historic
    Conditions</strong></p></td>
    <td class="confluenceTd"><p><strong>Forecasting Future
    Conditions</strong></p></td>
    <td class="confluenceTd"><p><strong>Planning Studies for Hypothetical
    Conditions</strong></p></td>
    </tr>
    <tr class="even">
    <td class="confluenceTd"><p>Boundary Tide</p></td>
    <td class="confluenceTd"><p>Historic or astronomical tide</p></td>
    <td class="confluenceTd"><p>Historic and projected astronomical forecast
    tide</p></td>
    <td class="confluenceTd"><p>Historic, astronomical</p></td>
    </tr>
    <tr class="odd">
    <td class="confluenceTd"><p>Input Data</p></td>
    <td class="confluenceTd"><p>Historic inflows and exports<br />
    Average Delta consumptive use</p></td>
    <td class="confluenceTd"><p>Recent and current inflows and exports<br />
    Average Delta consumptive use</p></td>
    <td class="confluenceTd"><p>CalSim-II statewide operations studies
    provide inflows and exports<br />
    Average Delta consumptive use</p></td>
    </tr>
    <tr class="even">
    <td class="confluenceTd"><p>Simulation Period</p></td>
    <td class="confluenceTd"><p>1990-2001 are currently possible</p></td>
    <td class="confluenceTd"><p>1-10 weeks into the future</p></td>
    <td class="confluenceTd"><p>1976-1991 sequence from CalSim-II statewide
    operations studies</p></td>
    </tr>
    </tbody>
    </table>
