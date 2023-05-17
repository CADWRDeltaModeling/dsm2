# CALSIM - DSM2 Integration

## Background

CALSIM is a water operations simulation model. It meets demands using
reservoir release operations and other operational criteria. A crucial
operational criteria is meeting the salinity and X2 standards in the
Delta.

CALSIM relies on DSM2 simulation of water quality standards. However
DSM2 is computationally expensive to run in repeated scenarios needed by
CALSIM. CALSIM relies on a linear programming approach and needs flow
salinity relationships to estimate the flow needed to meet a particular
water quality standard. Furthermore CALSIM is a monthly model and needs
to make assumptions pertaining to that limitation. 

## Artificial Neural Networks (ANNs)

To make it computationally feasible, the flow salinity relationships are
derived from DSM2 simulations with perturbations of inputs that are of
concern to CALSIM. These flow relationship information is used as
training data for Artificial Neural Networks (ANNs); more specifically
Feed-forward Neural Networks (FNNs).  These ANNs then are surrogate
models for DSM2 and are supposed to represent the impact of operations
on X2 and salinity standards.

## Full circle analysis

To verify the results derived from having a surrogate DSM2 (ANN) model
in CALSIM, the CALSIM flows and gate conditions are converted into daily
inputs (with assumptions for monthly to daily) for DSM2 and the output
salinity is checked against the X2 or salinity standards in CALSIM. This
is called a "full circle analysis".  Typically  these have been done for
a select period of 16 years but can be extended to the entire period of
82 years of simulation if desired.

## DSM2 boundary conditions

DSM2 needs flow and stage boundary conditions, i.e. the inputs at the
edges of the domain that would drive the simulation.

1.  Flow boundaries: CALSIM operates the reservoirs upstream of the
    Delta and as a result the flow conditions are established by CALSIM
    simulations, though on a monthly time step resolution.
2.  Gate positions:  CALSIM operates these to satisfy regulations and
    other constraints. 
3.  Stage boundary: The only one is the ocean boundary at Martinez that
    is derived from astronomical stage at San Francisco with regression
    using historical data to transfer to Martinez (Planning tide
    generator)
4.  Martinez EC boundary: This is derived from a flow salinity
    relationship based on G model and stage boundaries (Planning
    Martinez EC generator)
5.  Vernalis EC boundary. Derived from flow regression equations (Needs
    reference here)
6.  Consumptive Use. These are represented in DSM2 at 258 nodes, CALSIM
    does not directly simulate these, however they are provided as input
    to CALSIM based on consumptive use models
7.  Agricultural Drain EC. These are the most uncertain of the boundary
    conditions and are represented in DSM2 as annually repeating values.
8.  Waste water treatment plants ??

## Implementation

These boundary conditions are explicitly mapped in this document between
the CALSIM and DSM2 schematics. [Schematics and
Boundaries](Schematics_and_Boundaries)

## Resolving Monthly - Daily conversions

CALSIM is a monthly time step model and DSM2 runs on 15 min or lower
time steps. The input data for CALSIM is monthly averaged i.e. a single
value for the entire month. DSM2 typically takes daily input values and
is also capable of hourly or sub hourly resolved values.  This mismatch
has to be resolved when doing this integration.

For daily to monthly conversions, it is simply a monthly averaging
technique. For certain quantities, such as gate positions, a count of
values may be computed ?

For monthly to daily conversions, there is huge impedance. This means a
lot of information that is lost has to be either estimated or left as
the same value repeated over the days of the month. This is usually the
case for the flows, except that for stability reasons ( hydrodynamic
models ) the transition days between months employ a volume conserving
spline to smooth the transition. 

-   Discuss daily variation issue here

## Version Control

CALSIM and DSM2 have different versions, evolving at different rates for
different needs. As a result is important to manage these versions and
the mappings between them. Draft_CALSIMII_DCU_Modification_081809

-    What if CALSIM schematic changes?  Implication for the integration
    above?

  

## Notes

Martinez stage has been adjusted a little bit on 24DEC1967 to overcome a
dry-up breakdown at channel 201. The correction reside in a
timeseries ${DSM2}\timeseries\Planning_Tide_82years.dss. Planning study
users should add it to replace the regular timeseries.

  

  

  

  

  

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[bat_prep.png](attachments/87228590/87228589.png) (image/png)  
