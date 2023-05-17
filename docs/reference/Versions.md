# Versions

If grid/arc is changed (move/split/merge) at any boundaries of the
Delta,  CALSIM → DSM2 preprocess scripts (and DSM2 configuration) should
change, i.e. a different version. Some typical keywords are usually used
as part of the scenario version name:

-   Existing (EST) as the current Delta condition (could be different as
    time goes), No Action Alternative (NAA) as future scenarios without
    major grid change, Proposed Alternative (PA) as future scenarios
    with any major grid change (construction, etc)
-   Level of development (LOD, 2005, 2030 etc) represent Land of Use
    info, etc. Since census are not usually conduct very frequently,
    different scenarios could use the same LOD.
-   Sea level rise projection (SLR, 15cm, 45cm etc) represent climate
    change scenarios.

California Water Fix settings of Calsim and DSM2 (page 70, Table B-8)
could be referred as an example.

<a
href="http://baydeltaconservationplan.com/Libraries/Dynamic_Document_Library/Final_EIR-EIS_Appendix_5A_-_CALSIM_II_and_DSM2_Modeling_Simulations_and_Assumptions_Appendix_-_Section_B.sflb.ashx"
rel="nofollow">http://baydeltaconservationplan.com/Libraries/Dynamic_Document_Library/Final_EIR-EIS_Appendix_5A_-_CALSIM_II_and_DSM2_Modeling_Simulations_and_Assumptions_Appendix_-_Section_B.sflb.ashx</a>

Other than the above, operation change or constraints of standard
usually only affects CalSIM results, not DSM2 settings. Thus the DSM2
preprocess and configuration don't need changes, i.e. we can just place
in the results. A practical routine is use the same file
name/modifier/path for various scenarios within 1 version category
(modify folder name, or use unique name for the post-process).

  

Some commonly used versions in the office are listed below.

-   Original scripts package in DSM2/scripts or vista/scripts/dsm2. This
    is the original version.
-   CH2M helped preparing CWF related EST, NAA, PA, in combination of
    LOD and SLR. The earlier 2 are widely used as template in recent
    years.
-   SWP Delivery Capability Report (DCR), Water Storage Investment
    Program (WSIP)
-   Most widely used versions are in DSM2 v806 for now. Effort has been
    made to update it to DSM2 v812 with 1 practice for EST. (Annual
    Report [2017](http://msb-confluence/display/DM/2017) Chapter 1)
-   One recent related practice is a new version for [CALSIM3 to
    DSM2](http://msb-confluence/display/~knam/CALSIM+to+DSM2) in [SWP
    Fingerprinting
    study](http://msb-confluence/display/~knam/SWP+Fingerprinting+study).
