# Versions

If the grid/arc is changed (moved/split/merged) at any boundaries of the Delta, CALSIM → DSM2 preprocess scripts (and DSM2 configuration) should change, i.e., a different version. Some typical keywords are usually used as part of the scenario version name:

- **Existing (EST)**: Represents the current Delta condition (could change over time).
- **No Action Alternative (NAA)**: Future scenarios without major grid changes.
- **Proposed Alternative (PA)**: Future scenarios with major grid changes (e.g., construction).
- **Level of Development (LOD)**: Represents land use information (e.g., 2005, 2030). Since censuses are infrequent, different scenarios may use the same LOD.
- **Sea Level Rise Projection (SLR)**: Represents climate change scenarios (e.g., 15cm, 45cm).

Refer to California Water Fix settings of CALSIM and DSM2 (page 70, Table B-8) for examples:
[Final EIR-EIS Appendix](http://baydeltaconservationplan.com/Libraries/Dynamic_Document_Library/Final_EIR-EIS_Appendix_5A_-_CALSIM_II_and_DSM2_Modeling_Simulations_and_Assumptions_Appendix_-_Section_B.sflb.ashx).

Operational changes or constraints usually only affect CALSIM results, not DSM2 settings. Thus, DSM2 preprocess and configuration often remain unchanged. A practical routine is to use the same file name/modifier/path for various scenarios within one version category (modify folder name or use unique names for post-processing).

### Commonly Used Versions

- Original scripts package in `DSM2/scripts` or `vista/scripts/dsm2`.
- CH2M prepared CWF-related EST, NAA, PA, combining LOD and SLR.
- SWP Delivery Capability Report (DCR), Water Storage Investment Program (WSIP).
- Widely used versions in DSM2 v806, with updates to DSM2 v812.
- Recent practice: CALSIM3 to DSM2 in SWP Fingerprinting study ([details](http://msb-confluence/display/~knam/CALSIM+to+DSM2)).
