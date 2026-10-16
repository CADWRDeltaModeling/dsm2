# CSDP Network Summary Report

The CSDP Network Summary Report is created by the CSDP. It helps identify issues and potential problems with cross-sections in the currently loaded network file. It also contains comparisons of DSM2 Virtual Cross-Section volume with GIS-calculated volumes.

## Input Files

1. **Channels.inp file**: Used to get existing channel lengths for comparison and determine channel connectivity.
2. **Network file**: The currently loaded network file.
3. **DSM2 output (.hof) file**: Created by running DSM2-Hydro with geometry from the network file.
4. **2m DEM CutFill validity file**: Created based on visual inspection of channel polygon coverage in the 2m DEM files.
5. **CutFill results files**: Contain results from CutFill operations for a given DEM.
6. **Optional channel groups list**: Default is the list of groups for which polygons were created and used in CutFill operations.

## Report Contents

1. **Channel**: DSM2 channel name/number or group of channels.
2. **Length Comparison**:
   - **Channels.inp length**: Length specified in the DSM2 channels file.
   - **CSDP length**: Length calculated by the CSDP.
   - **% Change**: Difference between CSDP and Channels.inp lengths.
3. **CSDP Average Width**: Used for GIS volume estimate validity.
4. **Volume Comparison**:
   - **CSDP Volume**: Channel volume calculated by CSDP.
   - **DSM2 Volume**: Volume from DSM2 output.

## Creating the Report

1. Load a bathymetry file.
2. Load or create a network file.
3. Select `Network > Reports > Network Summary Report`.
4. Fill in the dialog and save the results to a tab-delimited `.txt` file.
5. Import the file into Excel for further analysis.

## Attachments

- [image2019-3-26_14-13-18.png](attachments/87228828/87228827.png)
- [image2019-3-25_16-9-52.png](attachments/87228828/87228829.png)
- [image2019-3-25_16-9-41.png](attachments/87228828/87228830.png)
- [networkSummary.txt](attachments/87228828/87228831.txt)
- [image2019-3-25_16-8-30.png](attachments/87228828/87228832.png)
- [networkSummary20190308.xlsx](attachments/87228828/87228833.xlsx)
- [image2019-1-7_14-32-27.png](attachments/87228828/87228834.png)
- [networkSummaryWithoutHof.xltx](attachments/87228828/87228835.xltx)
- [networkSummaryWithHof.xltx](attachments/87228828/87228836.xltx)
- [networkSummaryWithoutHof.xlsx](attachments/87228828/87228837.xlsx)
- [networkSummaryWithHof.xlsx](attachments/87228828/87228838.xlsx)
- [networkSummaryWithoutHof.txt](attachments/87228828/87228839.txt)
