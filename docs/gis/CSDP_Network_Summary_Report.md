# CSDP Network Summary Report

The CSDP Network Summary Report is created by the CSDP. It can be used
to help identify issues and potential issues with cross-sections in the
currently loaded network file.

It also contains important comparisons of DSM2 Virtual Cross-Section
volume with GIS calculated volumes.

**A network summary report uses the following input files:**

1.  An existing **channels.inp file** (such as
    channel_std_delta_grid_NAVD_20150129.inp). This file is used to get
    existing channel lengths for comparison, and to determine channel
    connectivity.
2.  The currently loaded **network file**.
3.  A **DSM2 output (.hof) file** which was created from the network
    file by running DSM2-Hydro with geometry created using the currently
    loaded network file with printlevel\>=5
4.  A **2m meter DEM CutFill validity** **file**, which was created
    based upon a visual inspection of the extent of the coverage of
    channel polygons with data in the 2m DEM files, using ArcGIS. If
    coverage is complete or very nearly complete, the validity is true.
5.  **CutFill results** files, each containing results from the CutFill
    operations for a given DEM.
6.  (Optional): **a list of channel groups**. Default is the list of
    groups for which polygons were created and used in the CutFill
    operations: "448_449_572_573, 439_440_441_451_452_453_454,
    438_443_444_450_570_571_574_575,290-294,281_282_295_296_297_301".
    You can add to this list.

**The report contains, for a given stage (usually 0.0 NAVD)**

1.  **Channel**: The name/number of the DSM2 channel. Could also be a
    group of channels. Examples: Sherman Lake would be identified as:
    290-294, Grizzly Bay would be identified as: 448_449_572_573
2.  Comparison of channels.inp length vs CSDP length:
    1.  **Channels.inp length**: length specified for DSM2 in the DSM2
        channels file above.
    2.  **CSDP length**: length calculated by the CSDP that will be used
        to replace the 'Channels.inp length'.
    3.  **% change**: the change in length CSDP vs Channels.inp
3.  **CSDP Average width**: For determining GIS volume
    estimate validity–average width should be at least 3 times the DEM
    grid size.
4.  If CSDP Volume is significantly different from DSM2 Volume, that
    would mean the effects of interpolation should be considered when
    modifying cross-sections.  
    1.  **CSDP Volume**: Channel volume calculated by CSDP for specified
        elevation assuming no inter-channel interpolation. Not used for
        comparison, but may be of interest to some.
5.  Not used for comparison, but may be of interest to some.
    1.  **CSDP Wetted Area**: Wetted area calculated by CSDP for
        specified elevation assuming no inter-channel interpolation. Not
        used for comparison, but may be of interest to some.
    2.  **CSDP Surface Area**: Surface area calculated by CSDP for
        specified elevation assuming no inter-channel interpolation.
    3.  **CSDP Max Area Ratio**: The maximum ratio of cross-sectional
        areas within a channel using CSDP cross-sections. <u>Important
        for numerical stability. Max area ratios should be \< 2.0.</u>
6.  If CSDP Volume is significantly different from DSM2 Volume, that
    would mean the effects of interpolation should be considered when
    modifying cross-sections.
    1.  **DSM2 Volume**: Channel volume calculated at specified
        elevation using virtual cross-sections from DSM2 output file.
        Used for comparison with GIS volumes.
7.  Not used for comparison, but may be of interest to some:
    1.  **DSM2 Wetted Area**: Wetted area calculated at specified
        elevation using virtual cross-sections from DSM2 output file
    2.  **DSM2 Surface Area**: Surface area calculated at specified
        elevation using virtual cross-sections from DSM2 output file
    3.  **DSM2 Max Area Ratio**: The maximum ratio of cross-sectional
        areas within a channel using virtual cross-sections.
        <u>Important for numerical stability. Max area ratios should be
        \< 2.0.</u>
8.  These results include valid and invalid values (see "2m Validity"
    and "10m Validity" below), so these are **probably not what you want
    to use**.
    1.  **GIS 2m Max\* Volume:** The GIS calculated channel volume,
        converted to ft3, using 2m DEM.
    2.  **GIS 2m Max\* Area:** The GIS calculated 2d area, converted to
        ft2, using 2m DEM.
    3.  **GIS 10 Max\* Volume:** The GIS calculated channel volume,
        converted to ft3, using 10m DEM.
    4.  **GIS 10m Max\* Area:** The GIS calculated 2D area, converted to
        ft2, using 10m DEM.
    5.  **DSM2-2m Vol:** The difference between the DSM2 virtual
        cross-section volume and the 2m DEM volume.
    6.  **DSM2-10m Vol:** The difference between the DSM2 virtual
        cross-section volume and the 10m DEM volume.
    7.  **2m Vol % diff:** The % difference between the DSM2 virtual
        cross-section volume and the 2m DEM volume.
    8.  **10m Vol % diff:** The % difference between the DSM2 virtual
        cross-section volume and the 10m DEM volume.
    9.  **CSDP Avg Width:** The average width of all the CSDP
        cross-sections in a channel at the specified elevation.
9.  **2m Width Ratio:** the CSDP Avg Width / 2m.
10. **10m Width Ratio:** the CSDP Avg Width / 10m.
11. **2m Validity:** 2m DEM volume and area calculations will be
    considered valid if a 2m DEM covers (or nearly covers) the entire
    channel polygon, and the **2m Width Ratio** \>= 3.0.
12. **10m Validity:** 10m DEM volume and area calculations will be
    considered valid if the **10m Width Ratio** \>= 3.0. Coverage is
    assumed to be complete for all channels.
13. **Valid Values: These are the ones you want to use:**
    1.  **Valid 2m Vol:** The value of **GIS 2m Volume**, if **2m
        Validity**==true, null otherwise.
    2.  **Valid 10m Vol:** The value of **GIS 10m Volume**, if **10m
        Validity**==true, null otherwise.
    3.  **DSM2-Valid 2m Vol:** The value of **DSM2-2m Vol** if **2m
        Validity**==true, null otherwise.
    4.  **DSM2-Valid 10m Vol:** The value of **DSM2-10m Vol** if **10m
        Validity**==true, null otherwise.
    5.  **Valid 2m Vol % diff:** The value of **2m Vol % diff** if **2m
        Validity**==true, null otherwise.
    6.  **Valid 10m Vol % diff:** The value of **10m Vol % diff** if
        **10m Validity**==true, null otherwise.
14. **CSDP highest bottom elevation**: The highest bottom elevation of
    all the cross-sections within the channel. Can help identify
    cross-sections that are likely to dry up.
15. **CSDP XS with no points**: The indices of the cross-sections in the
    channel that have no points. These cross-sections should be removed
    or edited.
16. **CSDP XS within 500.0 feet**: The indices of the cross-sections in
    the channel that are within 500.0 feet of each other. This could
    help identify duplicate cross-sections or unnecessary
    cross-sections. 
17. **These can help identify cross-sections that need to be adjusted to
    improve Max Area Ratio.**
    1.  **CSDP XS with Min area**: The index of the cross-section in the
        channel that has the smallest area at the specified elevation
    2.  **CSDP XS with Max area**: The index of the cross-section in the
        channel that has the largest area at the specified elevation
18. **CSDP XS with duplicate stations**: The indices of the
    cross-sections in the channel that have duplicate station values.
    These cross-section need to be fixed.
19. **We no longer care about negative dConveyance, so these can
    probably be ignored:**
    1.  **CSDP XS with -dK**: The indices of the cross-sections in the
        channel that have negative dConveyance at any elevation.
    2.  **CSDP XS with -dK in intertidal zone**: the indices of the
        cross-sections in the channel that have negative dConveyance in
        the intertidal zone. (intertidal zone is assumed to be limited
        to the range -2.5 \< Z \< 17.5 ft NAVD88)

\*When calculating GIS results, some channels overlap more than one
DEM.  When this happens, only the largest values of Volume and 2D Area
are used, because they are assumed to be associated with the DEM that
covers a greater portion of the polygon. If the coverage is not
complete, the value should be invalidated visually in the "2m DEM
Validity" file.

**Creating the network summary report:**

1.  Load a bathymetry file.
2.  Load or create a network file.
3.  Select Network-\>Reports→Network Summary Report
4.  In the following dialog:   
    <img src="attachments/87228828/87228832.png"
    data-image-src="attachments/87228828/87228832.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228832"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2019-3-25_16-8-30.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228828"
    data-linked-resource-container-version="1" height="250" />
5.  Either
    1.  click the "Load Dialog Values" button to populate the dialog
        using values read from a file, OR
    2.  Populate the dialog one field at a time by clicking the "Select
        File" buttons to specify
        1.  the channels.inp file (in the current DSM2 setup, this is
            channel_std_delta_grid_NAVD-20121214.inp),
        2.  optionally a .hof file created by running DSM2 with
            printlevel=5,
        3.  A string representing an array of channels to use for
            aggregating results (for example, all the channels
            representing Grizzly Bay). This string can contain lists of
            channels for which polygons were created for the GIS CutFill
            operation, or a custom list of channels.
        4.  A list of all files containing CutFill results from GIS,
        5.  A 2m DEM CutFill Validity file, which I created by visually
            inspecting the DEM coverage of polygons.
        6.  An output path
6.  The results will be written to a tab delimited .txt file specified
    above. Import the file into Excel, specifying tab as a delimiter.
7.  After the results are written, another window will appear containing
    graphs of the results.
8.  Save results if desired.

Here is the current network summary report:

There are many rows above the table which define the various quantities.
You may want to hide these rows when using the spreadsheet.

<a
href="/download/attachments/87228828/networkSummary.txt?version=1&amp;modificationDate=1611798718168&amp;api=v2"
data-nice-type="Text File"
data-file-src="/download/attachments/87228828/networkSummary.txt?version=1&amp;modificationDate=1611798718168&amp;api=v2"
data-linked-resource-id="87228831"
data-linked-resource-type="attachment"
data-linked-resource-container-id="87228828"
data-linked-resource-default-alias="networkSummary.txt"
data-mime-type="text/plain" data-has-thumbnail="true"
data-linked-resource-version="1" data-can-edit="true"
aria-label="networkSummary.txt"><img
src="rest/documentConversion/latest/conversion/thumbnail/87228831/1"
height="250" /></a><img src="attachments/87228828/87228827.png"
data-image-src="attachments/87228828/87228827.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228827"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2019-3-26_14-13-18.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228828"
data-linked-resource-container-version="1" height="250" />

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-3-26_14-13-18.png](attachments/87228828/87228827.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-3-25_16-9-52.png](attachments/87228828/87228829.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-3-25_16-9-41.png](attachments/87228828/87228830.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[networkSummary.txt](attachments/87228828/87228831.txt) (text/plain)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-3-25_16-8-30.png](attachments/87228828/87228832.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[networkSummary20190308.xlsx](attachments/87228828/87228833.xlsx)
(application/vnd.openxmlformats-officedocument.spreadsheetml.sheet)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-1-7_14-32-27.png](attachments/87228828/87228834.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[networkSummaryWithoutHof.xltx](attachments/87228828/87228835.xltx)
(application/vnd.openxmlformats-officedocument.spreadsheetml.template)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[networkSummaryWithHof.xltx](attachments/87228828/87228836.xltx)
(application/vnd.openxmlformats-officedocument.spreadsheetml.template)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[networkSummaryWithoutHof.xlsx](attachments/87228828/87228837.xlsx)
(application/vnd.openxmlformats-officedocument.spreadsheetml.sheet)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[networkSummaryWithHof.xlsx](attachments/87228828/87228838.xlsx)
(application/vnd.openxmlformats-officedocument.spreadsheetml.sheet)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[networkSummaryWithoutHof.txt](attachments/87228828/87228839.txt)
(text/plain)  
