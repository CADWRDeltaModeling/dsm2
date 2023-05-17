# Importing Digital Elevation Maps (DEMs) into CSDP

## Background

CSDP was developed in the late 1990s and can only consume point features
in its custom format. This document outlines the process of converting
modern DEMs in raster form into files that CSDP can use to bring in the
latest bathymetry information that is developed in modern tools such as
ArcGIS

## CSDP File Format

CSDP supports bathymetry data as point features in XYZ format along with
columns for year and source of data. In addition CSDP allows for a
metadata defining the projection system (it only supports two UTM NAD83
and NAD27)

Below is a sample header from a CSDP bathymetry file

    ;HorizontalDatum:  UTMNAD83
    ;HorizontalZone:   10
    ;HorizontalUnits:  Meters
    ;VerticalDatum:    NAVD88
    ;VerticalUnits:    USSurveyFeet
    ;Filetype: bathmetry
    ;NumElements: 1544252
    563970.000000000 4234180.000000000 112.7323 2012 SF_DEM
    563990.000000000 4234180.000000000 117.6413 2012 SF_DEM

  

## Steps

1.  Use Arc Toolbox \> Conversion Tools \> From Raster \> Raster to
    ASCII to output DEM as text file. 

    For large dems, click on the Environments in the dialog box in step
    1 and make sure the "Output Coordinates" are in NAD83, zone 10, in
    meters in UTM projection, and the vertical datum should be NAVD88 in
    meters. and that the "Processing Extent" is "Same as Display". Zoom
    in to the relevant portion before running the tool in step1 and that
    should limit the DEM output to just the viewable area.

2.  Use this program: <a
    href="https://github.com/CADWRDeltaModeling/csdp/blob/master/csdp/resources/ASCIIGridToCSDPConverter.exe"
    rel="nofollow">ASCIIGridToCSDPConverter</a> \<raster ascii
    filename\> \<prn output filename\>

3.  Open \<prn output filename\> in CSDP

4.  You can also use the CSDP:

    <table class="confluenceTable">
    <tbody>
    <tr class="header">
    <th class="confluenceTh"><div class="content-wrapper">
    <p>Select Bathymetry-Import Bathymetry from ASCII Raster</p>
    <p><img src="attachments/87228847/87228848.png"
    data-image-src="attachments/87228847/87228848.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228848"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2019-6-14_11-20-28.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228847"
    data-linked-resource-container-version="1" height="250" /></p>
    </div></th>
    <th class="confluenceTh"><div class="content-wrapper">
    <p>Fill in the dialog. If dataset is more dense than you need, you can
    enter a factor greater than 1</p>
    <p><img src="attachments/87228847/87228846.png"
    data-image-src="attachments/87228847/87228846.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228846"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2019-6-14_11-20-44.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228847"
    data-linked-resource-container-version="1" height="250" /></p>
    </div></th>
    </tr>
    &#10;</tbody>
    </table>

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-6-14_11-20-44.png](attachments/87228847/87228846.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-6-14_11-20-28.png](attachments/87228847/87228848.png)
(image/png)  
