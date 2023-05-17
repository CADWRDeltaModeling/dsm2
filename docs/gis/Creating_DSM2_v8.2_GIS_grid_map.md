# Creating DSM2 v8.2 GIS grid map

# Creating shapefiles

The existing CSDP network file for the DSM2 8.2 network that is
incomplete and contains errors. The network file for DSM2 v8.3 is
accurate, but contains some additional channels and nodes that were
moved. The easiest way to create shapefiles for the DSM2 v8.2 grid is to
modify the network and landmark (nodes) for the 8.3 grid.

# Verification

The goal is to display the pdf gridmap as a background image in ArcGIS
to verify that all the channel and node number are correct and in the
correct locations.

1.  The following command (using ghostscript in Cygwin) creates a tif
    file from the dsm2 pdf grid map file:  
    gs -q -dNOPAUSE -sDEVICE=tiffg4 -sOutputFile=gridmappdf.tif
    "DSM2_Grid2.0 (1).pdf" -c quit
2.  Create a copy of the tif file, with "marsh" in the filename. This is
    because the pdf gridmap has the Suisun Marsh disconnected from the
    delta and printed in a different scale.
3.  Next step is to identify a few landmarks that are easily
    identifiable on both the pdf grid map and on the basemap in ArcGIS.
    I chose 3 points: one in the north delta, near the confluence, and
    in the south delta. 
4.  In QGIS, select Plugins-Manage and Install Plugins:  
    <img src="attachments/87228656/87228664.png"
    data-image-src="attachments/87228656/87228664.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228664"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-12-43.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="151" />
5.  Search for "GDAL", check the box "Georeferencer GDAL", then click
    close:   
    <img src="attachments/87228656/87228663.png"
    data-image-src="attachments/87228656/87228663.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228663"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-14-8.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="250" />
6.  Select Raster-Georeferencer:   
    <img src="attachments/87228656/87228662.png"
    data-image-src="attachments/87228656/87228662.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228662"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-14-51.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="246" />
7.  Click the Open Raster
    button <img src="attachments/87228656/87228661.png"
    data-image-src="attachments/87228656/87228661.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228661"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-15-45.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="94" />'
8.  Select the pdf file.
9.  Select Settings-Transformaion
    Settings<img src="attachments/87228656/87228660.png"
    data-image-src="attachments/87228656/87228660.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228660"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-16-53.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="131" />
10. Use the following settings, including an output
    filename: <img src="attachments/87228656/87228659.png"
    data-image-src="attachments/87228656/87228659.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228659"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-17-15.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="250" />
11. Click on a point in the map, and enter UTM coordinates, then click
    OK: <img src="attachments/87228656/87228658.png"
    data-image-src="attachments/87228656/87228658.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228658"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-17-47.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="250" />
12. When you have specified coordinates for all your points, click the
    start georeferencing button. A tif file will be created, which you
    can load into ArcGIS.<img src="attachments/87228656/87228657.png"
    data-image-src="attachments/87228656/87228657.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228657"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_7-18-51.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="99" />
13. In ArcGIS, adjust the layer
    transparency. <img src="attachments/87228656/87228655.png"
    data-image-src="attachments/87228656/87228655.png"
    data-unresolved-comment-count="0" data-linked-resource-id="87228655"
    data-linked-resource-version="1" data-linked-resource-type="attachment"
    data-linked-resource-default-alias="image2020-5-12_9-25-2.png"
    data-base-url="http://msb-confluence"
    data-linked-resource-content-type="image/png"
    data-linked-resource-container-id="87228656"
    data-linked-resource-container-version="1" height="250" />
14. Now you can easily compare the pdf gridmap to the GIS data.

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_9-25-2.png](attachments/87228656/87228655.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-18-51.png](attachments/87228656/87228657.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-17-47.png](attachments/87228656/87228658.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-17-15.png](attachments/87228656/87228659.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-16-53.png](attachments/87228656/87228660.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-15-45.png](attachments/87228656/87228661.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-14-51.png](attachments/87228656/87228662.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-14-8.png](attachments/87228656/87228663.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-5-12_7-12-43.png](attachments/87228656/87228664.png)
(image/png)  
