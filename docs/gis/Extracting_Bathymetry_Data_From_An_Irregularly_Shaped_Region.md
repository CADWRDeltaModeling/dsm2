# Extracting Bathymetry Data From An Irregularly Shaped Region

Using the CSDP, create a new centerline using the **Centerline-Create**
menu item.

The name of the centerline does not matter.

Add points to the centerline until it outlines the data you want to
extract. See example below. The endpoints do not need to be in the same
place. A polygon will be created whose vertices are all of the
centerline points, so the first and last points will be connected. Save
the network file.

I used a simple Java program called
ExtractShipChannelLeveesFromYoloBypassDEM, which uses hard-coded
filenames for both the input (network file and the bathymetry file) and
the output (bathymetry file). Eventually this code will be added to the
**Bathymetry** menu in the CSDP, which will export the data surrounded
by a polygon created from the selected centerline to a specified
filename.

  
<img src="attachments/87228843/87228842.png"
data-image-src="attachments/87228843/87228842.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228842"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2018-12-3_13-53-22.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228843"
data-linked-resource-container-version="1" />

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2018-12-3_13-53-22.png](attachments/87228843/87228842.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2018-12-3_13-53-12.png](attachments/87228843/87228844.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2018-12-3_13-52-46.png](attachments/87228843/87228845.png)
(image/png)  
