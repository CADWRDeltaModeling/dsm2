# Exporting CSDP Information into GIS

## Background 

CSDP contains channel outline, cross-section locations, and
cross-section profile as well as node locations. These are referenced in
NAVD88 vertical datum and NAD83 horizontal datum in UTM Zone 10N
projection.

## Methods

QGIS is a tool that can import WKT
((<a href="https://en.wikipedia.org/wiki/Well-known_text"
rel="nofollow">https://en.wikipedia.org/wiki/Well-known_text</a>) format
into a text based layer that can be then exported ArcGIS. 

There is Java code available for 

1.  Exporting CSDP channel outlines to WKT. <a
    href="https://github.com/CADWRDeltaModeling/dsm2-vista/blob/master/dsm2-input-model/src/gov/ca/dsm2/input/csdp/CSDPChannelNetworkToWKT.java"
    rel="nofollow">https://github.com/CADWRDeltaModeling/dsm2-vista/blob/master/dsm2-input-model/src/gov/ca/dsm2/input/csdp/CSDPChannelNetworkToWKT.java</a>
2.  Exporting CSDP node locations to WKT. <a
    href="https://github.com/CADWRDeltaModeling/dsm2-vista/blob/master/dsm2-input-model/src/gov/ca/dsm2/input/csdp/CSDPNodeCDLToWKT.java"
    rel="nofollow">https://github.com/CADWRDeltaModeling/dsm2-vista/blob/master/dsm2-input-model/src/gov/ca/dsm2/input/csdp/CSDPNodeCDLToWKT.java</a>

-   Nicky SandhuNeed to make these standalone generic utilities to be
    run from command line
-    Brad Tomcan you take a look at this code and see if it can be
    integrated into CSDP easily
