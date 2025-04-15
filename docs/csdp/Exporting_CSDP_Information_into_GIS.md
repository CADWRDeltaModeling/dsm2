# Exporting CSDP Information into GIS

## Background

CSDP contains channel outlines, cross-section locations, cross-section profiles, and node locations. These are referenced in NAVD88 vertical datum and NAD83 horizontal datum in UTM Zone 10N projection.

## Methods

QGIS can import WKT ([Well-Known Text](https://en.wikipedia.org/wiki/Well-known_text)) format into a text-based layer, which can then be exported to ArcGIS.

### Java Utilities

1. Exporting CSDP channel outlines to WKT: [CSDPChannelNetworkToWKT.java](https://github.com/CADWRDeltaModeling/dsm2-vista/blob/master/dsm2-input-model/src/gov/ca/dsm2/input/csdp/CSDPChannelNetworkToWKT.java)
2. Exporting CSDP node locations to WKT: [CSDPNodeCDLToWKT.java](https://github.com/CADWRDeltaModeling/dsm2-vista/blob/master/dsm2-input-model/src/gov/ca/dsm2/input/csdp/CSDPNodeCDLToWKT.java)

These utilities should be made standalone and generic for command-line use.
