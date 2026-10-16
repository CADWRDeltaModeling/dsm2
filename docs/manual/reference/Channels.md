# Channels

## Overview

Channels are the fundamental objects of the DSM2 grid. The Channels table allows you to enter channel connectivity, parameters, and geometry. Channel connectivity is defined by upstream and downstream node numbers. Two child tables describe the locations and geometry of user-defined cross-sections in the selected channel. Note that a default initial condition is required for every channel number in the DSM2 grid, entered separately in the Channel Initial Conditions table.

## Tables

- [CHANNEL](#CHANNEL)
- [XSECT](#XSECT)
- [XSECT_LAYER](#XSECT_LAYER)

### CHANNEL

The CHANNEL table defines the connectivity, length, friction, and dispersion characteristics of a channel.

#### Field Descriptions

- **CHAN_NO**: Channel number. This is the identifier of the channel and corresponds to the number you typically see on a grid map.
- **LENGTH (ft)**: Length of the channel reach.
- **MANNING**: Manning's n friction coefficient for the whole reach.
- **DISPERSION**: Dimensional dispersion factor.
- **UPNODE**: Number of the upstream node at which the channel is connected.
- **DOWNNODE**: Number of the downstream node at which the channel is connected.

#### Table Info

- **Identifier**: CHAN_NO
- **Parent Table**: Table is parent
- **Include Block**: GRID

### XSECT

This table lists files where bathymetric cross-sections are specified by the user using the CSDP format. The table lists the fraction of the distance along the reach (from upstream to downstream) at which the user cross-section is located. These cross-sections will be interpolated by the model at computational points.

#### Field Descriptions

- **CHAN_NO**: Channel number where the cross-section is located.
- **DIST**: Fraction of distance from upstream node to downstream node where the cross-section is located.
- **FILE**: CSDP-formatted file where cross-section geometry is defined.

#### Table Info

- **Identifier**: CHAN_NO, DIST
- **Parent Table**: CHANNEL
- **Parent Identifier**: CHAN_NO
- **Include Block**: GRID

### XSECT_LAYER

The Cross-Section Layer Table lists geometric information about each cross-section. This information is in the form of lookup tables of hydraulically important quantities such as area, width, and wetted perimeter.

#### Field Descriptions

- **CHAN_NO**: Channel number in which the cross-section is located.
- **DIST**: Fraction of distance from upstream node to downstream node where the cross-section is located.
- **ELEV**: Elevation from the bottom at which properties are known. The area, width, etc., apply to this elevation, and channel properties between elevations are linearly interpolated.
- **AREA**: Area of the channel from the bottom to the cross-section (sq ft). Ignored if Area disagrees with the integral of WIDTH.
- **WIDTH**: Width of the channel at the top (ft).
- **WET_PERIM**: Wetted perimeter of the channel at the given elevation.

#### Table Info

- **Identifier**: CHAN_NO, DIST, ELEV
- **Parent Table**: CHANNEL
- **Parent Identifier**: CHAN_NO
- **Include Block**: GRID

## Examples

### CHANNEL with XSECT_LAYER cross-section

```text
# CHANNEL SPECS
CHANNEL
CHAN_NO LENGTH MANNING DISPERSION UPNODE DOWNNODE
1        15000   0.035        0.3      1        2 
2        15000   0.035        0.3      2        3
END

# XSECT_LAYER
XSECT_LAYER
CHAN_NO DIST  ELEV   AREA WIDTH WET_PERIM
1        0.5 -24.0    0.0  40.0      40.0 
1        0.5   0.0  960.0  80.0     91.22 
1        0.5  20.0 2640.0 160.0     133.6 
2        0.5 -24.0    0.0  40.0      40.0 
2        0.5   0.0  960.0  80.0     91.22 
2        0.5  20.0 2640.0 160.0     133.6 
END
```

### CHANNEL with XSECT (csdp) cross-section

```text
# CHANNEL SPECS
CHANNEL
CHAN_NO LENGTH MANNING DISPERSION UPNODE DOWNNODE
1        15000   0.035        0.3      1        2 
2        15000   0.035        0.3      2        3
END

# XSECT
XSECT
CHAN_NO DIST     FILE
1           0.5   1_0.50000.txt
2           0.5   2_0.50000.txt
END
```

## Notes

- All channels must have an initial condition and at least one cross-section.
- Avoid overspecifying cross-sections longitudinally or vertically. Ensure features are well-resolved by the model's spatial resolution.



