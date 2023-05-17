# Channels

## Overview:

Channels are the fundamental objects of the DSM2 grid. The Channels
table allows you to enter channel connectivity, parameters and geometry.
Channels connectivity is defined by an upstream and downstream node
numbers of the channels. Two child tables describe the locations and
geometry of user-described cross-sections in the selected channel. Note
that a default initial condition is required for every channel number in
the DSM2 grid, and this is entered seperately in the Channel Initial
Conditions table.

  

## Tables:

-   [CHANNEL](#Channels-channel)
-   [XSECT](#Channels-xsect)
-   [XSECT_LAYER](#Channels-xsect_layer)

  

### CHANNEL

The CHANNEL table defines the connectivity, length, friction and
dispersion characteristics of a channel.

#### Field Descriptions

CHAN_NOChannel number. This is the identifier of the channel, and
corresponds to the number you typically see on a grid map.LENGTH
(ft)Length of the channel reachMANNINGManning's n friction coefficient
for the whole reach.DISPERSIONDimensional dispersion factor.UPNODENumber
of the upstream node at which channel is connected.DOWNNODENumber of the
downstream node at which channel is connected.

#### Table Info

Identifier:CHAN_NOParent Table:Table is parentInclude Block:GRID

------------------------------------------------------------------------

  

### XSECT

This table lists files where bathymetric cross-sections are specified by
the user using the CSDP format. The table lists the fraction of the
distance along the reach (from upstream to downstream) at which the user
cross-section is located. These cross-sections will be interpolated by
the model at computational points. Overspecification of geometry is a
frequent source of user error/misconception, please see usage
notes below. Also note that this style of input and the XSECT_LAYER
"single file" format below should not be freely mixed for a given
channel -- use one or the other.

#### Field Descriptions

CHAN_NOChannel number where cross-section is locatedDISTFraction of
distance from upstream node to downstream node where cross-section is
locatedFILECSDP-formatted file where cross-section geometry is defined.

#### Table Info

Identifier:CHAN_NO, DISTParent Table:CHANNELParent
Identifier:CHAN_NOInclude Block:GRID

------------------------------------------------------------------------

  

### XSECT_LAYER

The Cross-Section Layer Table lists geometric information about each
cross-section. This information is in the form of lookup tables of
hydraulically important quantities such as area, width and wetted
perimeter.

#### Field Descriptions

CHAN_NOChannel number in which cross-section is located.DISTFraction of
distance from upstream node to downstream node where cross-section is
locatedELEVElevation from bottom at which properties are known. The
area, width, etc. apply to this elevation, and channel properties
between elevations are linearly interpolated.AREAArea of channel from
bottom to cross section(sq ft). Ignored if Area disagrees with the
integral of WIDTH.WIDTHWidth of channel at top (ft).WET_PERIMWetted
perimeter of channel at given elevation.

#### Table Info

Identifier:CHAN_NO, DIST, ELEVParent Table:CHANNELParent
Identifier:CHAN_NOInclude Block:GRID

------------------------------------------------------------------------

  

## Examples:

  

CHANNEL with XSECT_LAYER cross-section

    # This example shows channels using cross-sections
    # In the XSECT_LAYER format. The benefit of this format
    # is that the input can all be put in one file.
    # This can be useful for archiving or echoing back input

    # CHANNEL SPECS
    CHANNEL
    CHAN_NO LENGTH MANNING DISPERSION UPNODE DOWNNODE
    1        15000   0.035        0.3      1        2 
    2        15000   0.035        0.3      2        3
    END

    # This is a child table. Its rows must "link" to a parent
    # using the parent id (CHAN_NO in this case).
    # Note that two cross-sections are defined here,
    # one in each channel, halfway downstream, with three
    # layers each. 
    XSECT_LAYER
    CHAN_NO DIST  ELEV   AREA WIDTH WET_PERIM
    1        0.5 -24.0    0.0  40.0      40.0 
    1        0.5   0.0  960.0  80.0     91.22 
    1        0.5  20.0 2640.0 160.0     133.6 
    2        0.5 -24.0    0.0  40.0      40.0 
    2        0.5   0.0  960.0  80.0     91.22 
    2        0.5  20.0 2640.0 160.0     133.6 
    END

CHANNEL with XSECT (csdp) cross-section

    # This example shows channels using cross-sections
    # In the XSECT format. The specification is not 
    # complete -- we are really referring to 
    # Cross-Section Development Program (CSDP) files
    # which are in there own format.

    # CHANNEL SPECS
    CHANNEL
    CHAN_NO LENGTH MANNING DISPERSION UPNODE DOWNNODE
    1        15000   0.035        0.3      1        2 
    2        15000   0.035        0.3      2        3
    END

    # This is a child table. It is an alternative to the 
    # XSECT_LAYER table (the two can co-exist, but you 
    # should not mix input for a channel). The FILE column
    # points to a file that contains the real data which
    # would normally come out of the CSDP or other tool.
    XSECT
    CHAN_NO DIST     FILE
    1           0.5   1_0.50000.txt
    2           0.5   2_0.50000.txt
    END

   
  

------------------------------------------------------------------------

-   All channels must have an initial condition and at least one
    cross-section.
-   Older versions of DSM2 had the notion of a "regular" cross-section
    (meaning rectangular). In the current DSM2 this is just a
    cross-section with two layers.
-   Users frequently overspecificy cross-sections, either by specifying
    more cross-sections longitudinally than the model can possibly use
    or by describing cross-sections vertically in such a way as to
    capture highly local features such as small constrictions, sills and
    undulations. DSM2 is commonly used with spatial resolution (delta x)
    of several thousand feet. You should only include features that are
    well resolved by this resolution, which means changes that persist
    over several miles. Even more importantly, you should avoid adjacent
    cross-sections with bottoms that vary greatly in elevation because
    they can cause odd behavior when cross-sections are interpolated to
    computation points. The bottoms layers of cross sections should
    represent the "overall" slope of the channel.

  
  
