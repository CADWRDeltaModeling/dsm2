# Layers

## Overview:

DSM2 batches input data into files or "layers" in order to achieve the
following goals:

-   To group input into cohesive packages with similar content(examples:
    the standard grid, sdip operating rules).
-   To identify which items are changed when a new group of inputs is
    added to an existing simulation.

For example, consider the two layers of channels in the figure below.
The first layer defines seven channels and would have seven entries in
the CHANNEL table. This might represent a "base" grid. The second layer
changes the properties of Channel 2, adds a Channel 8 and removes
Channel 7. The second layer will have only three entries, shown in red.
These entries represent the changes relative to Layer 1, and presumably
are thematically related.

  

![Layer system example](../../attachments/87228551/87228550.png)

------------------------------------------------------------------------

### Example: Channel

Consider the above example using text input. We are going to
create CHANNEL tables representing the channel connectivity, and assume
the geometry is provided with CSDP style cross-sections listed in
an XSECT table (child items are always associated with parent items in
the same file).

The base data will be in a file *channel_base.inp*:

**channel_base.inp**

``` text
CHANNEL
CHAN_NO LENGTH MANNING DISPERSION UP_NODE DOWN_NODE
1        18000   0.030       0.80       1         2
2         8000   0.040       0.80       2         3
3        18000   0.040       0.80       3         4
4        18000   0.040       0.80       4         5
5        18000   0.040       0.80       3         5
6        22000   0.040       0.80       5         6
7        14000   0.040       0.80       6         7
END

XSECT
CHAN_NO   DISTANCE    FILE
1         0.200       1_0_200.txt
1         0.800       1_0_800.txt
2         0.500       2_0_500.txt
...
7         0.900       7_0_900.txt
END
```

  

The revisions are in *channel_revise.inp*:

**channel_revise.inp**

``` text
CHANNEL
CHAN_NO LENGTH MANNING DISPERSION UP_NODE DOWN_NODE
2         8000   0.030       0.80       2         3 # Masks + Alters
#3        9000   0.000       0.00      19        20 # Has no effect
^7       14000   0.040       0.80       6         7 # Masks + Deletes
...
8        16000   0.040       0.80       8         3 # Adds
END

XSECT
CHAN_NO DISTANCE  FILE
2          0.100  2_0_500.txt  # Masks lower level x-sects 
2          0.700  2_0_500.txt  #
7          0.900  7_0_900.txt  # Will be ignored
8          0.500  8_0_500.txt  # 
END
```

  

The two layers are managed by the model input file that is given
directly to the model, in this case *hydro_layering.inp*. The two
channel files are listed in a GRID include block that lists the layers
in increasing priority.

**hydro.inp**

``` text
GRID
channel_base.inp
channel_revise.inp
END
```

  

Now lets consider the details...

  

#### Include Blocks

Include blocks are input blocks in the master file that list other
files. The data from these other files is "included" in the order
listed. Priority is given to files read later, and these are assigned a
higher "layer number"

Include blocks can only contain specific types of input data. For
instance, a GRID input block only contains channel, gate, reservoir and
transfer physical specifications (not boundary conditions attached to
them). So the trick to using include blocks is knowing, say, that a
CHANNEL table belongs in a file in a GRID include block and
BOUNDARY_FLOW table belongs in a file in a HYDRO_TIME_SERIES block. In
the reference documentation, the include blocks should be listed for
each table in the *Table Information* section..

The only exception is the master file that is the one sent to the model
on the command line (often named something like hydro.inp, qual.inp,
ptm.inp). Data in this file always take precedence over other input.

### Layer Overriding

Layer overriding occurs when the same data item is defined in multiple
layers (files) in the same model. Files that are read later are given a
higher "layer number" and take precendence over files read earlier.
Within the same file it is an error to redefine an entry.

#### Identifiers

To use layering, you have to know what constitutes redefining an entry.
Whether two items represent the same data item depends on the identifier
for the table, which is some combination of columns that uniquely
identify the item using a name or number. Identifiers for each table are
listed in the reference documents. In the above example it is channel
number CHAN_NO. The trickiest identifiers are in the output, because
they involve two (NAME, VARIABLE) or three (NAME, VARIABLE,SOURCE_NAME)
columns. In the reference documentation, the identifier is listed for
each table in the *Table Information* section.

#### Parent-child Tables

When parent-child tables are present in a file (e.g., Channels, Cross
Section, Cross Section Layer), overriding is assessed at the level of
the parent or *top-level table*. When you override on a top-level table,
its child table information is completely replaced as well. So, for
instance, the cross-section at Channel 2 Distance 0.500 in
channel_base.inp in the example is completely ignored. The model makes
no attempt to "mix it in" with the replacement version of Channel 2.

Child tables must be in the same file as their parent tables. This is a
departure from earlier versions of DSM2, but is necessary to make
layering well-defined.

#### Deleting lower level data

Occasionally, the motivation for overriding an item is to eliminate it.
You can do this on any top-level table by prepending a carat ^ at the
beginning of the line. This will remove items on lower levels with the
same identifier. Note that it doesn't matter what other data you put in
the parent fields (you do need placeholders). Also you needn't add child
information if the only reason for the parent entry is to delete it --
but sometimes it is nice to have the child data there if you are
toggling back and forth.

Deleting data is quite different from commenting it out (using a \#
sign). Commenting data out on a high level would merely mean that the
input reader would skip over the line. It would not affect any data with
the same identifier on a lower level.

