# DSM2 - How to read hdf5 output files

DSM2 writes output in HDF5 format. This format can be read by Vista and
vscript 

## Step-by-step guide

To open a HDF5 file

1.  Open Vista 
2.  Drag and drop or use Session \> Open \> Tidefile from the menu
    options
3.  Select the data items needed and use the  Data \> Export \> Export
    Data To DSS menu item to export the Data to DSS files

<a
href="https://www.youtube.com/watch?v=5PCyxdUC4qw&amp;feature=youtu.be"
rel="nofollow">Video of How to read DSM2 - HDF5 files using VISTA</a>

  

Alternatively here is a snippet of vscript code that does something
similar

**Getting average concentrations from Qual HDF5 file**

``` py
from vtidefile import opentidefile
from vdss import writedss
from vutils import *
import vdisplay
from vdisplay import plot
import sys
import string

def get_avg_conc(tidefile, chan, twstr):
    tf=opentidefile(tidefile)
    if twstr != None:
        print 'Timewindow: %s'%twstr
        tw=timewindow(twstr)
    else:
        tw=None
    refs=tf.find(['','^%s$'%chan,'AVG CONC'])
    if refs and len(refs)==1:
        print "Getting data %s"%(str(chan))
        if tw!=None:
            ref=DataReference.create(refs[0],tw)
        else:
            ref=refs[0]
        return ref.data
    else:
        raise "No data found for %s in file %s"%(chan, tidefile)
if __name__ == '__main__':
    if len(sys.argv) != 2:
        print "Usage: vscript 
    tidefile=sys.argv[1]
    twstr="01JUL2014 0000 - 01AUG2014 0000"
    chans=[291,290,436,435,434,433]
    chan_concs=[]
    for chan in chans:
        chan_concs.append(get_avg_conc(tidefile, chan, twstr))
        
    for conc in chan_concs:
        plot(conc)
```

  

DSM2-vista also supports export data to hec-dss format (One or multiple
timeseries path could be selected) See the following menu option as
example. 

<img src="attachments/87229021/87229020.png"
data-image-src="attachments/87229021/87229020.png"
data-unresolved-comment-count="0" data-linked-resource-id="87229020"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2019-3-18_12-8-58.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87229021"
data-linked-resource-container-version="1" />

## Related articles

  

-   Page:

    [Updating the DSM2 historical
    simulation](/display/DSM2/Updating+the+DSM2+historical+simulation)

-   Page:

    [DSM2 Modernization Y2022](/display/DSM2/DSM2+Modernization+Y2022)

-   Page:

    [DSM2 Fortran Coding
    Standards](/display/DSM2/DSM2+Fortran+Coding+Standards)

-   Page:

    [DSM2 - How to read hdf5 output
    files](/display/DSM2/DSM2+-+How+to+read+hdf5+output+files)

-   Page:

    [Mini Calibration (2009)](/pages/viewpage.action?pageId=18055205)

  

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-3-18_12-8-58.png](attachments/87229021/87229020.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[VistaOpeningHDF5Files.mp4](attachments/87229021/87229022.mp4)
(video/mp4)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[VistaOpeningHDF5Files.gif](attachments/87229021/87229023.gif)
(image/gif)  
