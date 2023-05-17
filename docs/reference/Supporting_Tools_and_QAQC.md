# Supporting Tools and QAQC

Currently, all the preprocess scripts are written with
DSM2-[Vista](http://msb-confluence/display/DM/Vista). It's recommended
to use the most updated version at
<a href="https://github.com/CADWRDeltaModeling/dsm2-vista"
rel="nofollow">https://github.com/CADWRDeltaModeling/dsm2-vista</a> (not
the one included in DSM2 package). \* Note to change the relevant
environment variables.

1\.

It is good practice to review and compare CalSIM outputs and its
preprocessed results (\*.dss) before running DSM2.
[HEC-DSS](http://msb-confluence/display/DM/HEC+Manuals) and
DSM2-[Vista](http://msb-confluence/display/DM/Vista) are the most widely
used tools.

<a href="http://www.hec.usace.army.mil/software/hec-dss/"
rel="nofollow">http://www.hec.usace.army.mil/software/hec-dss/</a>

2\.

WRIMS' report tool is useful to compare CalSIM outputs, i.e. DSM2
inputs (timeseries\CALSIM\\DV.dss)

<a
href="https://www.water.ca.gov/Library/Modeling-and-Analysis/Modeling-Platforms/Water-Resource-Integrated-Modeling-System"
rel="nofollow">https://www.water.ca.gov/Library/Modeling-and-Analysis/Modeling-Platforms/Water-Resource-Integrated-Modeling-System</a>

\* Note to keep consistence in the pathnames, time windows, etc between
the comparison scenarios.

<img src="attachments/87228626/87228630.png"
data-image-src="attachments/87228626/87228630.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228630"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="wrimsReport.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228626"
data-linked-resource-container-version="1" />

<img src="attachments/87228626/87228629.jpg"
data-image-src="attachments/87228626/87228629.jpg"
data-unresolved-comment-count="0" data-linked-resource-id="87228629"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="wrimsReport1.JPG"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/jpeg"
data-linked-resource-container-id="87228626"
data-linked-resource-container-version="1" />

<img src="attachments/87228626/87228628.jpg"
data-image-src="attachments/87228626/87228628.jpg"
data-unresolved-comment-count="0" data-linked-resource-id="87228628"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="wrimsReport2.JPG"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/jpeg"
data-linked-resource-container-id="87228626"
data-linked-resource-container-version="1" />

  

  

3\.

Another good tool to compare between scenarios dss (in general) is the
scripts [Compare DSS
Tool](http://msb-confluence/display/DM/Compare+DSS+Tool) in
DSM2-[Vista](http://msb-confluence/display/DM/Vista).

${vista}\bin\compare_dss.bat

<img src="attachments/87228626/87228627.jpg"
data-image-src="attachments/87228626/87228627.jpg"
data-unresolved-comment-count="0" data-linked-resource-id="87228627"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="compareDSS.JPG"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/jpeg"
data-linked-resource-container-id="87228626"
data-linked-resource-container-version="1" />

or a simplified version to check
consistence ${vista}\bin\compare_dss_files.bat

\* One good practice is to locate changes first (maybe the big ones),
then use compare_dss.bat to specify and illustrate them.

<img src="attachments/87228626/87228625.jpg"
data-image-src="attachments/87228626/87228625.jpg"
data-unresolved-comment-count="0" data-linked-resource-id="87228625"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="compareDSSfiles.JPG"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/jpeg"
data-linked-resource-container-id="87228626"
data-linked-resource-container-version="1" />

  

4\.

A quick way to check accuracy of preprocess is to compare Net Delta Flow
(NDO) = inflows-outflow-CU

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[compareDSSfiles.JPG](attachments/87228626/87228625.jpg) (image/jpeg)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[compareDSS.JPG](attachments/87228626/87228627.jpg) (image/jpeg)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[wrimsReport2.JPG](attachments/87228626/87228628.jpg) (image/jpeg)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[wrimsReport1.JPG](attachments/87228626/87228629.jpg) (image/jpeg)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[wrimsReport.png](attachments/87228626/87228630.png) (image/png)  
