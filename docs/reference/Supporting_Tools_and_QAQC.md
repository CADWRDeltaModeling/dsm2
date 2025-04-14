# Supporting Tools and QA/QC

All preprocess scripts are written with DSM2-[Vista](http://msb-confluence/display/DM/Vista). Use the most updated version available at [DSM2 Vista GitHub](https://github.com/CADWRDeltaModeling/dsm2-vista). Ensure relevant environment variables are updated.

### Recommended Practices

1. **Review and Compare CALSIM Outputs**
   - Use [HEC-DSS](http://msb-confluence/display/DM/HEC+Manuals) and DSM2-[Vista](http://msb-confluence/display/DM/Vista).
   - Ensure consistency in pathnames, time windows, etc., between comparison scenarios.

2. **WRIMS Report Tool**
   - Useful for comparing CALSIM outputs (e.g., DSM2 inputs in `timeseries\CALSIM\\DV.dss`).
   - [WRIMS Tool Details](https://www.water.ca.gov/Library/Modeling-and-Analysis/Modeling-Platforms/Water-Resource-Integrated-Modeling-System).

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

3. **Compare DSS Tool**
   - Use the [Compare DSS Tool](http://msb-confluence/display/DM/Compare+DSS+Tool) in DSM2-[Vista](http://msb-confluence/display/DM/Vista).
   - Scripts: `${vista}\bin\compare_dss.bat` or `${vista}\bin\compare_dss_files.bat`.

<img src="attachments/87228626/87228627.jpg"
data-image-src="attachments/87228626/87228627.jpg"
data-unresolved-comment-count="0" data-linked-resource-id="87228627"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="compareDSS.JPG"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/jpeg"
data-linked-resource-container-id="87228626"
data-linked-resource-container-version="1" />

\* One good practice is to locate changes first (maybe the big ones), then use compare_dss.bat to specify and illustrate them.

<img src="attachments/87228626/87228625.jpg"
data-image-src="attachments/87228626/87228625.jpg"
data-unresolved-comment-count="0" data-linked-resource-id="87228625"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="compareDSSfiles.JPG"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/jpeg"
data-linked-resource-container-id="87228626"
data-linked-resource-container-version="1" />

4. **Net Delta Flow (NDO) Check**
   - Compare NDO = inflows - outflows - CU for accuracy.

### Attachments

- [compareDSSfiles.JPG](attachments/87228626/87228625.jpg)
- [compareDSS.JPG](attachments/87228626/87228627.jpg)
- [wrimsReport2.JPG](attachments/87228626/87228628.jpg)
- [wrimsReport1.JPG](attachments/87228626/87228629.jpg)
- [wrimsReport.png](attachments/87228626/87228630.png)
