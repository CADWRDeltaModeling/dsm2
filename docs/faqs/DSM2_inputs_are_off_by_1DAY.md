# DSM2 inputs are off by 1DAY

DSM2-241 - Check daily inflows and warn if INST-VAL Open

A problem was reported with a run that was supposed to be a historical
run with inflows scaled up by a factor. 

<table class="wrapped confluenceTable">
<tbody>
<tr class="header">
<th class="confluenceTh"><div class="content-wrapper">
<ol>
<li>Here's a plot of model output at Vernalis</li>
</ol>
<ul>
<li>Blue=historical stage output</li>
<li>Red=scaled up stage output</li>
<li>Green=historical flow output</li>
<li>Black=scaled up flow output</li>
</ul>
<p>On February 14, 1992, both the scaled up flow and stage outputs are
higher than historical.</p>
<p><img src="attachments/87228873/87228877.png"
data-image-src="attachments/87228873/87228877.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228877"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-1-15_8-19-39.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228873"
data-linked-resource-container-version="1" height="400" /></p>
</div></th>
<th class="confluenceTh"><div class="content-wrapper">
<p>2. The problem was caused by the "Type" of the scaled up flow input
tine series. The user had created a series with a type of INST-VAL.</p>
<p>This results in changes in inflow taking effect at the end of the day
rather than the beginning of the day.</p>
<p>One way to tell that a 1DAY time series is PER-AVER vs INST-VAL is
that HEC-DSSVue plots PER-AVER time series as a square wave, but not
INST-VAL.</p>
<p><img src="attachments/87228873/87228876.png"
data-image-src="attachments/87228873/87228876.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228876"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-1-15_8-29-2.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228873"
data-linked-resource-container-version="1" height="400" /></p>
</div>
<p><br />
</p></th>
</tr>
&#10;<tr class="odd">
<td class="confluenceTd"><div class="content-wrapper">
<p><strong>3. To check Type and to change it, in HEC-DSSVue, right click
on the series and select Edit, and </strong></p>
<p><img src="attachments/87228873/87228875.png"
data-image-src="attachments/87228873/87228875.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228875"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-1-15_8-21-6.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228873"
data-linked-resource-container-version="1" height="250" /></p>
<p><strong>use the dropdown to select a new type.</strong></p>
<p><img src="attachments/87228873/87228874.png"
data-image-src="attachments/87228873/87228874.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228874"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-1-15_8-20-52.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228873"
data-linked-resource-container-version="1" height="250" /></p>
</div></td>
<td class="confluenceTd"><p><strong>4. After re-running with the change,
results are as expected.</strong></p>
<p><img src="attachments/87228873/87228872.png"
data-image-src="attachments/87228873/87228872.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228872"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-1-15_8-19-11.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228873"
data-linked-resource-container-version="1" height="400" /></p>
<div class="content-wrapper">
<p><br />
</p>
</div></td>
</tr>
</tbody>
</table>

  

  

  

  

  

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-1-15_8-19-11.png](attachments/87228873/87228872.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-1-15_8-20-52.png](attachments/87228873/87228874.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-1-15_8-21-6.png](attachments/87228873/87228875.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-1-15_8-29-2.png](attachments/87228873/87228876.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-1-15_8-19-39.png](attachments/87228873/87228877.png)
(image/png)  
