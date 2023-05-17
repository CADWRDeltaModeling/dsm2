# DSM2 Seems to indicate missing irregular (gate position) data

## Problem: Hydro displays the following. 

<table class="wrapped confluenceTable">
<tbody>
<tr class="header">
<th class="confluenceTh"><p>Error in reading time-varying data:<br />
Current time is 01SEP2001 2400; earliest data time for<br />
/HIST+GATE/MTZSL/BOATLOCK_OP//IR-DECADE/DWR-ESO/<br />
is </p>
<p><br />
</p></th>
</tr>
&#10;</tbody>
</table>

Brad Tom Related to Jira issue. I think we should open an issue there as
you are doing the practical fix for this known
issue <img src="images/icons/emoticons/smile.svg"
class="emoticon emoticon-smile" data-emoticon-name="smile"
alt="(smile)" />

 

|                                                           |                                                                           |                                                                                                 |              |              |     |              |               |                                                               |          |            |
|-----------------------------------------------------------|---------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------|--------------|--------------|-----|--------------|---------------|---------------------------------------------------------------|----------|------------|
|                                                           |                                                                           |                                                                                                 |              |              |     |              |               |                                                               |          |            |
| Key                                                       | Summary                                                                   | T                                                                                               | Created      | Updated      | Due | Assignee     | Reporter      | P                                                             | Status   | Resolution |
| [DSM2-106](http://msb-jira/browse/DSM2-106?src=confmacro) | [Timeseries interpolation](http://msb-jira/browse/DSM2-106?src=confmacro) | [<img                                                                                           
                                                                                                                                         src="http://msb-jira/secure/viewavatar?size=xsmall&amp;avatarId=10303&amp;avatarType=issuetype"  
                                                                                                                                         class="icon" alt="Bug" />](http://msb-jira/browse/DSM2-106?src=confmacro)                        | Nov 30, 2011 | Jan 03, 2022 |     | Nicky Sandhu | Ines Ferreira | <img src="http://msb-jira/images/icons/priorities/medium.svg" 
                                                                                                                                                                                                                                                                                                              class="icon" alt="Medium" />                                   | Resolved | Won't Do   |

1 issue

But there are data values in this time series with dates before the
current date.

The problem is: When using an IR-DECADE dss path, there must be a value
with a timestamp that is at the beginning of the current decade. In this
case, a record is required that has a timestamp of 31DEC2000 2400.

## To fix this:

<table class="wrapped confluenceTable">
<tbody>
<tr class="header">
<th class="confluenceTh"><div class="content-wrapper">
<ol>
<li>Tabulate the data in HEC-DssVue. There is no beginning of decade
timestamp.</li>
</ol>
<p><img src="attachments/87228880/87228886.png"
data-image-src="attachments/87228880/87228886.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228886"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-3-25_10-36-6.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228880"
data-linked-resource-container-version="1" height="250" /></p>
</div></th>
<th class="confluenceTh"><div class="content-wrapper">
<p>2. Turn on "Allow Editing"</p>
<p><img src="attachments/87228880/87228885.png"
data-image-src="attachments/87228880/87228885.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228885"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-3-25_10-37-42.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228880"
data-linked-resource-container-version="1" height="250" /></p>
</div></th>
</tr>
&#10;<tr class="odd">
<td class="confluenceTd"><div class="content-wrapper">
<p>3. Select the row before the end of the previous decade, and select
"Insert Rows":</p>
<p><img src="attachments/87228880/87228884.png"
data-image-src="attachments/87228880/87228884.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228884"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-3-25_10-38-16.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228880"
data-linked-resource-container-version="1" height="250" /></p>
</div></td>
<td class="confluenceTd"><div class="content-wrapper">
<p>4. Change "Number Rows" to 1.</p>
<p><img src="attachments/87228880/87228883.png"
data-image-src="attachments/87228880/87228883.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228883"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-3-25_10-38-32.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228880"
data-linked-resource-container-version="1" height="250" /></p>
</div></td>
</tr>
<tr class="even">
<td class="confluenceTd"><div class="content-wrapper">
<p>5.Enter the timestamp that is needed, with a value equal to the value
in the previous record: </p>
<p><img src="attachments/87228880/87228882.png"
data-image-src="attachments/87228880/87228882.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228882"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-3-25_10-39-7.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228880"
data-linked-resource-container-version="1" height="250" /></p>
</div></td>
<td class="confluenceTd"><div class="content-wrapper">
<p>6. Save the data:</p>
<p><img src="attachments/87228880/87228881.png"
data-image-src="attachments/87228880/87228881.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228881"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2020-3-25_10-39-44.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228880"
data-linked-resource-container-version="1" height="250" /></p>
</div></td>
</tr>
</tbody>
</table>

  

  

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-3-25_11-0-58.png](attachments/87228880/87228879.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-3-25_10-39-44.png](attachments/87228880/87228881.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-3-25_10-39-7.png](attachments/87228880/87228882.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-3-25_10-38-32.png](attachments/87228880/87228883.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-3-25_10-38-16.png](attachments/87228880/87228884.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-3-25_10-37-42.png](attachments/87228880/87228885.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2020-3-25_10-36-6.png](attachments/87228880/87228886.png)
(image/png)  
