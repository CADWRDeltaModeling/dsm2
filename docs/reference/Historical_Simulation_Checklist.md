# Historical Simulation Checklist

## Verifying an Extension of the Historical Simulation

Bob Suits 10/16/2020

## Verify Input Hydrology

###  Check Boundary Conditions

-   Sacramento River inflow
-   San Joaquin River Inflow
-   Sacramento River + Yolo Bypass Inflow
-   Banks pumping
-   Jones pumping

### Get Observed Data

Preferably get the Sacramento and San Joaquin River inflows and Banks
and Jones pumping from DAYFLOW. If DAYFLOW isn’t complete, get remainder
daily average flow from CDEC. It needs to be independent of the DSM2
set-up.

  
Get reported flow at SRV (Rio Vista) and generate daily average flow.

###  Generate daily average flow from DSM2 simulation at:

  
VCU, ORI, OH4, OBD, GLC, RSAC101, RSAN115, RSAC155

### Compare daily average observed flows to DSM2-simulated flows at boundaries

<table class="wrapped confluenceTable">
<tbody>
<tr class="header">
<th class="confluenceTh">Flow</th>
<th class="confluenceTh">CDEC Stations</th>
<th class="confluenceTh">Operating Agency</th>
<th class="confluenceTh">Simulated</th>
</tr>
&#10;<tr class="odd">
<td class="confluenceTd">Sac River Inflow</td>
<td class="confluenceTd">FPT</td>
<td class="confluenceTd">USGS</td>
<td class="confluenceTd">RSAC155</td>
</tr>
<tr class="even">
<td class="confluenceTd">SJR Inflow</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd">RSAN112</td>
</tr>
<tr class="odd">
<td class="confluenceTd">SRV</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd">RSAC101</td>
</tr>
<tr class="even">
<td class="confluenceTd">Banks Pumping</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd">VCU + ORI – OH4</td>
</tr>
<tr class="odd">
<td class="confluenceTd">Jones Pumping</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd">OBD + GLC – ORI</td>
</tr>
<tr class="even">
<td class="confluenceTd">Banks + Jones</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd"><br />
</td>
<td class="confluenceTd">VCU + OBD + GLC – OH4</td>
</tr>
</tbody>
</table>

  

### Verify timing of installation and removal of temporary barriers and operation of Montezuma Control Structure and Delta Cross Channel Gates

1.  Create a dss file with the observed and simulated 15-minute data.
    Compare observed and simulated stages just upstream and downstream
    of each barrier site. This would already have been done with
    observed data in establishing the timings by looking at observed
    stages. Now repeat the analysis in order to confirm that you got the
    operation timing correct.

    |                   |                          |
    |-------------------|--------------------------|
    | Barrier           | Stations to use to check |
    |  Middle River     | ​MUP and (MAB or MTB)     |
    | Grant Line Canal  | GCT and GLE              |
    | Old River         | OBD and (OAD or ODM)     |
    | Old River at Head | OH1 and SJL              |

2.   Compare internal daily average flows affected by gate operations

    |          |                                             |
    |----------|---------------------------------------------|
    | Observed | Simulated                                   |
    | DLC      | DLC Delta Cross Channel                     |
    | NSL      | SLMZU025 Montezuma Slough at National Steel |

3.  Check key internal flows for overall circulation of Delta waters

    |          |                                        |
    |----------|----------------------------------------|
    | Observed | Simulated                              |
    | GSS      | GSS                                    |
    | TRN      | TRN                                    |
    | OBI      | OBI (ROLD024)                          |
    | MDM      |  Subtract RMID015-145 from RMID015-144 |
    | OH4      | OH4 (ROLD034)                          |
    | VCU      | VCU                                    |
    | OH1      | OH1                                    |
    | OLD      | OLD                                    |
    | GLE      | GLE                                    |

### Verifying EC 

Get OCO’s monthly updated EC estimates at: Banks, Jones, OH4, OBI to
compare to  
Delta Modeling Section’s historical simulation and reported EC.

|           |           |
|-----------|-----------|
| Observed  | Simulated |
| BANKS     | BANKS     |
| JONES     | JONES     |
| OBI       | ROLD024   |
| OH4       | ROLD034   |

  
Compare observed EC to simulated EC at other key locations

|           |               |
|-----------|---------------|
| Observed  | Simulated     |
| ANH       | ANH (RSAN007) |
| EMM       | EMM (RSAC092) |
| JER       | RSAN018       |
| MDM       | RMID015       |
| VCU       | CHVCT000      |
| OH1       | ROLD074       |
| OLD       | ROLD059       |

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavdba6ec7d80d8e6d0f248fa3e9c1a9f2c.png](attachments/87229300/87229299.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav1c660f167bf01e13f8c74d8d923445b8.png](attachments/87229300/87229301.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavc1eb73f1cc8b69c78afeeda15ca65f9f.png](attachments/87229300/87229302.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavffb24ad6008690b469796d092cb3e822.png](attachments/87229300/87229303.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav7e51b7bfbf042cd6018456316774c8ac.png](attachments/87229300/87229304.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav46b8e2022c959a0ec4979c9f7a8a9b76.png](attachments/87229300/87229305.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav096303e2931d512411acda589b39c3cb.png](attachments/87229300/87229306.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavac6c9b93520fd08679d0ccfdc60b7b66.png](attachments/87229300/87229307.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav417450fd7fccb9d246170ebb7f24d040.png](attachments/87229300/87229308.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddava898e0b2ae4581b97e766bf78287b5c1.png](attachments/87229300/87229309.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav68b2cbedb7a858f3c9b7f1a1e3ab74ac.png](attachments/87229300/87229310.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav5b3d61166388b0d6b2f355755006cda2.png](attachments/87229300/87229311.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav8c7ed9ee05ac17e5e96e011f3f2618cd.png](attachments/87229300/87229312.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav1767755d061f0eab1f3ca3f72ac8cef2.png](attachments/87229300/87229313.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavf80c76565c2ad18b4d77a1e3e7d95206.png](attachments/87229300/87229314.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavcbc13ea92d122ff1199746fedcb5d095.png](attachments/87229300/87229315.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavbec1a14c56fb496c1411aa3f37418f8f.png](attachments/87229300/87229316.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav7b88059002b010dabef7c4b1535bb124.png](attachments/87229300/87229317.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav5bb8f683823e3bf73c38767313d66e71.png](attachments/87229300/87229318.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavc305fa1a938290814fd2248549ba4430.png](attachments/87229300/87229319.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavdfbad699bc5178a523225eb4db224138.png](attachments/87229300/87229320.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav4ea5b554fecf90d7309db37a565d3a27.png](attachments/87229300/87229321.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav97cef626ad442220331e25e5e8e8107f.png](attachments/87229300/87229322.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddave19a27a3183a9cd911129f634e5785e1.png](attachments/87229300/87229323.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavcafee77d96b6996a2ba1b263d685acf2.png](attachments/87229300/87229324.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav586bb64e084e848f881c6b85898711d9.png](attachments/87229300/87229325.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav06a87c364207da504d16cfe87f616b71.png](attachments/87229300/87229326.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavfed7fe74c73acf86c9270728c405a08c.png](attachments/87229300/87229327.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav1b9bd5a7d0e6c5fe479a6a2f41178b3d.png](attachments/87229300/87229328.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav252bc8a83f17f3aaf5122fc24dc34a7b.png](attachments/87229300/87229329.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavaefc8f4547e26bc30a4e1e2e6d9649e4.png](attachments/87229300/87229330.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav43dc9470b95f7adadead62075f065c27.png](attachments/87229300/87229331.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav44b58de538e094cc6f56fa252e0b3775.png](attachments/87229300/87229332.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavf81ccfffae04537644dbc328afa65327.png](attachments/87229300/87229333.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavfb16076aa13e127fd8eb1d3b049dec6e.png](attachments/87229300/87229334.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav9a6b20dc0a73de6bf9dab0456647125f.png](attachments/87229300/87229335.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav230f2ffdb48f91fc3c31cb101e946de0.png](attachments/87229300/87229336.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddave2ddaa02d082a0f4038f1aa1cae93a47.png](attachments/87229300/87229337.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav41ab2bbaa193e682b05098057d919df2.png](attachments/87229300/87229338.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddavcb34498bae50e99b02ddf21594a946c0.png](attachments/87229300/87229339.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav248f48ce932b6488517e2109896fdb56.png](attachments/87229300/87229340.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddave25679296b7bc483b7fb1156d722e8b6.png](attachments/87229300/87229341.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav44d59b8b5a711866ad8dde94f0d42d27.png](attachments/87229300/87229342.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[worddav660ebda068b8ef166732e0aef200cc6e.png](attachments/87229300/87229343.png)
(image/png)  
