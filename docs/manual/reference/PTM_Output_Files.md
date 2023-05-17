# PTM Output Files

PTM outputs a trace.out and animation.bin file in addition to the .dss
files. 

The animation binary file outputs in Java binary format the snapshot
location of all particles in the simulation. 

The trace output file only records the event (timestamp) when each
particle passes from one waterbody to another waterbody.

All indices are internal global index of grid. All times are in Julian
time.

Table. Content in Trace.out

  

<table class="confluenceTable">
<tbody>
<tr class="header">
<th class="confluenceTh"><p><br />
</p></th>
<th class="confluenceTh"><p>1<sup>st</sup> col</p></th>
<th class="confluenceTh"><p>2<sup>nd</sup> col</p></th>
<th class="confluenceTh"><p>3<sup>rd</sup> col</p></th>
<th class="confluenceTh"><p>4<sup>th</sup> col</p></th>
</tr>
&#10;<tr class="odd">
<td class="confluenceTd"><p>header row</p></td>
<td class="confluenceTd"><p>start time</p></td>
<td class="confluenceTd"><p>end time</p></td>
<td class="confluenceTd"><p>time step</p></td>
<td class="confluenceTd"><p>total particle number</p></td>
</tr>
<tr class="even">
<td class="confluenceTd"><p>content row</p></td>
<td class="confluenceTd"><p>event time</p></td>
<td class="confluenceTd"><p>particle id</p></td>
<td class="confluenceTd"><p>node id particle passing</p></td>
<td class="confluenceTd"><p>waterbody particle entering</p></td>
</tr>
</tbody>
</table>

trace.out is written by <a
href="https://github.com/CADWRDeltaModeling/dsm2/blob/master/dsm2/src/ptm/DWR/DMS/PTM/ParticleObserver.java"
rel="nofollow">ParticleObserver</a>, which is incorporated in each
particle, then read by flux class.

Time is in Julian minute.
