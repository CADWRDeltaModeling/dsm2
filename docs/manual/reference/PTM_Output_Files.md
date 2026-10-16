# PTM Output Files

## Overview

PTM outputs a `trace.out` and `animation.bin` file in addition to the `.dss` files.

- The animation binary file outputs in Java binary format the snapshot location of all particles in the simulation.
- The trace output file only records the event (timestamp) when each particle passes from one waterbody to another waterbody.

All indices are internal global indices of the grid. All times are in Julian time.

### Trace.out Content

| Column | Description                     |
|--------|---------------------------------|
| 1      | Start time                      |
| 2      | End time                        |
| 3      | Time step                       |
| 4      | Total particle number           |
| Event  | Event time                      |
| ID     | Particle ID                     |
| Node   | Node ID particle passing        |
| Water  | Waterbody particle entering     |

`trace.out` is written by [ParticleObserver](https://github.com/CADWRDeltaModeling/dsm2/blob/master/dsm2/src/ptm/DWR/DMS/PTM/ParticleObserver.java), which is incorporated in each particle, then read by the flux class.

> Time is in Julian minute.
