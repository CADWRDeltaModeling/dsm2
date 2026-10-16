# Particle Insertion

## Overview

*Particle Insertion* is a section in the PTM input that specifies the insertion of particles in water bodies over time. The PTM can insert multiple sets of particles.

## Tables

### Example

```text
PARTICLE_INSERTION
NODE NPARTS DELAY DURATION
1 1000 0hour 1day
13 1000 1day 0hour
END
```

The PARTICLE_INSERTION table defines the insertion of particles at specific nodes over a given time interval.

### Field Descriptions

- **NODE**: The node at which the insertion is made.
- **NPARTS**: Number of particles.
- **DELAY**: Delay before the first insertion after the beginning of the PTM run. The unit of time needs to be attached without spaces.
- **DURATION**: Interval over which insertion is evenly distributed in time. If the time is set as zero, all the particles are inserted instantaneously. The unit of time needs to be attached without spaces.






