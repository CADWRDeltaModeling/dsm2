# DSM2 Documentation

## General Description

DSM2 is a river, estuary, and land modeling system:

- **River**: Can simulate riverine systems and has been extended from Sacramento to Shasta Dam. It has also been tested with high flow/stage simulations for flood modeling.
- **Estuary**: A completely flexible estuary model; stages and flows may be specified at boundary and internal points.
- **Land**: Includes effects from land-based processes, such as consumptive use and agricultural runoff.

DSM2 can calculate stages, flows, velocities, and many mass transport processes, including salts, multiple non-conservative constituents, temperature, THM formation potential, and individual particles.

---

## Overview

See [Overview](dsm2_overview.md)

### Modules

DSM2 currently consists of three modules, all included in the distribution:

1. **HYDRO**: Simulates one-dimensional hydrodynamics, including flows, velocities, depth, and water surface elevations. HYDRO provides the flow input for QUAL and PTM.
2. **QUAL**: Simulates one-dimensional fate and transport of conservative and non-conservative water quality constituents based on the flow field simulated by HYDRO.
3. **PTM**: Simulates pseudo-3D transport of neutrally buoyant particles based on the flow field simulated by HYDRO. PTM has applications ranging from visualization of flow patterns to simulating discrete organisms such as fish eggs and larvae.
4. **GTM**: Simulates generalized transport processes, including sediment transport and other mass transport phenomena, extending the capabilities of DSM2 to model additional environmental processes.

---

## Copyright

The model is copyrighted by the State of California, Department of Water Resources. It is licensed under the GNU General Public License, version 2. This means:

- It can be copied, distributed, and modified freely.
- You may not restrict others in their ability to copy, distribute, and modify it.

See the License for more details. Also, note the list of protected routines.

DSM2 release packages are available on the [CNRA Open Data website](https://data.cnra.ca.gov/dataset/dsm2).

For comments or questions, please contact Min Yu at [minyu@water.ca.gov](mailto:minyu@water.ca.gov).

---

## For More Information
- [Manual](manual/index.md)
- [DSM2 Learning Series](https://cadwrdeltamodeling.github.io/dsm2/dsm2_learning_series/)
- [Tutorials](tutorials/An_Introduction_to_DSM2_Tutorials.md)
- [DSM2 Source Code GitHub Repository](https://github.com/CADWRDeltaModeling/dsm2)
