# DSM2
DSM2 is a one-dimensional mathematical model for dynamic simulation of tidal hydraulics,
water quality, and particle tracking in a network of riverine or estuarine channels. DSM2 can
calculate stages, flows, velocities, transport of individual particles, and mass transport processes
for conservative and non-conservative constituents, including salts, water temperature, dissolved
oxygen (DO), and dissolved organic carbon (DOC)

## HYDRO
The partial differential equations of mass and momentum in the DSM2 hydrodynamic model
component (HYDRO) are based on an implicit finite difference scheme. As a one-dimensional
formulation, the channel length is divided into discrete reaches and the partial differential
equations are transformed into finite difference forms for the discrete reaches by integrating
numerically in time and space. The resulting equations are then linearized over a single iteration
in terms of incremental changes in unknown variables (flow rate and water level) using
approximations from truncated series, representing a function as an infinite sum of terms
calculated from the values of its derivatives at a single point. When the discretized equations are
written for all computational cells at the current time and the next time lines, it forms a system of
equations which are solved simultaneously using an implicit algorithm

## QUAL
The DSM2 water quality numerical solution (QUAL) is based on a model in which advection dispersion
equation is solved numerically using a coordinate system where computational nodes
move with the flow

## PTM
The DSM2 particle tracking component (PTM) computes the location of an individual particle at
any time step within a channel based on velocity, flow and water level information provided by
HYDRO. The longitudinal movement is based on transverse and vertical velocity
profiles computed from mean channel velocity provided by HYDRO. Mean channel
velocity is multiplied by a factor which depends on particle’s transverse location in the
channel resulting in a transverse velocity profile resulting in slower moving particles
closer to the shore. Mean channel velocity is also converted to vertical velocity profile
using a logarithmic profile to account for slower particles closer to the channel bottom. The
longitudinal movement is then the sum of transverse and vertical velocities multiplied by time
step. Particles also move across the channel and in vertical direction along the depth due to
mixing. A random factor and mixing coefficients and the length of time step is used to compute
the movement of particle in transverse and vertical direction.
