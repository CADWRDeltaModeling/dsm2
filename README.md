# DSM2
DSM2 is a one-dimensional mathematical model for dynamic simulation of tidal hydraulics,
water quality, and particle tracking in a network of riverine or estuarine channels. DSM2 can
calculate stages, flows, velocities, transport of individual particles, and mass transport processes
for conservative and non-conservative constituents, including salts, water temperature, dissolved
oxygen (DO), and dissolved organic carbon (DOC).

## Documentation
The DSM2 online documentation is available at the following link:
[DSM2 Documentation](https://cadwrdeltamodeling.github.io/dsm2/).

## Installation
On Windows, just unzip the DSM2 zip file to a folder of your choice.

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
equations which are solved simultaneously using an implicit algorithm.

## QUAL
The DSM2 water quality numerical solution (QUAL) is based on a model in which advection dispersion
equation is solved numerically using a coordinate system where computational nodes
move with the flow. The Qual module uses a Lagrangian approach, which is originally based on
Branched Lagrangian Transport Model (BLTM).

## GTM
The DSM2 GTM (Generalized Transport Model) component solves the advection-dispersion equation for multiple
constituents using a semi-implicit finite volume scheme, unlike the QUAL module which uses the Lagrangian
approach.

## HYDRO-GTM
The DSM2 HYDRO-GTM couples HYDRO and GTM, and it solves the hydrodynamic and
water quality equations at the same time. This coupling allows operating rules with
water quality constraints.

## Support
If you find any issues, please report them on the GitHub Issues page.
