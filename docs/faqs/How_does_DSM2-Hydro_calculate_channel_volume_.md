# How does DSM2-Hydro calculate channel volume?

-   Hydro creates virtual cross-sections by interpolating cross-section
    input (see [Tutorial 1: Channels](../tutorials/Tutorial_1_Channels.md))
    to create virtual cross-sections. Virtual cross-sections are created
    and used internally in Hydro by interpolating cross-section input.
-   Virtual cross-sections are usually not seen by the user. If the
    variable printlevel \>= 5 in the SCALAR input section, virtual
    cross-sections will be written to output .hof file.
-   The 2012 Annual Report describes a change in the way volume is
    calculated: it used to use only the area of the cross-section in the
    middle of a computational reach, but now it uses all 3 of the
    cross-sections in a computational reach. Also, it describes an
    important change to the longitudinal interpolation used to create
    virtual cross-sections.
-   The 2016 Annual Report, section 3.4.2 indicates that the volume of a
    channel is calculated by multiplying the average of two
    cross-sectional areas by the distance between them. This process
    would then be repeated twice for each computational reach to find
    the volume.
-   Hydro will not converge well if cross-sectional area is not
    interpolated correctly. Previously, area at a given elevation
    between cross-section layers was calculated by interpolating area
    linearly between two layers. It has been changed to a =
    a1+(.5\*(w1+w2))\*h, where
    -   a1 = area at lower elevation
    -   w1 = width at lower elevation
    -   w2 = width at higher elevation
    -   h = distance from lower elevation to given elevation

## References

Annual reports can be found [here](https://data.cnra.ca.gov/dataset/methodology-for-flow-and-salinity-estimates-in-the-sacramento-san-joaquin-delta-and-suisun-marsh).

Ferreira I. and Sandhu, N. 2016 "Chapter 3: DSM2 Extension: A GIS-Based
Approach."  In: Methodology for Flow and Salinity Estimates in the
Sacramento-San Joaquin Delta and Suisun Marsh. 37th Annual Progress
Report to the State Water Resources Control Board. California Department
of Water Resources.

Liu L., Ateljevich E., and Sandhu P. 2012. “Chapter 2: Improved Geometry
Interpolation in DSM2-Hydro.” In: Methodology for Flow and Salinity
Estimates in the Sacramento-San Joaquin Delta and Suisun Marsh. 33rd
Annual Progress Report to the State Water Resources Control Board.
California Department of Water Resources.

Tom B. 1998. “Chapter 6: Cross-Section Development Program.” In:
Methodology for Flow and Salinity Estimates in the Sacramento-San
Joaquin Delta and Suisun Marsh. 19th Annual Progress Report to the State
Water Resources Control Board. California Department of Water Resources.
