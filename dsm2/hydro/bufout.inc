*===== BOF bufout.inc ==================================================
*   Version 93.01, January, 1993

*   Note: "Network.inc" must precede this include.

      INTEGER PlotType
      PARAMETER (PlotType = 2)

      INTEGER MSS
      PARAMETER ( MSS = 5 )
      REAL*8   SpaceTimes(MSS)


      INTEGER MNS
      PARAMETER ( MNS = 5 )
      REAL*8   TimeLocations(MNS)

      INTEGER UnitTimeSeries, UnitSpaceSeries

      REAL*8   TempX(MaxLocations)
      REAL*8   TempQ(MaxLocations)
      REAL*8   TempZ(MaxLocations)
      REAL*8   TempH(MaxLocations)

      LOGICAL TIMFLG, SPCFLG


      COMMON / BufOut / SpaceTimes, TimeLocations,
     #                  UnitTimeSeries, UnitSpaceSeries,
     #                  TempX, TempQ, TempZ, TempH,
     #                  SPCFLG, TIMFLG
      SAVE   / BufOut /

*   Definitions:

*     PlotType  - Destination of plot
*                 [1] Telegraf
*                 [2] Tab delimited (for micros)

*     MSS - Maximum number of space series.
*     UnitSpaceSeries  - file unit number for space series.
*     SpaceTimes(j) - time of space series.
*     j - space-series number.

*     MNS - Maximum number of points for time series plots.
*     UnitTimeSeries - file unit number for time series.
*     TimeLocations(k) - downstream location of time series.
*     k - time-series number.

*     TempX(i) - downstream coordinates.
*     TempQ(i) - streamflow.
*     TempZ(i) - water-surface elevation.
*     TempH(i) - depth of flow.
*      i - location number.

*     Flags:
*     TIMFLG - Time-series flag,
*               [.TRUE.]  - time series data is stored in buffer file.
*               [.FALSE.] - time-series data is not stored in buffer
*                           file.
*     SPCFLG - space-series flag,
*               [.TRUE.]  - space series data is stored in buffer file.
*               [.FALSE.] - space-series data is not stored in buffer
*                           file.

*===== EOF bufout.inc ==================================================
