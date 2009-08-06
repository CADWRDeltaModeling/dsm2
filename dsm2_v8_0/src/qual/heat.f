C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

 

C + + + + + + + + + + + + + + +
      SUBROUTINE HEAT

c-----It computes net solar radiation for the time interval delta t
c-----and rate of atmospheric radiation
c-----exchanged through the air-water interface
      Use IO_Units
      use common_qual
      use runtime_data
      use logging
      implicit none

      include 'param.inc'
      include 'bltm3.inc'

      logical first_call        ! true if first call to subroutine

      integer istat             ! return status
     &     ,iyr,imon            ! year, month
     &     ,iymdjl              ! HEC function to return day of year

      integer*4
     &     dayof_year           ! julian day of year (1-366)
     &     ,timeof_day          ! minute of day (0-1440)

      real*8 rearth, declon, tana, tanb, acs, xx, ste, vpwb, dewpt
      real*8 cnl, tb, te, alpha, talt, y, pwc, a1, a2, ar, br, rs
      real*8 oam, clc, atc

      real*8 CON1,CON2,CON3,CON4,CON5,CON6
      real*8 SOLCON
c-----declare variables for diagnostics
      integer num, nummax
      real*8 pi,day_tsolhr, day_ha, tot_tsolhr, tot_ha

      parameter (
     &     PI=3.141592654,
     &     CON1=2.0*PI/365.0,
     &     CON3=180.0/PI,
     &     CON4=23.45*PI/180.0,
     &     CON5=PI/12.0,
     &     CON6=12.0/PI,
     &     SOLCON=438.0
     &     )

      data first_call /.true./

      timeof_day=mod(julmin,60*24)
      call datymd(current_date(:9),iyr,imon,iday,istat)
      dayof_year=iymdjl(iyr,imon,iday)-iymdjl(iyr,1,1)+1

c!    Compute and/or define required constants.

      if (first_call) then
         first_call=.false.
         CON2=PI/180.0*LAT
         deltsl=(LONGITUDE-LONG_STD_MERID)/15.0
         ELEXP=EXP(-ELEV/2532.0)
      endif

      ACS = TANA*TANB
      REARTH=1.0+0.017*COS(CON1*(186-DAYOF_YEAR))
      DECLIN=CON4*COS(CON1*(172-DAYOF_YEAR))
      RR=REARTH**2
      EQTIME=0.000121-0.12319*SIN(CON1*(DAYOF_YEAR-1)-0.07014)
     &     -0.16549*SIN(2.0*CON1*(DAYOF_YEAR-1)+0.3088)
      DECLON=ABS(DECLIN)
c-----Replace TAN function with SIN/COS.
      TANA = SIN(CON2)/COS(CON2)
      TANB = SIN(DECLON)/COS(DECLON)
	ACS = TANA*TANB
      IF (ACS.EQ.0.0) GO TO 8
      XX=SQRT(1.0-ACS*ACS)
      XX=XX/ACS
      ACS=ATAN(XX)
      IF (DECLIN.GT.0.0) ACS=PI-ACS
      GO TO 9
    8 ACS=PI/2.0
    9 CONTINUE

c!    Calculate the standard time of
c!    sunrise (STR) and sunset (STS).

      STR=12.0-CON6*ACS+DELTSL
      STS=24.0-STR+2.0*DELTSL

c!    Increment the variables that define the
c!    time of the beginning(STB) and the
c!    end (STE) of the time interval.

      stb=dble(timeof_day)/60. ! in decimal hours (e.g. 13.5 is 1330)
      ste=stb+dble(time_step)/60.

c!    Compute vapor pressures (VPWB and
c!    VPAIR), dew point (DEWPT), AND
c!    dampening effect of clouds (CNS and CNL). 

      VPWB=0.1001*EXP(0.03*WETBLB)-0.0837
      VPAIR=VPWB-0.000367*ATMPR*(DRYBLB-WETBLB)
     &     *(1.0+(WETBLB-32.0)/1571.0)
      DEWPT=LOG((VPAIR+0.0837)/0.1001)/0.03
      CS=1.0-0.65*CLOUD**2
      IF (CLOUD.GT.0.9) CS=0.50
      CNL=CLOUD*10.0+1.0
      NL=CNL
 82   CONTINUE
      IF (STS.LE.STB.OR.STR.GE.STE) GO TO 35
      IF(STR.GT.STB.AND.STR.LT.STE) GO TO 41
      IF (STS.LT.STE.AND.STS.GT.STB) GO TO 42

      TB=STB-12.0-DELTSL+EQTIME
      TE=STE-12.0-DELTSL+EQTIME
      GO TO 43
 41   TB=STR-12.0-DELTSL+EQTIME
      TE=STE-12.0-DELTSL+EQTIME
      GO TO 43
 42   TB=STB-12.0-DELTSL+EQTIME
      TE=STS-12.0-DELTSL+EQTIME
 43   CONTINUE
      TALT=(TB+TE)/2.0

c!    Compute amount of clear sky, solar radiation(SOLAR),
c!    and altitude of the sun (ALPHA).

      SOLAR=SOLCON/RR*(SIN(CON2)*SIN(DECLIN)*(TE-TB)+CON6*COS(CON2)*
     &     COS(DECLIN)*(SIN(CON5*TE)-SIN(CON5*TB)))

C       A SIGNIFICANT CHANGE OF ORIGINAL CODE, TO GET A AVERGED FLUX OVER TIME STEP, JON	
	SOLAR=SOLAR/(dble(time_step)/60.)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      ALPHA=SIN(CON2)*SIN(DECLIN)+COS(CON2)*COS(DECLIN)*COS(CON5*TALT)
      IF (ABS(ALPHA).EQ.1.0) GO TO 4
      Y=SQRT(1.0-ALPHA*ALPHA)
      Y=ALPHA/Y
      ALPHA=ATAN(Y)
      GO TO 5
    4 IF (ALPHA.EQ.-1.0) GO TO 6
      ALPHA=PI/2.0
      GO TO 5
    6 ALPHA=-PI/2.0
    5 CONTINUE
      IF (ALPHA.LT.0.01) GO TO 35

c!    Compute absorption and scattering due to atmospheric conditions.

      PWC=0.00614*EXP(0.0489*DEWPT)
      OAM=ELEXP/(SIN(ALPHA)+0.15*(ALPHA*CON3+3.885)**(-1.253))
      A1=EXP(-(0.465+0.0408*PWC)*(0.129+0.171*EXP(-0.880*OAM))*OAM)
      A2=EXP(-(0.465+0.0408*PWC)*(0.179+0.421*EXP(-0.721*OAM))*OAM)

c!    Compute reflectivity coefficient (RS).

      GO TO (30,31,31,31,31,31,32,32,32,32,33), NL
 30   AR=1.18
      BR=-0.77
      GO TO 34
 31   AR=2.20
      BR=-0.97
      GO TO 34
 32   AR=0.95
      BR=-0.75
      GO TO 34
 33   AR=0.35
      BR=-0.45
 34   CONTINUE
      RS=AR*(CON3*ALPHA)**BR

      IF(RS.GE.1.0) GO TO 35

c!    Compute atmospheric transmission term (ATC).

      ATC=(A2+0.5*(1.0-A1-DUST_ATTCOEFF))/(1.0-0.5*RS*(1.0-A1+DUST_ATTCOEFF))

c!    Compute net solar radiation for the time interval delta t


      TSOLHR=SOLAR*ATC*CS*(1.0-RS)
      GO TO 36
 35   TSOLHR=0.0
 36   CONTINUE
      CLC=1.0+0.17*CLOUD**2

c!    Compute heat flux due to long wave atmospheric radiation (HA).

      HA=0.97*1.73E-09*2.89E-06*(DRYBLB+460.0)**6*CLC

      if(print_level.eq.3)then
         nummax=24*60/dble(time_step)
         if (num. eq. 0) then
            tot_tsolhr=0.
            tot_ha=0.
         endif
         num=num+1
         tot_tsolhr=tot_tsolhr+tsolhr
         tot_ha=tot_ha+ha
         if (num .eq. nummax)then
            day_tsolhr=24*tot_tsolhr/nummax
            day_ha=24*tot_ha/nummax
            write(unit_screen,999) current_date, day_tsolhr,day_ha
            num=0
         endif
      endif
 999  format(a,'daily solar rad =',f10.2, 'daily atm. rad =',f10.2)

      RETURN
      END
 
 
