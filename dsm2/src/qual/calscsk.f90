!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

      subroutine CALSCSK (c)

!     This subroutine uses the existing estimate of constituent concentrations
!     in c and the  model parameters to
!     construct the Source/Sink matrix, SCSK.  The SCSK matrix contains
!     values of the net gain or loss  due to growth, decay, settling,
!     benthic supply/demand, and constituent interaction for each constituent
!     at the current time step.
      use common_qual
      implicit none

      include 'param.inc'
      integer  i ,ii

      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)

!     Loop through each branch and constituent included in the simulation
!     and call the appropriate subroutine to calculate source/sink

      integer idisp
      parameter (idisp=51)

      do  ii=1,no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)
         scsk(i) = 0.
      enddo

      if(mtemp.gt.0) call tempfactors(c(mtemp))


      if(mbod.gt.0 ) call calbod (c)

      if(mdo.gt.0  ) call caldo (c)

      if(morgn.gt.0) call calorgn (c)

      if(mnh3.gt.0 ) call calnh3 (c)

      if(mno2.gt.0 ) call calno2 (c)

      if(mno3.gt.0 ) call calno3 (c)

      if(morgp.gt.0) call calorgp (c)

      if(mpo4.gt.0 ) call calpo4 (c)

      if(malg.gt.0 ) call calalg (c)

      if(mtemp.gt.0) call caltemp (c)
!-----add write routine here for scsk of oxygen if necessary for analysis

      return
      end


      subroutine CALBOD (c)

!-----calculate source, sink terms for BOD
      use common_qual
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
      real*8 xk, set
!-----contribution due to reactions

      xk = rcoef(mbod, decay) * c(mbod)

!-----loss of mass due to settling

      set = rcoef(mbod,settle) * c(mbod)

      if (mtemp .gt. 0) then
         xk = xk*thetadj(temp_bod_decay)
         set = set*thetadj(temp_bod_set)
      endif

      scsk(mbod) = scsk(mbod) - xk - set

!-----if DO is simulated, calculate demand on DO

      if (mdo .gt. 0) scsk(mdo) = scsk(mdo) - xk

      return
      end

      subroutine CALDO (c)

!-----calculate source, sink terms for DO
      use common_qual
      use logging
      use runtime_data
      implicit none

      include 'param.inc'
      include 'bltm1.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8   c(max_constituent)
      real*8  k2, k2min, k2max,tw, p, pwv, twk, osf,oss, temp, reaer, os, phi
      real*8  twksq,twk4,tempval, kwind

      real*8 benth
      parameter(k2max=0.5)

      p =1
      if (mtemp .eq. 0) then
         tw = 20
      else
         tw = c(mtemp)
      endif

      twk = tw+273
      twksq=twk*twk
      twk4=twksq*twksq
      phi = 9.75e-4 - 1.426e-5*tw + 6.436e-8*(tw*tw)
      pwv = exp(11.8571 - 3840.7/twk - 216961/(twksq))
      tempval=-139.3441 + 1.575701e5/twk - 6.642308e7/(twksq) &
          + 1.2438e10/(twk*twksq) - 8.621949e11/(twk4)
      osf = exp(tempval)

!-----if conservative constituent/s
!-----are simulated, then include the effects of salinity or chloride on DO

      if (mtds .gt. 0) then
         oss = exp(tempval-0.001*c(mtds)*(0.017674-10.754/twk &
             + 2140.7/(twksq)))
      elseif (mcl .gt. 0) then
         oss = exp(tempval-0.001*1.80655*c(mcl)*(0.017674-10.754/twk &
             + 2140.7/(twksq)))
      elseif (mec .gt. 0) then
         oss = exp(tempval-0.001*0.64*c(mec)*(0.017674-10.754/twk &
             + 2140.7/(twksq)))
      else
         oss = osf
      endif

      if(p.eq.1)then
         os=oss
      else
         os = oss*p*(1-pwv/p)*(1-phi*p)/(1-pwv)/(1-phi)
      endif

!-----compute reaeration using O'Connor Dobbins' formulation

      kwind = 0.015*2.1511*wind**2/depth
      k2 = 12.96*(sqrt(abs(vel))/(depth*depth*depth))+kwind
      k2min = 3./depth
      if (k2.lt.k2min) k2 = k2min
      temp = k2
      k2 = k2/24.
      if(k2.gt.k2max)then
         k2=k2max
      endif

         reaer = k2*(os-c(mdo))
!-----account for benthic oxygen demand

      benth = 0.0353*rcoef(mdo,benthic)/depth

      if (mtemp .gt. 0) then
         reaer = reaer*thetadj(temp_reaer)
         benth = benth*thetadj(temp_do_ben)
      endif

      scsk(mdo) = scsk(mdo) + reaer - benth

      if(print_level.ge.3)then
         if (iskip .eq. 1) then
            if (mod(julmin, 120) .eq. 0) then
               if(chan_res.eq.1) then
                  if(int2ext(n) .eq. 20) then
                     write(idoout,999)current_date,int2ext(n),osf,oss,temp, os, c(mdo), &
                         reaer,vel,k2min,benth,kwind,dqq(int2ext(n)),c(mec)
                  endif
               elseif(chan_res.eq.2) then
                  write(irdoout,998)current_date,irev,osf,oss,temp, os, &
                      c(mdo),reaer,vel,k2min,benth,kwind,depth
               endif
            endif
         endif
      endif
 999  format(a,i4,5f8.2,5f8.3,f8.2,f6.1)
 998  format(a,i4,5f8.2,5f8.3,f8.1)
      return
      end

      subroutine CALORGN (c)

!-----calculate source, sink terms for ORG-N
      use common_qual
      implicit none
      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
      real*8  xk, set

!-----hydrolysis of org. N to NH3-N

      xk = rcoef(morgn,decay) * c(morgn)

!-----loss of mass due to settling
      set = rcoef(morgn,settle) * c(morgn)

      if (mtemp .gt. 0) then
         xk = xk*thetadj(temp_orgn_decay)
         set = set*thetadj(temp_orgn_set)
      endif

      scsk(morgn) = scsk(morgn) - xk - set

!-----if NH3 is simulated, decay of ORG-N is a source of NH3

      if (mnh3 .gt. 0) scsk(mnh3) = scsk(mnh3) + xk

      return
      end

      subroutine CALNH3 (c)

!-----calculate source, sink terms for NH3-N
      use common_qual
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
      real*8  xk, benth

!-----contribution due to reactions

      xk = rcoef(mnh3,decay) * c(mnh3)

!-----account for benthos source

      benth = rcoef(mnh3,benthic)/depth

      if (mtemp .gt. 0) then
         xk = xk*thetadj(temp_nh3_decay)
         benth = benth*thetadj(temp_nh3_ben)
      endif

      scsk(mnh3) = scsk(mnh3) - xk + benth

!-----if NO2 is simulated, biolog. oxidation of NH3 is a source of NO2

      if (mno2 .gt. 0) scsk(mno2) = scsk(mno2) + xk

!-----if DO is simulated, oxidation of NH3 exerts demand on DO

      if (mdo .gt. 0) scsk(mdo) = scsk(mdo) - oxy_nh3*xk
      return
      end

      subroutine CALNO2 (c)

!-----calculate source, sink terms for NO2-N
      use common_qual
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
      real*8  xk

!-----contribution due to reactions

      xk = rcoef(mno2,decay) * c(mno2)

      if (mtemp .gt. 0)  xk = xk*thetadj(temp_no2_decay)

      scsk(mno2) = scsk(mno2) - xk

!-----if NO3 is simulated, biolog. oxidation of NO2 is a source of NO3

      if (mno3 .gt. 0) scsk(mno3) = scsk(mno3) + xk

!-----if DO is simulated, oxidation of NO2 exerts demand on DO

      if (mdo .gt. 0) scsk(mdo) = scsk(mdo) - oxy_no2*xk
      return
      end

      subroutine CALNO3 (c)
      use common_qual
      implicit none
      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)

      return
      end

      subroutine CALORGP (c)

!-----calculate source, sink terms for ORG-P
      use common_qual
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
      real*8  xk, set

!-----contribution due to reactions

      xk = rcoef(morgp,decay) * c(morgp)

!-----loss of mass due to settling

      set = rcoef(morgp,settle) * c(morgp)

      if (mtemp .gt. 0) then
         xk = xk*thetadj(temp_orgp_decay)
         set = set*thetadj(temp_orgp_set)
      endif

      scsk(morgp) = scsk(morgp) - xk - set

!-----if PO4 is simulated, decay of ORG-P is a source of PO4

      if (mpo4 .gt. 0) scsk(mpo4) = scsk(mpo4) + xk

      return
      end

      subroutine CALPO4 (c)

!-----calculate source, sink terms for PO4
      use common_qual
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
      real*8  benth

!-----account for benthos source

      benth = rcoef(mpo4,benthic)/depth

      if (mtemp .gt. 0) then
        benth = benth*thetadj(temp_po4_ben)
      endif

      scsk(mpo4) = scsk(mpo4) + benth

      return
      end

      subroutine CALALG (c)
      use common_qual
      use logging
      use runtime_data
!-----calculate source, sink terms for ALGAE
!      use IO_Units,only: unit_output
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
	real*8 xn, fn, fp, lambda, grw, fl, resp, factor, mort
      real*8 cmalg,cmpo4

      real*8  inten
      real*8  xk, set,fnnn

      cmalg=dble(c(malg))

!-----calculate growth

      xn = 0

      if (mnh3 .gt. 0) xn = c(mnh3)
      if (mno3 .gt. 0) xn = xn + c(mno3)
      if (xn .gt. 0) then
         fn = xn/(xn+knit_half)
      else
         fn = 1
      endif

      if (mpo4 .gt. 0) then
         cmpo4 = max(0., c(mpo4))   !@todo eli added this. What is the deal with negative conc?
         fp = cmpo4/(cmpo4+kpho_half)
      else
         fp = 1
      endif

!-----account for algal self shading

      lambda = lambda0 + lambda1*alg_chl_ratio*cmalg + &
          lambda2 * (alg_chl_ratio*cmalg)**(2./3.)
      inten = tsolhr

      fl = 1/(lambda*depth)*log((klight_half+inten)/(klight_half+inten* &
          exp(-lambda*depth)))
      fnnn=fl*min(fn,fp)
      grw = rcoef(malg,alg_grow) * cmalg * fnnn

!-----respiration

      resp = rcoef(malg,alg_resp) * cmalg

!-----settling

      set = rcoef(malg,settle)/depth * cmalg

!-----mortality

      mort = rcoef(malg,alg_die) * cmalg

      if (mtemp .gt. 0) then
         grw = grw * thetadj(temp_alg_grow)
         resp = resp * thetadj(temp_alg_resp)
         set = set * thetadj(temp_alg_set)
         mort = mort * thetadj(temp_alg_die)
      endif

      xk = grw -resp -set -mort

      scsk(malg) = scsk(malg) + xk

!-----if BOD is simulated, algae mortality is a source of BOD

      if (mbod .gt. 0) scsk(mbod) = scsk(mbod) + alg_bod*mort

!-----if ORG-N is simulated, algae respiration is a source of ORG-N

      if (morgn .gt. 0) scsk(morgn) = scsk(morgn) + algaefract_n*resp

!-----if NH3-N is simulated, algae growth demands NH3-N

      if (mnh3 .gt. 0 .and. mno3 .eq. 0) then
         scsk(mnh3) = scsk(mnh3) - algaefract_n*grw

!--------if NO3-N is simulated, algae growth demands NO3-N

      elseif (mno3 .gt. 0 .and. mnh3 .eq. 0)then
         scsk(mno3) = scsk(mno3) - algaefract_n*grw

!--------if NH3-N and NO3-N are both simulated, a preference factor is used.

      elseif (mnh3.gt.0 .and. mno3.gt.0) then
         if (c(mnh3).eq.0 .and. c(mno3).eq.0)then
            factor = pref_factor
         else

            factor = pref_factor*c(mnh3)/(pref_factor*c(mnh3)+(1-pref_factor) &
                *c(mno3))
            scsk(mnh3) = scsk(mnh3) - factor*algaefract_n*grw
            scsk(mno3) = scsk(mno3) - (1-factor)*algaefract_n*grw
         end if
      end if

!-----if ORG-P is simulated, algae respiration is a source of ORG-P

      if (morgp .gt. 0) scsk(morgp) = scsk(morgp) + algaefract_p*resp

!-----if PO4-P is simulated, algae growth demands PO4-P

      if (mpo4 .gt. 0) scsk(mpo4) = scsk(mpo4) - algaefract_p * grw

!-----if DO is simulated, algae growth is a source, resp. demands DO

      if (mdo .gt. 0) scsk(mdo) = scsk(mdo)+oxy_photo *grw - oxy_resp *resp
!-----the following write statements are for diagnostics purposes only
      if(print_level.ge.3)then
         if(iskip .eq. 1)then
            if (mod(julmin, 120) .eq. 0) then
               if (chan_res .eq. 1) then
                  if(int2ext(n) .eq. 20) then
                     write(ialgout,999)current_date,int2ext(n),c(mdo),cmalg,c(mbod), &
                         c(mnh3),c(mno3),c(mpo4),c(mtemp),grw,tsolhr,fl,fn,fp,mort
                  endif
               elseif (chan_res .eq. 2)then
                  write(iralgout,998)current_date,irev,c(mdo),cmalg,c(mbod), &
                      c(mnh3),c(mno3),c(mpo4),c(mtemp),grw,tsolhr,fl,depth,mort
               endif
            endif
         endif
      endif
 999  format(a,i4,6f9.2,f9.1,2f9.2,4f9.3)
 998  format(a,i4,6f9.2,2f9.1,3f9.2,2f9.3)
      return
      end

      SUBROUTINE CALTEMP (c)

!-----Calculate source sink term for TEMPERATURE

!-----It computes the net amount of heat
!---- exchanged through the air-water interface.
!-----The energy budget considers solar radiation, atmospheric
!---- radiation, back radiation, conduction and evaporation.

!-----Solar radiation and atmospheric radiation are
!-----calculated in subroutine heat
      use common_qual
      use logging
      use runtime_data
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

      real*8 c(max_constituent)
      real*8  tw, vpw, evapor, hb, hc, he, hsnet,evap_rate
      tw = c(mtemp)*1.8 + 32

!-----Water surface back radiation (HB)

      hb=1.6781e-09*(tw+460.0)**4

!-----Evaporation (HE)
!-----vpair is calculated in heat.f; latent heat of vap.
!-----is represented by (1084.0-0.5*tw) in HE & HC

      vpw=0.1001*exp(0.03*tw)-0.0837
      evapor=62.4*(evapcoeff_a + evapcoeff_b*wind)
      he=evapor*(vpw-vpair)*(1084.0-0.5*tw)

!-----Conduction (HC)
      hc=0.01*evapor*(dryblb-tw)*(1084.0-0.5*tw)

!-----Compute net heat flux from all sources

!      A SIGNIFICANT CHANGE OF ORIGINAL CODE, FOR TSOLHR HAS ALRADEY CHANGED TO FLUX NOW IN HEAT.F
!      NEED TO BE CONFIRMED JON
!       hsnet = tsolhr+ (ha + hc - hb - he)*dble(time_step)/60.       !v6
        hsnet = tsolhr+ (ha + hc - hb - he)                            !v8


!-----Contribution to SCSK is net radiation/(depth*density*heat cap)

!      ALSO A SIGNIFICANT CHANGE HERE
!       scsk(mtemp) = (hsnet/(depth*62.4*(dble(time_step)/60.)))/1.8   !v6
       scsk(mtemp) = (hsnet/(depth*62.4))/1.8                         ! v8



!-----the following write statements are for diagnostics purposes only
      if(print_level.ge.3)then
         if (iskip .eq. 1) then
            if (mod(julmin, 120) .eq. 0) then
               evap_rate=(evapcoeff_a + evapcoeff_b*wind)*(vpw-vpair)*8640
               if(chan_res.eq.1)then
                  if(int2ext(n) .eq. 20) then
                     write(ihout,999) current_date,int2ext(n),tsolhr,ha, &
                         hb,he,hc,hsnet,c(mtemp),evap_rate,dryblb
                  endif
               elseif(chan_res.eq.2)then
                  write(irhout,998) current_date,irev,tsolhr,ha, &
                      hb,he,hc,hsnet,c(mtemp),evap_rate
               endif
            endif
         endif
      endif
 999  format(a,i4,f10.1,f10.2,f9.1,f10.1,f13.3,5f10.2)
 998  format(a,i5,f10.1,f10.2,f9.1,f10.1,f13.3,3f10.2)

      return
      end

      subroutine tempfactors(temp)
      use common_qual
      implicit none

      include 'param.inc'
      include 'bltm3.inc'
      real*8 temp
	real*8 tmp,frac,dtemp
      real*8,save :: templastused = -901.d0
	integer i,it,itemp

	dtemp=temp
!-----integer numcalled

      if(dtemp.eq. miss_val_r)then
!--------Create the lookup table
         do it=1,80
            tmp=dble(it)*dble(0.5)-dble(20.)
            do i=1,temp_coeff_type
               thettbl(i,it)=thet(i)**(tmp)
            enddo
         enddo
      !elseif(abs(dtemp-templastused).le.0.1)then
!--------temperature has changed by less than 0.1,
!--------no need to update the values
      !   return
      elseif(dtemp.ge. 5.D-01 .and. dtemp.le. 40.)then
!--------temperature in the right range,
!--------it's ok to use the table
!--------interpolate between values
!--------numcalled=numcalled+1
         itemp=int(dtemp*2.)
         frac=dtemp*2.-dble(itemp)
         do i=1,temp_coeff_type
            thetadj(i)=(1.-frac)*thettbl(i,itemp)+ &
                frac*thettbl(i,itemp+1)
         enddo
      else
!--------temperature not in the right range,
!--------go ahead and calculate the values
         do i=1,temp_coeff_type
            thetadj(i)=thet(i)**(dtemp-20.)
         enddo
      endif
      templastused=dtemp
      return
      end
