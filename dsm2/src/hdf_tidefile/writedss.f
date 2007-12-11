C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!
C!    For more information about DSM2, contact:
C!
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

      subroutine writedss(pathnumber, cdt, in_values, nvals)

c-----Write out a block of data to DSS
      use IO_Units
      implicit none

      include '../fixed/common.f'
      include 'dss.inc'
      include 'writedss.inc'

      logical
     &     isopen(max_dssoutfiles) ! true if DSS output file is already zopened

      character
     &     cdt*(*)              ! date/time for start of data
     &     ,cdatx*9             ! date
     &     ,ctimx*4             ! time
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,cunits*8            ! units (e.g. FEET, CFS)

      integer
     &     nvals ,istat, iostat
     &     ,pathnumber          ! global pathnumber
     &     ,npath,na,nb,nc,nd,ne,nf

      integer i
      REAL*8 in_values(nvals)
      REAL*4 values(nvals)

      data isopen /max_dssoutfiles * .false./

      if (nvals .le. 0) return
      do i =1,nvals
         values(i) = sngl(in_values(i))
      end do

      cdatx=cdt(1:9)
      ctimx=cdt(11:14)

      if (.not. isopen(pathoutput(pathnumber).ndx_file)) then
c--------Open the DSS file for writing
c--------exclusive write lock, works faster over NFS
c@@@         call zset('WLOCK','ON',0)
c--------set module name as data creator
         call zset('PROGRAM',dsm2_name,0)
c--------preset for a very large .dss file
         call zset('SIZE', ' ', 1000000)

         call zopen (ifltab_out(1,pathoutput(pathnumber).ndx_file),
     &        pathoutput(pathnumber).filename, iostat)
         if (iostat .gt. 0) then
            write(unit_error,'(a,a)') 'Unable to open the file ',
     &           pathoutput(pathnumber).filename
            call exit(2)
         endif
         isopen(pathoutput(pathnumber).ndx_file)=.true.
      endif

c-----write the time block
      cunits=pathoutput(pathnumber).units
      call upcase(cunits)
      call zsrts(ifltab_out(1,pathoutput(pathnumber).ndx_file),
     &     pathoutput(pathnumber).path,
     &     cdatx, ctimx, nvals, values, cunits,
     &     per_type_names(pathoutput(pathnumber).per_type), 0, istat)

      return
      end
