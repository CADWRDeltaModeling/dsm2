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

      subroutine read_fixed(init_input_file,env_only,istat)

c-----Read the fixed (non-time-varying) input data for DSM2 modules.
      use IO_Units
      USE DFLIB                 !! <NT>
      implicit none
      
      include '../hydrolib/network.inc'

      include 'common.f'
      include 'common_ptm.inc'
      include '../hdf_tidefile/common_tide.f'
      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'
      include '../hdf_tidefile/tide.inc'

c-----local variable declaration

      character*(*) init_input_file ! initial input file (provided) [optional]

      logical env_only          ! read only environment variables

      character
     &     line*250             ! processed input line
     &     ,input_line*250      ! raw input line; should be same size as line
     &     ,ctemp1*20,ctemp2*20,ctmpl*250 ! temporary

      character
     &     input_files(max_inp_files)*150 ! list of input files to read
      common /com_files/ input_files
      common /input_lines/ input_line

      integer
     &     istat                ! status variable (returned)
     &     ,nlines              ! line counter
     &     ,nsects              ! num of section keywords from input file
     &     ,nfields             ! num of field keywords from input file
     &     ,nflds               ! num of field keywords for this section
     &     ,lnblnk              ! index of last non-blank character
     &     ,nl_line             ! lnblnk index for a line
     &     ,vsect               ! valid section counter
     &     ,nfld1               ! input field counter
     &     ,nfld2               ! valid field counter
     &     ,ifld(max_fields)    ! ifld(i)=order header keyword i occurs in file
     &     ,rifld(0:max_fields) ! reverse ifld
     &     ,ifile               ! files counter
     &     ,nfiles              ! number of files to read
     &     ,itmp1,itmp2         ! temp variables
     &     ,i,j,k               ! indices
     &     ,getpid              ! unix fortran system call to get process ID
     &     ,line_size           ! size of character line
     &     ,len                 ! size of character variable function
     &     ,nenv,repl_envvars   ! environment var replacement

c!OTHER      real
c!OTHER     &     rand                 ! fortran intrinsic

      logical
     &     lsect                ! true if program is expecting section keyword
     &     ,lfield              ! true if program is expecting field keywords
     &     ,ldefault            ! true if values are for defaults

      common /read_fix_l/ ldefault

c-----DSS subroutine variables
      integer
     &     ibegf(max_fields)    ! beginning position of each field in line
     &     ,ilenf(max_fields)   ! length of each field in line
     &     ,idelmt(max_fields)  ! type of delimiter for each field
     &     ,idelmp(max_fields)  ! position in delimiter string of delimiter
     &     ,itbl(128)           ! needed by findlm routine; for most inputs
     &     ,itbl_u(128)         ! needed by findlm routine; for unique inputs

      character*15 field_names(max_fields) ! copy of hdr_form.fld(*)

      data
     &     input_files /max_inp_files * ' '/

c-----write formats
 610  format (/a,i5
     &     /a
     &     /'File line number: ',i5
     &     /'File name: ',a)
 612  format (/a
     &     /a
     &     /'File line number: ',i5
     &     /'File name: ',a)
 620  format (/a,a
     &     /'Max allowed:',i2,'  Read in:',i3
     &     /a
     &     /'File line number: ',i5
     &     /'File name: ',a)
 630  format (/a,a
     &     /'Field keyword: ',a
     &     /'Field number: ',i3
     &     /'File line number: ',i5
     &     /'File name: ',a)

 640  format(/'Data fields do not match number of field headers'
     &     /'Number of field headers:',i5
     &     /'Number of data fields:',i5
     &     /a
     &     /'File line number:',i5
     &     /'File name: ',a)

 645  format(/'Too many input title lines; maximum allowed is:',i5)

 650  format(/'Input error in file ',a
     &     /'at line number:',i5
     &     /a
     &     /'File name: ',a)

 651  format(/'Input warning in file ',a
     &     /'at line number:',i5
     &     /a
     &     /'File name: ',a)

 660  format(/'Too many input filenames:',i5
     &     /'Filename is: ',a
     &     /'File line number:',i5
     &     /'File name: ',a)

 670  format(/'Too many input dsm channel numbers:',i5
     &     /'Channel number is: ',a
     &     /'File line number:',i5
     &     /'File name: ',a)

 680  format(/a,a)

c-----local initialization
      do i=1,max_sections
         hdr_form(i).fldnum=0
         hdr_form(i).sect=' '
         hdr_form(i).repeat=.false.
         do j=1,max_fields
            hdr_form(i).fld(j)=' '
            field_names(j)=' '
         enddo
      enddo

      nfiles=1
      ifile=1

c-----set line size for later DSS calls
      line_size=len(line)

c-----keywords

      nsects=1
      hdr_form(nsects).sect='outputpaths'
      hdr_form(nsects).fld(outpath_filename)='filename'
      hdr_form(nsects).fld(outpath_a_part)='a_part'
      hdr_form(nsects).fld(outpath_b_part)='b_part'
      hdr_form(nsects).fld(outpath_c_part)='c_part'
      hdr_form(nsects).fld(outpath_e_part)='e_part'
      hdr_form(nsects).fld(outpath_f_part)='f_part'
      hdr_form(nsects).fld(outpath_name)='name'
      hdr_form(nsects).fld(outpath_chan)='chan'
      hdr_form(nsects).fld(outpath_dist)='dist'
      hdr_form(nsects).fld(outpath_node)='node'
      hdr_form(nsects).fld(outpath_res_name)='reservoir'
      hdr_form(nsects).fld(outpath_res_node)='reservoir_node'
      hdr_form(nsects).fld(outpath_type)='type'
      hdr_form(nsects).fld(outpath_interval)='interval'
      hdr_form(nsects).fld(outpath_period)='period'
      hdr_form(nsects).fld(outpath_modifier)='modifier'
      hdr_form(nsects).fld(outpath_from_name)='from_name'
      hdr_form(nsects).fld(outpath_source_group)='source_group'
      hdr_form(nsects).fld(outpath_variable)='variable'
      hdr_form(nsects).fld(outpath_fromwb)='from_wb'
      hdr_form(nsects).fld(outpath_towb)='to_wb'

      nsects=nsects+1
      hdr_form(nsects).sect='io_files'
      hdr_form(nsects).fld(io_model)='model'
      hdr_form(nsects).fld(io_type)='type'
      hdr_form(nsects).fld(io_io)='io'
      hdr_form(nsects).fld(io_interval)='interval'
      hdr_form(nsects).fld(io_filename)='filename'

      nsects=nsects+1
      hdr_form(nsects).sect='inp_files'

      nsects=nsects+1
      hdr_form(nsects).sect='titles'

      nsects=nsects+1
      hdr_form(nsects).sect='scalar'

      nsects=nsects+1
      hdr_form(nsects).sect='particle_flux'
      hdr_form(nsects).fld(ptm_from_wb)='from_wb'
      hdr_form(nsects).fld(ptm_to_wb)='to_wb'
      hdr_form(nsects).fld(ptm_interval)='interval'
      hdr_form(nsects).fld(ptm_filename)='filename'
      hdr_form(nsects).fld(b_part)='b_part'
c      hdr_form(nsects).fld(max_fields)=delimiter ! denotes repeating fields for this section

      nsects=nsects+1
      hdr_form(nsects).sect='group_output'
      hdr_form(nsects).fld(ptm_group)='group'
      hdr_form(nsects).fld(ptm_interval)='interval'
      hdr_form(nsects).fld(ptm_filename)='filename'
      hdr_form(nsects).fld(b_part)='b_part'

      nsects=nsects+1
      hdr_form(nsects).sect='partinp'
      hdr_form(nsects).fld(partno_node)='node'
      hdr_form(nsects).fld(partno_nparts)='nparts'
      hdr_form(nsects).fld(partno_slength)='slength'
      hdr_form(nsects).fld(partno_length)='length'
      hdr_form(nsects).fld(partno_sdate)='sdate'
      hdr_form(nsects).fld(partno_stime)='stime'
      hdr_form(nsects).fld(partno_edate)='edate'
      hdr_form(nsects).fld(partno_etime)='etime'
      hdr_form(nsects).fld(partno_type)='type'

      nsects=nsects+1
      hdr_form(nsects).sect='groups'
      hdr_form(nsects).fld(group_name)='group_name'
      hdr_form(nsects).fld(group_memtype)='member_type'
      hdr_form(nsects).fld(group_memid)='member_id'

      nsects=nsects+1
      hdr_form(nsects).sect='tidefile'
      hdr_form(nsects).fld(tide_sdate)='start_date'
      hdr_form(nsects).fld(tide_stime)='start_time'
      hdr_form(nsects).fld(tide_edate)='end_date'
      hdr_form(nsects).fld(tide_etime)='end_time'
      hdr_form(nsects).fld(tide_fname)='filename'

      nsects=nsects+1
      hdr_form(nsects).sect='qual_binary'
      hdr_form(nsects).fld(binary_fname)='filename'

      nsects=nsects+1
      hdr_form(nsects).sect='rate_coeffs'
      hdr_form(nsects).fld(coeff_chan)='channel'
      hdr_form(nsects).fld(coeff_res)='reservoir'
      hdr_form(nsects).fld(coeff_type)='type'
      hdr_form(nsects).fld(coeff_const)='constituent'
      hdr_form(nsects).fld(coeff_value)='value'

      nsects=nsects+1
      hdr_form(nsects).sect='envvars'
      hdr_form(nsects).fld(envvar_name)='name'
      hdr_form(nsects).fld(envvar_value)='value'

c-----determine the number of allowable fields for each section keyword
      i=1
      do while (i .le. max_sections .and. hdr_form(i).sect .ne. ' ')
         k=0
         do j=1,max_fields
            if (hdr_form(i).fld(j) .ne. ' ' .and.
     &           hdr_form(i).fld(j) .ne. delimiter) k=k+1
         enddo
         hdr_form(i).fldnum=k
         hdr_form(i).repeat=hdr_form(i).fld(max_fields) .eq. delimiter
         i=i+1
      enddo

c-----use spaces and tabs as delimiters
      call setdlm(3,' ',1,0,itbl) ! don't use string delimiters
      call setdlm(2, '	 ',1,2,itbl) ! space and tab
      call setdlm(1, ' ',1,0,itbl) ! don't use type 1 delimiters

c-----get starting input filename from input arg,
c-----or use default
      if (lnblnk(init_input_file) .eq. 0) then
         input_line='dsm2.inp'
      else
         input_line=init_input_file
      endif
      input_files(1)=input_line

      do while (input_files(ifile) .ne. ' ')
         open (
     &        unit=unit_input
     &        ,file=input_files(ifile)
     &        ,status='old'
     &        ,iostat=istat
     &        ,err=902
     &        )

c--------read keywords from a file
         lsect=.true.
         lfield=.false.
         nlines=0
         nsects=0

 100     continue
         istat=0
         read(unit_input,'(a)',end=901) line
         nlines=nlines+1
c--------remove comment portion of line
         itmp1=index(line,'#')
         if (itmp1 .eq. 1) line=' '
         if (itmp1 .gt. 1) line=line(1:itmp1-1)

c--------get rid of ending tab chars
         nl_line=len_trim(line)
         line=trim(line)
         if (nl_line .gt. 0)then
         do while ( nl_line .gt. 0 .and.
     &        line(nl_line:nl_line) .eq. '	') ! tab char
            nl_line=nl_line-1
            if (nl_line .ne. 0) line=line(:nl_line)
         enddo
         end if
         if (nl_line.eq.0) goto 100 ! comment or blank line

c--------replace environment variables in line with their value
c--------env vars are of this form: $[({]string[)}]
c--------pseudo/internal env vars (from the ENVVARS section) will
c--------be replaced too
         nenv=repl_envvars(line,ctmpl)
         line=ctmpl

         input_line=line ! (almost) raw input line

         call locase(line)      ! convert all input to lower case

c--------check 'end' keyword   ! eid added len_trim test for section end
         if (line(1:3).eq.'end' .and. len_trim(line).eq.3) then
            lsect=.true.
            lfield=.false.
            goto 100
         endif

         nfields=max_fields
         ibegf=0
	   ilenf=0
	   call findlm(line,1,line_size,nfields,ibegf,ilenf,idelmt,idelmp
     &        ,itbl)

         if (lsect) then
c-----------process section headers
            if (nfields.gt.2) then
               write(unit_error,610)
     &              'Too many words in section keyword: ',nfields
     &              ,line(:nl_line),nlines
     &              ,trim(input_files(ifile))
               goto 900
            endif
c-----------check that section keyword is valid
            vsect=1
            do while (vsect .le. max_sections .and.
     &           (hdr_form(vsect).sect .ne. ' ') .and.
     &           (line(ibegf(1):ibegf(1)+ilenf(1)-1).ne
     &           .hdr_form(vsect).sect) )
               vsect=vsect+1
            enddo
            if (vsect.gt.max_sections .or.
     &           hdr_form(vsect).sect .eq. ' ') then
               write(unit_error,612)
     &              'Invalid section keyword: '
     &              ,line(:nl_line),nlines
     &              ,trim(input_files(ifile))
               goto 900
            endif

            nsects=nsects+1
            lsect=.false.
            lfield=.true.
c-----------check if field values will be for default
            if (nfields .eq. 2)then
               ldefault=(line(ibegf(2):ibegf(2)+3) .eq. 'def')
            else
               ldefault=.false.
            endif
c-----------if section has just 1 field, no need for field headers
            if (hdr_form(vsect).fldnum .eq. 0) lfield=.false.
            goto 100
         endif

         if (lfield) then
c-----------process field keywords
            if (hdr_form(vsect).fldnum .gt. 0 .and.
     &           .not. hdr_form(vsect).repeat .and.
     &           nfields .gt. hdr_form(vsect).fldnum) then
               write(unit_error,620)
     &              'Too many fields for the section ',hdr_form(vsect)
     &              .sect,hdr_form(vsect).fldnum,nfields
     &              ,line(:nl_line),nlines,
     &              trim(input_files(ifile))
               goto 900
            endif

c-----------initialize field logic
            do nfld1=1,max_fields
               ifld(nfld1)=0
               rifld(nfld1)=0
            end do

c-----------process field headers
            nfld1=0             ! true field header counter (skip delimiter chars)
            do i=1,nfields      ! input line fields (includes delimiter chars)
               if (line(ibegf(i):ibegf(i)+ilenf(i)-1)
     &              .ne. delimiter) then ! skip delimiter indicators
                  nfld1=nfld1+1
                  nfld2=1       ! keyword counter
                  do while (.true.)
                     if (nfld2 .gt. max_fields) exit
                     if (.not. (
     &                    trim(hdr_form(vsect).fld(nfld2)) .ne.
     &                    line(ibegf(i):ibegf(i)+ilenf(i)-1)) ) exit
                     nfld2=nfld2+1
                  enddo
                  if (nfld2 .gt. max_fields) then
                     write(unit_error,630)
     &                    'Invalid field keyword in the section: '
     &                    ,hdr_form(vsect).sect
     &                    ,line(ibegf(i):ibegf(i)+ilenf(i)-1)
     &                    ,nfld1,nlines
     &                    ,trim(input_files(ifile))
                     goto 900
                  endif
                  ifld(nfld2)=nfld1
                  rifld(nfld1)=nfld2
                  field_names(nfld2)=hdr_form(vsect).fld(nfld2)
               endif
            enddo
            lfield=.false.
            nflds=nfields
            goto 100
         endif

 200     continue
c--------section header and field headers ok;
c--------read the data into appropriate variables

c--------check that data fields are same as number of headers
         if (hdr_form(vsect).fldnum .ne. 0 .and.
     &        .not. hdr_form(vsect).repeat .and.
     &        nfields .ne. nflds) then
            write(unit_error, 640)
     &           nflds, nfields
     &           ,trim(input_line)
     &           ,nlines
     &           ,trim(input_files(ifile))
            goto 900
         endif

c--------pass the data as char strings to appropriate section handler
         if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'titles') then
            ntitles=ntitles+1
            if (ntitles .le. max_titles) then
               title(ntitles)=' '
               title(ntitles)=trim(input_line)
            else
               write(unit_error, 645)
     &              max_titles
            endif


         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'outputpaths') then
            call input_outputpath(field_names, max_fields, nfields,
     &           nflds, ifld, rifld(1), line, ibegf, ilenf, istat)

         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'io_files') then
            call input_iofiles(field_names, max_fields, nfields, nflds,
     &           ifld, rifld(1), line, ibegf, ilenf, istat)


         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'quad') then
            call input_quadrature(field_names, max_fields, nfields,
     &           nflds, ifld, rifld(1), line, ibegf, ilenf, istat)

         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'inp_files') then
            nfiles=nfiles+1
            if (nfiles .le. max_inp_files) then
               input_files(nfiles)=input_line(ibegf(1):ibegf(1)
     &              +ilenf(1)-1)
            else
               write(unit_error, 660) max_inp_files
     &              ,line(ibegf(1):ibegf(1)+ilenf(1)-1)
     &              ,nlines
     &              ,trim(input_files(ifile))
               goto 900
            endif

         else if (hdr_form(vsect).sect .eq. 'envvars') then
            call input_envvar(field_names, max_fields, nfields, nflds, ifld,
     &           rifld(1), line, ibegf, ilenf, istat)

         else if ( (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'scalar') .or. 
     &           (index(line,"model_name") .gt. 0) .or. 
     &           (index(line,"database") .gt. 0)   
     &              ) then
            call input_scalar(field_names, max_fields, nfields, nflds,
     &           ifld, rifld(1), line, ibegf, ilenf, istat)

         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'particle_flux') then
            call setdlm(3,' ',1,0,itbl_u) ! don't use string delimiters
            call setdlm(2, '	 ',1,2,itbl_u) ! space and tab
            call setdlm(1, ',',1,1,itbl_u) ! use type 1 delimiters (comma only)
            call findlm(line,1,line_size,nfields,ibegf,ilenf,idelmt,idelmp
     &           ,itbl_u)
            call input_particle_flux(field_names, max_fields, nfields, nflds,
     &           ifld, rifld(1), line, ibegf, ilenf, idelmt, istat)
         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'group_output') then
            call input_group_output(field_names, max_fields, nfields, nflds,
     &           ifld, rifld(1), line, ibegf, ilenf, idelmt, istat)
         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'partinp') then
            call input_partno(field_names, max_fields, nfields, nflds,
     &           ifld, rifld(1), line, ibegf, ilenf, istat)
         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'groups') then
            call input_groups(field_names, max_fields, nfields, nflds,
     &           ifld, rifld(1), line, ibegf, ilenf,  istat)
         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'tidefile') then
            call input_tidefile(field_names, max_fields, nfields, nflds, ifld,
     &           rifld(1), line, ibegf, ilenf, istat)

         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'qual_binary') then
            call input_qualbin(field_names, max_fields, nfields, nflds, ifld,
     &           rifld(1), line, ibegf, ilenf, istat)

         else if (.not. env_only .and.
     &           hdr_form(vsect).sect .eq. 'rate_coeffs') then
            call input_rate_coeffs(field_names, max_fields, nfields, nflds, ifld,
     &           rifld(1), line, ibegf, ilenf, istat)
         endif

         if (istat .lt. 0) then
            write(unit_error, 650)
     &           trim(input_files(ifile))
     &           ,nlines
     &           ,trim(line)
     &           ,trim(input_files(ifile))
            goto 900
         endif

         if (istat .gt. 0) then
            write(unit_error, 651)
     &           trim(input_files(ifile))
     &           ,nlines
     &           ,trim(line)
     &           ,trim(input_files(ifile))
         endif

         goto 100

 901     continue
         if (nsects .eq. 0) then
            write(unit_error,*)
     &           'No section keywords were read for file ',
     &           trim(input_files(ifile))
         endif

         close(unit=unit_input)
         ifile=ifile+1
         if (ifile .gt. max_inp_files) then
            write(unit_error, *)
     &           'Too many input files; increase size of max_inp_files.'
            goto 900
         endif
      enddo
      return

 902  continue                  ! here for file open error

      if ( ifile .eq. 1 .and. input_files(ifile) .eq. 'dsm2.inp') then
      	write(unit_output,'(/a)') 'No initial text input file given. Run will be loaded from database.'
          istat=0
	    return
	else
          write(unit_error,'(/a,a)')
     &     'Could not open file ',trim(input_files(ifile))
      end if

 900  continue

      istat=-1

      return
      end
