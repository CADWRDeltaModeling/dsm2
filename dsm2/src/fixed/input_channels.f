      subroutine input_channels(field_names, mxflds, nfields, nflds,
     &     ifld, rifld, line, ibegf, ilenf, istat)

c-----process a character line into data arrays for channel info
      use io_units
      use constants
      implicit none


      logical
     &     ldefault             ! true if values are for defaults
      common /read_fix_l/ ldefault

c-----local variables

      integer
     &     mxflds               ! maximum number of fields
     &     ,nfields             ! number of fields in data line (input)
     &     ,nflds               ! number of fields in headers (input)
     &     ,ifld(mxflds)        ! ifld(i)=order header keyword i occurs in file (input)
     &     ,rifld(mxflds)       ! reverse ifld
     &     ,ibegf(mxflds)       ! beginning position of each field in line (input)
     &     ,ilenf(mxflds)       ! length of each field in line (input)
     &     ,istat               ! conversion status of this line (output)

      integer
     &     id
     &     ,channo              ! channel number
     &     ,prev_channo         ! track same channel numbers
     &     ,chan_len            ! channel length
     &     ,chan_downnode       ! channel downstream node
     &     ,chan_upnode         ! channel upstream node
     &     ,counter

!     todo: get rid of real*4. Time to get past the 1990s
      real*8
     &     chan_manning
     &     ,chan_dispersion

      character line*(*)        ! line from file (input)
      character*15 field_names(mxflds) ! copy of hdr_form.fld(*)

      integer
     &     i,j,k               ! indices

      character
     &     cstring*32           ! string field



c-----channel number is required for each line, except for default
      if (.not. ldefault .and. ifld(chan_no) .eq. 0) then
         write(unit_error, *)
     &        'No channel number given.'
         istat=-1
         goto 900
      endif

      if (ldefault) then
         channo=0
      else
         cstring=line(ibegf(ifld(chan_no)):ibegf(ifld(chan_no))+
     &        ilenf(ifld(chan_no))-1)
         read(cstring,'(i5)',err=810) channo
      endif

      k=1                       ! header field index
      i=1                       ! data field index
      do while (i .le. nfields)
         if (rifld(k) .eq. chan_no) goto 100 ! channel number already processed
         cstring=' '
         cstring=trim(line(ibegf(i):ibegf(i)+ilenf(i)-1))
         if (rifld(k) .eq. length) then
            read(cstring,'(i10)', err=810) chan_len
         else if (rifld(k) .eq. manning) then
            read(cstring,'(f10.0)', err=810) chan_manning
         else if (rifld(k) .eq. upnode) then
            read(cstring,'(i10)', err=810) chan_upnode
         else if (rifld(k) .eq. downnode) then
            read(cstring,'(i10)', err=810) chan_downnode
         else if (rifld(k) .eq. disp) then
            read(cstring,'(f10.0)', err=810) chan_dispersion
         endif
 100     continue
         k=k+1
         i=i+1
      enddo
      call process_channel(
     &                     counter,
     &                     channo,  ! don't really know what to do with this
     &                     channo,
     &                     chan_len,
     &                     chan_manning,
     &                     chan_dispersion,
     &                     chan_downnode,
     &                     chan_upnode)

      return

c-----char-to-value conversion errors
 610  format(/a)
 620  format(/a
     &     /'Input string is: ',a)
 630  format(/a,i5)
 810  continue
      write(unit_error, 620) 'Conversion error on field ' //
     &     field_names(rifld(i)), cstring

      istat=-2

 900  continue                  ! fatal error

      return
      end




