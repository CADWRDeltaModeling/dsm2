      subroutine read_qual_bin_head()

      implicit none

      include 'common_qual_bin.inc'
      include '../fixed/common.f'
      include '../fixed/common_ptm.inc'
      character*150 filenm      ! [INPUT]
      integer l
c      integer neq

      integer second(max_qual_nodes),first(max_qual_nodes)
      character*80 interval
c      integer*4 start_julmin
      character*14 start_date
      integer*4 cdt2jmin

      filenm = qual_bin_file.filename

      if(filenm .eq. ' ')then
         return
      endif

      neq = 1
      close(unit_binary)

      open(unit=unit_binary
     &     ,file=filenm
     &     ,status='old'
     &     ,form='unformatted'
     &     ,convert='big_endian'          !! <NT>
     &     ,err=900
     &     )
      

      read(unit_binary) qual_bin_file.version
      read(unit_binary) qual_bin_file.start_date
      read(unit_binary) qual_bin_file.start_julmin_file
      read(unit_binary) interval
      read(unit_binary) numnode
      read(unit_binary) neq
      read(unit_binary) (first(l),l=1,numnode) 
      read(unit_binary) (second(l),l=1,numnode) 
      
      QualTime = qual_bin_file.start_julmin_file

      do l = 1,numnode
         qual2node(second(l)) = first(l)
      enddo

c-----temporary until added in qual tidefile
      qual_bin_file.constituent(1)='EC'
c-----**************************************

      goto 901

 900  continue                  ! error on opening binary file
      write(unit_error,777) filenm
      call exit(2)
 777  format(/'Could not open input qual binary file:',/a)

 901  continue
      end
      

      subroutine read_quality_bin()

      implicit none

      include 'common_qual_bin.inc'
      include '../fixed/common.f'
      include '../fixed/common_ptm.inc'

      integer*4 cdt2jmin
      integer i,j
      integer*4 StartTime
      character*14 current_time, jmin2cdt, thistime
      if(qual_bin_file.filename .ne. ' ') then
         do while (QualTime .lt. julmin)
            read(unit_binary,end=100) current_time,((Qnode(i,j),i=1,numnode),j=1,neq)
            QualTime = cdt2jmin(current_time)
         enddo

         thistime = jmin2cdt(QualTime)
         
      endif

      return

 100  continue                  ! EOF on tide file

      write(unit_error,*)'Internal error end of binaray file'
      close(unit_binary)
      call exit(2)

      end


      subroutine init_qual_bin()

      implicit none

      include 'common_qual_bin.inc'
      include '../fixed/common_ptm.inc'

      character*150 filenm      ! [INPUT]
      character*14 current_time
      integer*4 cdt2jmin
      integer i,j

      call read_qual_bin_head()

      if(qual_bin_file.filename .ne. ' ') then
         do while (.true.)
            read(unit_binary,end=100) current_time,((Qnode(i,j),i=1,numnode),j=1,neq)
         enddo
      endif

 100  continue
      qual_bin_file.end_julmin_file = cdt2jmin(current_time)
      qual_bin_file.end_date = current_time

      call read_qual_bin_head()

      return

      end
      
