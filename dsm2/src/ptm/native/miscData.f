c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_model_date(jmin, date)
      implicit none
      integer*4 jmin
      character*(*) date
      character*9 ndate
      character*4 ntime
      call convert2stringdates(jmin,ndate,ntime)
      date = ndate(1:9) // char(0)
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_model_time(jmin, time)
      implicit none
      integer*4 jmin
      character*(*) time
      character*9 ndate
      character*4 ntime
      call convert2stringdates(jmin,ndate,ntime)
      time = ntime(1:4) // char(0)
      return 
      end
