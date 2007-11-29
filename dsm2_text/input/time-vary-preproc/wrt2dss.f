      SUBROUTINE wrt2dss (cpath, cdate, ctime, itimes, nvals, values,
     *     cunits, ctype, dssfile, irregular)

      CHARACTER cpath*80, cdate*20, ctime*4, cunits*8, ctype*8, cname*64
      CHARACTER dssfile*32
      INTEGER nvals, itimes(nvals), ifltab(600) ,istat, iostat,
     *     irregular
      REAL values(nvals)

      npath = lnblnk(cpath)
      cpath = cpath(1: npath)
      cdate = cdate(1:lnblnk(cdate))   
      cunits = cunits(1:lnblnk(cunits)) 
      ctype = ctype(1:lnblnk(ctype))   
      dssfile = dssfile(1:lnblnk(dssfile))

C-----Attach the needed units
      CALL attach (6,'OUTPUT','STDOUT', ' ', cname, istat)
      CALL attach (idum, 'DSSFILE', ' ', 'NOP', cname, istat)
      CALL attend

C-----Open the file for writing
      CALL zopen (ifltab, dssfile, iostat)
      IF (iostat .GT. 0) THEN
         PRINT *, 'UNABLE TO OPEN THE FILE ', dssfile 
      ENDIF
      
C-----Determine if regular data or irregular data
      IF (irregular) THEN 
C--------Write the irregular time-series data to the DSS file
         CALL zsits (ifltab, cpath, itimes, values, nvals, 0, 
     *        cunits, ctype, 0, istat)
         IF (istat .GT. 10) THEN
            PRINT *, 'FATAL ERROR. PATHNAME NOT STORED.'
         ENDIF
C--------Write the regular time-series data to the DSS file
      ELSE
         CALL zsrts (ifltab, cpath, cdate, ctime, nvals, values,
     *        cunits, ctype, 0, istat)
         IF (istat .GT. 10) THEN
            PRINT *, 'FATAL ERROR. PATHNAME NOT STORED.'
         ENDIF
      ENDIF
      
      CALL zclose (ifltab)
      RETURN
      END


