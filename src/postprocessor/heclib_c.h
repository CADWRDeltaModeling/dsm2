/**
 * C wrapper of the FORTRAN HEClib
 */

#ifndef _heclib_c_h_
#define _heclib_c_h_

#ifdef _MSC_VER
#define STDCALL 
#define STRLEN_TYPE int

#define zopen ZOPEN
#define zclose ZCLOSE
#define zset ZSET
#define zsrts ZSRTS
#define juldat JULDAT
#else
#define STDCALL __stdcall
#define STRLENT_TYPE int
#endif

extern "C" {

// ifltab(600)
void STDCALL zopen(int* ifltab, char* cname, int* iostat, STRLEN_TYPE len);
void STDCALL zclose(int* ifltab);

//  citem(4), cstr(6)
void STDCALL zset(const char* citem, const char* cstr, const int* inumb,
                  STRLEN_TYPE ctime_len, STRLEN_TYPE cstr_len);

//     Z-Store Regular interval Time-Series data
//     Data is stored according to the
//     time window set from CDATE/CTIME and NVALS.
// 
//     Input:
//        IFLTAB:  Working DSS array used in ZOPEN call.  Must be
//                 be dimensioned as INTEGER with 1200 words
//        CPATH:   Pathname of the data to be stored.  The "D" part
//                 is ignored.  CPATH sould be declared as
//                 CHARACTER*80.
//        CDATE:   Beginning date of the time window.  This may be
//                 a standard military style date (e.g. 01MAR74).
//        CTIME:   Beginning time of the time window.  This must be
//                 a standard 24 hour clock time (e.g. 1630).  Any
//                 time offset is implied by this date and time.
//                 CTIME should be declared as CHARACTER*4.
//        NVALS:   The number of data values to store.  This number
//                 defines the end of the time window.
//        VALUES:  The data to be stored.
//        CUNITS:  Character string containing the units of the data.
//                 CUNITS must be declared CHARACTER*8
//        CTYPE:   Character string containing the type of the data
//                 (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
//        IPLAN:   A flag indicating if existing data should be written
//                 over or not:
//                 IPLAN = 0  Always replace data.
//                 IPLAN = 1  Only replace missing data.
//                 IPLAN = 4  Do not allow a missing input data to
//                            replace a valid data piece.
//     Output:
//        ISTAT:   Integer status parameter, indicating the
//                 successfullness of the data storage.
//                 ISTAT = 0  All ok.
//                 ISTAT = 4  All missing data flags - no data stored
//                            unless IPLAN set to 2.
//                 ISTAT > 9  Illegal call to ZSRTSvoid STDCALL zsrts(int* ifltab, const char* cpath, const char* cdate,
// ifltab(600), cpath(80), cdate(20), ctime(4), cunits(8) ctype(8)
void STDCALL zsrts(int* ifltab, const char* cpath, const char* cdate,
                   const char* ctime, const int* nvals, const void* values,
                   const char* cunit, const char* ctype,
                   const int* iplan, int* istat,
                   STRLEN_TYPE cpath_len, STRLEN_TYPE cdate_len,
                   STRLEN_TYPE ctime_len, STRLEN_TYPE cunits_len,
                   STRLEN_TYPE ctype_len);

// convert the date to year, month, day
// cdate(*)
void STDCALL datjul(const char* cdate, int* jul, int* ierr, const int cdate_len);
// convert an DEC style julian date into a character date
//     Converts an HEC style julian date (days since Dec 31, 1899),
//     Into a character date of various styles, as shown below
//
//     Input:
//        JUL:  The julian date, in days since Dec 31, 1899
//     Output:
//        CDATE:  A character variable to contain the date (should be
//                long enough to hold the date
//        NDATE:  The number of characters in CDATE
//
//
//  ISTYLE  Form   ISTYLE   Form      ISTYLE   Form      ISTYLE   Form
//      LC, 4 CH YR       LC, 2 CH YR     UC, 4 CH YR      UC, 2 CH YR
//  0:  June 2, 1985  10:  June 2, 85  100:  JUNE 2, 1985  110:  JUNE 2, 85
//  1:  Jun 2, 1985   11:  Jun 2, 85   101:  JUN 2, 1985   111:  JUN 2, 85
//  2:  2 June 1985   12:  2 June 85   102:  2 JUNE 1985   112:  2 JUNE 85
//  3:  June 1985     13:  June 85     103:  JUNE 1985     113:  JUNE 85
//  4:  02Jun1985     14:  02Jun85     104:  02JUN1985     114:  02JUN85
//  5:  2Jun1985      15:  2Jun85      105:  2JUN1985      115:  2JUN85
//  6:  Jun1985       16:  Jun85       106:  JUN1985       116:  JUN85
//  7:  02 Jun 1985   17:  02 Jun 85   107:  02 JUN 1985   117:  02 JUN 85
//  8:  2 Jun 1985    18:  2 Jun 85    108:  2 JUN 1985    118:  2 JUN 85
//  9:  Jun 1985      19:  Jun 85      109:  JUN 1985      119:  JUN 85
//
//     ISTYLE=-1:  CDATE = 6/2/85       ISTYLE=-11:  CDATE = 06/02/85
//     ISTYLE=-2:  CDATE = 6-2-85       ISTYLE=-12:  CDATE = 06-02-85
//
//     If ISTYLE is zero, it defaults to style 1.
// cdate(*)
void STDCALL juldat(const int* jul, const int* istyle,
                    char* cdate, const int* ndate, int cdate_len);
}

#endif
