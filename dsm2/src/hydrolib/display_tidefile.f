      program display_tidefile

c-----display values in a hydro tidefile

      implicit none

      include '../input/fixed/common.f'
      include '../input/time-varying/tide.inc'
      include '../input/time-varying/common_tide.f'

c-----local variables
      integer
     &     tide_block_no        ! tide block number used (INPUT & RETURNED)
     &     ,lnblnk              ! intrinsic
     &     ,dim_res,dim_chan    ! reservoir and channel array dimensions
     &     ,n_res,n_chan        ! reservoir and channels used in tidefile
     &     ,i,j                 ! loop indices
     &     ,unit
     &     ,extchan,intchan     ! external and internal channel numbers

      integer*2
     &     extndx,intndx,One       ! external and internal index numbers
      Parameter (One=1)
	

c      integer*4
c     &     tidetime

      real
     &     chan_flow_avg(max_channels,2) ! channel flow averaged over tidefile
     &     ,res_gate_flow_avg(max_reservoirs,maxresnodes) ! reser gate flow averaged over tidefile
     &     ,res_net_flow(max_reservoirs) ! reser net flow each tideblock
     &     ,res_net_flow_avg(max_reservoirs) ! reser net flow averaged over tidefile
     &     ,aveq                ! average flow

      character
     &     tidefile*200         ! tidefile name
     &     ,datetime*14
     &     ,jmin2cdt*14

      external jmin2cdt

      call dsm2_init
      call list                 ! form internal/external channel sequence

      unit=30
      io_files(qual,io_tide,io_read).unit=unit
c-----get tidefile name from command line arg
      call getarg(One,tidefile)
      if (lnblnk(tidefile) .eq. 0) then ! no command line arg
 610     format('Usage: display_tidefile tidefile-name')
         write(unit_error,610)
         call exit(1)
      endif

c-----read and display header info
      call read_tide_head(tidefile,.false.)
      print *
     &     , 'dim_res',dim_res
     &     ,' dim_chan',dim_chan
     &     , ' n_res',n_res
     &     ,' n_chan',n_chan

      print *
      print *, 'repeating_tidefile: ',repeating_tidefile

      print *
      print *, 'Channel bottom elevations:'
      do intchan=1,nchans
            print *
     &           ,chan_geom(intchan).chan_no
     &           ,chan_geom(intchan).bottomelev(1)
     &           ,chan_geom(intchan).bottomelev(2)
         endif
      enddo

      print *
      print *, 'Object-to-object info:'
      print *, 'nobj2obj',nobj2obj
      do i=1,nobj2obj
         print *, i
         print *, 'from:'
         print *, 'object: ',obj_names(obj2obj(i).from.object)(:5)
         print *, 'name: ',obj2obj(i).from.obj_name(:5)
         print *, 'number: ',obj2obj(i).from.object_no
         print *, 'mass_frac: ',obj2obj(i).from.mass_frac

         print *, 'to:'
         print *, 'object: ',obj_names(obj2obj(i).to.object)(:5)
         print *, 'name: ',obj2obj(i).to.obj_name(:5)
         print *, 'number: ',obj2obj(i).to.object_no
         print *, 'mass_frac: ',obj2obj(i).to.mass_frac

         print *
         print *, 'name: ',obj2obj(i).name
      enddo

      print *
      print *, 'External flow info:'
      print *, 'nqext',nqext
      do i=1,nqext
 620     format(i4,' Name: ',a10,
     &        ' Attached to: ',a,' ',a,' ',i3,
     &        ' mass_frac: ',f5.3)
         write(6,620) i,qext(i).obj_name,
     &        obj_names(qext(i).attach.object)(:5),
     &        qext(i).attach.obj_name(:5),
     &        qext(i).attach.object_no,
     &        qext(i).mass_frac
      enddo

      print *
      print *, 'Stage boundary:'
      print *, ('node ', stgbnd(i).node, ' ',stgbnd(i).name(:5), i=1,nstgbnd)

      tide_block_no=0
      do i=1,max_channels
         chan_flow_avg(i,1)=0.0
         chan_flow_avg(i,2)=0.0
      enddo
      do i=1,max_reservoirs
         res_net_flow_avg(i)=0.0
         do j=1,maxresnodes
            res_gate_flow_avg(i,j)=0.0
         enddo
      enddo
      
c-----print time-varying info for each tideblock
 100  continue
      print *
      read(unit,end=200) TideTime ! time at end of tide averaged block
c-----instaneous values at start of tide block
      read(unit) Eresv          ! reservoir elevation
      read(unit) Ychan          ! channel depth at each end
      read(unit) Achan          ! channel cross-sectional flow area at each end
      read(unit) Achan_Avg      ! average channel cross-sectional flow area

c-----the averages are between this TideTime and previous TideTime
      read(unit) QResv          ! average reservoir flow thru gates
      read(unit) (obj2obj(i).flow_avg,i=1,nobj2obj)
      read(unit) Qchan          ! average channel flow at each end
      read(unit) nqext_changed  ! external flows

      if (nqext_changed .gt. 0) then
         read(unit) (
     &        qext(i).changed_ndx, ! external flow index number
     &        qext(qext(i).changed_ndx).avg, ! average external flow value
     &        i=1,nqext_changed)
      endif
      tide_block_no=tide_block_no+1

c-----averaged channel flow over tidecycle
      do i=1,max_channels
         chan_flow_avg(i,1)=chan_flow_avg(i,1)+qchan(i,1)
         chan_flow_avg(i,2)=chan_flow_avg(i,2)+qchan(i,2)
      enddo

c-----averaged reservoir flow thru gates over tidecycle
      do i=1,max_reservoirs
         res_net_flow(i)=0.0
         do j=1,maxresnodes
            res_gate_flow_avg(i,j)=res_gate_flow_avg(i,j)+qresv(i,j)
            res_net_flow(i)=res_net_flow(i)-qresv(i,j)
         enddo
         j=1
         do while (res_geom(i).qext(j) .ne. 0)
            extndx=res_geom(i).qext(j)
            res_net_flow(i)=res_net_flow(i)+qext(extndx).avg
            j=j+1
         enddo
         j=1
         do while (res_geom(i).qint(j) .ne. 0)
            intndx=res_geom(i).qint(j)
            if (obj2obj(intndx).from.object .eq. obj_reservoir .and.
     &           obj2obj(intndx).from.object_no .eq. i) then
               res_net_flow(i)=res_net_flow(i)-obj2obj(intndx).flow_avg
            endif
            if (obj2obj(intndx).to.object .eq. obj_reservoir .and.
     &           obj2obj(intndx).to.object_no .eq. i) then
               res_net_flow(i)=res_net_flow(i)+obj2obj(intndx).flow_avg
            endif
            j=j+1
         enddo
         res_net_flow_avg(i)=res_net_flow_avg(i)+res_net_flow(i)
      enddo

      datetime=jmin2cdt(tidetime)
      print *, 'tide block: ',tide_block_no,' tide time: ',datetime
      print *
      print *, 'Reservoir flow thru gates:'
      do i=1,max_reservoirs
         if (qresv(i,1) .ne. 0.0 .and. qresv(i,2) .ne. 0.0)
     &        print *, i,(j,qresv(i,j),j=1,5)
      enddo

      print *
      print *, 'Reservoir elevations and net flow:'
      do i=1,max_reservoirs
         if (eresv(i) .ne. 0.0) print *, i,eresv(i),res_net_flow(i)
      enddo

      print *
      print *, 'Channel stages, channel end and ave flows, ave xsect area:'
      do intchan=1,nchans
            print *, chan_geom(intchan).chan_no
     &           ychan(intchan,1)+chan_geom(intchan).bottomelev(1),
     &           ychan(intchan,2)+chan_geom(intchan).bottomelev(2),
     &           qchan(intchan,1),qchan(intchan,2),
     &           (qchan(intchan,1)+qchan(intchan,2))/2.,
     &           achan_avg(intchan)
         endif
      enddo

      print *
      print *, 'obj2obj flows:'
      do i=1,nobj2obj
         print *, i,obj2obj(i).flow_avg
      enddo
      print *
      print *, 'qext flows:'
      do i=1,nqext_changed
         print *, qext(i).changed_ndx,qext(qext(i).changed_ndx).avg
      enddo
      
      goto 100
 200  continue

c-----end of tidefile

      print *
      print *, 'Reservoir net flow averaged over tidefile:'
      do i=1,max_reservoirs
         if (res_net_flow_avg(i) .ne. 0.0)
     &        print *, i,res_net_flow_avg(i)/tide_block_no
      enddo

      print *
      print *, 'Channel flow averaged over tidefile:'
      do extchan=1,max_channels
         intchan=ext2int(extchan)
         if (intchan .ne. miss_val_i) then
            aveq=(chan_flow_avg(intchan,1)+chan_flow_avg(intchan,2))/
     &           (2.*tide_block_no)
            print *, extchan,aveq
         endif
      enddo

      print *
      print *, 'Channel end flow diff averaged over tidefile:'
      do extchan=1,max_channels
         intchan=ext2int(extchan)
         if (intchan .ne. miss_val_i) then
            aveq=(chan_flow_avg(intchan,1)-chan_flow_avg(intchan,2))/
     &           tide_block_no
            if (abs(aveq) .gt. 1.0)
     &           print *, extchan,aveq
         endif
      enddo

      end

      subroutine list
c-----list channel sequence
      implicit none

      include '../input/fixed/common.f'

      integer
     &     int2ext_tmp(0:max_channels)
     &     ,extchan,intchan     ! external and internal channel numbers

      data int2ext_tmp /
     &     0,
     &     1,
     &     2,
     &     3,
     &     4,
     &     5,
     &     6,
     &     7,
     &     8,
     &     9,
     &     10,
     &     11,
     &     12,
     &     13,
     &     14,
     &     15,
     &     169,
     &     170,
     &     171,
     &     16,
     &     17,
     &     560,
     &     561,
     &     18,
     &     19,
     &     20,
     &     21,
     &     22,
     &     23,
     &     311,
     &     312,
     &     313,
     &     24,
     &     25,
     &     54,
     &     55,
     &     56,
     &     57,
     &     58,
     &     125,
     &     126,
     &     127,
     &     128,
     &     129,
     &     130,
     &     131,
     &     132,
     &     133,
     &     134,
     &     59,
     &     60,
     &     196,
     &     197,
     &     198,
     &     199,
     &     61,
     &     62,
     &     63,
     &     64,
     &     65,
     &     66,
     &     67,
     &     68,
     &     69,
     &     201,
     &     202,
     &     203,
     &     183,
     &     184,
     &     185,
     &     186,
     &     187,
     &     188,
     &     189,
     &     190,
     &     191,
     &     192,
     &     193,
     &     194,
     &     204,
     &     205,
     &     206,
     &     207,
     &     208,
     &     209,
     &     210,
     &     211,
     &     212,
     &     213,
     &     70,
     &     71,
     &     72,
     &     73,
     &     74,
     &     75,
     &     76,
     &     77,
     &     78,
     &     79,
     &     80,
     &     81,
     &     214,
     &     216,
     &     82,
     &     83,
     &     84,
     &     85,
     &     217,
     &     218,
     &     219,
     &     220,
     &     221,
     &     222,
     &     223,
     &     224,
     &     225,
     &     226,
     &     227,
     &     228,
     &     229,
     &     230,
     &     231,
     &     135,
     &     136,
     &     137,
     &     138,
     &     86,
     &     252,
     &     253,
     &     254,
     &     255,
     &     257,
     &     88,
     &     89,
     &     90,
     &     91,
     &     92,
     &     233,
     &     234,
     &     235,
     &     139,
     &     140,
     &     141,
     &     142,
     &     143,
     &     236,
     &     237,
     &     238,
     &     239,
     &     240,
     &     241,
     &     242,
     &     243,
     &     244,
     &     245,
     &     246,
     &     247,
     &     258,
     &     259,
     &     94,
     &     95,
     &     96,
     &     97,
     &     98,
     &     99,
     &     100,
     &     101,
     &     102,
     &     103,
     &     104,
     &     105,
     &     106,
     &     107,
     &     108,
     &     109,
     &     110,
     &     111,
     &     112,
     &     113,
     &     114,
     &     115,
     &     116,
     &     117,
     &     118,
     &     119,
     &     120,
     &     121,
     &     122,
     &     123,
     &     124,
     &     26,
     &     27,
     &     28,
     &     29,
     &     30,
     &     31,
     &     314,
     &     315,
     &     316,
     &     317,
     &     318,
     &     319,
     &     320,
     &     321,
     &     322,
     &     172,
     &     173,
     &     174,
     &     175,
     &     176,
     &     177,
     &     178,
     &     179,
     &     180,
     &     181,
     &     182,
     &     144,
     &     145,
     &     146,
     &     147,
     &     148,
     &     149,
     &     150,
     &     151,
     &     152,
     &     153,
     &     154,
     &     155,
     &     156,
     &     157,
     &     158,
     &     159,
     &     160,
     &     161,
     &     162,
     &     163,
     &     164,
     &     165,
     &     166,
     &     167,
     &     168,
     &     248,
     &     249,
     &     250,
     &     251,
     &     261,
     &     262,
     &     263,
     &     264,
     &     265,
     &     266,
     &     267,
     &     268,
     &     269,
     &     270,
     &     271,
     &     272,
     &     273,
     &     274,
     &     275,
     &     276,
     &     277,
     &     278,
     &     279,
     &     280,
     &     32,
     &     33,
     &     34,
     &     35,
     &     36,
     &     37,
     &     38,
     &     39,
     &     40,
     &     41,
     &     42,
     &     43,
     &     44,
     &     45,
     &     46,
     &     47,
     &     48,
     &     49,
     &     50,
     &     51,
     &     52,
     &     53,
     &     307,
     &     308,
     &     309,
     &     310,
     &     323,
     &     324,
     &     325,
     &     326,
     &     327,
     &     328,
     &     329,
     &     330,
     &     331,
     &     332,
     &     333,
     &     549,
     &     550,
     &     551,
     &     552,
     &     553,
     &     555,
     &     556,
     &     557,
     &     558,
     &     559,
     &     334,
     &     335,
     &     591,
     &     336,
     &     337,
     &     338,
     &     339,
     &     340,
     &     341,
     &     342,
     &     343,
     &     344,
     &     345,
     &     346,
     &     347,
     &     348,
     &     590,
     &     349,
     &     350,
     &     351,
     &     352,
     &     353,
     &     354,
     &     355,
     &     356,
     &     357,
     &     592,
     &     358,
     &     359,
     &     360,
     &     593,
     &     361,
     &     362,
     &     363,
     &     594,
     &     364,
     &     365,
     &     366,
     &     367,
     &     368,
     &     369,
     &     370,
     &     371,
     &     372,
     &     373,
     &     374,
     &     375,
     &     376,
     &     377,
     &     378,
     &     379,
     &     380,
     &     381,
     &     382,
     &     383,
     &     384,
     &     385,
     &     386,
     &     387,
     &     388,
     &     389,
     &     390,
     &     391,
     &     392,
     &     393,
     &     394,
     &     395,
     &     396,
     &     397,
     &     398,
     &     410,
     &     411,
     &     412,
     &     413,
     &     414,
     &     415,
     &     416,
     &     417,
     &     418,
     &     419,
     &     420,
     &     421,
     &     422,
     &     423,
     &     424,
     &     425,
     &     426,
     &     427,
     &     428,
     &     429,
     &     399,
     &     400,
     &     401,
     &     402,
     &     403,
     &     404,
     &     405,
     &     406,
     &     407,
     &     408,
     &     409,
     &     430,
     &     431,
     &     432,
     &     433,
     &     434,
     &     435,
     &     511,
     &     436,
     &     281,
     &     437,
     &     438,
     &     439,
     &     440,
     &     441,
     &     530,
     &     531,
     &     532,
     &     513,
     &     515,
     &     442,
     &     443,
     &     444,
     &     454,
     &     445,
     &     446,
     &     447,
     &     448,
     &     449,
     &     450,
     &     451,
     &     452,
     &     453,
     &     497,
     &     498,
     &     499,
     &     502,
     &     503,
     &     505,
     &     506,
     &     509,
     &     516,
     &     517,
     &     522,
     &     523,
     &     527,
     &     528,
     &     471,
     &     472,
     &     473,
     &     458,
     &     459,
     &     460,
     &     461,
     &     462,
     &     463,
     &     464,
     &     465,
     &     466,
     &     483,
     &     484,
     &     485,
     &     486,
     &     488,
     &     489,
     &     490,
     &     491,
     &     492,
     &     494,
     &     495,
     &     535,
     &     537,
     &     538,
     &     539,
     &     542,
     &     543,
     &     544,
     &     467,
     &     468,
     &     469,
     &     474,
     &     470,
     &     482,
     &     475,
     &     476,
     &     477,
     &     478,
     &     479,
     &     480,
     &     481,
     &     99 * miss_val_i
     &     /

      do intchan=1,max_channels
         ext2int(intchan)=miss_val_i
      enddo

      do intchan=1,max_channels
         int2ext(intchan)=int2ext_tmp(intchan)
      enddo

      do extchan=1,max_channels
         if (int2ext(extchan) .ne. miss_val_i)
     &        ext2int(int2ext(extchan))=extchan
      enddo

      return
      end
