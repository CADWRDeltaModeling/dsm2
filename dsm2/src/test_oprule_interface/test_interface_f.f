

      subroutine test_setup_f()
	use gates_data, only: gateArray,nGate,PIPE,WEIR
      implicit none
	include '../../hydro/network.inc'
	include 'common.f'
      include '../../hydro/chstatus.inc'
      include '../../hydro/chnlcomp.inc'
      integer istatus
      integer julday
      character*14 jmin2cdt
      call datjul("07 MAR 1990",julday, istatus)
	julmin=julday*24*60+15

	if (istatus .ne. 0) print*,"Error in fortran setup for date"

      nchans=3
      int2ext(1)=1
	int2ext(2)=3
	int2ext(3)=4
      chan_geom(1).chan_no=1
	chan_geom(1).length=15000
	chan_geom(2).chan_no=3
	chan_geom(2).length=14700
	chan_geom(3).chan_no=4
	chan_geom(3).length=5000
      UpCompPointer(1)=1
	DownCompPointer(1)=3
      UpCompPointer(2)=4
	DownCompPointer(2)=6
	UpCompPointer(3)=7
	DownCompPointer(3)=8
	CompLocation=0
	CompLocation(1)=0.
	CompLocation(2)=chan_geom(1).length/2.
	CompLocation(3)=chan_geom(1).length
	CompLocation(4)=0.
	CompLocation(5)=chan_geom(2).length/2.
	CompLocation(6)=chan_geom(2).length
	CompLocation(7)=0
	CompLocation(8)=chan_geom(3).length



	Q(1) = 1000.
	Q(2) = 2000.
	Q(3) = 4000.
	Q(4) = 8000.
	Q(5) = 16000.
	Q(6) = 32000.
	Q(7) = 64000.
	Q(8) = 128000.
	WS(1) = 1.
	WS(2) = 2.
      WS(3) = 3.
	WS(4) = 4.
	WS(5) = 5.
	WS(6) = 6.
	WS(7) = 7.
	WS(8) = 8.

	nGate=2
      GateArray(1).Name="gate 1"
	GateArray(1).NDevice=4
      GateArray(1).flow=1000.
      GateArray(1).Devices(1).Name = "pipe 1"
      GateArray(1).Devices(1).structureType = PIPE
	GateArray(1).Devices(1).maxWidth=1.
	GateArray(1).Devices(1).baseElev=-1.
	GateArray(1).Devices(1).flowCoefToNode=1.0
	GateArray(1).Devices(1).flowCoefFromNode=0.8


      GateArray(1).Devices(2).name = "pipe 2"
      GateArray(1).Devices(2).structureType = PIPE
	GateArray(1).Devices(2).maxWidth=2.
	GateArray(1).Devices(2).baseElev=-2.
	GateArray(1).Devices(2).flowCoefToNode=0.8
	GateArray(1).Devices(2).flowCoefFromNode=0.6

      GateArray(1).Devices(1).name = "weir 1"
      GateArray(1).Devices(1).structureType = WEIR
	GateArray(1).Devices(1).maxWidth=1.
	GateArray(1).Devices(1).baseElev=-1.
	GateArray(1).Devices(1).flowCoefToNode=1.0
	GateArray(1).Devices(1).flowCoefFromNode=0.8

      GateArray(1).Devices(2).name = "weir 2"
      GateArray(1).Devices(2).structureType = WEIR
	GateArray(1).Devices(2).maxWidth=2.
	GateArray(1).Devices(2).baseElev=-2.
	GateArray(1).Devices(2).flowCoefToNode=1.0
	GateArray(1).Devices(2).flowCoefFromNode=0.8
      GateArray(1).Devices(2).opCoefToNode=1.0
      GateArray(1).Devices(2).opCoefFromNode=1.0

      GateArray(2).Name="gate 2"
	GateArray(2).NDevice=1
      GateArray(2).flow=1000.

      GateArray(2).Devices(1).name = "weir 1"
      GateArray(2).Devices(1).structureType = WEIR
	GateArray(2).Devices(1).maxWidth=3.
	GateArray(2).Devices(1).baseElev=-3.
	GateArray(2).Devices(1).flowCoefToNode=1.0
	GateArray(2).Devices(1).flowCoefFromNode=0.8
      GateArray(2).Devices(1).opCoefToNode=0.2
      GateArray(2).Devices(1).opCoefToNode=0.5

      nobj2obj=2
      obj2obj(1).name="Transfer 1"
	obj2obj(1).flow=2000
	obj2obj(2).name="Transfer 2"
	obj2obj(2).flow=3000


      ninpaths=3
      pathinput(1).name="TS1"
	pathinput(1).value=1.
	pathinput(2).name="TS2"
	pathinput(2).value=2.
	pathinput(3).name="TS3"
	pathinput(3).value=3.

	return
	end subroutine



