import os, glob, shutil
from vtimeseries import *
from vista.set import *
from vista.db.dss import *
from vutils import *
from gov.ca.dsm2.input.parser import Parser
from gov.ca.dsm2.input.parser import Tables
from gov.ca.dsm2.input.model import *

if __name__ == "__main__":
    # Generate runs of DSM2 Sensitivity Tests using Condor
    perturbedRuns = ['ManN_Ch','Disp_Ch','XTopW_Ch','XElev_Ch','DICU-QDIV_Nd','DICU-QRET_Nd','DICU-ECRET_Nd']
    for perturbed in perturbedRuns:
        x=prepCondorDir(perturbed)
        x=sys.run('c:/condor/bin/condor_submit dsm2-'+xxx+'.sub')
#
def prepCondorDir(perturbed):
    rootDir='d:/delta/models'
    sensDir=rootDir + '/studies/2010-Calibration/SensitivityTests'
    DSM2Module='both'
    DSM2Run='HIST-CLB2K-BASE-1991-v81_0'
    hydroExe=sensDir + '/hydro.exe'
    qualExe=sensDir + '/qual.exe'
    commonDir=rootDir + '/common_input'
    tsDir=rootDir + '/timeseries'
    configFile='config.inp'
    tsFiles='events.dss, hist_19902007.dss, gates_db.dss, dicu_200705.dss,' + \
            'dicu_200905.dss, hist_19902007.dss, dicuwq_200611_expand.dss, climate-5years.dss'
    cmnFiles='boundary_flow_calsim_delta_20090715.inp,' + \
            'boundary_flow_delta_historical_20090715.inp,source_flow_jones_hydro_20090806.inp,' + \
            'boundary_stage_adjust_astro_tide_20090715.inp,' + \
            'boundary_stage_delta_historical_20090715.inp,' + \
            'channel_historical_stability_revision_20090715.inp,channel_ic_std_delta_grid_20090715.inp,' + \
            'channel_planning_stability_revision_20090715.inp,channel_std_delta_grid_20090715.inp,' + \
            'channel_tapered_tracy_blvd_dredge_20090715.inp,gate_permanent_barriers_20090715.inp,' + \
            'gate_std_delta_grid_20090827.inp,gate_temp_barriers_20090715.inp,' + \
            'generate.py,group_rate_coeff_groups_20090715.inp,group_sourcetracking_20090715.inp,' + \
            'input_climate_delta_historical_climate_20090715.inp,' + \
            'input_gate_planning_ccfb_dcc_timing_20090715.inp,' + \
            'node_concentration_calsim_delta_ec_20090715.inp,' + \
            'node_concentration_delta_historical_do_20090715.inp,' + \
            'node_concentration_delta_historical_qual_20090715.inp,' + \
            'node_concentration_dicu_do_20090715.inp,node_concentration_dicu_ec_20090715.inp,' + \
            'node_concentration_jones_qual_20090715.inp,node_concentration_mtz_planning_ec_20090715.inp,' + \
            'oprule_ccc_dual_intake_20090715.inp,oprule_hist_temp_barriers_20090715.inp,' + \
            'oprule_historical_gate_20090715.inp,oprule_montezuma_planning_gate_20090715.inp,' + \
            'oprule_sdip_20090715.inp,oprule_temp_barriers_planning_20090715.inp,' + \
            'output_channel_std_hydro_boundary_20090715.inp,output_channel_std_hydro_calib.inp,' + \
            'output_channel_std_qual_calib.inp,output_gate_sdip_20090715.inp,' + \
            'output_gate_temporary_barriers_20090715.inp,output_reservoir_std_hydro_named_20090715.inp,' + \
            'output_reservoir_std_qual_named_20090715.inp,output_reservoir_source_track_ec_20090715.inp,' + \
            'rate_coefficient_delta_ncc_20090715.inp,reservoir_concentration_dicu_ec_20090715.inp,' + \
            'reservoir_ic_std_delta_grid_20090715.inp,reservoir_std_delta_grid_20090715.inp,' + \
            'scalar_hydro_std_20090715.inp,scalar_qual_do_parameters_20090715.inp,' + \
            'scalar_qual_std_20090715.inp,source_flow_calsim_delta_20090715.inp,' + \
            'source_flow_delta_historical_20090715.inp,source_flow_dicu_20090715.inp,' + \
            'source_flow_dicu_historical_20090715.inp,source_flow_ocap_diversions_20090715.inp'
    condorDir='condor-' + perturbed
    perturbedDir='PtbInFiles-' + perturbed
    
    if not exist condorDir mkdir condorDir
    del /q condorDir\*
    
    @copy /v /b %HYDROEXE% condorDir\
    @copy /v /b %QUALEXE% condorDir\
    @xcopy /i/q %TSDIR% condorDir
    @xcopy /i/q %COMMONDIR% condorDir
    
    @copy /v /y %CONFIGFILE% condorDir\
    @copy /v /y dsm2-base.sub condorDir\
    @copy /v /y %SENSDIR%\BaseRun-0\%DSM2RUN%.?rf condorDir\
    @copy /v /y hydro_restart.inp condorDir\hydro.inp
    @copy /v /y qual_ec_restart.inp condorDir\qual_ec.inp
    set START_DATE="01OCT1992"
    
    rem restart from Base Run, or cold run (no restart)?
    if NOT %UseRestart% == 1 (
    rem No Restart here
    @copy /v /y hydro_norst.inp condorDir\hydro.inp
    @copy /v /y qual_ec_norst.inp condorDir\qual_ec.inp
    set START_DATE="01OCT1992"
    )
    
    cd condorDir
    
    if NOT %DSM2MODULE% == qual (
    rem Create condor_dsm2.bat file for Hydro
    echo echo Running hydro >> condor_dsm2.bat
    echo time /t >> condor_dsm2.bat
    echo hydro.exe %%1 >> condor_dsm2.bat
    echo time /t >> condor_dsm2.bat
    )
    if NOT %DSM2MODULE% == hydro (
    rem Finish condor_dsm2.bat file for Qual
    echo echo Running qual >> condor_dsm2.bat
    echo time /t >> condor_dsm2.bat
    echo qual.exe %%2 >> condor_dsm2.bat
    echo time /t >> condor_dsm2.bat
    )
    
    echo del /f/q *.h5 *.??f >> condor_dsm2.bat
    echo del /f/q *echo*.inp >> condor_dsm2.bat
    
    if %PRTBPREFIX% == Depth_Res (
    set FLIST=%SENSDIR%\%PTBDIR%\mildred,%SENSDIR%\%PTBDIR%\franks_tract,%SENSDIR%\%PTBDIR%\discovery_bay,%SENSDIR%\%PTBDIR%\clifton_court,%SENSDIR%\%PTBDIR%\bethel
    ) else (
    set FLIST=%SENSDIR%\%PTBDIR%\???
    )
    
    for /F usebackq %%F in (`dir/b/o:n %FLIST%`) do call :Condor %%F
    rem Next line for testing purposes
    rem for %%F in (001) do call :Condor %%F
    cd %SENSDIR%
    goto :EOF
    
    :Condor
    rem input arg is "001", "002", etc. for channel or node numbers
    rem input arg is reservoir names for reservoirs
    echo %1
    @copy /y %SENSDIR%\%PTBDIR%\%1 .
    @copy /y %SENSDIR%\%PTBDIR%\PerturbedInp%PRTBPREFIX%%1.inp .
    @copy /y dsm2-base.sub dsm2-%1.sub
    echo error = dsm2-%1.err >> dsm2-%1.sub
    echo log = dsm2-%1.log >> dsm2-%1.sub
    echo output = dsm2-%1.out >> dsm2-%1.sub
    
    REM if DICU setup, include the perturbed DICU DSS file, otherwise don't
    echo %PRTBPREFIX% | c:\windows\system32\find.exe /i "DICU"
    if errorlevel 1 (
    echo transfer_input_files = hydro.exe, qual.exe, %DSM2RUN%.qrf, %DSM2RUN%.hrf, %CONFIGFILE%, hydro%1.inp, qual_ec%1.inp, %1, PerturbedInp%PRTBPREFIX%%1.inp, %CMNFILES%, %TSFILES% >> dsm2-%1.sub
    set BLOCKTYPE=GRID
    ) else (
    @copy /y %SENSDIR%\%PTBDIR%\PerturbedInp%PRTBPREFIX%%1.dss .
    echo transfer_input_files = hydro.exe, qual.exe, %DSM2RUN%.qrf, %DSM2RUN%.hrf, %CONFIGFILE%, hydro%1.inp, qual_ec%1.inp, %1, PerturbedInp%PRTBPREFIX%%1.inp, PerturbedInp%PRTBPREFIX%%1.dss, %CMNFILES%, %TSFILES% >> dsm2-%1.sub
    
    REM detect if DICU node flow or qual change
    echo %PRTBPREFIX% | c:\windows\system32\find.exe /i "ECRET"
    if errorlevel 1 (
    set BLOCKTYPE=HYDRO_TIME_SERIES
    ) else (
    set BLOCKTYPE=QUAL_TIME_SERIES
    )
    )
    
    echo arguments =  hydro%1.inp, qual_ec%1.inp >> dsm2-%1.sub
    echo queue >> dsm2-%1.sub
    
    echo CONFIGURATION > tempEnv.inp
    echo %1 >> tempEnv.inp
    echo config.inp >> tempEnv.inp
    echo END >> tempEnv.inp
    
    echo %BLOCKTYPE% > tempInp.inp
    echo PerturbedInp%PRTBPREFIX%%1.inp >> tempInp.inp
    echo END >> tempInp.inp
    
    if "%BLOCKTYPE%" == "GRID" (
    @copy /y tempEnv.inp + hydro.inp + tempInp.inp hydro%1.inp
    @copy /y tempEnv.inp + qual_ec.inp qual_ec%1.inp 
    )
    if "%BLOCKTYPE%" == "HYDRO_TIME_SERIES" (
    @copy /y tempEnv.inp + hydro.inp + tempInp.inp hydro%1.inp
    @copy /y tempEnv.inp + qual_ec.inp qual_ec%1.inp 
    )
    if "%BLOCKTYPE%" == "QUAL_TIME_SERIES" (
    @copy /y tempEnv.inp + hydro.inp hydro%1.inp
    @copy /y tempEnv.inp + qual_ec.inp + tempInp.inp qual_ec%1.inp
    )
    
    c:\condor\bin\condor_submit dsm2-%1.sub
    sleep 1.0
    goto :EOF
#