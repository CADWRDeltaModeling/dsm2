"""
    Script specifically for studies with limited EWA. This
    script assumes that limited EWA for Clifton Court is pre-calculated
    in CALSIM. It retrieves the pulse and non-pulse info from CALSIM,
    and then prepares a consolidated path with both.

    The script is based on prep_vamp.py, and handles CVP using routines
    from that file. The special treatment here is for SWP.

"""
from prep_vamp import *

def prep_limited_ewa_vamp_exports(calsimfile,outfile,fpart,fpart_mod,sjr_process):
    """Driver routine to construct SWP and CVP export flows.
      This routine extracts required time series from the
      CALSIM file, calls a few high level routines to orchestrate
      the calculation, then writes the output
    """

    # retrieve data
    path="/CALSIM/D419/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)
    swp_average_exports=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/D418/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)
    cvp_average_exports=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/EXPRATIO_/EI-RATIO-STD//1MON/fpart/".replace("fpart",fpart)
    ei_ratio=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/DINFLOW/INFLOW-PULSE//1MON/fpart/".replace("fpart",fpart)
    delta_inflow=dss_retrieve_ts(calsimfile,path)
    path = get_calsim_path(sjr_process, fpart)
    total_export_limit=dss_retrieve_ts(calsimfile,path)

    swp_non_pulse_path="/CALSIM/BANKSNPULSEWAFNL/APRMAYFLOW//1MON/fpart/".replace("fpart",fpart)
    swp_pulse_path="/CALSIM/BANKSPULSEWAFNL/APRMAYFLOW//1MON/fpart/".replace("fpart",fpart)
    #outpath="/CALSIM-VAMP/D419/FLOW-EXPORT//1DAY/fpart/".replace("fpart",fpart_mod)

    swp_non_pulse_exports=dss_retrieve_ts(calsimfile, swp_non_pulse_path)
    swp_pulse_exports=dss_retrieve_ts(calsimfile, swp_pulse_path)
    swp_vamp = replace_vamp(swp_non_pulse_exports,swp_pulse_exports)


    swp_limit,cvp_limit=project_export_limits(
                    total_export_limit,ei_ratio,delta_inflow)

    #swp_limit_corrected_for_vol_avg=calculate_exports(swp_limit,swp_average_exports)
    #assert ts_max(swp) <= SWP_MAX_PUMP, "SWP pumping exceeds physical bounds. This was assumed not to happen, so the preprocessor needs fixing"
    cvp_limit_corrected_for_vol_avg=calculate_exports(cvp_limit,cvp_average_exports)
    #assert ts_max(swp) <= CVP_MAX_PUMP, "CVP pumping exceeds physical bounds. This was assumed not to happen, so the preprocessor needs fixing"

    swp = replace_vamp(swp_average_exports,swp_vamp,include_shoulder=1)
    swp_path="/CALSIM-VAMP/D419/FLOW-EXPORT//1DAY/fpart/".replace("fpart",fpart_mod)
    dss_store_ts(outfile,swp_path,swp)

    cvp = replace_vamp(cvp_average_exports,cvp_limit_corrected_for_vol_avg,include_shoulder=1)
    cvp_path="/CALSIM-VAMP/D418/FLOW-EXPORT//1DAY/fpart/".replace("fpart",fpart_mod)
    dss_store_ts(outfile,cvp_path,cvp)

use = '''
Usage:
vscript prep_vamp.py configfile
- OR -
vscript.bat prep_vamp.py calsimdss outdss
    configfile  the input file for configuration variables
    calsimdss   the CALSIM file to be processed
    outdss      the destination dss file for calculated flows

The command line version does not include a dss FPART in its search.
It assumes that the CALSIM file does includes only one version (FPART)
for each of the required inputs. .
'''
def main():
    if len(sys.argv) == 3 or len(sys.argv) == 4:
        calsimdss=sys.argv[1]
        outdss=sys.argv[2]
        #config=0
        if len(sys.argv) == 4:
            fpart=sys.argv[3]
        else:
            fpart=""   # will match anything, so duplicates will give unexpected behavior
        fpart_modified=fpart
        if not(calsimdss.endswith(".dss") and outdss.endswith("dss")):
            raise SystemExit(use)
    elif len(sys.argv) == 2:
        configfile=sys.argv[1]
        config.setConfigVars(configfile)
        calsimdss=config.getAttr("CALSIMFILE")
        sjr_process=config.getAttr("SJR_PROCESS")
        outdss=config.getAttr("CALSIM_VAMP")
        fpart=calsim_study_fpart(modify=0)
        fpart_modified=calsim_study_fpart(modify=1)
    else:
        raise "wrong number of arguments in script prep_vamp"

    prep_vamp_vernalis(calsimdss,outdss,fpart,fpart_modified)
    prep_limited_ewa_vamp_exports(calsimdss,outdss,fpart,fpart_modified,sjr_process)
    sys.exit()

if __name__ == '__main__':
    main()
