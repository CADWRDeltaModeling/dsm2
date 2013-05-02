from vtimeseries import interpolate
from prep_vamp import dss_retrieve_ts
import string
"""
"""
# remember this function always return a daily rts
def calc_vamp_delta_ndo(calsimfile,vamp_dss,fpart,fpart_mod,sjr_process):
    # sjr flow
    path="/CALSIM/C639/FLOW-CHANNEL//1MON/fpart/".replace("fpart",fpart)
    sjr_average_flow = dss_retrieve_ts(calsimfile,path)
    path = "/CALSIM-VAMP/C639/FLOW//1DAY/fpart/".replace("fpart",fpart_mod)
    sjr_vamp_flow = dss_retrieve_ts(vamp_dss,path)
    delta_sjr_flow = sjr_vamp_flow - interpolate(sjr_average_flow,"1DAY") 
    
    #cvp swp export
    path="/CALSIM/D419/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)   
    swp_average_exports=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/D418/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)
    cvp_average_exports=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM-VAMP/D419/FLOW-EXPORT//1DAY/fpart/".replace("fpart",fpart_mod)
    swp_vamp_exports=dss_retrieve_ts(vamp_dss,path)
    path="/CALSIM-VAMP/D418/FLOW-EXPORT//1DAY/fpart/".replace("fpart",fpart_mod)
    cvp_vamp_exports=dss_retrieve_ts(vamp_dss,path)
    delta_cvp_exports = cvp_vamp_exports - interpolate(cvp_average_exports,"1DAY")
    delta_swp_exports = swp_vamp_exports - interpolate(swp_average_exports,"1DAY")
    return delta_sjr_flow - delta_cvp_exports - delta_swp_exports
                
            
