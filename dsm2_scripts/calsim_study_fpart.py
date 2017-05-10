"""Helper function to retrieve an fpart from the config file
   The function assumes you may want to modify the fpart.
"""


import config
def calsim_study_fpart(modify=0):
    """ Safely return the calsim study fpart for a study
        If modify=0 the search will start with CALSIMSTUDY_ORIGINAL and
        then fall back to CALSIMSTUDY

        If modify=1 the search will start with CALSIMSTUDY_MODIFIED and
        then fall back to CALSIMSTUDY
    """
    if modify:
        fpart=config.getAttr('CALSIMSTUDY_MODIFIED')
    else:
        fpart=config.getAttr('CALSIMSTUDY_ORIGINAL')
    if not fpart:
        fpart=config.getAttr('CALSIMSTUDY')
    if not fpart:
        raise "None of CALSIM, CALSIMSTUDY_ORIGINAL or CALSIMSTUDY_MODIFIED provided"
        
    return fpart
