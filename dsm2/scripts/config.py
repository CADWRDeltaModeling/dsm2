# config.py
# 
# Purpose:
#         To define the environmental variables required for gates, ec at mtz, hydro and qual

# Input: s
#        input-envvars.inp
# Output:
#        different envars variables for input and outputs of gates, ec estimate at mtz, hydro and qual
# initialize some of the environments

config={}

# Substitute config values for environmental variables
# Args:
#     val   input string that possibly includes environmental variables
def subEnv(val):
  import string
  import re
  if string.find(val,'$')>=0:
    value=val
    for key in config.keys():
       # Substitute ${ENV} or $(ENV) or $ENV- or $ENV_ or $ENV<whitespace>
       brack=r"(\${"+key+"})|\$\("+key+"\)|(\$"+key+"(?!(\w|_|-)))"    # See python re module docs
       value=re.sub(brack,config[key],value)
  if string.find(value,'$')>=0:
    raise "Unknown environmental variable in string: %s" % value
  return value


def setConfigVars(infile):
  import re
  from jnios import os
  import string
  global config
  envfh = open(infile)
  start = 0
  end = 0
  matchstr = re.compile(r"""(${)(.*)(})""",re.IGNORECASE)
  for line in envfh.readlines():
      body = string.strip(string.split( line, '#')[0])
      if body:
        if string.lower(body) == 'envvars':
          start = 1
        elif string.lower(body) == 'end':
          end = 1
        elif start and not end:
          vals = string.split(body)
          if len(vals) != 2:
            continue
          name=string.lower(vals[0])
          # replace backward slashes with forward to avoid escape key confusion
          val=string.replace(string.lower(vals[1]),'\\','/')

          # substitute envvars that have been previously defined.
          if string.find(val,'$')>=0:
            val=subEnv(val)
          config[name]=val
          os.environ[name]=val       
  if not start or not end:
    raise 'Keyword ENVVARS or END is missing in ENVVARS FILE'
  return config


def getAttr(attrName):
  import string
  name=string.lower(attrName)
  if name in config.keys():
      return config[name]
  else:
      return None



  
