# config_python.py derived from config.py for python 3.0+
#
# A configuration utility for converting name-value pairs
# in a "configuration file" into python-accessible values


config={}

def subEnv(val):
  """Performs a DSM2 ENVVAR type substitution.
     Input val A string, possibly referencing config variables
               For instance if OUTFILE is a config variable
               'studydir/${OUTPUT}/etc' contains a reference
     Output a string with the substitution
  """
  import re
  import os
  osenv=os.environ.copy()
  if 'PATH' in osenv: del osenv['PATH']

  if '$' in val:
    value = val
    for key in config.keys():
       # Substitute ${ENV} or $(ENV) or $ENV- or $ENV_ or $ENV<whitespace>
       brack = r"(\${"+key+"})|\$\("+key+"\)"    # See python re module docs
       value = re.sub(brack,config[key],value)
  if '$' in value:
    env_pattern = r"(\${(.*)})|\$\((.*)\)"    # See python re module docs
    env_match = re.findall(env_pattern,value)
    if env_match:
      for m in env_match:
        env_str = m[1]
        full_str = m[0]
        if env_str.upper() in osenv:
          subenv = osenv[env_str.upper()]
          value = value.replace(full_str,subenv)
    else:
      raise "Unknown environmental variable in string: %s" % value
    
  return value


def setConfigVars(infile):
  """Add name-value pairs from a file
     Input: infile name of a 'config' file whose contents
            look like a single ENVVARS section in dsm2 input
     Output: the dictionary containing the name-value pairs.
            the name-value pairs from the file will also
            be added to the system environment.
  """

  import re
  import os

  global config
  envfh = open(infile)
  start = 0
  end = 0
  matchstr = re.compile(r"""(${)(.*)(})""",re.IGNORECASE)
  for line in envfh.readlines():
      body = line.split('#')[0].strip()
      if body:
        if body.lower() == 'envvars' or body.lower() == "envvar":
          start = 1
        elif body.lower() == 'end':
          end = 1
        elif start and not end:
          vals = body.split()
          if len(vals) != 2:
            continue
          name = vals[0].lower()
          # replace backward slashes with forward to avoid escape key confusion
          val = vals[1].lower().replace('\\','/')

          # substitute envvars that have been previously defined.
          if '$' in val:
            val=subEnv(val)
          config[name]=val
          os.environ[name]=val
  if not start or not end:
    raise ValueError('Keyword ENVVARS or END is missing in config file')
  
  return config


def getAttr(attrName):
  """Retrieve a config value by name"""
  
  name = attrName.lower()
  if name in config.keys():
      return config[name]
  else:
      return None




