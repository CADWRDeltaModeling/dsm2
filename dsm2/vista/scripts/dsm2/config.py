# config.py
#
# A configuration utility for converting name-value pairs
# in a "configuration file" into jython-accessible values


config={}

def subEnv(val):
  """Performs a DSM2 ENVVAR type substitution.
     Input val A string, possibly referencing config variables
               For instance if OUTFILE is a config variable
               'studydir/${OUTPUT}/etc' contains a reference
     Output a string with the substitution
  """
  import string
  import re
  import os
  osenv=os.environ.copy()
  if osenv.has_key('PATH'): del osenv['PATH']
  
  if string.find(val,'$')>=0:
    value=val
    for key in config.keys():
       # Substitute ${ENV} or $(ENV) or $ENV- or $ENV_ or $ENV<whitespace>
       brack=r"(\${"+key+"})|\$\("+key+"\)"    # See python re module docs
       value=re.sub(brack,config[key],value)
  if string.find(value,'$')>=0:
    env_pattern=r"(\${(.*)})|\$\((.*)\)"    # See python re module docs    
    env_match=re.findall(env_pattern,value)
    if env_match:
      for m in env_match:
        env_str=m[1]
        full_str=m[0]
        if osenv.has_key(env_str.upper()):
          subenv=osenv[env_str.upper()]
          value=value.replace(full_str,subenv)
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
  import string
  global config
  envfh = open(infile)
  start = 0
  end = 0
  matchstr = re.compile(r"""(${)(.*)(})""",re.IGNORECASE)
  for line in envfh.readlines():
      body = string.strip(string.split( line, '#')[0])
      if body:
        if string.lower(body) == 'envvars' or string.lower(body) == "envvar":
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
    raise ValueError('Keyword ENVVARS or END is missing in config file')
  return config


def getAttr(attrName):
  """Retrieve a config value by name"""
  import string
  name=string.lower(attrName)
  if name in config.keys():
      return config[name]
  else:
      return None



  
