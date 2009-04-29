# Change this to set the current version.
__version__ = "0.1b1"
# This is the list of all subpackages to import on import all.
__all__ = ("Base",)
# Uncomment this to add the mostly nonfunctiontal LinearAlgebra.
# This requires that Jama be installed and be on the classpath.
#__all__ = ("LinearAlgebra", "Base")

# Set up paths and grab JNumeric package.
import os, sys
__path__[:0] = [__path__[0]+os.sep+"JNumeric-"+__version__]
sys.add_package("JNumeric")

# Import the Numeric packages.
from JNumeric import umath
from Base import *



