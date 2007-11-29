
COMPILE NOTES:
heclib implementations for visual studio were compiled using Visual Studio and the Intel Fortran Compiler. They use the "default" names for Fortran and C++. They use the default search rules for libraries, which is typical for mixed language programming.

NAMING:
An example name is heclib_vs7_debug.lib. The name starts with heclib and adds:

A. The version of visual studio, e.g. vs7 (2003) or vs8 (2005).

B. The build configuration.
   VS7)  The default runtime library is single-thread, static linked release. 
         Other configurations are indicated by a series of optional suffixes:
       _mt for multi thread
       _dll for dll
       _debug for debug.

        Examples:
       heclib_vs7.lib (single thread, static link, release)
       heclib_vs7_debug.lib (single thread, static link, debug)
       heclib_vs7_mt_dll_debug.lib (multi thread, dll link, debug)
       heclib_vs7_mt_dll.lib (multi thread, dll link, release)

   VS8) Single thread runtime libraries are no longer available. The default 
        runtime library is multi-thread, static linked release. 
        Other configurations are indicated by optional suffixes:
       _dll for dll
       _debug for debug.
        Examples:
       heclib_vs8.lib (multi thread, static link, release)
       heclib_vs8_debug.lib (multi thread, static link, debug)
       heclib_vs8_dll_debug.lib (multi thread, dll link, debug)
       heclib_vs8_dll.lib (multi thread, dll link, release)




