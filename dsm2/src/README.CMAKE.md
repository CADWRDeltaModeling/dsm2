#CMake Instructions

Create a build directory BUILD under dsm2
```
mkdir BUILD
cd BUILD
```

First setup path 
```
"c:\Program Files (x86)\Intel\Composer XE 2013\bin\compilervars.bat" ia32 vs2008
```

Next execute for VS2008 the cmake command
```
cmake -G "Visual Studio 9 2008" ..\src
```

Finally open the DSM2.sln file in VS 2008 and compile

or compile from command line with this command
```
cmake --build . --target hydro --config Debug
```