call "%IFORT_COMPILER16%/bin/ifortvars.bat" ia32

set PATH="c:/Program Files/CMake/bin";%PATH%
set PATH=c:/cygwin/bin;%PATH%

set WZL_INSTALL_BASE="${CMAKE_CURRENT_SOURCE_DIR}/install"

set CC=cl
set CXX=cl
set FC=ifort