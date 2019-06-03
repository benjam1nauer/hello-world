get_filename_component(SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
list(APPEND CMAKE_MODULE_PATH ${SELF_DIR})
include(UseFortranPreProcessor)
include(WriteVersionResources)

if(WIN32)
  set(_library_dir bin) # .dll are in PATH, like executables
  set(PYTHON_MODULE_SUFFIX ".pyd")
else(WIN32)
  set(_library_dir lib) # .so and .dyn
  set(PYTHON_MODULE_SUFFIX ".so")
endif(WIN32)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin" CACHE INTERNAL 
  "Where to put the executables"
)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${_library_dir}" 
  CACHE INTERNAL "Where to put the libraries"
)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib" CACHE INTERNAL 
  "Where to put the archives"
)

set(WZL_DEFAULT_INSTALL_DESTINATIONS
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION ${_library_dir} 
  ARCHIVE DESTINATION lib
)

set(_library_dir) # unset variable

if(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_EXE_LINKER_FLAGS "-Wl,--no-undefined")
  set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--no-undefined")
endif(CMAKE_COMPILER_IS_GNUCXX)

option(QWIN "Grafik Unterstuetzung aktivieren.  Benoetigt QWin!" off)
option(WZL_STATIC_LINKING "Laufzeitbibliotheken des Compilers statisch linken!" on)
if(QWIN AND NOT WZL_STATIC_LINKING)
  message(FATAL_ERROR "QWIN kann nur mit WZL_STATIC_LINKING benutzt werden")
endif()

if(WIN32)
  if(MSVC)    
    if(WZL_STATIC_LINKING)
      set(_laufzeit_debug "/MTd")
      set(_laufzeit_release "/MT")
    else()
      set(_laufzeit_debug "/MDd")
      set(_laufzeit_release "/MD")
    endif()
    set(CMAKE_CXX_FLAGS_DEBUG "/D_DEBUG /Z7  /Ob0 /Od /RTC1 ${_laufzeit_debug}")
    set(CMAKE_CXX_FLAGS_RELEASE " /O2 /Ob2 /D NDEBUG ${_laufzeit_release}")
    set(CMAKE_C_FLAGS_DEBUG "/D_DEBUG /Z7  /Ob0 /Od /RTC1 ${_laufzeit_debug}")
    set(CMAKE_C_FLAGS_RELEASE "/O2 /Ob2 /D NDEBUG ${_laufzeit_release}")
  endif(MSVC)
  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS_DEBUG "/Od /debug:full /traceback /dbglibs /warn:uninitialized /warn:unused /warn:declarations /debug-parameters /check:bounds /stand:f08")   
    set(CMAKE_Fortran_FLAGS_RELEASE "/O3 /Oy- /D NDEBUG")

	# Workaround fuer in Plantform/windows-{df/ifort}.cmake fest eingetragenes  Linker-Flag "INCREMENTAL:YES" das "/traceback" unbrauchbar macht.  
	# TODO: Bugreport
    IF (CMAKE_EXE_LINKER_FLAGS MATCHES "/INCREMENTAL:YES")	  
      STRING (REGEX REPLACE "/INCREMENTAL:YES" "" CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS})
    ENDIF()
	
    if(QWIN)
      set(CMAKE_Fortran_FLAGS "/W1 /nologo /fpp /libs:qwin /threads")        
    elseif(WZL_STATIC_LINKING)
      set(CMAKE_Fortran_FLAGS "/W1 /nologo /fpp /libs:static /threads")  
    else()
      set(CMAKE_Fortran_FLAGS "/W1 /nologo /fpp /libs:dll /threads")      
    endif()
  endif()
endif()

include(${SELF_DIR}/doxygen_functions.cmake)