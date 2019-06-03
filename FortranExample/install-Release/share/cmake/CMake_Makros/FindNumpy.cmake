# Try to find numarray python package
# Once done this will define
#
# NUMPY_FOUND        - system has numarray development package and it should be used
# NUMPY_INCLUDE_DIR  - directory where the arrayobject.h header file can be found
#
#

    FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/det_npp.py "try: import numpy; print numpy.get_include()\nexcept: pass\n")
    EXEC_PROGRAM("${PYTHON_EXECUTABLE}"
      ARGS "\"${CMAKE_CURRENT_BINARY_DIR}/det_npp.py\""
      OUTPUT_VARIABLE NUMPY_PATH
    )
  FIND_PATH(NUMPY_INCLUDE_DIR numpy/arrayobject.h
    "${NUMPY_PATH}"
    DOC "Directory where the arrayobject.h header file can be found. This file is part of the numarray package"
    )
  #message(FATAL_ERROR "${NUMPY_INCLUDE_DIR}")

  IF(NUMPY_INCLUDE_DIR)
    SET (NUMPY_FOUND 1 CACHE INTERNAL "Python numarray development package is available")
  ENDIF(NUMPY_INCLUDE_DIR)

