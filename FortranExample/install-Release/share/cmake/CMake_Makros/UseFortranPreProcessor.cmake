MACRO(PREPROCESS_FORTRAN_TARGET_SOURCES _target)
  GET_TARGET_PROPERTY(_target_flags ${_target} COMPILE_FLAGS)
  IF(_target_flags STREQUAL "_target_flags-NOTFOUND")
    SET(_target_flags)
  ENDIF()
  
  IF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")     
    SET_TARGET_PROPERTIES( ${_target}
      PROPERTIES COMPILE_FLAGS "${_target_flags} -cpp"
    )   
  ENDIF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  
  IF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    SET_TARGET_PROPERTIES( ${_target}
      PROPERTIES COMPILE_FLAGS "${_target_flags} -fpp"
    )
  ENDIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  
  IF(CMAKE_Fortran_COMPILER_ID STREQUAL "SunPro")
    SET_TARGET_PROPERTIES( ${_target}
      PROPERTIES COMPILE_FLAGS "${_target_flags} -fpp"
    )
  ENDIF(CMAKE_Fortran_COMPILER_ID STREQUAL "SunPro")  
ENDMACRO(PREPROCESS_FORTRAN_TARGET_SOURCES)


