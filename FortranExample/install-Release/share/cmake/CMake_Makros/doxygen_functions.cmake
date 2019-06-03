# Maybe useful for C++ application and module writers, thus a reusable function
function(generate_doxygen_documentation _target_name _doxygen_config_file)
  find_package(Doxygen)
  if(NOT DOXYGEN_FOUND )
    message(STATUS "Doxygen not found - Doxygen based docs won't be generated")
    return()
  endif()

  if(NOT EXISTS ${_doxygen_config_file})
    message( STATUS "Doxygen configuration file not found - Doxygen based docs won't be generated")
    return()
  endif()

  add_custom_target(${_target_name}
    COMMAND ${DOXYGEN_EXECUTABLE} "${DOXYGEN_CONFIG_FILE}"
  )
endfunction(generate_doxygen_documentation)