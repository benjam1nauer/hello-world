if(NOT DEFINED SPIRIT2X_ROOT)
  if(NOT $ENV{SPIRIT2X_ROOT} STREQUAL "")
    set(SPIRIT2X_ROOT $ENV{SPIRIT2X_ROOT})   
  endif(NOT $ENV{SPIRIT2X_ROOT} STREQUAL "")
endif(NOT DEFINED SPIRIT2X_ROOT)

find_path(SPIRIT2X_INCLUDE_DIR 
  NAMES boost/spirit/home/qi/reference.hpp
  HINTS ${SPIRIT2X_ROOT}
)

include(FindPackageMessage)
find_package_message(Spirit2x "SPIRIT2X_INCLUDE_DIR: ${SPIRIT2X_INCLUDE_DIR}"
  "[${SPIRIT2X_INCLUDE_DIR}]")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Spirit2x DEFAULT_MSG 
  SPIRIT2X_ROOT
  SPIRIT2X_INCLUDE_DIR
)

MARK_AS_ADVANCED(
  SPIRIT2X_ROOT
  SPIRIT2X_INCLUDE_DIR
)
