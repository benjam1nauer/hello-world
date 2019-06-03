SET(BLAS_LIBRARY_DIR "c:/Programme/Intel/MKL721/ia32/lib" 
  CACHE PATH 
  "Ordner in dem die mkl Bibliotheken liegen"
)
  
SET(BLAS_LIBRARIES )

find_library(BLAS_MKL_C_LIBRARY
  NAMES mkl_c
  PATHS ${BLAS_LIBRARY_DIR} )  
if(BLAS_MKL_C_LIBRARY STREQUAL "BLAS_MKL_C_LIBRARY-NOTFOUND")
  IF(MKL_BLAS_FIND_REQUIRED)
    message(FATAL_ERROR "mkl_c nicht gefunden")
  ELSE(MKL_BLAS_FIND_REQUIRED)
    message(STATUS "mkl_c nicht gefunden")
  ENDIF(MKL_BLAS_FIND_REQUIRED)
endif(BLAS_MKL_C_LIBRARY STREQUAL "BLAS_MKL_C_LIBRARY-NOTFOUND")
    
list(APPEND BLAS_LIBRARIES ${BLAS_MKL_C_LIBRARY})    

  
find_library(BLAS_MKL_IA32_LIBRARY
  NAMES mkl_ia32
  PATHS ${BLAS_LIBRARY_DIR} )  
if(BLAS_MKL_IA32_LIBRARY STREQUAL "BLAS_MKL_IA32_LIBRARY-NOTFOUND")
  IF(MKL_BLAS_FIND_REQUIRED)
    message(FATAL_ERROR "mkl_ia32 nicht gefunden")
  ELSE(MKL_BLAS_FIND_REQUIRED)
    message(STATUS "mkl_ia32 nicht gefunden")    
  ENDIF(MKL_BLAS_FIND_REQUIRED)
endif(BLAS_MKL_IA32_LIBRARY STREQUAL "BLAS_MKL_IA32_LIBRARY-NOTFOUND")    
  
list(APPEND BLAS_LIBRARIES ${BLAS_MKL_IA32_LIBRARY})

