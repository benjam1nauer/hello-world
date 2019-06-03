cmake_minimum_required(VERSION 2.8)
#
### Argumente:
## WZLGT_VERSION_BASE: Format: "2.4"
#     Basisversion fuer die Anwendung. Wird automatisch verwendet, wenn weder
#     die Umgebungsvariable "CMAKE_BUILD" noch "BUILD_NUMBER" gesetzt wird.
#     Die Variable "BUILD_NUMBER" wird automatisch vom Jenkins Server verwaltet
#     und sollte niemals manuell gesetzt werden. Ist die Umgebungsvariable
#     "CMAKE_BUILD" definiert werden keine automatischen Dateien mit Versions-
#     informationen geschrieben und des werden die Daten aus dem Quelldateien
#     selbst verwendet. Dies sollte nur fuer Release-Versionen benutzt werden,
#     die an den Endkunden verschickt werden sollen.
## CLUSTER_DEV: Format: "1"
#     Lokale Entwicklerversion. Wird immer dann verwendet, wenn keine der
#     Umgebungsvariablen "CMAKE_BUILD" oder "BUILD_NUMBER" explizit gesetzt
#     wurden.
## CLUSTER_JENKINS: Format: "2"
#     Versionsgruppe fuer die Jenkins Builds mit dem Intel Compiler. Wird
#     automatisch auf dem Jenkins Server verwendet. Die Versionsnummer der
#     Programme wird als "JI%BUILD_NUMBER%" eingetragen.
## CLUSTER_JENKINS_GCC: Format: "3"
#     Versionsgruppe fuer die Jenkins Builds mit dem GCC Compiler. Wird
#     automatisch auf dem Jenkins Server verwendet wenn unter MinGW uebersetzt
#     wird.  Die Versionsnummer der Programme wird als "JG%BUILD_NUMBER%"
#     eingetragen.
#
### Weitere Variablen:
## WZLGT_VERSION_PATH
#     Relativer Pfad zum Verzeichnis in dem die 'Version_svn.inc' gespeichert
#     werden soll. Ist der Wert der Variable nicht explizit gesetzt wird der
#     Ordner 'Source' als Ziel fuer diese Datei gesetzt.
## WZLGT_RESOURCE_PATH
#     Relativer Pfad zum Verzeichnis in dem die 'Version.h' mit den Infos zur
#     Versionsnummer der Resourcedatei gespeichert werden soll. Ist der Wert
#     der Variable nicht explizit gesetzt wird der Ordner 'ResourceFiles' als
#     Ziel fuer diese Datei gesetzt.
#     Wird der Wert dieser Variable auf "<none>" gesetzt, wird keine Datei
#     fuer die Resourcen erzeugt.
## WZLGT_VERSION_C_HEADER
#     Ist diese Variable gesetzt wird eine C-Headerdatei 'Version_svn.h' 
#     geschrieben statt einer Fortran 'Version_svn.inc' Datei.
#
macro(WRITE_WZLGT_VERSION WZLGT_VERSION_BASE 
  CLUSTER_DEV CLUSTER_JENKINS CLUSTER_JENKINS_GCC)
##--- --- WZLGT_VERSION start --- ---
#Setzen der WZL Versionsnummer.
#Bitte nur die Stellen mit der "### VERSION ###"-Markierung anpassen.
set(WZLGT_VERSION)
### VERSION ###
#set(WZLGT_VERSION_BASE "4.0")
if(NOT "$ENV{BUILD_NUMBER}" STREQUAL "")
  #
  # Buildnummer wird von Jenkins gesetzt.
  #
  #Versionsnummer der "Cluster" siehe auch in der Datei 'exe_versions.md'
  if(MINGW)
    set(WZLGT_VERSION "${WZLGT_VERSION_BASE}, JG$ENV{BUILD_NUMBER}")
    set(WZLGT_VERSION_CLUSTER_BASE ${CLUSTER_JENKINS_GCC})
  else()
    set(WZLGT_VERSION "${WZLGT_VERSION_BASE}, JI$ENV{BUILD_NUMBER}")
    set(WZLGT_VERSION_CLUSTER_BASE ${CLUSTER_JENKINS})
  endif(MINGW)
  set(WZLGT_VERSION_CLUSTER_BUILD "$ENV{BUILD_NUMBER}")
elseif (NOT "$ENV{CMAKE_BUILD}" STREQUAL "")
  #
  # WZL-interne Entwicklerversion
  #
  set(Versiontext "$ENV{CMAKE_BUILD}")
  STRING(FIND "${Versiontext}" "_" VERSION_Extra REVERSE)
  if(VERSION_Extra)
    STRING(SUBSTRING "${Versiontext}" 0 ${VERSION_Extra} Versiontext)
  endif(VERSION_Extra)
  STRING(REPLACE "M" "" Versiontext "${Versiontext}")

  #Versionsnummer der "Cluster" siehe auch in der Datei 'exe_versions.md'
  set(WZLGT_VERSION "${WZLGT_VERSION_BASE} SVN-Rev.$ENV{CMAKE_BUILD}")
  ### VERSION ###
  set(WZLGT_VERSION_CLUSTER_BASE ${CLUSTER_DEV})
  set(WZLGT_VERSION_CLUSTER_BUILD "${Versiontext}")
endif()

#Versions-Quellen anpassen.
if(WZLGT_VERSION)
  message("-- WZLGT_VERSION: ${WZLGT_VERSION}")
  if(MSVC)
    #  fuer Getriebekreis-Releases darf in set_vars diese Umgebungsvariable nicht gesetzt sein
    #  RC Compiler (werden aber scheinbar von CMAKE ignoriert)
    set(CMAKE_RC_FLAGS "${CMAKE_RC_FLAGS} -D CMAKE_BUILD")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /D CMAKE_BUILD")  
  elseif(MINGW)
    set(CMAKE_RC_FLAGS "${CMAKE_RC_FLAGS} -DCMAKE_BUILD")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DCMAKE_BUILD")
  endif(MSVC)

  #Vollstaendige Dateiversionsnummer= <major>.<minor>.<cluster>.<build>
  set(WZLGT_VERSION_CLUSTER "${WZLGT_VERSION_BASE}.${WZLGT_VERSION_CLUSTER_BASE}.${WZLGT_VERSION_CLUSTER_BUILD}")
  STRING(REPLACE "." "," WZLGT_VERSION_CLUSTER_RC "${WZLGT_VERSION_CLUSTER}")
  #TODO: Das Verzeichnis mit der "Version.inc" finden.
  if("${WZLGT_VERSION_PATH}" STREQUAL "")
    if(EXISTS "${CMAKE_SOURCE_DIR}/for/")
      set(WZLGT_VERSION_PATH "for")
    else()
      set(WZLGT_VERSION_PATH "Source")
    endif()
  endif()

  if(WZLGT_VERSION_C_HEADER)
    ## Ausgabe als C-Header mit #define
    #TODO: CMAke 3.0 kann "string(CONCAT"
    #string(CONCAT FILE_CONTENT
    #  "// GENERATED FILE. DO NOT EDIT!\n"
    #  "#ifndef BUILD_NUMBER\n" 
    #  "#define BUILD_NUMBER \"${WZLGT_VERSION}\"\n" 
    #  "#endif\n"
    #)
    set(FILE_CONTENT "// GENERATED FILE. DO NOT EDIT!
#ifndef BUILD_NUMBER
#define BUILD_NUMBER \"${WZLGT_VERSION}\"
#endif")
    FILE( WRITE ${WZLGT_VERSION_PATH}/Version_svn.h "${FILE_CONTENT}")
  else(WZLGT_VERSION_C_HEADER)
    ## Ausgabe als Fortran Include mit PARAMETER
    ##-----
    #TODO: CMAke 3.0 kann "string(CONCAT"
    #string(CONCAT FILE_CONTENT
    #  "! GENERATED FILE. DO NOT EDIT!\n" 
    #  "CHARACTER(LEN=*), PARAMETER :: BUILD_NUMBER = '${WZLGT_VERSION}'\n"
    #)
    set(FILE_CONTENT "! GENERATED FILE. DO NOT EDIT!
CHARACTER(LEN=*), PARAMETER :: BUILD_NUMBER = '${WZLGT_VERSION}'")
    FILE( WRITE ${WZLGT_VERSION_PATH}/Version_svn.inc "${FILE_CONTENT}")
  endif(WZLGT_VERSION_C_HEADER)

  #ResourceFiles
  if(NOT "${WZLGT_RESOURCE_PATH}" STREQUAL "<none>")

    if("${WZLGT_RESOURCE_PATH}" STREQUAL "")
      #TODO: Das Verzeichnis mit der "resource.h" finden.
      set(WZLGT_RESOURCE_PATH "ResourceFiles")
    endif()

    #TODO: CMAke 3.0 kann "string(CONCAT"
    FILE( WRITE ${WZLGT_RESOURCE_PATH}/Version.h 
"// GENERATED FILE. DO NOT EDIT!
#define BUILD_NUMBER ${WZLGT_VERSION_CLUSTER_RC}
#define BUILD_NUMBER_STR \"${WZLGT_VERSION_CLUSTER}\\0\"\n"
    )
  endif() #"${WZLGT_RESOURCE_PATH}" STREQUAL "<none>"
endif(WZLGT_VERSION)
##--- --- WZLGT_VERSION end --- ---
endmacro(WRITE_WZLGT_VERSION)
