SUBROUTINE penetration(simData, kinData, ierr)
  USE Datatypes
  USE CC_ContourCalculation
  USE KIN_kinematics
  USE Vector
  USE fIO_FileIO
  
  TYPE(CC_data) :: simData
  TYPE(KIN_data) :: kinData
  INTEGER, INTENT(OUT) :: ierr
  REAL(real_norm), DIMENSION(4,4) :: trafo
  TYPE(line4) :: tool, gear
  INTEGER :: noOfIntersections, iInc
  ! Zum Steuern von Debuggingausgaben waehrend des Testens
  LOGICAL :: DEBUG = .FALSE.
  REAL(real_norm), DIMENSION(:), ALLOCATABLE :: thickness
  REAL(real_norm) :: area
  
  ierr = 0
  
  ! Werkzeug auslesen
  CALL CC_getContour(simData, tool, 'rigid')
  ! Ausgabe wenn DEBUG ist .TRUE.
  IF (DEBUG) CALL fIO_write(tool, 'wzg_0.txt', ierr)
  ! Werkstueck auslesen und ausgeben
  CALL CC_getContour(simData, gear, 'soft')
  IF (DEBUG) CALL fIO_write(gear, 'wst_0.txt', ierr)
  
  ! Schliefe ueber die einzelnen Simualtionsinkremente
  !   Wenn diese Schleife fuer ganz viele Ebenen durchlaufen wird, erhaelt man
  !   eine vollstaendige 3D Durchdringungsrechnung
  simInc: DO iInc = 1, KIN_getNoIncrements(kinData)
    ! Abruf der transformationsmatrix
    trafo = KIN_getTransformation(kinData, iInc)
    ! Uebergeben des transformierten Werkzeugs an die Durchdringungsrechnung
    CALL CC_setContour(simData, trafo*tool, 'rigid')
    ! Durchdringungsrechnung durchfuehren
    CALL CC_cutContour(simData, noOfIntersections, ierr)
    ! Wenn ein Fehler aufgetreten ist Abbruch
    IF (ierr /= 0) RETURN
    ! Neue Kontur des Werkstuecks auslesen
    CALL CC_getContour(simData, gear, 'soft')
    ! Wenn eine Durchdringung stattgefunden hat, dann di Dicke auslesen
    IF (noOfIntersections > 0) CALL CC_getDifference(simData, thickness)
    ! Gleiches fuer die Flache der Durchdringung
    IF (noOfIntersections > 0) CALL CC_getDifference(simData, area)
    ! Evtl Ausgabe des Werkstuecks in form einer txt Datei
    IF (DEBUG) CALL fIO_write(gear, 'wst_i.txt', ierr)
  END DO simInc
  
END SUBROUTINE penetration