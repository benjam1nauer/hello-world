!> \\title FortranExampel
!!
!! Bitte die readme.md Datei im Ordner 00_Readme lesen.
!!
!! Allgemeine Hilfe bei Fortran Befehlen \\see https://software.intel.com/en-us/node/691996
!!
!! Und immer dran denken: https://xkcd.com/1513/ sowie http://phdcomics.com/comics.php?f=1690
!!
PROGRAM FortranExample
  USE Datatypes
  USE Vector
  USE CC_ContourCalculation
  USE fIO_fileIO
  USE STE_file
  USE KIN_kinematics

  IMPLICIT NONE
  
  ! Lokale Variablen
  INTEGER :: ierr
  TYPE(line4) :: gear, tool
  TYPE(STE_data) :: controlData
  TYPE(CC_data) :: simData
  TYPE(KIN_data) :: kinData
  REAL(real_norm) :: inc
  INTEGER, DIMENSION(100) :: simControl
  INTEGER :: i
  
  ierr = 0
  
  !# Steuerdatei einlesen
  CALL STE_load(controlData, 'control.ste', ierr)
  
  !#
  !#  Aufbau des Simulationsablaufs
  !#
  simControl = 0
  SELECT CASE(controlData%processType)
    CASE(1)
      CALL addSimulation(simControl, 1) ! Werkstueck einlesen
      CALL addSimulation(simControl, 2) ! Werkzeg einlesen
      CALL addSimulation(simControl, 3) ! Kinematik einlesen
      CALL addSimulation(simControl, 4) ! Durchdringungsrechnung
      CALL addSimulation(simControl, 99) ! Ausgabe der Kennwerte
    CASE(2)
      CALL addSimulation(simControl, 1) ! Werkstueck einlesen
      CALL addSimulation(simControl, 2) ! Werkzeg einlesen
      CALL addSimulation(simControl, 5) ! Kinematik des Abwaelzens aufbauen
      CALL addSimulation(simControl, 4) ! Durchdringungsrechnung
      CALL addSimulation(simControl, 99) ! Ausgabe der Kennwerte
    CASE DEFAULT
      ! In diesem Fall passiert nichts
      CONTINUE
  END SELECT
  
  !#
  !#  Simulationsablauf
  !#
  simulationControl: DO i = 1, SIZE(simControl)
    SELECT CASE(simControl(i))
    CASE(1)
      !# Werkstueck einlesen
      ! Richtig schoenes Programmieren wuerde so auf die den Dateinamen zugreifen
      CALL fIO_load(gear, getFilenameGear(controlData), ierr)
    CASE(2)
      !# Werkzeug einlesen
      ! Aber weil das echt etwas umstaendlich ist, kann man es auch so machen.
      !   Damit das funktioniert, darf in der TYPE Dekalaration kein PRIVATE stehen.
      CALL fIO_load(tool, controlData%filenameTool, ierr)
      ! Wenn die Laenge des ersten Vektors Null ist, dann Normalen bestimmen
      IF (vectorLength(tool%normals(1)) < 0.001) CALL calcLineNormals(tool)
    CASE(3)
      !# Kinematik einlesen
      CALL KIN_load(kinData, controlData%filenameKinematic, ierr)
      CALL KIN_setupKinematic(kinData, controlData%startPosition, controlData%endPosition, inc)
    CASE(4)
      !# Durchdringungsrechnung
      ! Die eingelesenen Linien fuer die Durchdringungsrechnung abspeichern
      CALL CC_setContour(simData, gear, 'soft')
      CALL CC_setContour(simData, tool, 'rigid')
      ! Die Durchdringungsrechnung durchfuehren
      CALL penetration(simData, kinData, ierr)
    CASE(5)
      !# Aufbau einer einfachen Abwaelzkinematik   
      ! Werkzeug um 180 Grad drehen
      CALL KIN_addMovement(kinData, 3, pi)
      ! Werkzeug auf Achsabstand verfahren
      CALL KIN_addMovement(kinData, 5, controlData%centerDistance)
      ! Werkzeug entlang des Teilkreisradius verfahren
      CALL KIN_addMovement(kinData, 4, 0.0D0, 0.5D0*controlData%pitchDia)
      ! Rotation des Werkzeugs um die Werkstueckdrehachse
      CALL KIN_addMovement(kinData, 3, 0.0D0, 1.0D0)
      ! Randbedingungen der Simulation vorgeben
      ! Inkrement zwischen zwei Simulationsschritten bestimmen
      inc = (controlData%endPosition - controlData%startPosition)/REAL(controlData%noIncrements)
      ! Alles in der Kinematik abspeichern
      CALL KIN_setupKinematic(kinData, controlData%startPosition, controlData%endPosition, inc)
    CASE(99)
      ! Ausgaben
      CALL CC_getContour(simData, gear, 'soft')
      CALL fIO_write(gear, 'gear_end.txt', ierr)
    CASE DEFAULT
      CONTINUE    
    END SELECT
  END DO simulationControl
  
CONTAINS
  
    !> \\brief Fuegt eine Simulation ans Ende der Simulationskette an
    !          Optional kann eine Position in der Simulationskette uebergeben werden
    SUBROUTINE addSimulation(simControl, simType, position)
      INTEGER, DIMENSION(:), INTENT(INOUT) :: simControl
      INTEGER, INTENT(IN) :: simType
      INTEGER, OPTIONAL, INTENT(IN) :: position
      INTEGER :: i
      ! Wenn der optionale Paramter psoition uebergeben wurde...
      IF (PRESENT(position)) THEN
        ! ... und dieser kleiner als die gesamte Anzahl an Simulationen ist ...
        IF (position <= SIZE(simControl)) THEN
          ! ... dann wird die entsprechende Simulation ueberschrieben
          simControl(position) = simType
          ! und es wird zurueckgesprungen
          RETURN
        END IF
      END IF
      
      ! Ansonsten wird das Feld durchlaufen bis eine freie Position
      !   fuer die Simualtion gefunden wurde
      DO i = 1, SIZE(simControl)
        IF (simControl(i) == 0) THEN
          simControl(i) = simType
          RETURN
        END IF
      END DO
    END SUBROUTINE addSimulation
  
END PROGRAM FortranExample
