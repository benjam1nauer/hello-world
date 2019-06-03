MODULE KIN_Kinematics
USE Datatypes
USE Vector
USE fIO_FileIO
IMPLICIT NONE

PRIVATE

PUBLIC :: KIN_data
PUBLIC :: KIN_getTransformation
PUBLIC :: KIN_addMovement
PUBLIC :: KIN_setupKinematic
PUBLIC :: KIN_load
PUBLIC :: KIN_getNoIncrements

!> \\ brief Datentyp zum speichern der Kinematik
TYPE :: KIN_data
  PRIVATE ! Kann on aussen nicht direkt aufgerufen werden
  INTEGER, DIMENSION(20) :: moveType = 0 ! Art der Bewegung
  INTEGER, DIMENSION(20) :: movePol = 0 ! Art des Polynoms
  REAL(real_norm), DIMENSION(20,3) :: moveValue = 0.0D0 ! Wert fuer die Bewegung
  INTEGER :: nMovements = 0 ! Anzahl an Bewegungen
  REAL(real_norm) :: startPos = 0.0D0 ! Start der Bewegung
  REAL(real_norm) :: endPos = 0.0D0 ! Ende der Bewegung
  REAL(real_norm) :: inc = 0.0D0 ! Simulationsinkrement
END TYPE KIN_data

!> \\ brief Abruf der Transformationsmatrix
INTERFACE KIN_getTransformation
  MODULE PROCEDURE getTransformationAtT
  MODULE PROCEDURE getTransformationInc
END INTERFACE KIN_getTransformation

!> \\ brief Wieviele Inkremente muessen simuliert werden
INTERFACE KIN_getNoIncrements
  MODULE PROCEDURE getNoIncrements
END INTERFACE KIN_getNoIncrements

!> \\brief Interface, damit getMatrix mit unterschiedlichen Datentypen
!!         aufgerufen werden kann.
INTERFACE getMatrix
  MODULE PROCEDURE getMatrixDefault
  MODULE PROCEDURE getMatrixInt
  MODULE PROCEDURE getMatrixReal
END INTERFACE getMatrix

!> \\brief Bewegungen der Kinematik hinzufuegen
INTERFACE KIN_addMovement
  MODULE PROCEDURE addMovement1
  MODULE PROCEDURE addMovement2
  MODULE PROCEDURE addMovement3
END INTERFACE KIN_addMovement

!> \\ brief Legt die Start, Endwerte einer Kinematik fest
INTERFACE KIN_setupKinematic
  MODULE PROCEDURE setupKinematic
END INTERFACE KIN_setupKinematic

!> \\biref Liest eine Kinematikdatei ein
INTERFACE KIN_load
  MODULE PROCEDURE loadKinFormTXT
END INTERFACE KIN_load

CONTAINS
  
  !> \\ brief Ermoeglicht das Einlesen einer Kinematik aus einer Textdatei
  SUBROUTINE loadKinFormTXT(this, filename, ierr)
    TYPE(KIN_data), INTENT(INOUT) :: this
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT) :: ierr
    CHARACTER(char_norm), DIMENSION(:), ALLOCATABLE :: dataArray
    INTEGER :: iLine, nCols, pos, moveType
    REAL(real_norm) :: moveValue1, moveValue2, moveValue3
    
    ierr = 0
    ! Datei einlesen
    CALL fIO_load(dataArray, filename, ierr)
    
    ! Die Datei Zeile fuer Zeile durchgehen und die Werte in die Kinematik einfuegen
    readFile: DO iLine = 1, SIZE(dataArray)
      ! Anzahl an Spalten und damit Art des zeitlichen Verlaufs bestimmen
      nCols = fIO_getNoCols(dataArray(iLine), ' ')
      IF (nCols == 3) THEN
        ! Konstant
        READ(dataArray(iLine),*,IOSTAT=ierr) pos, moveType, moveValue1
        CALL KIN_addMovement(this, moveType, moveValue1, pos)
      ELSE IF (nCols == 4) THEN
        ! Linear
        READ(dataArray(iLine),*,IOSTAT=ierr) pos, moveType, moveValue1, moveValue2
        CALL KIN_addMovement(this, moveType, moveValue1, moveValue2, pos)
      ELSE IF (nCols == 5) THEN
        ! quadratisch
        READ(dataArray(iLine),*,IOSTAT=ierr) pos, moveType, moveValue1, moveValue2, moveValue3
        CALL KIN_addMovement(this, moveType, moveValue1, moveValue2, moveValue3, pos)
      END IF
    END DO readFile
  END SUBROUTINE loadKinFormTXT
  
  !> \\brief Dient zum setzen der Kinematischen Randbdeingungen
  SUBROUTINE setupKinematic(this, startPos, endPos, inc)
    TYPE(KIN_data), INTENT(INOUT) :: this
    REAL(real_norm), INTENT(IN) :: startPos
    REAL(real_norm), INTENT(IN) :: endPos
    REAL(real_norm), INTENT(IN) :: inc
    
    this%startPos = startPos
    this%endPos = endPos
    this%inc = inc
  END SUBROUTINE setupKinematic
  
  !> \\brief Erstellt eine konstante Bewegung
  !!
  SUBROUTINE addMovement1(this, moveType, moveValue, position)
    TYPE(KIN_data), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: moveType
    REAL(real_norm), INTENT(IN) :: moveValue
    INTEGER, OPTIONAL, INTENT(IN) :: position
    INTEGER :: pos
    
    pos = this%nMovements + 1
    IF (PRESENT(position)) THEN
      pos = position
    END IF
    
    this%moveType(pos) = moveType
    this%movePol(pos) = 1
    this%moveValue(pos,1) = moveValue
    this%nMovements = pos    
  END SUBROUTINE addMovement1
  
  !> \\brief Erstellt eine linear zeitlich veraenderliche Bewegung
  !!
  SUBROUTINE addMovement2(this, moveType, moveValue1, moveValue2, position)
    TYPE(KIN_data), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: moveType
    REAL(real_norm), INTENT(IN) :: moveValue1
    REAL(real_norm), INTENT(IN) :: moveValue2
    INTEGER, OPTIONAL, INTENT(IN) :: position
    INTEGER :: pos
    
    pos = this%nMovements + 1
    IF (PRESENT(position)) THEN
      pos = position
    END IF
    
    this%moveType(pos) = moveType
    this%movePol(pos) = 2
    this%moveValue(pos,1) = moveValue1
    this%moveValue(pos,2) = moveValue2
    this%nMovements = pos    
  END SUBROUTINE addMovement2
  
  !> \\brief Erstellt eine quadratisch mit der Zeit veraenderliche Bewegung
  !!
  SUBROUTINE addMovement3(this, moveType, moveValue1, moveValue2, moveValue3, position)
    TYPE(KIN_data), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: moveType
    REAL(real_norm), INTENT(IN) :: moveValue1
    REAL(real_norm), INTENT(IN) :: moveValue2
    REAL(real_norm), INTENT(IN) :: moveValue3
    INTEGER, OPTIONAL, INTENT(IN) :: position
    INTEGER :: pos
    
    pos = this%nMovements + 1
    IF (PRESENT(position)) THEN
      pos = position
    END IF
    
    this%moveType(pos) = moveType
    this%movePol(pos) = 3
    this%moveValue(pos,1) = moveValue1
    this%moveValue(pos,2) = moveValue2
    this%moveValue(pos,3) = moveValue3
    this%nMovements = pos    
  END SUBROUTINE addMovement3
  
  !> \\brief Liefert die Matrix zu einem bestimmten Simulationsinkrement zurueck
  !!
  FUNCTION getTransformationInc(this, position) RESULT(trafo)
    TYPE(KIN_data), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: position
    REAL(real_norm), DIMENSION(4,4) :: trafo
    REAL(real_norm) :: time
    time = this%startPos+REAL(this%inc)*(position-1)
    trafo =  getTransformationAtT(this, time)
  END FUNCTION getTransformationInc
  
  !> \\brief Liefert die Matrix zu einem bestimmten Zeitpunkt time
  !!
  FUNCTION getTransformationAtT(this, time) RESULT(trafo)
    TYPE(KIN_data), INTENT(IN) :: this
    REAL(real_norm), INTENT(IN) :: time
    REAL(real_norm), DIMENSION(4,4) :: trafo
    REAL(real_norm), DIMENSION(4,4) :: currentMove
    INTEGER :: i
    
    ! Einheitsmatrix abrufen, uebergeben werden zwei Integer
    trafo = getMatrix(0,0)
    
    DO i = 1, this%nMovements
      IF (this%movePol(i) == 1) THEN
        ! Bewegung ist ein konstanter Wert, uebergeben werden zwei Real
        currentMove = getMatrix(this%moveType(i),this%moveValue(i,1))
      ELSE IF (this%movePol(i) == 2) THEN
        ! Bewegung ist linear abhaengig von der Zeit
        currentMove = MATMUL(getMatrix(this%moveType(i),this%moveValue(i,1)), &
          getMatrix(this%moveType(i),this%moveValue(i,2)*time)) 
      ELSE IF (this%movePol(i) == 3) THEN
        ! Bewegung ist quadratisch abhaengig von der Zeit
        currentMove = getMatrix(this%moveType(i),this%moveValue(i,1)) * &
          getMatrix(this%moveType(i),this%moveValue(i,2)*time) * &
          getMatrix(this%moveType(i),this%moveValue(i,3)*time*time)
      END IF
      trafo =  MATMUL(currentMove,trafo)
    END DO
  END FUNCTION getTransformationAtT
    
  !> \\brief Liefert Bewegungsmatrizen oder die Einheitsmatrix zurueck
  !!
  FUNCTION getMatrixDefault(moveType, moveValue) RESULT(trafo)
    INTEGER, INTENT(IN) :: moveType
    REAL(real_norm), INTENT(IN) :: moveValue
    REAL(real_norm), DIMENSION(4,4) :: trafo
    REAL(real_norm) :: c, s, e, n
    
    trafo = 0.0D0
        
    c = COS(moveValue)
    s = SIN(moveValue)
    e = 1.0D0
    n = 0.0D0
    SELECT CASE (moveType)
      CASE(1)
        ! 4-Dim Rotationsmatrix um die x-Achse wenn moveType 1
        !   \\see https://en.wikipedia.org/wiki/Fortran_95_language_features#Arrays
        trafo = TRANSPOSE(RESHAPE((/ e, n, n, n, &
                                     n, c,-s, n, &
                                     n, s, c, n, &
                                     n, n, n, e /), SHAPE(trafo)))
      CASE(2)
        ! 4-Dim Rotationsmatrix um die y-Achse wenn moveType 2
        trafo = TRANSPOSE(RESHAPE((/ c, n, s, n, &
                                     n, e, n, n, &
                                    -s, n, c, n, &
                                     n, n, n, e /), SHAPE(trafo)))
      CASE(3)
        ! 4-Dim Rotationsmatrix um die z-Achse wenn moveType 3
        trafo = TRANSPOSE(RESHAPE((/ c,-s, n, n, &
                                     s, c, n, n, &
                                     n, n, e, n, &
                                     n, n, n, e /), SHAPE(trafo)))
      CASE(4)
        ! 4-Dim Translationsmatrix entlang der x-Achse wenn moveType 4
        c = moveValue
        trafo = TRANSPOSE(RESHAPE((/ e, n, n, c, &
                                     n, e, n, n, &
                                     n, n, e, n, &
                                     n, n, n, e /), SHAPE(trafo)))
      CASE(5)
        ! 4-Dim Translationsmatrix entlang der y-Achse wenn moveType 5
        c = moveValue
        trafo = TRANSPOSE(RESHAPE((/ e, n, n, n, &
                                     n, e, n, c, &
                                     n, n, e, n, &
                                     n, n, n, e /), SHAPE(trafo)))
      CASE(6)
          ! 4-Dim Translationsmatrix entlang der z-Achse wenn moveType 6
          c = moveValue
          trafo = TRANSPOSE(RESHAPE((/ e, n, n, n, &
                                       n, e, n, n, &
                                       n, n, e, c, &
                                       n, n, n, e /), SHAPE(trafo)))
      CASE DEFAULT
        ! Einheitsmatrix
        trafo = TRANSPOSE(RESHAPE((/ e, n, n, n, &
                                      n, e, n, n, &
                                      n, n, e, n, &
                                      n, n, n, e /), SHAPE(trafo)))
    END SELECT
  END FUNCTION getMatrixDefault
  
  !> \\brief getMatrix fuer Integer Eingangsgroessen
  FUNCTION getMatrixInt(moveType, moveValue) RESULT(trafo)
    INTEGER, INTENT(IN) :: moveType, moveValue
    REAL(real_norm), DIMENSION(4,4) :: trafo
    trafo = getMatrixDefault(moveType, REAL(moveValue,real_norm))
  END FUNCTION getMatrixInt
  
  !> \\brief getMatrix fuer Real Eingangsgroessen
  FUNCTION getMatrixReal(moveType, moveValue) RESULT(trafo)
    REAL(real_norm), INTENT(IN) :: moveType, moveValue
    REAL(real_norm), DIMENSION(4,4) :: trafo
    trafo = getMatrixDefault(INT(moveType), moveValue)
  END FUNCTION getMatrixReal
  
  !> \\brief getMatrix fuer Real Eingangsgroessen
  FUNCTION getNoIncrements(this) RESULT(number)
    TYPE(KIN_data), INTENT(IN) :: this
    INTEGER :: number
    
    number = 0
    number = (this%endPos - this%startPos)/REAL(this%inc)
  END FUNCTION getNoIncrements
    
END MODULE KIN_kinematics