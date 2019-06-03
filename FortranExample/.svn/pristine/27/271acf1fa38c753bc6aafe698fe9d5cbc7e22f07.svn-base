!> \\brief Dieses MODULE stellt die Datenstruktur sowie einige Basisrechenoperationen
!          fuer einen 4-Dimensionalen Vektor zur verfuegung. Diese Vektoren werden genutzt
!          um Translationen und Rotationen in einem Schritt durchzufueren. Punkte haben als
!          vierte Diemension eine 1 und Normalen eine 0. Damit werden Transformationen nur auf
!          auf Punkte und nicht auf Normalen durchgefuehrt. Transformationen sind die vierte
!          Spalte in einer 4x4 Kinematikmatrix.
! \\todo Zukuenftig sollten die mathematischen Operatoren Skalarprodukt (.dot.) und 
!          Kreuzprodukt (.cross.) hinzugefuegt werden.
MODULE Vector
USE Datatypes

IMPLICIT NONE

! PRIVATE Bedeutet, dass man nicht von ausserhalb auf Funktionen und Datentypen
!   des Moduls zugreifen kann...
PRIVATE
! ... ausser auf Dinge die in PUBLIC defineirt sind. Fuer diese gibt es dann unten
!   ein INTERFACE. Das ist zwar nicht notwendig, erleichtert aber das Erweitern der
!   Programme ungemein und ist deutlich uebersichtlicher.
!   \\hint Das wird bisher nur sehr selten gemacht :(
PUBLIC :: line4
PUBLIC :: vector4
PUBLIC :: allocateLine
PUBLIC :: deallocateLine
PUBLIC :: vectorLength
PUBLIC :: calcLineNormals
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: OPERATOR(+)
PUBLIC :: OPERATOR(-)
PUBLIC :: OPERATOR(*)
PUBLIC :: OPERATOR(/)

!> \\brief Vektor mit vier Dimensionen
TYPE :: vector4
  REAL(real_norm), DIMENSION(4) :: v4
END TYPE vector4

!> \\brief Linie aus Vektoren
TYPE :: line4
  TYPE(vector4), DIMENSION(:), ALLOCATABLE :: points
  TYPE(vector4), DIMENSION(:), ALLOCATABLE :: normals
  INTEGER, DIMENSION(:), ALLOCATABLE :: indx
  INTEGER :: nElements
END TYPE line4

!> \\brief Allokieren einer Linie
INTERFACE allocateLine
  MODULE PROCEDURE allocateLine4
END INTERFACE allocateLine

!> \\brief Deallokieren einer Linie
INTERFACE deallocateLine
  MODULE PROCEDURE deallocateLine4
END INTERFACE deallocateLine

!> \\brief Laenge eines Vektors
INTERFACE vectorLength
  MODULE PROCEDURE vectorLength4
END INTERFACE vectorLength

!> \\brief Ermittelt die Normalen einer line4
INTERFACE calcLineNormals
  MODULE PROCEDURE calcLineNormals4
END INTERFACE calcLineNormals
  
! Da die Struktur vector eine selbst definierter Datentyp ist (und evtl. nicht der beste ;) )
!   muessen saemtliche Rechenoperationen dafuer auch selber definiert werden. Dies
!   funktioniert ueber ASSIGNMENT und OPERATOR mit welchem die mathematischen Operatoren
!   ueberladen werden.
!   \\see https://courses.physics.illinois.edu/phys466/sp2010/comp_info/overload.html
!   \\see https://www.ibm.com/developerworks/community/blogs/b10932b4-0edd-4e61-89f2-6e478ccba9aa/entry/object_oriented_fortran_does_fortran_support_operator_overloading55?lang=en
!
!> \\brief Kopieren der Datenstrukturen ermoeglichen
INTERFACE ASSIGNMENT (=)
  MODULE PROCEDURE assignLine4
  MODULE PROCEDURE assignVector4
  MODULE PROCEDURE assignVector4Real
  MODULE PROCEDURE assignVector4Int
END INTERFACE ASSIGNMENT (=)

!> \\brief Addieren von Vektoren
INTERFACE OPERATOR (+)
  MODULE PROCEDURE plusVector4
  MODULE PROCEDURE plusVector4Real
  MODULE PROCEDURE plusVector4Int
END INTERFACE OPERATOR (+)

!> \\brief Subtrahieren von Vektoren
INTERFACE OPERATOR (-)
  MODULE PROCEDURE minusVector4
  MODULE PROCEDURE minusVector4Real
  MODULE PROCEDURE minusVector4Int
  MODULE PROCEDURE minusVector4Change
END INTERFACE OPERATOR (-)

!> \\brief Multiplikation von Vektoren
INTERFACE OPERATOR (*)
  MODULE PROCEDURE multiplyVector4
  MODULE PROCEDURE multiplyVector4Real
  MODULE PROCEDURE multiplyVector4Real2
  MODULE PROCEDURE multiplyArrayLine4
END INTERFACE OPERATOR (*)
  
!> \\brief Division von Vektoren
INTERFACE OPERATOR (/)
  MODULE PROCEDURE divideVector4
  MODULE PROCEDURE divideVector4Real
END INTERFACE OPERATOR (/)

CONTAINS
  
  !> \\brief Deallokieren der Datenstruktur
  !!
  SUBROUTINE deallocateLine4(line)
    TYPE(line4), INTENT(INOUT) :: line
    
    line%nElements = 0
    ! Wenn man etwas deallokieren will, dann muss es auch allkoiert sein
    !   Ueberpruefen ob allkokiert, wenn ja deallokieren
    !   Solch kurze IF Abfragen ruhig in eine Zeile
    IF (ALLOCATED(line%points)) DEALLOCATE(line%points)
    IF (ALLOCATED(line%normals)) DEALLOCATE(line%normals)
    IF (ALLOCATED(line%indx)) DEALLOCATE(line%indx)
  END SUBROUTINE deallocateLine4
  
  !> \\brief Allokieren der Datenstruktur
  !!
  SUBROUTINE allocateLine4(line, nValues)
    TYPE(line4), INTENT(INOUT) :: line
    INTEGER, INTENT(IN) :: nValues
    
    ! Bevor man etwas allokiert muss sichergestellt werden, dass es deallokiert ist
    CALL deallocateLine4(line)
    ! Nun allkoieren    
    ALLOCATE(line%points(nValues))
    ALLOCATE(line%normals(nValues))
    ALLOCATE(line%indx(nValues))
    
    ! Initialisieren der Struktur
    line%points(:)%v4(1) = 0.0D0
    line%points(:)%v4(2) = 0.0D0
    line%points(:)%v4(3) = 0.0D0
    line%points(:)%v4(4) = 1.0D0
    line%normals(:)%v4(1) = 0.0D0
    line%normals(:)%v4(2) = 0.0D0
    line%normals(:)%v4(3) = 0.0D0
    line%normals(:)%v4(4) = 0.0D0
    line%indx(:) = 0
    line%nElements = 0
  END SUBROUTINE allocateLine4

  !> \\brief Zuweisen einer Linie
  !!
  SUBROUTINE assignLine4(lineOut, lineIn)
    TYPE(line4), INTENT(INOUT) :: lineOut
    TYPE(line4), INTENT(IN) :: lineIn
    
    IF (SIZE(lineOut%points) < lineIn%nElements) THEN
      CALL allocateLine(lineOut, lineIn%nElements)
    END IF
    
    lineOut%nElements = lineIn%nElements
    ! Wie in matlab kann man mit : auf alle Elemente zugreifen oder mit (n:m) auf einen Teil
    !   array(10:) wuerde zum Beispiel auf alle Elemente von 10 bis zum Ende zugreifen
    lineOut%points(1:lineIn%nElements) = lineIn%points(:)
    lineOut%normals(1:lineIn%nElements) = lineIn%normals(:)
    lineOut%indx(1:lineIn%nElements) = lineIn%indx
  END SUBROUTINE assignLine4
  
  !> \\brief Zuweisen eines Vektors
  !!
  ! Eine ELEMENTAL SUBROUTINE oder auch FUNCTION hat keine Seiteneffekte (side effects).
  !   Solche Routinen kann man besonders einfach parallelisieren, also ruhig nutzen.
  !   \\see http://fortranwiki.org/fortran/show/elemental
  ELEMENTAL SUBROUTINE assignVector4(v1, v2)
    TYPE(vector4), INTENT(INOUT) :: v1
    TYPE(vector4), INTENT(IN) :: v2
  
    v1%v4(1:4) = V2%v4(1:4)
  END SUBROUTINE assignVector4

  !> \\brief Weist einem Vektor einen Real Wert zu
  !!
  ELEMENTAL SUBROUTINE assignVector4Real(v1, rValue)
    TYPE(vector4), INTENT(INOUT) :: v1
    REAL(real_norm), INTENT(IN) :: rValue
    
    v1%v4(1:4) = rValue
  END SUBROUTINE assignVector4Real
  
  !> Weist einem Vektor einen Integer Wert zu
  !!
  ELEMENTAL SUBROUTINE assignVector4Int(v1, iValue)
    TYPE(vector4), INTENT(INOUT) :: v1
    INTEGER, INTENT(IN) :: iValue
    
    v1%v4(1:4) = REAL(iValue)
  END SUBROUTINE assignVector4Int
  
  !> \\brief Weist einem Vektor einen Integer Wert zu
  !!
  ELEMENTAL FUNCTION vectorLength4(v1) RESULT(length)
    TYPE(vector4), INTENT(IN) :: v1
    REAL(real_norm) :: length
    
    length = SQRT(v1%v4(1)**2+v1%v4(2)**2+v1%v4(3)**2)
    
  END FUNCTION vectorLength4
  
  !> \\brief Addiert zwei 4D-Vektoren
  !!
  ELEMENTAL FUNCTION plusVector4(v1, v2) RESULT(sum)
    TYPE(vector4), INTENT(IN) :: v1, v2
    TYPE(vector4) :: sum
    
    sum%v4(1:4) = v1%v4(1:4) + v2%v4(1:4)
  END FUNCTION plusVector4
  
  !> \\brief Addiert einen 4D-Vektor und einen Real Wert
  !!
  ELEMENTAL FUNCTION plusVector4Real(v1, rValue) RESULT(sum)
    TYPE(vector4), INTENT(IN) :: v1
    REAL(real_norm), INTENT(IN) :: rValue
    TYPE(vector4) :: sum
    
    sum%v4(1:4) = v1%v4(1:4) + rValue
  END FUNCTION plusVector4Real
  
  !> \\brief Addiert einen 4D-Vektor und einen Integer Wert
  !!
  ELEMENTAL FUNCTION plusVector4Int(v1, ivalue) RESULT(sum)
    TYPE(vector4), INTENT(IN) :: v1
    INTEGER, INTENT(IN) :: iValue
    TYPE(vector4) :: sum
    
    sum%v4(1:4) = v1%v4(1:4) + iValue
  END FUNCTION plusVector4Int
  
  !> \\brief Subtrahiert zwei 4D-Vektoren
  !!
  ELEMENTAL FUNCTION minusVector4(v1, v2) RESULT(sum)
    TYPE(vector4), INTENT(IN) :: v1, v2
    TYPE(vector4) :: sum
    
    sum%v4(1:4) = v1%v4(1:4) - v2%v4(1:4)
  END FUNCTION minusVector4
  
  !> \\brief Subtrahiert einen 4D-Vektor und einen Real Wert
  !!
  ELEMENTAL FUNCTION minusVector4Real(v1, rValue) RESULT(sum)
    TYPE(vector4), INTENT(IN) :: v1
    REAL(real_norm), INTENT(IN) :: rValue
    TYPE(vector4) :: sum
    
    sum%v4(1:4) = v1%v4(1:4) - rValue
  END FUNCTION minusVector4Real
  
  !> \\brief Subtrahiert einen 4D-Vektor und einen Integer Wert
  !!
  ELEMENTAL FUNCTION minusVector4Int(v1, ivalue) RESULT(sum)
    TYPE(vector4), INTENT(IN) :: v1
    INTEGER, INTENT(IN) :: iValue
    TYPE(vector4) :: sum
    
    sum%v4(1:4) = v1%v4(1:4) - iValue
  END FUNCTION minusVector4Int
  
  ELEMENTAL FUNCTION minusVector4Change (v) RESULT(sum)
    TYPE(vector4), INTENT(IN) :: v
    TYPE(vector4) :: sum
    
    sum%v4 = -v%v4
  END FUNCTION minusVector4Change
 
  !> \\brief Multipliziert Skalar und 4D-Vektor
  !!
  ELEMENTAL FUNCTION multiplyVector4Real(skalar, v1) RESULT(product)
    TYPE(vector4), INTENT(IN) :: v1
    REAL(real_norm), INTENT(IN) :: skalar
    TYPE(vector4) :: product
    !
    product%v4(1:4) = v1%v4(1:4)*skalar
  END FUNCTION multiplyVector4Real
  
  !> \\brief  Multipliziert 4D-Vektor und einem Skalar
  !!
  ELEMENTAL FUNCTION multiplyVector4Real2(v1, skalar) RESULT(product)
    TYPE(vector4), INTENT(IN) :: v1
    REAL(real_norm), INTENT(IN) :: skalar
    TYPE(vector4) :: product
    
    product%v4(1:4) = v1%v4(1:4)*skalar
  END FUNCTION multiplyVector4Real2
  
  !> \\brief  Multipliziert zwei 4D-Vektoren
  !!
  ELEMENTAL FUNCTION multiplyVector4(v1, v2) RESULT(product)
    TYPE(vector4), INTENT(IN) :: v1, v2
    TYPE(vector4) :: product
    
    product%v4(1:4) = v1%v4(1:4)*v2%v4(1:4)
  END FUNCTION multiplyVector4
  
  !> \\brief  Multipliziert ein 4x4 Array mit einer line4
  !!
  FUNCTION multiplyArrayLine4(m, line) RESULT(resultline)
    REAL(real_norm), DIMENSION(4,4), INTENT(IN) :: m
    TYPE(line4), INTENT(IN) :: line
    TYPE(line4) :: resultline
    INTEGER :: i
    
    CALL allocateLine(resultline, line%nElements)
    
    DO i = 1, line%nElements
      resultline%points(i)%v4(1:4) = MATMUL(m,line%points(i)%v4)
      resultline%normals(i)%v4(1:4) = MATMUL(m,line%normals(i)%v4)
    END DO
    resultline%indx(1:line%nElements) = line%indx(1:line%nElements)
    resultline%nElements = line%nElements
  END FUNCTION multiplyArrayLine4
  
  !> \\brief Dividiert einen 4D-Vektor durch ein Skalar
  !!
  ELEMENTAL FUNCTION divideVector4Real(v1, skalar) RESULT(product)
    TYPE(vector4), INTENT(IN) :: v1
    REAL(real_norm), INTENT(IN) :: skalar
    TYPE(vector4) :: product
    
    product%v4(1:4) = v1%v4(1:4)/skalar
  END FUNCTION divideVector4Real
  
  !> \\brief  Dividiert zwei 4D-Vektoren
  !!
  ELEMENTAL FUNCTION divideVector4(v1, v2) RESULT(product)
    TYPE(vector4), INTENT(IN) :: v1, v2
    TYPE(vector4) :: product
    
    product%v4(1:4) = v1%v4(1:4)/v2%v4(1:4)
  END FUNCTION divideVector4
  
  !> \\brief Berechnet die Normalen zu einer gegebenen Linie
  !!
  ! \\todo aktuell wird die Z-Komponente als Null gesetzt
  SUBROUTINE calcLineNormals4(line)
    TYPE(line4), INTENT(INOUT) :: line
    INTEGER :: i
    
    line%normals(1)%v4(1) = line%points(2)%v4(2)-line%points(1)%v4(2)
    line%normals(1)%v4(2) = line%points(1)%v4(1)-line%points(2)%v4(1)
    line%normals(1)%v4(3) = 0.0D0
    
    DO i = 2, line%nElements-1
      line%normals(i)%v4(1) = 0.5D0*(line%points(i)%v4(2)-line%points(i-1)%v4(2))! + &
        !line%points(i+1)%v4(2)-line%points(i)%v4(2))
      line%normals(i)%v4(2) = 0.5D0*(line%points(i-1)%v4(1)-line%points(i)%v4(1))! + &
        !line%points(i+1)%v4(1)-line%points(i)%v4(1))
    END DO
    
    line%normals(line%nElements)%v4(1) = line%points(line%nElements)%v4(2)-&
      line%points(line%nElements-1)%v4(2)
    line%normals(line%nElements)%v4(2) = line%points(line%nElements-1)%v4(1)-&
      line%points(line%nElements)%v4(1)
    line%normals(line%nElements)%v4(3) = 0.0D0
    
    ! Am Ende dafuer sorgen, dass die Normalen auch wirklich eine Laenge von 1 haben
    DO i = 1, line%nElements
      line%normals(i) = line%normals(i)/vectorLength(line%normals(i))
    END DO
    
  END SUBROUTINE calcLineNormals4
    
END MODULE Vector