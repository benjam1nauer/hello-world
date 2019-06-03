MODULE Datatypes
IMPLICIT NONE
! Laenge von CHARACTERS ueber einen Parameter definieren, dann kann
!   man es schnell ueberall im Program aendern
INTEGER, PARAMETER :: char_norm = 255
INTEGER, PARAMETER :: real_norm = 8
  
! Pi ist auch eine konstante die man unbedingt benoetigt
REAL(real_norm), PARAMETER :: pi = 3.14159265358979D0
! Und die Umrechnung von Grad in Rad und vice versa
REAL(real_norm), PARAMETER :: rad2deg = REAL(180.0D0)/pi
REAL(real_norm), PARAMETER :: deg2rad = pi/REAL(180.0D0)  
END MODULE Datatypes  