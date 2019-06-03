cmake_minimum_required(VERSION 2.6)
project(FortranExample Fortran)

find_package(CMake_Makros REQUIRED)

set(Quellen
  Source/FortranExample.f90
  Source/CC_ContourCalculation.f90
  Source/Datatypes.f90
  Source/fIO_FileIO.f90
  Source/KIN_Kinematics.f90
  Source/penetration.f90
  Source/STE_File.f90
  Source/Vector.f90
)

add_executable(${PROJECT_NAME} ${Quellen})