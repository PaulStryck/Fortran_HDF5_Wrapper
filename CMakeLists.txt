cmake_minimum_required (VERSION 3.8.0)

project(BufferedCallback)

enable_language(Fortran)

# find_package(MPI  REQUIRED)

# include_directories(${MPI_Fortran_INCLUDE_PATH})
# add_compile_options(${MPI_Fortran_COMPILE_FLAGS})


find_package(HDF5 REQUIRED COMPONENTS Fortran HL Fortran_HL)
include_directories(${HDF5_INCLUDE_DIRS})
add_compile_options(${HDF5_DEFINITIONS})

file(GLOB SOURCES src/*.f90)

add_executable(main ${SOURCES})
# target_link_libraries(main ${MPI_Fortran_LIBRARIES})
target_link_libraries(main ${HDF5_LIBRARIES} ${HDF5_HL_LIBRARIES})
# set_property(TARGET main APPEND_STRING PROPERTY LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
set_property(TARGET main APPEND_STRING PROPERTY LINK_FLAGS )
