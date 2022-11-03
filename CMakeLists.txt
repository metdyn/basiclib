## project(basiclib)

set(src_string
    stringut.F90)
#set( src_math
#	mathut.F90 )
#set( src_math_str
#	mathut.F90 stringut.F90 )
#

add_library(basic ${src_string})
#add_library(basic ALIAS ${PROJECT_NAME})

# use make install here:
install (TARGETS basic
         LIBRARY DESTINATION  ${CMAKE_BINARY_DIR}/lib
	)
