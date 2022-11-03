project(basiclib)

set(src_string
    stringut.F90)
#set( src_math
#	mathut.F90 )
#set( src_math_str
#	mathut.F90 stringut.F90 )
#

add_library(${PROJECT_NAME} ${src_string})
add_library(basic ALIAS ${PROJECT_NAME})
