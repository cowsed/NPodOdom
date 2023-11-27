cmake_minimum_required(VERSION 3.5)

project(NPodOdom LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_compile_options(-fsanitize=undefined)
add_link_options(-fsanitize=undefined)


add_executable(NPodOdom main.cpp
    Vec.h
    Mat.h
    odometry.h odometry.cpp)

include(GNUInstallDirs)
install(TARGETS NPodOdom
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
