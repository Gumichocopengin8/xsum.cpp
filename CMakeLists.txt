cmake_minimum_required(VERSION 4.0.1)
project(xsum VERSION 0.1.0 LANGUAGES C CXX)
set(CMAKE_CXX_STANDARD 23)

add_executable(
  xsum
  main.cpp
  Xsum.cpp
  Xsum.h
  XsumOriginal.c
  XsumOriginal.h
  TestRunner.cpp
  TestRunner.h
  BenchmarkRunner.h
)
