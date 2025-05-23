#pragma once

#include "Xsum.h"
#include <vector>

namespace Test {

namespace {

void isValid(double actual, double expected);

void sameValue(std::vector<double> &vec, double expected);

} // namespace

void runTest();

} // namespace Test
