#include "XsumSmall.h"
#include <iostream>
#include <span>
#include <vector>
#include "TestRunner.h"

int main() {
    std::vector<double> vec{1e20, 0.1, -1e20, 1e20, 0.1, -1e20, 1e20, 0.1, -1e20};
    XSUM::XsumSmall xsmall;
    xsmall.addv(vec);
    std::cout << xsmall.computeRound() << std::endl;
    Test::runTest();
    return 0;
}
