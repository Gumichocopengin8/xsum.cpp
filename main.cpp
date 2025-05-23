#include "TestRunner.h"
#include "XsumSmall.h"
#include <iostream>
#include <span>
#include <vector>

int main() {
    std::vector<double> vec{1e20, 0.1, -1e20, 1e20, 0.1, -1e20, 1e20, 0.1, -1e20};
    XSUM::XsumSmall xsmall;
    xsmall.addv(vec);
    std::cout << "XsumSmall: " << xsmall.computeRound() << std::endl;
    XSUM::XsumLarge xlarge;
    xlarge.addv(vec);
    std::cout << "XsumLarge: " << xlarge.computeRound() << std::endl;
    XSUM::XsumAuto xauto;
    xauto.addv(vec);
    std::cout << "XsumAuto: " << xauto.computeRound() << std::endl;
    Test::runTest();
    return 0;
}
