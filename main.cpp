#include "BenchmarkRunner.h"
#include "TestRunner.h"
#include "Xsum.h"
#include <iostream>
#include <span>
#include <vector>

int main() {
    // Example
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

    // Test
    Test::runTest();

    // Benchmark
    // XsumSmall
    std::cout << "-------XsumSmall-------" << std::endl;
    Benchmark::runXsumSmallBenchmarkWithAddV();
    Benchmark::runXsumSmallBenchmarkWithAdd1();

    // XsumLarge
    std::cout << "-------XsumLarge-------" << std::endl;
    Benchmark::runXsumLargeBenchmarkWithAddV();
    Benchmark::runXsumLargeBenchmarkWithAdd1();

    // XsumAuto
    std::cout << "-------XsumAuto-------" << std::endl;
    Benchmark::runXsumAutoBenchmarkWithAddV();
    Benchmark::runXsumAutoBenchmarkWithAdd1();

    // XsumOriginal
    std::cout << "-------XsumOriginal(Small)-------" << std::endl;
    Benchmark::runXsumOriginalSmallBenchmarkWithAddv();
    Benchmark::runXsumOriginalSmallBenchmarkWithAdd1();
    std::cout << "-------XsumOriginal(Large)-------" << std::endl;
    Benchmark::runXsumOriginalLargeBenchmarkWithAddv();
    Benchmark::runXsumOriginalLargeBenchmarkWithAdd1();
    return 0;
}
