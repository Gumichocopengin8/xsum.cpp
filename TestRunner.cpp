#include "TestRunner.h"

namespace Test {

namespace {

void isValid(double actual, double expected) {
    if (!std::isnan(actual) && (actual != expected || std::signbit(actual) != std::signbit(expected))) {
        throw std::format("Bad result: {}! Expected value: {}", actual, expected);
    }
    if (!std::isnan(actual) && std::isnan(expected)) {
        throw std::format("Bad result: {}! Expected value: {}", actual, expected);
    }
}

void sameValue(const std::vector<double> &vec, double expected) {
    XSUM::XsumSmall xsmall;
    xsmall.addv(vec);
    const auto xsmallRes = xsmall.computeRound();
    isValid(xsmallRes, expected);

    XSUM::XsumLarge xlarge;
    xlarge.addv(vec);
    const auto xlargeRes = xlarge.computeRound();
    isValid(xlargeRes, expected);

    XSUM::XsumAuto xauto;
    xauto.addv(vec);
    const auto xautoRes = xauto.computeRound();
    isValid(xautoRes, expected);
}

} // namespace

void runTest() {
    const double Infinity = std::numeric_limits<double>::infinity();
    const double NaN = std::numeric_limits<double>::quiet_NaN();

    try {
        sameValue({1, 2, 3}, 6);
        sameValue({1e308}, 1e308);
        sameValue({1e308, -1e308}, 0.0);
        sameValue({0.1}, 0.1);
        sameValue({0.1, 0.1}, 0.2);
        sameValue({0.1, -0.1}, 0.0);
        sameValue({1e308, 1e308, 0.1, 0.1, 1e30, 0.1, -1e30, -1e308, -1e308}, 0.30000000000000004);
        sameValue({1e20, 0.1, -1e20, 1e20, 0.1, -1e20, 1e20, 0.1, -1e20}, 0.30000000000000004);
        sameValue({1e30, 0.1, -1e30}, 0.1);

        // These cases are chosen for having exercised bugs in real implementations.
        // There is no other logic behind choice of constants.
        sameValue({8.98846567431158e+307, 8.988465674311579e+307, -1.7976931348623157e+308}, 9.9792015476736e+291);
        sameValue({-5.630637621603525e+255, 9.565271205476345e+307, 2.9937604643020797e+292}, 9.565271205476347e+307);
        sameValue({6.739986666787661e+66, 2, -1.2689709186578243e-116, 1.7046015739467354e+308, -9.979201547673601e+291,
                   6.160926733208294e+307, -3.179557053031852e+234, -7.027282978772846e+307, -0.7500000000000001},
                  1.61796594939028e+308);
        sameValue({0.31150493246968836, -8.988465674311582e+307, 1.8315037361673755e-270, -15.999999999999996,
                   2.9999999999999996, 7.345200721499384e+164, -2.033582473639399, -8.98846567431158e+307,
                   -3.5737295155405993e+292, 4.13894772383715e-124, -3.6111186457260667e-35, 2.387234887098013e+180,
                   7.645295562778372e-298, 3.395189016861822e-103, -2.6331611115768973e-149},
                  -Infinity);
        sameValue({-1.1442589134409902e+308, 9.593842098384855e+138, 4.494232837155791e+307, -1.3482698511467367e+308,
                   4.494232837155792e+307},
                  -1.5936821971565685e+308);
        sameValue({-1.1442589134409902e+308, 4.494232837155791e+307, -1.3482698511467367e+308, 4.494232837155792e+307},
                  -1.5936821971565687e+308);
        sameValue({9.593842098384855e+138, -6.948356297254111e+307, -1.3482698511467367e+308, 4.494232837155792e+307},
                  -1.5936821971565685e+308);
        sameValue({-2.534858246857893e+115, 8.988465674311579e+307, 8.98846567431158e+307}, 1.7976931348623157e+308);
        sameValue({1.3588124894186193e+308, 1.4803986201152006e+223, 6.741349255733684e+307}, Infinity);
        sameValue({6.741349255733684e+307, 1.7976931348623155e+308, -7.388327292663961e+41}, Infinity);
        sameValue({-1.9807040628566093e+28, 1.7976931348623157e+308, 9.9792015476736e+291}, 1.7976931348623157e+308);
        sameValue({-1.0214557991173964e+61, 1.7976931348623157e+308, 8.98846567431158e+307, -8.988465674311579e+307},
                  1.7976931348623157e+308);
        sameValue({1.7976931348623157e+308, 7.999999999999999, -1.908963895403937e-230, 1.6445950082320264e+292,
                   2.0734856707605806e+205},
                  Infinity);
        sameValue({6.197409167220438e-223, -9.979201547673601e+291, -1.7976931348623157e+308}, -Infinity);
        sameValue({4.49423283715579e+307, 8.944251746776101e+307, -0.0002441406250000001, 1.1752060710043817e+308,
                   4.940846717201632e+292, -1.6836699406454528e+308},
                  8.353845887521184e+307);
        sameValue({8.988465674311579e+307, 7.999999999999998, 7.029158107234023e-308, -2.2303483759420562e-172,
                   -1.7976931348623157e+308, -8.98846567431158e+307},
                  -1.7976931348623157e+308);
        sameValue({8.98846567431158e+307, 8.98846567431158e+307}, Infinity);

        sameValue({NaN}, NaN);
        sameValue({Infinity, -Infinity}, NaN);
        sameValue({-Infinity, Infinity}, NaN);

        sameValue({Infinity}, Infinity);
        sameValue({Infinity, Infinity}, Infinity);
        sameValue({-Infinity}, -Infinity);
        sameValue({-Infinity, -Infinity}, -Infinity);

        // https://gitlab.com/radfordneal/xsum/-/blob/master/api-doc?ref_type=heads#L203-L206
        // xsum wont return -0 (negative zero);
        sameValue({}, -0.0);
        sameValue({-0.0}, -0.0);
        sameValue({-0.0, -0.0}, -0.0);
        sameValue({-0.0, 0.0}, 0.0);
        sameValue({0.0}, 0.0);
    } catch (const std::string &ex) {
        std::cout << ex << std::endl;
    }
}

} // namespace Test
