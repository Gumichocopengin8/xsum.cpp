# XSum C++

This is the C++ implementation of [xsum](https://arxiv.org/abs/1505.05571) which is
fast exact summation using small and large superaccumulators.

This code include test cases in [TestRunner.cpp](./TestRunner.cpp).

This code is based on [this commit](https://gitlab.com/radfordneal/xsum/-/commit/b766b3e104cdbd6769c8df033fb430700fce32dd)
from Radford M. Neal's xsum repository.

## How to Run

- Make sure that `cmake` is installed
- In the root dir, run `cmake --build ./build --config Release --target all -j 12 --`
- Then run `./build/xsum`

## Note

- xsum wont return `-0` (negative zero), however, this handle `-0`. See test cases in [TestRunner.cpp](./TestRunner.cpp).
  - https://gitlab.com/radfordneal/xsum/-/blob/master/api-doc?ref_type=heads#L203-L206

## References

- https://arxiv.org/abs/1505.05571
- https://gitlab.com/radfordneal/xsum
- https://github.com/tc39/proposal-math-sum
