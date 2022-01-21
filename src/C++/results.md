# Results of the Comparison of Multiple Imputations using Stochastic Regression 

All results where calculated on an AMD Ryzen 7 3700X CPU with 8 cores and 16 threads under Ubuntu 20.04.03 in WSL in Windows 10.

## C++

The code was written following a modern C++20 standard and compiled with `g++`. Parallelization is applied using `std::thread`.

### No additional compiler optimization
```
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ g++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++2a -lpthread 
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ ./stochastic_regression.out
(Serial) Runtime = 3024950 [µs]
(Serial) Runtime = 3.02495 [s]
(Serial) Mean = 12.8487

Running in parallel on 16 core(s).
(Parallel) Runtime = 410583 [µs]
(Parallel) Runtime = 0.410583 [s]
(Parallel) Mean = 12.8487
```

### Level 1 compiler optimization
```
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ g++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++2a -lpthread -O1
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ ./stochastic_regression.out
(Serial) Runtime = 280657 [µs]
(Serial) Runtime = 0.280657 [s]
(Serial) Mean = 12.8494

Running in parallel on 16 core(s).
(Parallel) Runtime = 49459 [µs]
(Parallel) Runtime = 0.049459 [s]
(Parallel) Mean = 12.8479
```

### Level 2 compiler optimization

```
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ g++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++2a -lpthread -O2
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ ./stochastic_regression.out
(Serial) Runtime = 254457 [µs]
(Serial) Runtime = 0.254457 [s]
(Serial) Mean = 12.849

Running in parallel on 16 core(s).
(Parallel) Runtime = 48597 [µs]
(Parallel) Runtime = 0.048597 [s]
(Parallel) Mean = 12.8485
```

### Level 3 (highest) compiler optimization
```
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ g++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++2a -lpthread -O3
joshuasimon@DESKTOP-LTOR84H:~/github/Parallelization-Of-MI-Algorithms/src/C++$ ./stochastic_regression.out
(Serial) Runtime = 251478 [µs]
(Serial) Runtime = 0.251478 [s]
(Serial) Mean = 12.8485

Running in parallel on 16 core(s).
(Parallel) Runtime = 47040 [µs]
(Parallel) Runtime = 0.04704 [s]
(Parallel) Mean = 12.8486
```

## R
R Version 4.1.2 was used. Parallelization is applied via the `foreach` package using a `FORK` backend.
```
> source("/home/joshuasimon/github/Parallelization-Of-MI-Algorithms/src/C++/stochastic_regression.R", encoding = "UTF-8")
[1] "(Serial) Runtime: 11.9761893749237 seconds."
[1] "(Parallel) Runtime: 1.76731586456299 seconds."
[1] "(Serial) mean: 12.7614942086748"
[1] "(Parallel) mean: 12.7436226006538"
```