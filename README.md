# StrathE2E2
R package containing Strath End-to-end ecosystem model.

## Prerequisites

StrathE2E2 runs on the following platforms:

* Mac OS X
* Linux (64-bit)
* Windows (64-bit, Windows 10 recommended)

For Windows/Mac OS X there are [binary distributions](https://github.com/strathmarine/StrathE2E2/releases) available.
For Linux you must build from source.

If the binary versions do not match your R installation then building from source is required.

For your R installation, you need a reasonably up to date version of R, plus some additional packages:

* R 3.5 or later
* Package deSolve
* Package NetIndices
* Package devtools

Package 'devtools' makes installing packages directly from 'github' very easy.

## Installing StrathE2E2 binary package

For Windows/Mac OS X, download a binary release and use the menu option "Packages->Install from local zip file" to install it.

You can also use devtools to install it directly:
```
devtools::install_url("strathmarine/StrathE2E2/releases/latest.zip", auth_token="bad4074bb7f13c00bee44b580ffd92e1d646ec28")
```

## Building StrathE2E2 from source package on Linux

For Linux use 'devtools' to install the package:
```
devtools::install_github("strathmarine/StrathE2E2", auth_token="bad4074bb7f13c00bee44b580ffd92e1d646ec28")
```
This will install the StrathE2E2 package in your local user R library.

## Building StrathE2E2 from source package on Windows

For Windows you must install 'Rtools' first. Download the 'Rtools' package from https://cran.r-project.org/bin/windows/Rtools/ selecting a download suitable for your R installation version.

Run the 'Rtools' installer which will install in the folder 'C:\Rtools'. 

Now use the same 'devtools' command to install the package from github:
```
devtools::install_github("strathmarine/StrathE2E2", auth_token="bad4074bb7f13c00bee44b580ffd92e1d646ec28")
```

## Getting started

At R prompt do:

```
library(StrathE2E2)
model<-read_model("NorthSea")
results<-StrathE2E(model)
plot_full_length_timeseries(results)
```

## Building StrathE2E2 on Mac OS X

TODO


