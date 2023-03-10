# interpolation

[![ubuntu with gfortran](https://github.com/furstj/interpolation/actions/workflows/test_gfortran_ubuntu.yml/badge.svg)](https://github.com/furstj/interpolation/actions/workflows/test_gfortran_ubuntu.yml)
[![msys windows gfortran](https://github.com/furstj/interpolation/actions/workflows/test_gfortran_msys_windows.yml/badge.svg)](https://github.com/furstj/interpolation/actions/workflows/test_gfortran_msys_windows.yml)
[![macos with gfortran](https://github.com/furstj/interpolation/actions/workflows/test_gfortran_macos.yml/badge.svg)](https://github.com/furstj/interpolation/actions/workflows/test_gfortran_macos.yml)

The library implements `interp1` function similar to Octave/MATLAB.

## Example program

```fortran
program example1

  use iso_fortran_env, only: real64
  use interpolation
  
  real(real64) :: x(5) = [1, 2, 3, 4, 5]
  real(real64) :: y(5) = [0, 1, 0, 1, 0]
  real(real64) :: xi, yi
  
  xi = 1.5
  yi = interp1(x, y, xi, method="pchip")
  
  print *, "Interpolated value is ", yi

end program example1
```

More examples can be found in `example` folder.

## Use in FPM project
Add the library as a dependency
```
[dependencies]
interpolation = { git = "https://github.com/furstj/interpolation" }
```

## API documentation
You can find the automatically [published documentation here](https://furstj.github.io/interpolation/).

[![Build and Deploy Documentation](https://github.com/furstj/interpolation/actions/workflows/ford.yaml/badge.svg)](https://github.com/furstj/interpolation/actions/workflows/ford.yaml)
