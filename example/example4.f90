program example4

  use iso_fortran_env, only: real64
  use interpolation
  
  real(real64) :: x(5) = [1, 2, 3, 4, 5]
  real(real64) :: y(5) = [0, 1, 0, 1, 0]
  real(real64) :: xi(3), yi(3)
  
  xi = [1.5, 2.5, 6.5]
  yi = interp1(x, y, xi, extrap='extrap')
  
  print *, "Interpolated values are ", yi
end program example4

