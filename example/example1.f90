program example1

  use iso_fortran_env, only: real64
  use interpolation
  
  real(real64) :: x(5) = [1, 2, 3, 4, 5]
  real(real64) :: y(5) = [0, 1, 0, 1, 0]
  real(real64) :: xi, yi
  
  xi = 1.5
  yi = interp1(x, y, xi)
  
  print *, "Interpolated value is ", yi

end program example1
