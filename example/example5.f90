program example5

  use iso_fortran_env, only: real64
  use interpolation
  
  real(real64) :: x(5)   = [1, 2, 3, 4, 5]
  real(real64) :: y(2,5) = reshape([0.0, -0.1, 1.0, -1.1, 0.0, -0.1, 1.0, -1.1 , 0.0, -0.1], [2, 5])
  real(real64) :: xi(3), yi(2,3)
  
  xi = [1.5, 2.5, 6.5]
  yi = interp1(x, y, xi, method='pchip', extrap='extrap')
  
  print *, "Interpolated values are "
  do i = 1, 3
     print *, yi(:,i)
  end do
end program example5

