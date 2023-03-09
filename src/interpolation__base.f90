module interpolation__base

  use iso_fortran_env, only: rkind => real64 

  implicit none
  private

  public :: rkind
  public :: find_interval
  
contains

  
  pure function find_interval(x, xi) result(i)
    real(rkind), intent(in) :: x(:)
    real(rkind), intent(in) :: xi
    integer :: i
    i = 1
    do while (i < size(x,1) - 1 .and. x(i+1) < xi)
       i = i + 1
    end do
  end function find_interval

  
end module interpolation__base
