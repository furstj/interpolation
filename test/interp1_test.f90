program interp1_test

  use, intrinsic :: iso_fortran_env, only: rkind => real64, error_unit
  use testdrive, only : run_testsuite, new_testsuite, testsuite_type, error_type, check

  implicit none

  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0
  
  testsuites = [ &
       new_testsuite("linear interpolation, scalar", collect_lin_scalar), &
       new_testsuite("linear interpolation, array", collect_lin_array), &
       new_testsuite("pchip interpolation, scalar", collect_pchip_scalar) &
       ]

  do is = 1, size(testsuites)
     write(error_unit, fmt) "Testing:", testsuites(is)%name
     call run_testsuite(testsuites(is)%collect, error_unit, stat)
  end do

  if (stat > 0) then
     write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
     error stop
  end if

  
contains

  !======================================================================
  ! linear interpolation of scalars
  !======================================================================
  
  ! Check linear interpolation of scalar data
  subroutine collect_lin_scalar(testsuite)
    use testdrive, only : new_unittest, unittest_type
    implicit none
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
         new_unittest("scalar interpolation", test_lin_interpolation), &
         new_unittest("scalar extrapolation", test_lin_extrapolation) &
         ]
  end subroutine collect_lin_scalar


  ! Tests linear interpolation of scalars
  subroutine test_lin_interpolation(error)
    use interpolation, only: interp1
    type(error_type), allocatable, intent(out) :: error
    real(rkind) :: x(3) = [ 1.0, 2.0, 4.0 ]
    real(rkind) :: y(3) = [ 5.0, 6.0, 4.0 ]

    call check(error, interp1(x, y, 1.5d0), 5.5d0)
    if (allocated(error)) return
    
    call check(error, interp1(x, y, 3.0d0), 5.0d0)
    if (allocated(error)) return
    
    call check(error, interp1(x, y, 3.0d0, "linear"), 5.0d0)
    if (allocated(error)) return
    
  end subroutine test_lin_interpolation


  ! Test linear extrapolation with scalar
  subroutine test_lin_extrapolation(error)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use interpolation, only: interp1
    type(error_type), allocatable, intent(out) :: error
    real(rkind) :: x(3) = [ 1.0, 2.0, 4.0 ]
    real(rkind) :: y(3) = [ 5.0, 6.0, 4.0 ]

    call check(error, ieee_is_nan(interp1(x, y, 0.5d0)))
    if (allocated(error)) return

    call check(error, ieee_is_nan(interp1(x, y, 4.5d0)))
    if (allocated(error)) return

    call check(error, interp1(x, y, 0.5d0, extrap="extrap"), 4.5d0)
    if (allocated(error)) return
    
    call check(error, interp1(x, y, 6.0d0, extrap="extrap"), 2.0d0)
    if (allocated(error)) return
    
  end subroutine test_lin_extrapolation



  !======================================================================
  ! linear interpolation of vectors (arrays of rank 1)
  !======================================================================

  ! Check linear interpolation of scalar data
  subroutine collect_lin_array(testsuite)
    use testdrive, only : new_unittest, unittest_type
    implicit none
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    
    testsuite = [ &
         new_unittest("array interpolation", test_lin_interpolation_array), &
         new_unittest("array extrapolation", test_lin_extrapolation_array) &
         ]
  end subroutine collect_lin_array

  
  ! Tests linear interpolation of vectors
  subroutine test_lin_interpolation_array(error)
    use interpolation, only: interp1
    type(error_type), allocatable, intent(out) :: error
    real(rkind) :: x(3)   = [ 1.0, 2.0, 4.0 ]
    real(rkind) :: y(2,3) = reshape([5.0, 7.0, 6.0, 8.0, 4.0, 7.0], [2,3])

    call check(error, all(abs(interp1(x, y, 1.5d0) - [5.5d0, 7.5d0]) < 1.e-6))
    if (allocated(error)) return

    call check(error, all(abs(interp1(x, y, 3.0d0) - [5.0d0, 7.5d0]) < 1.e-6))
    if (allocated(error)) return

    call check(error, all(abs(interp1(x, y, 3.0d0, "linear") - [5.0d0, 7.5d0]) < 1.e-6))
    if (allocated(error)) return

  end subroutine test_lin_interpolation_array


  ! Test linear extrapolation of vectors
  subroutine test_lin_extrapolation_array(error)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use interpolation, only: interp1
    type(error_type), allocatable, intent(out) :: error
    real(rkind) :: x(3)   = [ 1.0, 2.0, 4.0 ]
    real(rkind) :: y(2,3) = reshape([5.0, 7.0, 6.0, 8.0, 4.0, 7.0], [2,3])

    call check(error, all(ieee_is_nan(interp1(x, y, 0.5d0))))
    if (allocated(error)) return

    call check(error, all(ieee_is_nan(interp1(x, y, 4.5d0))))
    if (allocated(error)) return

    call check(error, all(abs(interp1(x, y, 0.5d0, extrap="extrap") - [4.5d0, 6.5d0]) < 1.e-6))
    if (allocated(error)) return

    call check(error, all(abs(interp1(x, y, 6.0d0, extrap="extrap") - [2.0d0, 6.0d0]) < 1.e-6))
    if (allocated(error)) return

  end subroutine test_lin_extrapolation_array


  !======================================================================
  ! pchip interpolation of scalars
  !======================================================================
  
  ! Check linear interpolation of scalar data
  subroutine collect_pchip_scalar(testsuite)
    use testdrive, only : new_unittest, unittest_type
    use interpolation
    implicit none
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    
    testsuite = [ &
         new_unittest("scalar interpolation", test_pchip_interpolation) &
         ]

  end subroutine collect_pchip_scalar


  ! Tests linear interpolation of vectors
  subroutine test_pchip_interpolation(error)
    use interpolation, only: interp1
    type(error_type), allocatable, intent(out) :: error
    real(rkind) :: x(7) = [ 1, 2, 3, 4, 5, 6, 7 ]
    real(rkind) :: y(7) = [ 0, 0, 0, 1, 2, 2, 2 ]

    call check(error, interp1(x, y, 1.5d0, method="pchip"), 0.0d0)
    if (allocated(error)) return

    call check(error, interp1(x, y, 3.2d0, method="pchip"), 7.2d-2)
    if (allocated(error)) return

    call check(error, interp1(x, y, 3.8d0, method="pchip"), 0.7680d0, thr=1.d-8)
    if (allocated(error)) return


  end subroutine test_pchip_interpolation


end program interp1_test
