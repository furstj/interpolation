module interpolation
   !! Library of Octave/MATLAB like interpolations
   !!
   !! The library supports following interpolation routine:
   !!
   !! - `yi = inerp1(x, y, xi, method, extrap`
   !!

   use, intrinsic :: iso_fortran_env, only: rkind => real64

   implicit none
   private

   interface interp1
      !! Generic interface to `interp1` function
      !!
      !! The `interp` function interpolates given table of values to
      !! given query points. The generic interface is
      !!
      !!     yi = interp1(x, y, xi, method, extrap)
      !!
      !! where
      !!
      !! - `x` is rank 1 array of sample points in ascending order
      !! - `y` is rank 1 or 2 array of values, for rank 2 array with elements
      !!   `y(i,j)` the index `i` corresponds to vector component and `j` is the
      !!   index of sample point, i.e. `size(x) == size(y,2)`
      !! - `xi` is scalar or rank 1 array of query points
      !! - `method` is an optional specification of interpolation method. It can be either
      !!   'linear', or 'pchip' (same as 'cubic'), default is 'linear'
      !!     - 'linear' does piecewise linear interpolation
      !!     - 'pchip' does smooth piecewise cubic interpolation
      !! - `extrap` is an optional parameter specifying the handling of query points
      !!   outside of sample point range. The parameter accepts
      !!     - 'NaN' values beyond the endpoints are set to NaN (**default**)
      !!     - 'extrap' values beyond the endpoints are extrapolated
      !!
      !! The result `yi` is either
      !!
      !! - single interpolated value if rank(y) == 1 and xi is scalar
      !! - rank 1 array of interpolated values if rank(y) == 1 and rank(xi) == 1,
      !!   in that case size(yi) == size(xi)
      !! - rank 1 array of interpolated values if rank(y) == 2 and xi is scalar
      !!   in that case size(yi) == size(y,1)
      !! - rank 2 array of interpolated values if rank(y) == 2 and rank(xi) == 1,
      !!   in that case size(yi,1) == size(y,1) and size(yi,2) == size(xi)
      !!
      module procedure interp1_rank00
      module procedure interp1_rank01
      module procedure interp1_rank10
      module procedure interp1_rank11
   end interface interp1


   type interp1_t
      !! Class implementing scalar interpolations
      !!
      !! The class implements interpolation of scalar quantity
      !! given at discrete points. It is a wraper to `interp`` function.
      !!
      !! @note
      !! The class supports only interpolation of scalars! For vectors
      !! use directly `interp1` function.

      real(rkind), allocatable :: x(:)         !! array of sample points
      real(rkind), allocatable :: y(:)         !! array of values in sample points
      character(len=:), allocatable :: method  !! interpolation method, @see interp1
      character(len=:), allocatable :: extrap  !! extrapolation method, @see interp1
   contains
      procedure :: at => interp1_t_at          !! returns interpolated value at given point
   end type interp1_t

   interface interp1_t
      module procedure create_interp1_t
   end interface interp1_t


   public :: interp1, interp1_t


contains

   !======================================================================
   ! class interp1_t
   !======================================================================
   function create_interp1_t(x, y, method, extrap) result(i1)
      real(rkind), intent(in)  :: x(:)
      real(rkind), intent(in)  :: y(size(x))
      character(len=*), intent(in), optional :: method
      character(len=*), intent(in), optional :: extrap
      type(interp1_t) :: i1

      i1%x = x
      i1%y = y
      if (present(method)) then
         i1%method = method
      else
         i1%method = "linear"
      end if

      if (present(extrap)) then
         i1%extrap = extrap
      else
         i1%extrap = "NaN"
      end if
   end function create_interp1_t


   pure function interp1_t_at(self, xi) result(yi)
      class(interp1_t), intent(in) :: self
      real(rkind), intent(in)      :: xi
      real(rkind)                  :: yi
      yi = interp1(self%x, self%y, xi, self%method, self%extrap)
   end function interp1_t_at


   !======================================================================
   ! function interp1
   !======================================================================

   pure function interp1_rank00(x, y, xi, method, extrap) result(yi)
      real(rkind), intent(in) :: x(:)
      real(rkind), intent(in) :: y(size(x))
      real(rkind), intent(in) :: xi
      character(len=*), intent(in), optional :: method
      character(len=*), intent(in), optional :: extrap
      real(rkind)             :: yi
      real(rkind)             :: yarray(1,1)

      yarray = interp1_dispatch(x, reshape(y,[1,size(x)]), [xi], method, extrap)
      yi = yarray(1,1)
   end function interp1_rank00


   pure function interp1_rank01(x, y, xi, method, extrap) result(yi)
      real(rkind), intent(in) :: x(:)
      real(rkind), intent(in) :: y(size(x))
      real(rkind), intent(in) :: xi(:)
      character(len=*), intent(in), optional :: method
      character(len=*), intent(in), optional :: extrap
      real(rkind)             :: yi(size(xi))
      real(rkind)             :: yarray(1,size(xi))

      yarray = interp1_dispatch(x, reshape(y,[1,size(x)]), xi, method, extrap)
      yi(:) = yarray(1,:)
   end function interp1_rank01


   pure function interp1_rank10(x, y, xi, method, extrap) result(yi)
      real(rkind), intent(in) :: x(:)
      real(rkind), intent(in) :: y(:,:)
      real(rkind), intent(in) :: xi
      character(len=*), intent(in), optional :: method
      character(len=*), intent(in), optional :: extrap
      real(rkind)             :: yi(size(y,1))
      real(rkind)             :: yarray(size(y,1),1)

      yarray = interp1_dispatch(x, y, [xi], method, extrap)
      yi(:) = yarray(:,1)
   end function interp1_rank10


   pure function interp1_rank11(x, y, xi, method, extrap) result(yi)
      use stdlib_optval, only: optval
      real(rkind), intent(in) :: x(:)
      real(rkind), intent(in) :: y(:,:)
      real(rkind), intent(in) :: xi(:)
      character(len=*), intent(in), optional :: method
      character(len=*), intent(in), optional :: extrap
      real(rkind)             :: yi(size(y,1),size(xi))
      real(rkind)             :: yarray(size(y,1),size(xi))

      yarray = interp1_dispatch(x, y, xi, method, extrap)
      yi(:,:) = yarray(:,:)
   end function interp1_rank11


   !======================================================================
   ! IMPLEMENTATION
   !======================================================================

   pure function interp1_dispatch(x, y, xi, method, extrap) result(yi)
      use stdlib_optval, only: optval
      real(rkind), intent(in) :: x(:)
      real(rkind), intent(in) :: y(:,:)
      real(rkind), intent(in) :: xi(:)
      character(len=*), intent(in), optional :: method
      character(len=*), intent(in), optional :: extrap
      real(rkind)             :: yi(size(y,1),size(xi))
      real(rkind)             :: yarray(size(y,1),size(xi))

      select case( optval(method, "linear") )

       case("linear")
         call interpolate_linearly(yarray, x, y, xi, optval(extrap, "NaN"))

       case("pchip", "cubic")
         call interpolate_pchip(yarray, x, y, xi, optval(extrap, "NaN"))

       case default
         error stop "interp1 supports only method='linear', 'pchip', or 'cubic'"

      end select

      yi(:,:) = yarray(:,:)
   end function interp1_dispatch


   pure subroutine interpolate_linearly(yi, x, y, xi, extrap)
      ! Calculates linear interpolation with given extrapolation method.
      ! The method is either 'extrap' or 'NaN'
      use stdlib_error, only: error_stop
      use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
      real(rkind), intent(out) :: yi(:,:)
      real(rkind), intent(in)  :: x(:)
      real(rkind), intent(in)  :: y(:,:)
      real(rkind), intent(in)  :: xi(:)
      character(len=*), intent(in) :: extrap

      integer :: i, k
      do k = 1, size(xi)
         if ((x(1) <= xi(k) .and. xi(k) <= x(size(x))) .or. extrap == "extrap") then
            i = find_interval(x, xi(k))
            yi(:,k) = y(:,i) + (xi(k) - x(i))/(x(i+1) - x(i))*(y(:,i+1) - y(:,i))
         elseif (extrap == "NaN") then
            yi(:,k) = ieee_value(1.0, ieee_quiet_nan)
         else
            error stop "interp1 supports only extrap='extrap' or 'NaN'"
         end if
      end do
   end subroutine interpolate_linearly


   pure subroutine interpolate_pchip(yi, x, y, xi, extrap)
      ! Calculates piecewise cubic interpolation with given extrapolation method.
      ! The method is either 'extrap' or 'NaN'
      use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
      real(rkind), intent(out) :: yi(:,:)
      real(rkind), intent(in)  :: x(:)
      real(rkind), intent(in)  :: y(:,:)
      real(rkind), intent(in)  :: xi(:)
      character(len=*), intent(in) :: extrap
      real(rkind)  :: h(0:size(x)), fpl(size(y,1)), fpr(size(y,1)), tl, tr
      integer :: i, k, l

      h(0) = 0
      h(1:size(x)-1) = abs(x(2:size(x)) - x(1:size(x)-1))
      h(size(x)) = 0

      do k = 1, size(xi)
         if ((x(1) <= xi(k) .and. xi(k) <= x(size(x))) .or. extrap == "extrap") then
            i = find_interval(x, xi(k))
            fpl(:) = (y(:,i+1) - y(:,i))/h(i)
            fpr(:) = fpl(:)
            if (h(i-1) > 0) then   ! limit fpl
               do l = 1, size(fpl)
                  fpl(l) = harmonic_mean((y(l,i) - y(l,i-1))/h(i-1), fpl(l), h(i-1), h(i))
               end do
            end if
            if (h(i+1) > 0) then   ! limit fpr
               do l = 1, size(fpr)
                  fpr(l) = harmonic_mean(fpr(l), (y(l,i+2) - y(l,i+1))/h(i+1), h(i), h(i+1))
               end do
            end if

            tl = (x(i+1) - xi(k))/h(i)
            tr = (xi(k) - x(i))/h(i)

            yi(:,k) = y(:,i)*(3*tl**2 - 2*tl**3) + y(:,i+1)*(3*tr**2 - 2*tr**3) &
               - fpl(:)*h(i)*(tl**3 - tl**2) + fpr(:)*h(i)*(tr**3 - tr**2)
         elseif (extrap == "NaN") then
            yi(:,k) = ieee_value(1.0, ieee_quiet_nan)
         else
            error stop "interp1 supports only extrap='extrap' or 'NaN'"
         end if
      end do

   contains

      elemental function harmonic_mean(da, db, ha, hb) result(c)
         !! Calculates harmonic mean of two numbers
         !! da and db are the derivatives at the two points, ha and hb are the step sizes
         real(rkind), intent(in) :: da, db
         real(rkind), intent(in) :: ha, hb
         real(rkind)             :: c
         if (da*db <= 0) then
            c = 0
         else
            c = 3*(ha + hb)/( (2*hb + ha)/da + (2*ha + hb)/db)
         end if
      end function harmonic_mean

   end subroutine interpolate_pchip


   !======================================================================
   ! private support functions
   !======================================================================

   pure function find_interval(x, xi) result(i)
      !! Finds starting index of interval in which xi is located
      !! Assumes that x is sorted in ascending order
      real(rkind), intent(in) :: x(:)
      real(rkind), intent(in) :: xi
      integer :: i
      integer :: low, high, mid
      low = 1
      high = size(x)
      do while (high - low > 1)
         mid = (high + low)/2
         if (xi < x(mid)) then
            high = mid
         else
            low = mid
         end if
      end do
      i = low
   end function find_interval


end module interpolation
