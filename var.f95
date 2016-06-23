      module var
      implicit none
      integer :: i, j, k, nd
      integer :: imax, jmax, wst, wen
      integer :: nphys, ntags
      integer :: file_type, data_size

      real :: version, z = 0.0

      character (len=40) :: ifilename, ofilename

      double precision, dimension(:,:), allocatable :: x,y
      end module var
