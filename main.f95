      module var
      implicit none
      integer :: i, j, k, nblocks
      integer :: imax, jmax, wst, wen
      integer :: nphys, ntags 
      integer :: file_type, data_size, nperiodic

      real :: version, z = 0.0
      double precision, dimension(:,:), allocatable :: x,y

      character (len=40) :: ifilename, ofilename
      end module var

      program converter
      use var
      implicit none
      
      call read_input

      open(1,file=ifilename)
      read(1,*) nblocks
      read(1,*) imax, jmax
      allocate(x(1:imax, 1:jmax), y(1:imax, 1:jmax))
      do k=1,nblocks
         read(1,*) ((x(i,j),i=1,imax),j=1,jmax), ((y(i,j),i=1,imax),j=1,jmax)
      enddo
      close(1)

      call check(x,y)
      call msh_ascii(x,y)
      end program
