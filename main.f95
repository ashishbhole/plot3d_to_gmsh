      program converter
      use var
      implicit none
      
      call read_input

      open(1,file=ifilename)
      read(1,*) nd
      read(1,*) imax, jmax
      allocate(x(1:imax, 1:jmax), y(1:imax, 1:jmax))
      do k=1,nd
         read(1,*) ((x(i,j),i=1,imax),j=1,jmax), ((y(i,j),i=1,imax),j=1,jmax)
      enddo
      close(1)

      call check(x,y)    ! optional: just to check reading of plot3d file
      call msh_ascii(x,y)
      end program
