      subroutine msh_ascii(xg,yg)
      use var
      implicit none
      integer :: kk, nnodes, nelems, eltype
      double precision, intent(in) :: xg(1:imax,1:jmax), yg(1:imax,1:jmax)
      integer, dimension(:,:), allocatable :: nid
      integer, dimension(:), allocatable :: tag
      double precision, dimension(:,:), allocatable :: elem_id


      nnodes = imax*jmax
      nelems = (imax-1)*(jmax-1) + 2*((imax-1)+(jmax-1))
      allocate(nid(1:nelems,1:4), elem_id(1:imax-1,1:jmax-1))
      allocate(tag(1:ntags))

      open(8, file=ofilename)
      write(8,'(A11)') "$MeshFormat"
      write(8,'(F4.2,X,I1,X,I1)') version, file_type, data_size
      write(8,'(A14)') "$EndMeshFormat"

      write(8,'(A13)') "$PhysicalNames"
      write(8,'(I1)') nphys
      write(8,'(I1,X,I1,X,A6)')  2,1,'"Mesh"'
      write(8,'(I1,X,I1,X,A6)')  1,2,'"Wall"'
      write(8,'(I1,X,I1,X,A10)') 1,3,'"FarField"'
      write(8,'(I1,X,I1,X,A11)') 1,4,'"FarField1"'
      write(8,'(I1,X,I1,X,A11)') 1,5,'"FarField2"'
      write(8,'(I1,X,I1,X,A8)')  1,6,'"patch1"'
      write(8,'(I1,X,I1,X,A8)')  1,7,'"patch2"'
      write(8,'(A16)') "$EndPhysicalNames"

      write(8,'(A6)') "$Nodes"
      write(8,'(I6)') nnodes
      k = 1
      do j = 1,jmax
      do i = 1,imax
         write(8,'(I6,X,E16.10,2X,E16.10,3X,E16.10)') k,xg(i,j),yg(i,j),z
      k = k+1
      enddo
      enddo
      write(8,'(A9)') "$EndNodes"

      write(8,'(A9)') "$Elements"
      write(8,'(I6)') nelems

      eltype = 3 ; tag(1) = 1 ; tag(2) = 1  ! for grid
      k = 1
      do j = 1,jmax-1                   
      do i = 1,imax-1
       elem_id(i,j) = k
       nid(k,1) = i + (j-1)*imax
       nid(k,2) = nid(k,1)+1
       nid(k,4) = i + j*imax
       nid(k,3) = nid(k,4)+1

       write(8,'(9(I6,X))') k,eltype,ntags,tag(1),tag(2),nid(k,1),nid(k,2),nid(k,3),nid(k,4)
      k = k+1
      enddo
      enddo

      eltype = 1 ; tag(1) = 2 ; tag(2) = 2  ! for airfoil wall
      j = 1
      do i = wst, wen
       kk=elem_id(i,j)
       nid(k,1) = nid(kk,1)
       nid(k,2) = nid(kk,2)

       write(8,'(7(I6,X))') k,eltype,ntags,tag(1),tag(2),nid(k,1),nid(k,2)
       k = k+1
      enddo

      eltype = 1 ; tag(1) = 3 ; tag(2) = 3  ! for farfield_1 (curved)
      j = jmax-1
      do i = 1, imax-1
       kk = elem_id(i,j)
       nid(k,1) = nid(kk,4)
       nid(k,2) = nid(kk,3)

       write(8,'(7(I6,X))') k,eltype,ntags,tag(1),tag(2),nid(k,1),nid(k,2)
       k = k+1
      enddo

      eltype = 1 ; tag(1) = 4 ; tag(2) = 4  ! for farfield_2 (vertical at bottom)
      i = 1
      do j = 1, jmax-1
       kk=elem_id(i,j)
       nid(k,1) = nid(kk,1)
       nid(k,2) = nid(kk,4)   !nid(k,1)+1

       write(8,'(7(I6,X))') k,eltype,ntags,tag(1),tag(2),nid(k,1),nid(k,2)
       k = k+1
      enddo

      eltype = 1 ; tag(1) = 5 ; tag(2) = 5  ! for farfield_3 (vertical at upside)
      i = imax-1
      do j = 1, jmax-1
       kk=elem_id(i,j)
       nid(k,1) = nid(kk,2)
       nid(k,2) = nid(kk,3)  !nid(k,1)+1

       write(8,'(7(I6,X))') k,eltype,ntags,tag(1),tag(2),nid(k,1),nid(k,2)
       k = k+1
      enddo

      eltype = 1 ; tag(1) = 6 ; tag(2) = 6  ! for periodicity (bottomside)
      j = 1
      do i = 1, wst-1
       kk=elem_id(i,j)
       nid(k,1) = nid(kk,1)
       nid(k,2) = nid(kk,2)

      write(8,'(7(I6,X))') k,eltype,ntags,tag(1),tag(2),nid(k,1),nid(k,2)
      k = k+1
      enddo

      eltype = 1 ; tag(1) = 7 ; tag(2) = 7  ! for periodicity (upside)
      j = 1
      do i = wen+1, imax-1
       kk=elem_id(i,j)
       nid(k,1) = nid(kk,1)
       nid(k,2) = nid(kk,2)

       write(8,'(7(I6,X))') k,eltype,ntags,tag(1),tag(2),nid(k,1),nid(k,2)
      k = k+1
      enddo

      write(8,'(A12)') "$EndElements"

      write(8,'(A9)') "$Periodic"
      write(8,'(I3)') wst-1
      write(8,'(X(I3))') 1,0,1
      write(8,'(I6)') 2*(wst-1)
      j = 1
      do i = 1,wst-1
      write(8,'(2(I6,X))') i,imax-(i-1)
      enddo
      write(8,'(A12)') "$EndPeriodic"

      !write(8,'(A9)') "$NodeData"
      !write(8,'(A12)') "$EndNodeData"

      !write(8,'(A12)') "$ElementData"
      !write(8,'(A15)') "$EndElementData"

      !write(8,'(A16)') "$ElementNodeData"
      !write(8,'(A19)') "$EndElementNodeData"

      !write(8,'(A20)') "$InterpolationScheme"
      !write(8,'(A23)') "$EndInterpolationScheme"
      close(8)

      end subroutine msh_ascii
