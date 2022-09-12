SUBROUTINE forcegen()
!use structvarsgen
IMPLICIT NONE

REAL(kind=8), DIMENSION(ndfel) :: u,f,ul,fg
REAL(kind=8), DIMENSION(ndfel,ndfel) :: rot,elst
INTEGER(kind=4) :: klc,n1,n2,lc,nel,k1,k2,j1,j2,i,i1,i2,j

lc=size(al,2)

reac=0.0
DO klc=1,lc
  DO nel=1,NE
    rot=0.0
    n1=int(elem_prop(nel,1))!%inc1
    n2=int(elem_prop(nel,2))!%inc2

    rot=rotmatgen(nel)
    !call printmatrix(rot,"rot")
    ! do i=1,ndfel
    !   write(fileout_unit,'(*(f15.2))') (rot(i,j),j=1,ndfel)
    ! enddo
    k1=ndf*(n1-1)
    k2=ndf*(n2-1)
    DO  i=1,ndf
      j1=k1+i
      j2=k2+i
      u(i)=al(j1,klc)
      u(i+ndf)=al(j2,klc)
    END DO

    !ul=rot .mv. u
    ul=matmul(rot,u)
    ! write(fileout_unit,'(*(2Pf15.2))') (u(i),i=1,ndfel)   
    ! write(fileout_unit,'(*(2Pf15.2))') (ul(i),i=1,ndfel)
!-------------------------------     COMPUTE MEMBER END FORCES IN LOCAL COORDINATES
    elst= elem_stiff(nel)
    !f=elst .mv. ul
    f=matmul(elst, ul)
!--------------------------------------- STORE MEMBER END FORCES IN ARRAY FORCE
    i1=ndfel*(nel-1)+NE*ndfel*(klc-1)
    DO  i=1,ndfel
      i2=i1+i
      intforc(i2)=f(i)
    END DO
!-------------    ROTATE MEMBER FORCES TO THE GLOBAL REFERENCE FRAME AND STORE IN ARRAY FG
    f(1:ndfel)=f(1:ndfel)+fem_dload(nel)%fem(1:ndfel)
    
    !fg=transpose(rot) .mv. f
    fg=matmul(transpose(rot), f)
    ! write(fileout_unit,'(*(f15.2))') (fg(i),i=1,ndfel)
    ! write(fileout_unit,'(*(f15.2))') (f(i),i=1,ndfel)
    ! write(fileout_unit,'(80("-"))')          
!-----------------     ADD ELEMENT CONTRIBUTION TO NODAL RESULTANTS IN ARRAY REAC
    DO  i=1,ndf
      j1=k1+i
      j2=k2+i
      reac(j1,klc)=reac(j1,klc)+fg(i)
      reac(j2,klc)=reac(j2,klc)+fg(i+ndf)
    END DO
  END DO
END DO

RETURN
END SUBROUTINE forcegen