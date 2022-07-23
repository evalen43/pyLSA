module pystruct
implicit none

!public :: eleme_prop, mat_table, sect_table

real(kind=8), parameter :: pi=3.141592
character(len=10), public :: strutype
integer(kind=4), public :: ndfel,ne,ndf,nne,n,ms
real(kind=8), allocatable :: tk(:,:)



contains

!-----------------------
!K Assem
!-----------------------
subroutine k_assem(ndfel,elem_prop,mat_table,sec_table) !bind(c,name='k_assem')
implicit none
integer(kind=4), intent(in) :: ndfel
integer :: nel
real(kind=8), intent(in) :: elem_prop(:,:),mat_table(:,:),sec_table(:,:)
real(kind=8), allocatable :: rot(:,:),elst(:,:)
integer(kind=4) ::con(2)
allocate(rot(ndfel,ndfel),elst(ndfel,ndfel),tk(n,ms))
do nel=1,ne
    elst=0.0
    rot=0.0
    con(1)=elem_prop(nel,1)!%inc1
    con(2) =elem_prop(nel,2)!%inc2
    elst=elem_stiff(nel,ndfel,mat_table,sec_table)
    !call elem_stiff(nel,elst)
    rot=rotmatgen(nel,ndfel,elem_prop)
    !call rotmatgen(nel,rot)
    elst=matmul(transpose(rot),matmul(elst,rot))
    call elassgen(tk,elst,con)           
enddo
deallocate(rot,elst) 
end subroutine k_assem 

! -----------------------
! element stiffness matrix
! -----------------------
function elem_stiff(nel,ndfel,elem_prop,mat_table,sec_table) result(kelst) !bind(c,name='elem_stiff')
implicit none

integer(kind=4),intent(in) :: nel,ndfel
real(kind=8), intent(in) :: elem_prop(:,:),mat_table(:,:),sec_table(:,:)
integer(kind=4) :: isec,imat
real(kind=8) :: eiz,eax,d,eiy,gix
real(kind=8) :: kelst(ndfel,ndfel) 

!allocate(kelst(ndfel,ndfel))
imat=elem_prop(nel,4)!%mat_no
isec=elem_prop(nel,3)!%sec_no
d=elem_prop(nel,5)!%elem_len

eax=mat_table(imat,1)*sec_table(isec,1)!%emod*sec_table(isec,1)!%ax
eiz=mat_table(imat,1)*sec_table(isec,4)!%iz
eiy=mat_table(imat,1)*sec_table(isec,8)!%iy
gix=mat_table(imat,1)*sec_table(isec,12)!%ix
!print *, imat,isec,d
!allocate(kelst(ndfel,ndfel))
kelst=0.0
select case (strutype )

case('2dtruss')
    kelst(1, 1) = eax / d
    kelst(2, 1) = 0.0;
    kelst(1, 2) = -eax / d
    kelst(2, 2) = 0.0
case('3dtruss')
    kelst(1, 1) = eax / d;
    kelst(4, 1) = -eax / d;
    kelst(1, 4) = -eax / d;
    kelst(4, 4) = eax / d;
case('2dframe')

    kelst(1,1) =eax/d
    kelst(1,4) =-kelst(1,1)
    kelst(2,2) =12.*eiz/(d**3)
    kelst(2,3) =6.*eiz/(d*d)
    kelst(2,5) =-kelst(2,2)
    kelst(2,6) =kelst(2,3)
    kelst(3,2) =kelst(2,3)
    kelst(3,3) =4.*eiz/d
    kelst(3,5) =-kelst(2,3)
    kelst(3,6) =2.*eiz/d
    kelst(4,1) =kelst(1,4)
    kelst(4,4) =kelst(1,1)
    kelst(5,2) =kelst(2,5)
    kelst(5,3) =kelst(3,5)
    kelst(5,5) =kelst(2,2)
    kelst(5,6) =kelst(3,5)
    kelst(6,2) =kelst(2,6)
    kelst(6,3) =kelst(3,6)
    kelst(6,5) =kelst(5,6)
    kelst(6,6) =kelst(3,3)
case('3dframe')
        kelst(1, 1) = eax / d                 !(0,0)
        kelst(7, 1) = -eax / d                !(6,0) 
        kelst(2, 2) = 12.0 * eiz / d**3       !(1,1)
        kelst(6, 2) = 6.0 * eiz / d**2        !(5,1)
        kelst(8, 2) = -12.0 * eiz / d ** 3    !(7,1)
        kelst(12, 2) = 6.0 * eiz / d ** 2     !(11,1)
        kelst(3, 3) = 12.0 * eiy / d ** 3     !(2,2)
        kelst(5, 3) = -6.0 * eiy / d ** 2     !(4,2)
        kelst(9, 3) = -12.0 * eiy / d ** 3    !(8,2)
        kelst(11, 3) = -6.0 * eiy / d ** 2    !(10,2)
        kelst(4, 4) = gix / d                 !(3,3)
        kelst(10, 4) = -gix / d               !(9,3)
        kelst(3, 5) = -6.0 * eiy / d ** 2     !(2,4)
        kelst(5, 5) = 4.0 * eiy / d           !(4,4)
        kelst(9, 5) = 6.0 * eiy / d ** 2      !(8,4)
        kelst(11, 5) = 2.0 * eiy / d          !(10,4)
        kelst(2, 6) = 6.0 * eiz / d ** 2      !(1,5)
        kelst(6, 6) = 4.0 * eiz / d           !(5,5)
        kelst(8, 6) = -6.0 * eiz / d ** 2     !(7,5)
        kelst(12, 6) = 2.0 * eiz / d          !(11,5)
        kelst(1, 7) = -eax / d                !(0,6)
        kelst(7, 7) = eax / d                 !(6,6)
        kelst(2, 8) = -12.0 * eiz / d ** 3    !(1,7)
        kelst(6, 8) = -6.0 * eiz / d ** 2     !(5,7)
        kelst(8, 8) = 12.0 * eiz / d ** 3     !(7,7)
        kelst(12, 8) = -6.0 * eiz / d ** 2    !(11,7)
        kelst(3, 9) = -12.0 * eiy / d ** 3    !(2,8)
        kelst(5, 9) = 6.0 * eiy / d ** 2      !(4,8)
        kelst(9, 9) = 12.0 * eiy / d ** 3     !(8,8)
        kelst(11, 9) = 6.0 * eiy / d ** 2     !(10,8)
        kelst(4, 10) = -gix / d               !(3,9)
        kelst(10, 10) = gix / d               !(9,9)
        kelst(3, 11) = -6.0 * eiy / d ** 2    !(2,10)
        kelst(5, 11) = 2.0 * eiy / d          !(4,10)
        kelst(9, 11) = 6.0 * eiy / d ** 2     !(8,10)
        kelst(11, 11) = 4.0 * eiy / d         !(10,10)
        kelst(2, 12) = 6.0 * eiz / d ** 2     !(1,11)
        kelst(6, 12) = 2.0 * eiz / d          !(5,11)
        kelst(8, 12) = -6.0 * eiz / d ** 2    !(7,11)
        kelst(12, 12) = 4.0 * eiz / d         !(11,11)   
!    call printmatrix(kelst,"kelst")
end select
!deallocate(kelst)
return
end function elem_stiff
!end subroutine elem_stiff

! -----------------------
! Element Rotation Matrix
! -----------------------
function rotmatgen(nel,ndfel,elem_prop) result(rot) !bind(c,name='rotmatgen')
!subroutine rotmatgen(nel,rot) bind(c,name='rotmatgen')
!use iso_c_binding
implicit none
real(kind=8), intent(in) :: elem_prop(:,:)
real(kind=8)  :: rot(ndfel,ndfel)
integer(kind=4), intent(in) :: nel,ndfel
real(kind=8) :: dt,beta,cx,cy,cz,cxz
integer(kind=4) :: i,j
!ndf=ndfel/2
!allocate(rot(ndfel,ndfel))
rot=0.0
dt=elem_prop(nel,5)!%elem_len !prop(kdsp+5)
beta=0.0!elem_prop(nel)%beta!prop(kdsp+1)
cx=elem_prop(nel,6)!%dx/dt !prop(kdsp+6)/dt
cy=elem_prop(nel,7)!%dy/dt !prop(kdsp+7)/dt
cz=0.0!elem_prop(nel)%dz/dt !prop(kdsp+8)/dt
cxz=sqrt(cx*cx+cz*cz)
select case ( strutype )
  case ('2dtruss')
    rot(1,1)=cx
    rot(1,2)=cy
    rot(2,1)=-cy
    rot(2,2)=cx
  case ('3dtruss')
    if(cxz /= 0.) then
      rot(1, 1) = cx;
      rot(1, 2) = cy;
      rot(1, 3) = cz;
      rot(2, 1) = -cx * cy / cxz;
      rot(2, 2) = cxz;
      rot(2, 3) = -cy * cz / cxz;
      rot(3, 1) = -cz / cxz;
      rot(3, 2) = 0.0;
      rot(3, 3) = cx / cxz;
    else if (cxz == 0) then 
      rot(1, 1) = 0.0;
      rot(1, 2) = cy;
      rot(1, 3) = 0.0;
      rot(2, 1) = -cy;
      rot(2, 2) = 0.0;
      rot(2, 3) = 0.0;
      rot(3, 1) = 0.0;
      rot(3, 2) = 0.0;
      rot(3, 3) = 1.0;      
    end if
  case ('2dframe')
    rot(1,1)=cx
    rot(1,2)=cy
    rot(2,1)=-cy
    rot(2,2)=cx
    rot(3,3)=1.0
  case ('3dframe')
 
    if(cxz /= 0.) then 
    rot(1,1)=cx
    rot(1,2)=cy
    rot(1,3)=cz
    rot(2,1)=(-cx*cy*cos(beta)-cz*sin(beta)) /cxz
    rot(2,2)=cxz*cos(beta)
    rot(2,3)=(-cy*cz*cos(beta)+cx*sin(beta))/cxz
    rot(3,1)=(cx*cy*sin(beta)-cz*cos(beta))/cxz
    rot(3,2)=-cxz*sin(beta)
    rot(3,3)=(cy*cz*sin(beta)+  cx*cos(beta))/cxz
    else
    rot(1,2)=cy
    rot(2,1)=-cy*cos(beta)
    rot(2,3)=sin(beta)
    rot(3,1)=cy*sin(beta)
    rot(3,3)=cos(beta)
    end if    
end select

do  i=1,ndf
  do  j=1,ndf
    rot(i+ndf,j+ndf)=rot(i,j)
  end do
end do
!deallocate(rot)
end function rotmatgen
!end subroutine rotmatgen

! -----------------------
! Assembling Global Stiffness Matrix
! -----------------------
subroutine elassgen(tk,elst,con) !bind(c,name='elassgen')
!use iso_c_binding
!use structvarsgen
implicit none
!nel =number of the current node
!n1 =number of the start node
!n2 =number of the end node
! -----------------------------------------------------------
!integer(c_int), intent(in) :: nne,ndf
integer(kind=4) :: n1,n2,kc,kr,i1,i2,i,j1,j,j2,k,ic9,k2,k1, ki,l
integer(kind=4),intent(in) :: con(:)
real(kind=8),intent(in) :: elst(:,:)
real(kind=8), intent(inout) :: tk(:,:)
 
do  i=1,nne
  n1 =con(i)
  i1 =ndf*(i-1)
  j1 =ndf*(n1-1)
  do  j=i,nne
    n2 =con(j)
    i2 =ndf*(j-1)
    j2 =ndf*(n2-1)
    do  k=1,ndf
      ki=1
      if(n1-n2 < 0) then
        go to    20
      else if (n1-n2 == 0) then
        go to    10
      else
        go to    30
      end if
!---store a diagonal submatrix
      10     ki=k
!---store an off diagonal submatrix
      20    kr=j1+k
      ic9=j2-kr+1
      k1=i1+k
      go to 40
!---store the transpose of an off diagonal matrix
      30    kr=j2+k
      ic9=j1-kr+1
      k2=i2+k
      40    do  l=ki,ndf
        kc=ic9+l
        if(n1-n2 > 0) then
          go to    46
        end if
        k2=i2+l
        go to 51
        46    k1=i1+l
        51    continue
        tk(kr,kc)=tk(kr,kc)+elst(k1,k2)
      end do
    end do
  end do
end do
!write(fileout_unit,'("subroutine elass: ",i5)')nel
return
end subroutine elassgen



end module pystruct