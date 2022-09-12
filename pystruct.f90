module stru3d
implicit none

character(len=10), public :: strutype
integer(kind=4), public :: ndfel,ne,ndf,nne,n,ms,nbn,nlc,nlmem,fileout_unit
real(kind=8), allocatable :: tk(:,:),elem_prop(:,:),mat_table(:,:), &
  sec_table(:,:),al(:,:),reac(:,:),fem_dload(:,:),mfem_load(:,:),mfem_param(:,:)
integer(kind=4),allocatable :: ib(:) 
logical, public :: kerr 

! interface operator(.mv.)
! module procedure matvec
! end interface

contains

!-----------------------
!K Assem
!-----------------------
subroutine k_assem()!mat_table,sec_table) !bind(c,name='k_assem')
implicit none
integer :: nel
!real(kind=8), intent(in) :: mat_table(:,:),sec_table(:,:)
real(kind=8), allocatable :: rot(:,:),elst(:,:)
integer(kind=4) ::con(2)
!allocate(rot(ndfel,ndfel),elst(ndfel,ndfel),tk(n,ms))
allocate(rot(ndfel,ndfel),elst(ndfel,ndfel)   )
do nel=1,ne
    elst=0.0
    rot=0.0
    con(1)=int(elem_prop(nel,1))!%inc1
    con(2) =int(elem_prop(nel,2))!%inc2
    !elst=elem_stiff(nel,ndfel,mat_table,sec_table)
    call elem_stiff(nel,elst)
    !rot=rotmatgen(nel,elem_prop)
    call rotmatgen(nel,rot)
    elst=matmul(transpose(rot),matmul(elst,rot))
    call elassgen(elst,con)           
enddo
deallocate(rot,elst)
write(*,'(a35,2x,a10)') "Stiffness Matrix Assembeld for:",strutype 
end subroutine k_assem 

! -----------------------
! element stiffness matrix
! -----------------------
!function elem_stiff(nel,ndfel,elem_prop,mat_table,sec_table) result(kelst) !bind(c,name='elem_stiff')
subroutine elem_stiff(nel,kelst)!,mat_table,sec_table,kelst)
implicit none

integer(kind=4),intent(in) :: nel
!real(kind=8), intent(in) :: mat_table(:,:),sec_table(:,:)
integer(kind=4) :: isec,imat
real(kind=8) :: eiz,eax,d,eiy,gix
real(kind=8),intent(inout) :: kelst(:,:) 

imat=int(elem_prop(nel,4))!%mat_no
isec=int(elem_prop(nel,3))!%sec_no
d=elem_prop(nel,5)!%elem_len

eax=mat_table(imat,1)*sec_table(isec,1)!%emod*sec_table(isec,1)!%ax
eiz=mat_table(imat,1)*sec_table(isec,4)!%iz
eiy=mat_table(imat,1)*sec_table(isec,8)!%iy
gix=mat_table(imat,1)*sec_table(isec,12)!%ix
!print *, imat,isec,d

kelst=0.0
select case (strutype )

case('Truss2D')
    kelst(1, 1) = eax / d
    kelst(2, 1) = 0.0;
    kelst(1, 2) = -eax / d
    kelst(2, 2) = 0.0
case('Truss3D')
    kelst(1, 1) = eax / d;
    kelst(4, 1) = -eax / d;
    kelst(1, 4) = -eax / d;
    kelst(4, 4) = eax / d;
case('Frame2D')

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
case('Frame3D')
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

return
!end function elem_stiff
end subroutine elem_stiff

! -----------------------
! Element Rotation Matrix
! -----------------------
!function rotmatgen(nel,ndfel,elem_prop) result(rot) !bind(c,name='rotmatgen')
subroutine rotmatgen(nel,rot) !bind(c,name='rotmatgen')
implicit none
!real(kind=8), intent(in) :: elem_prop(:,:)
real(kind=8), intent(inout)  :: rot(:,:)
integer(kind=4), intent(in) :: nel
real(kind=8) :: dt,beta,cx,cy,cz,cxz
integer(kind=4) :: i,j

rot=0.0
dt=elem_prop(nel,5)!%elem_len !prop(kdsp+5)
beta=0.0!elem_prop(nel)%beta!prop(kdsp+1)
cx=elem_prop(nel,6)!%dx/dt !prop(kdsp+6)/dt
cy=elem_prop(nel,7)!%dy/dt !prop(kdsp+7)/dt
cz=0.0!elem_prop(nel)%dz/dt !prop(kdsp+8)/dt
cxz=sqrt(cx*cx+cz*cz)
!write(*,*) strutype,dt,cx,cy
select case ( strutype )
  case ('Truss2D')
    rot(1,1)=cx
    rot(1,2)=cy
    rot(2,1)=-cy
    rot(2,2)=cx
  case ('Truss3D')
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
  case ('Frame2D')
    rot(1,1)=cx
    rot(1,2)=cy
    rot(2,1)=-cy
    rot(2,2)=cx
    rot(3,3)=1.0
  case ('Frame3D')

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

!end function rotmatgen
end subroutine rotmatgen

! -----------------------
! Assembling Global Stiffness Matrix
! -----------------------
subroutine elassgen(elst,con) !bind(c,name='elassgen')
!use iso_c_binding
!use structvarsgen
implicit none
!nel =number of the current node
!n1 =number of the start node
!n2 =number of the end node
! -----------------------------------------------------------
!integer(c_int), intent(in) :: nne,ndf
integer(kind=4) :: n1,n2,kc,kr,i1,i2,i,j1,j,j2,k,ic9,k2=0,k1=0, ki,l
integer(kind=4),intent(in) :: con(:)
real(kind=8),intent(in) :: elst(:,:)
!real(kind=8), intent(inout) :: tk(:,:)

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

SUBROUTINE boundgen() !bind(c,name='boundgen')
IMPLICIT NONE
!---INTRODUCTION OF THE BOUNDARY CONDITIONS
INTEGER(kind=4) :: l1,no,l2,k1,i,kr,kv,j,l,icol,nlc
nlc=size(reac,2)

DO  l=1,nbn
  
!---NO=NUMBER OF THE CURRENT BOUNDARY
  
  l1=(ndf+1)*(l-1)+1
  no=ib(l1)
  k1=ndf*(no-1)
  DO  i=1,ndf
    l2=l1+i
    IF(ib(l2) == 0) THEN
      GO TO    10
    ELSE
      cycle!GO TO   100
    END IF
    
!---SET DIAGONAL COEFFICIENT OF TK EQUAL TO 1 AND PLACE PRESCRIBED VALUE IN AL
    10      kr=k1+i
    DO  j=2,ms
      kv=kr+j-1
      IF(n-kv < 0) THEN
        GO TO    30
      END IF
!---MODIFY ROW OF TK AND CORRESPONDING ELEMENTS IN AL
      DO  icol=1,nlc
        al(kv,icol)=al(kv,icol)-tk(kr,j)*reac(kr,icol)
      END DO
      tk(kr,j)=0.
      30      kv=kr-j+1
      if(kv<=0) cycle
      
!---MODIFY COLUMN IN TK AND CORRESPONDING ELEMENT IN AL
      do icol=1,nlc
        al(kv,icol)=al(kv,icol)-tk(kv,j)*reac(kr,icol)
      END DO
      tk(kv,j)=0.
    END DO
    tk(kr,1)=1.
    DO  icol=1,nlc
      al(kr,icol)=reac(kr,icol)
    END DO
  END DO
END DO
RETURN
END SUBROUTINE boundgen

SUBROUTINE bgaussgen() !bind(c,name='bgaussgen')
!use iso_c_binding
!use structvarsgen, only:printmatrix,fileout_unit,n,nlc,kerr,ms,a =>tk, b => al
use iso_fortran_env
use ieee_exceptions
IMPLICIT NONE

!---N:     ROW DIMENSION OF A AND B
!---MS:     COLUMN DIMENSION OF A
!---LC:     COLUMN DIMENSION OF B
!---D:      AUXILIARY VECTOR
REAL(kind=8),  allocatable :: d(:)
REAL(kind=8) :: c
INTEGER(kind=4) :: n1,k,l,ni,k1,k2,j,icol,i,k3,nlc
!logical :: isnan
nlc=size(reac,2)
allocate(d(n))

kerr=.false.
n1=n-1
DO  k=1,n1
  c=tk(k,1)
  k1=k+1
  IF(ABS(c)-.000001 <= 0.0) THEN 
    WRITE(fileout_unit,'("**** SINGULARITY IN ROW",i5,1X,"****")') k
    kerr=.true.
    !call printmatrix(tk,"Stiff")
    return
  END IF  
!---DIVIDE ROW BY DIAGONAL COEFFICIENT
  
  ni=k1+ms-2
  l=MIN0(ni,n)
  DO  j=2,ms
    d(j)=tk(k,j)
  END DO
  DO  j=k1,l
    k2=j-k+1
    tk(k,k2)=tk(k,k2)/c
  END DO
  DO  icol=1,nlc
    al(k,icol)=al(k,icol)/c
  END DO
  
!       ELIMINATE UNKNOWN X(K) FROM ROW I
  
  DO  i=k1,l
    k2=i-k1+2
    c=d(k2)
    DO  j=i,l
      k2=j-i+1
      k3=j-k+1
      tk(i,k2)=tk(i,k2)-c*tk(k,k3)
    END DO
    DO  icol=1,nlc
      al(i,icol)=al(i,icol)-c*al(k,icol)
    END DO
  END DO
END DO

!---COMPUTE LAST UNKNOWN

IF(ABS(tk(n,1))-.000001 <= 0.0) THEN
  WRITE(fileout_unit,'("**** Back substitution: SINGULARITY IN ROW",i5,1X,"****")') n
  kerr=.false.
  !call printmatrix(tk,"Stiff")  
!  call exit(1)
END IF
DO  icol=1,nlc
  al(n,icol)=al(n,icol)/tk(n,1)
END DO

!---APPLY BACKSUBSTITUTION PROCESS TO COMPUTE REMAINING UNKNOWNS

DO  i=1,n1
  k=n-i
  k1=k+1
  ni=k1+ms-2
  l=MIN0(ni,n)
  DO  j=k1,l
    k2=j-k+1
    DO  icol=1,nlc
      al(k,icol)=al(k,icol)-tk(k,k2)*al(j,icol)
    END DO
  END DO
END DO
kerr=.true.
deallocate(d)

RETURN
END SUBROUTINE bgaussgen


SUBROUTINE dloadgen()
!use structvarsgen
implicit none
INTEGER(kind=4) :: klc

integer(kind=4) :: mn,j,isec,imat,inc1,inc2,kdsp1,kdsp2
real(kind=8) :: f(ndfel),vlocal(ndfel),vglob(ndfel),wload,ra,rma,dl,totalwht
real(kind=8), dimension(ndfel,ndfel) :: rot


totalwht=0.0
DO  mn=1,NE
  rot=0.0
  !fem_dload(mn)%fem=0.0
  isec=int(elem_prop(mn,3))!%sec_no
  imat=int(elem_prop(mn,4))!%mat_no
  inc1=int(elem_prop(mn,1))!%inc1
  inc2=int(elem_prop(mn,2))!%inc2
  dl=elem_prop(mn,5)!%elem_len
  if(sec_table(isec,1)==0) then
    wload=sec_table(isec,2)*mat_table(imat,2)!%matden 
  else
    wload=sec_table(isec,1)
  endif


!---- COMPUTE MOMENTS VECTOR ORIENTATION (Cross Product)---
  totalwht=totalwht+wload*dl

  kdsp1=ndf*(inc1-1) 
  kdsp2=ndf*(inc2-1) 
  ra=wload*dl/2.
  rma=wload*dl*dl/12.

  IF(strutype == 'Frame3D') then 
    f(1)=0.0; f(2)=-1.0; f(3)=0.0 ;f(4)=0.0; f(5)=0.0 ; f(6)=0.0
    f(7)=0.0; f(8)=-1.0; f(9)=0.0 ;f(10)=0.0; f(11)=0.0 ; f(12)=0.0 
    !rot=rotmatgen(mn)
    call rotmatgen(mn,rot)    
!    vlocal=rot .mv. f
    vlocal=matmul(rot , f   ) 
    vlocal(1)=ra*vlocal(1)
    vlocal(2)=ra*vlocal(2)   
    vlocal(3)=ra*vlocal(3)
    vlocal(4)=-rma*vlocal(4)
    vlocal(5)=-rma*vlocal(5)
    vlocal(6)=-rma*vlocal(6)
    vlocal(7)=ra*vlocal(7)
    vlocal(8)=ra*vlocal(8)
    vlocal(9)=ra*vlocal(9)
    vlocal(10)=rma*vlocal(10)
    vlocal(11)=rma*vlocal(11)
    vlocal(12)=rma*vlocal(12)
  else if(strutype == 'Frame2D') then
    f(1)=0.0; f(2)=-1.0; f(3)=0.0 ;f(4)=0.0; f(5)=-1.00 ; f(6)=0.0! dload unit Y vector    
    !rot=rotmatgen(mn)
    call rotmatgen(mn,rot)
    !vlocal=rot .mv. f
    vlocal=matmul(rot , f)
    vlocal(1)=ra*vlocal(1)
    vlocal(2)=ra*vlocal(2)    
    vlocal(3)=-rma*vlocal(3)
    vlocal(4)=ra*vlocal(4)
    vlocal(5)=ra*vlocal(5)
    vlocal(6)=rma*vlocal(6)
  end if 
  
  !vglob=transpose(rot) .mv. vlocal
  vglob=matmul(transpose(rot) , vlocal)
  !fem_dload(mn:%fem(1:ndfel)=fem_dload(mn)%fem(1:ndfel)-vlocal(1:ndfel)
  fem_dload(mn,1:ndfel)=fem_dload(mn,1:ndfel)-vlocal(1:ndfel)
  do klc=1,nlc
    DO  j=1,ndf
      al(kdsp1+j,klc)=al(kdsp1+j,klc)+vglob(j)
      al(kdsp2+j,klc)=al(kdsp2+j,klc)+vglob(j+ndf)
    END DO
  end do
END DO
RETURN
END SUBROUTINE dloadgen

SUBROUTINE mfemgen()
!use structvarsgen
implicit none

!real(kind=8),allocatable ::  vlocal(:),vglob(:),rot(:,:),f(:)
real(kind=8) ::  vlocal(ndfel),vglob(ndfel),rot(ndfel,ndfel),f(ndfel)
real(kind=8) :: wa=0,wb=0,a=0,dl,ra=0,rb=0,rma=0,rmb=0,p=0,b=0
integer(kind=4) :: n1,n2,kdsp1,kdsp2,mn,i,j,mltype,klc,mkdsp,kref
!integer(kind=4),INTENT(IN) :: klc
!real(kind=8),INTENT(IN), dimension(:) :: f
!allocate(vlocal(ndfel),vglob(ndfel),rot(ndfel,ndfel),f(ndfel))

DO  i=1,nlmem
  f=0.0
  mltype=int(mfem_param(i,1))
  klc=int(mfem_param(i,2))
  mn=int(mfem_param(i,3))!%mem_no
  kref=int(mfem_param(i,7))
  if(mltype==1) then
    wa=mfem_param(i,4)!%wa 
    wb=mfem_param(i,5)!%wb 
    a=mfem_param(i,6)!%a
  elseif(mltype==2) then
    p=mfem_param(i,4)
    a=mfem_param(i,5)
  
!    write(*,*) p,a
  endif

n1=int(elem_prop(mn,1))!%inc1 nne*(mn-1)
n2=int(elem_prop(mn,2))!%inc2 
dl=elem_prop(i,5)!%elem_len
a=a*dl 
!write(*,*) n1,n2,dl,a
!---- COMPUTE MOMENTS VECTOR ORIENTATION ---

  kdsp1=ndf*(n1-1)
  kdsp2=ndf*(n2-1)
!----- mltype=1 trapezoidal load
!----- mltype=2 concentrated load    
  if(mltype==1) then
    ra=wa*((dl-a)**3)*(dl+a)/(2.*dl**3)
    rb=((wa+wb)*(dl-a)/2.)- ra
    rma=wa*((dl-a)**3)*(dl+3.*a)/(12.*dl*dl)
    rmb=(ra*dl)-((wa*(dl-a)**2)/2.)-((wb-wa)*(dl-a)*(dl-a)/6.)+rma
  elseif(mltype==2) then
    b=dl-a
    ra=p*(3*a+b)*b**2/dl**3
    rb=p*(a+3*b)*a**2/dl**3
    rma=p*a*b**2/dl**2
    rmb=p*b*a**2/dl**2
!    write(*,*) b,ra,rb,rma,rmb
  endif  

  IF(strutype == 'Frame3D') then !GO TO 12
!    vlocal=rot .mv. f
    ! f(1)=0.0; f(2)=-1.0; f(3)=0.0 ;f(4)=0.0; f(5)=0.0 ; f(6)=0.0
    ! f(7)=0.0; f(8)=-1.0; f(9)=0.0 ;f(10)=0.0; f(11)=0.0 ; f(12)=0.0 
    vlocal=matmul(rot , f  )  
    vlocal(1)=ra*vlocal(1)
    vlocal(2)=ra*vlocal(2)   
    vlocal(3)=ra*vlocal(3)
    vlocal(4)=-rma*vlocal(4)
    vlocal(5)=-rma*vlocal(5)
    vlocal(6)=-rma*vlocal(6)
    vlocal(7)=ra*vlocal(7)
    vlocal(8)=ra*vlocal(8)
    vlocal(9)=ra*vlocal(9)
    vlocal(10)=rmb*vlocal(10)
    vlocal(11)=rmb*vlocal(11)
    vlocal(12)=rmb*vlocal(12)
  else if(strutype == 'Frame2D') then!GO TO 41
!    rot=rotmatgen(mn)
    call rotmatgen(mn,rot) 
!    write(*,*) rot   
!    vlocal=rot .mv. f
!    f(1)=1.0; f(2)=0.0; f(3)=0.0 ;f(4)=1.0; f(5)=0.00 ; f(6)=0.0
    if(kref==1)then
      f(1)=1.0
      f(4)=1.0
    else if(kref==2)then
      f(2)=1.0
      f(5)=1.0
    end if    
    vlocal=matmul(rot , f )   
    vlocal(1)=ra*vlocal(1)
    vlocal(2)=ra*vlocal(2)    
    vlocal(3)=-rma
    vlocal(4)=rb*vlocal(4)
    vlocal(5)=rb*vlocal(5)
    vlocal(6)=rmb
  end if !41    CONTINUE
  mkdsp=ne*(klc-1)
!  vglob=transpose(rot) .mv. vlocal
  vglob=matmul(transpose(rot) , vlocal )
  ! write(*,*) 'vlocal ',vlocal
  ! write(*,*) 'vglob', vglob 
  mfem_load(mkdsp+mn,1:ndfel)=mfem_load(mkdsp+mn,1:ndfel)-vlocal(1:ndfel)
  DO  j=1,ndf
    al(kdsp1+j,klc)=al(kdsp1+j,klc)+vglob(j)
    al(kdsp2+j,klc)=al(kdsp2+j,klc)+vglob(j+ndf)
  END DO
END DO
!deallocate(vlocal,vglob,rot,f)
RETURN
END SUBROUTINE mfemgen


end module stru3d