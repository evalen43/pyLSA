module structvarsgen
! Variables
use iso_c_binding
real(kind=c_double), public, allocatable :: x(:),y(:),z(:),prop(:),w(:),intforc(:)
real(kind=c_double), public, allocatable :: al(:,:),reac(:,:),tk(:,:)
integer(kind=c_int), public, allocatable :: ib(:),ic(:)
integer(kind=c_int), public :: NDFEL,nne,ndf,ms,n,nbn,nib,nforc,nn,ne,nlnode, &
filein_unit,fileout_unit,kip,kiter,slen,nsec,nmat,nlc,nlmem
INTEGER(kind=c_int) :: tmphour, tmpminute, tmpsecond, tmphund,tmpyear,tmpmonth,tmpday
INTEGER(kind=c_int) ::  values(8)
real(kind=c_double) :: UNITL,UNITF,E,totalwht
real(kind=c_double), parameter :: PI=3.141592
real(kind=c_double), parameter :: grav=9.806
character(kind=c_char,len=10), public, dimension(:), allocatable :: nodestring
character(kind=c_char,len=10), public :: strutype
CHARACTER(kind=c_char,len=20), public :: PNAME
CHARACTER(kind=c_char,len=40), public :: PTITLE
CHARACTER (kind=c_char,len=80) :: exampletitle
character(kind=c_char,len=60), public :: filein    
CHARACTER(kind=c_char,len=80) :: fileout
CHARACTER (kind=c_char,len=8)  :: date
CHARACTER (kind=c_char,len=10) :: time
CHARACTER (kind=c_char,len=5)  :: zone
CHARACTER (kind=c_char) :: mer

logical(kind=c_bool), public :: kerr

! public :: operator (.mv.)
! public :: operator (.mm.)
! public :: matvec

! interface operator(.mv.)
! module procedure matvec
! end interface

! interface operator (.mm.)
! module procedure mmult
! end interface

type, bind(c) :: distload
integer(c_int) :: mem_no
real(c_double) wa,wb,a,f(3)
end type

! type :: femloc
!     real(kind=c_double), allocatable, dimension (:) :: fem
!     integer(kind=c_int) :: mno
! end type femloc

type :: Element
    integer(kind=c_int) :: inc1,inc2,sec_no,mat_no,elem_no
    character(kind=c_char,len=10) :: mem_name
    real(kind=c_double) :: beta,dx,dy,dz,elem_len,axialf
end type element

type :: SECTION
    real(kind=c_double):: ax,iz, iy,ix,rz,ry,tube_od, tube_wth
    character(kind=c_char,len=10) :: sec_name
    ! contains
    ! procedure :: pipeparam
end type SECTION

type :: material
    REAL(kind=c_double) :: Emod,Gmod,matden,poisson
    integer(kind=c_int) :: matid
    CHARACTER (kind=c_char,len=8) :: matname
    ! contains
    ! procedure :: mat_param
END TYPE material

type(material), ALLOCATABLE, dimension(:) :: mat_table
type(element), ALLOCATABLE, dimension(:) :: elem_prop
type(section), ALLOCATABLE, dimension(:) :: sec_table
type(femloc), public, allocatable, dimension(:) :: fem_dload
type(distload),allocatable, dimension(:) :: trap_load

contains

subroutine inputgen() !bind(c,name='inputgen')
!use iso_c_binding
use iso_fortran_env
!use structvarsgen
implicit none
character(kind=c_char,len=8) :: stype,ulen,mtype,ufor,code,ulenb
integer(kind=c_int) :: elemno 
integer(kind=c_int) :: N1,N2,i,kelem,j,L1,L2,k,iostatus,clen,sec_id,kdsp,mat_id
real(kind=c_double) :: ax,iz,d,dx,dy,dz,Px,Py,Mz,pipeod,pipewth

namelist /structpar/ nn,ne,nlnode,nbn,strutype,nsec,nmat,nlmem,code,nlc
namelist /sectype/ stype, ax, iz, pipeod, pipewth, ulen
namelist /mat_type/ mtype
namelist /nodes/ ulen
namelist /loadednodes/ ufor
namelist /boundary/ ulenb

nne=2
ndf=3
nlc=1 

WRITE(output_UNIT, '(a)', ADVANCE='NO')' Enter name of input file: '
read(INPUT_UNIT,'(a)') filein
open(newunit=filein_unit,file=filein,status='old', IOSTAT=iostatus)
IF (iostatus /= 0) then      
    WRITE(error_unit, *) '** Unable to open file: ', filein
end if  

clen=INDEX(filein,".")
fileout=filein(1:clen-1) // "-out.txt"
open(newunit=fileout_unit,file=fileout)

nn=0;ne=0;nlnode=0;nbn=0;strutype='';nsec=0;nmat=0;nlmem=0;code=''

read(filein_unit,FMT='(A80)') exampletitle
read(filein_unit,nml=structpar)
!write(fileout_unit,nml=structpar)
allocate(mat_table(nmat),sec_table(nsec))
do i=1,nmat
    read(filein_unit,nml=mat_type)
!    write(fileout_unit,nml=mat_type)
    mat_table(i)%matname=mtype
    mat_table(i)%matid=i
    call mat_param(mat_table(i))
    write(fileout_unit,'(a8,2x,i5)') 'Material ',i
    WRITE(fileout_unit,22) mat_table(i)%Emod,mat_table(i)%Gmod,mat_table(i)%matden, &
      mat_table(i)%poisson
22    format(   'modulus of elasticity              ',g20.7,/, &
                'shear modulus                      ',g20.7,/, &
                'mass density                      ',g20.7,/, &
                'poisson number                       ',f10.3)
enddo

nib=nbn*(ndf+1)
ndfel=nne*ndf
n=nn*ndf
kerr=.true.
nforc=ndfel*ne*nlc
PNAME="LSA-2D"
PTITLE="Linear Static Analysis"

allocate(x(nn),y(nn),z(nn),ib(nib),al(n,nlc),reac(n,nlc), &
    w(ndf),ic(ndf), intforc(nforc),nodestring(nn), &
    elem_prop(ne))

allocate(fem_dload(ne))  

do i=1,ne
    fem_dload%mno=i
    allocate(fem_dload(i)%fem(ndfel))
enddo
al=0
reac=0

ms=0
write(fileout_unit,'(a18)') 'Section Properties'
do i=1,nsec
    ax=0.0;iz=0.0;pipeod=0.0;pipewth=0.0;ulen=' '
    read(filein_unit,nml=sectype)
!    write(fileout_unit,nml=sectype)
    UNITL=ulen2m(ulen)
    if(stype=="GEN") then
        sec_table(i)%ax=ax*UNITL**2
        sec_table(i)%iz=iz*UNITL**4
        sec_table(i)%sec_name=stype
    else if(stype=="PIPE") then
        write(fileout_unit,'(a10,i3,2x,a4,f10.3,2x,a4,f10.3)') 'section no. ',i,'od ', &
        pipeod,'wth ',pipewth
        sec_table(i)%tube_od=pipeod*UNITL
        sec_table(i)%tube_wth=pipewth*UNITL
        sec_table(i)%sec_name=stype
    !    write(fileout_unit,'(3f12.4)')unitl,sec_table(i)%tube_od,sec_table(i)%tube_wth
        call pipeparam(sec_table(i))
    end if
enddo  

      WRITE(fileout_unit,21)NN,NE,NLC,NBN,nlc!,Emod,Gmod,matden
21    format(   'number of nodes                    ',i5,/,&
                'number of elements                 ',i5,/,&
                'number of loading cases            ',i5,/,&
                'number of boundary nodes           ',i5,/,&
                'number of loading cases            ',i5)
call header()
call time_now()
read(filein_unit,nml=nodes)
UNITL=ulen2m(ulen)
write(fileout_unit,nml=nodes)
!write(fileout_unit,'("Nodes")')
!write(fileout_unit,nml=nodes)
do i=1,nn
    if(strutype=='2DFrame' .or. strutype=='2DTruss')then
      read(filein_unit,fmt=*) n1,x(i),y(i)
      write(nodestring(i),fmt='(i10)') n1
      write(fileout_unit,'(i5,2f10.2)') n1,x(i),y(i)
      z(i)=0.0; x(i)=x(i)*UNITL; y(i)=y(i)*UNITL
    else
      read(filein_unit,fmt=*) n1,x(i),y(i)
      write(nodestring(i),fmt='(i10)') n1
      write(fileout_unit,'(i5,2f10.2)') n1,x(i),y(i)
      x(i)=x(i)*UNITL; y(i)=y(i)*UNITL; z(i)=z(i)*UNITL
    endif
enddo
write(fileout_unit,'("Elements")')
do i=1,ne
    kelem=nne*(i-1)
    read(filein_unit,fmt=*) elemno,n1,n2, sec_id,mat_id
    write(fileout_unit,'(5i5)') elemno,n1,n2,sec_id,mat_id
    elem_prop(i)%inc1=n1
    elem_prop(i)%inc2=N2
    elem_prop(i)%sec_no=sec_id
    elem_prop(i)%elem_no=elemno
    elem_prop(i)%beta=0.0
    elem_prop(i)%mat_no=mat_id
    dx=x(n2)-x(n1)
    dy=y(n2)-y(n1)
    dz=z(n2)-z(n1)
    d=sqrt(dx**2+dy**2+dz**2)
    elem_prop(i)%elem_len=d
    elem_prop(i)%dx=dx
    elem_prop(i)%dy=dy
    elem_prop(i)%dz=dz

    j=abs(n2-n1)
    if(ms<j)ms=j
enddo
ms=ndf*(ms+1)
write(fileout_unit,'("Bandwidth",2x, i5)') ms
allocate(tk(n,ms))!v4.2.1LoadingSupportShopAccountLogout
tk=0.0
write(fileout_unit,'("Nodal Loads")')
read(filein_unit,nml=loadednodes)
write(fileout_unit,nml=loadednodes)
if(nlnode>0) then
do i=1,nlnode
    read(filein_unit,fmt=*) n1,Px,Py,Mz,dx,dy,dz
    write(fileout_unit,'(i5,6f12.2)') n1,Px,Py,Mz,dx,dy,dz    
    kdsp=ndf*(n1-1)
    al(kdsp+1,1)=Px*ufor2kN(ufor)
    al(kdsp+2,1)=Py*ufor2kN(ufor)
    al(kdsp+3,1)=Mz*ufor2kN(ufor)
    reac(kdsp+1,1)=dx
    reac(kdsp+2,1)=dy
    reac(kdsp+3,1)=dz    
enddo
end if
write(fileout_unit,'("Node Boundaries")')
write(fileout_unit,'(27x,"Status",14x,"Prescribed Values")')
write(fileout_unit,'(19x,"0:Prescribed, 1:Free")')
write(fileout_unit,'(7x,"Node",8x,"U",9x,"V",8x,"RZ",6x,"U",9x,"V",8x,"RZ")')
read(filein_unit,nml=boundary)
write(fileout_unit,nml=boundary)
UNITL=ulen2m(ulenb)
do i=1,nbn
    read(filein_unit,fmt=*) j,(ic(k),k=1,ndf),(w(k),k=1,ndf)
    write(fileout_unit,'(4i10,3f10.4)') j,(ic(k),k=1,ndf),(w(k),k=1,ndf)
      L1=(ndf+1)*(i-1)+1
      L2=ndf*(j-1)
    ib(L1)=j
    do k=1,ndf
        n1=L1+k
        n2=L2+k
        ib(n1)=ic(k)
        reac(n2,1)=w(k)*UNITL
    enddo
enddo
call dloadgen()

 end subroutine inputgen

subroutine mat_param(mymat) !bind(c,name='mat_param')
!  use iso_c_binding
    class(material) :: mymat

    select case(mymat%matname)
    case("STEEL")
        mymat%Emod=200e+06 !kN/m2 or 200GPa
        mymat%poisson=0.28
        mymat%matden=78.5!e+03 !kN/m3
        mymat%Gmod=79.3e+06 !kN/m2 or 79.3 GPa
    case("TITANIUM")
        mymat%Emod=113e+06 !kN/m2 or 120GPa
        mymat%poisson=0.3
        mymat%matden=44.13!e+03 !kN/m3
        mymat%Gmod=45e+06 !kN/m2 or 45 GPa
    case default
        mymat%Emod=200e+06 !kN/m2 or 200GPa
        mymat%poisson=0.28
        mymat%matden=78.5!e+03 !kN/m3
        mymat%Gmod=79.3e+06 !kN/m2 or 79.3 GPa
    end select

end subroutine mat_param
!---------------------------------
subroutine pipeparam(mysection) !bind(c,name='pipeparam')
!use iso_c_binding
class(section) :: mysection
real(kind=c_double) :: id,wth,od
od=mysection%tube_od
wth=mysection%tube_wth
id=od-2*wth
mysection%ax=0.25*PI*(od**2-id**2)
mysection%iz=PI*(od**4-id**4)/64.0
mysection%iy=PI*(od**4-id**4)/64.0
mysection%ix=2*mysection%iz
mysection%rz=SQRT(mysection%iz/mysection%ax)
mysection%ry=SQRT(mysection%iy/mysection%ax)
!write(fileout_unit,'(3f12.4)')od,id,mysection%ax
end subroutine pipeparam    

! -----------------------
! Units conversion
! -----------------------

FUNCTION ufor2kN(UNIT) result(factor) !bind(c,name='ufor2kN')
!use iso_c_binding
    IMPLICIT NONE
    CHARACTER(kind=c_char,len=8), intent(in) :: UNIT
    REAL(kind=c_double) :: factor
    select case (UNIT) ! Converts units to meter
    case ("lb")
        factor=0.454*grav/1000.0
    case("kip")
        factor=0.454*grav
    case("N")
        factor=0.001
    case("te")
        factor=1.0*grav        
    case default
        factor=1.0
    END select
END FUNCTION ufor2kN



FUNCTION ulen2m(UNIT) result(factor) !bind(c,name='ulen2m')
!use iso_c_binding
    IMPLICIT NONE
    CHARACTER(kind=c_char,len=8), intent(in) :: UNIT
    REAL(kind=c_double) :: factor
    select case (UNIT) ! Converts units to meter
    case ("inch")
        factor=0.0254
    case("feet")
        factor=0.3048
    case("cm")
        factor=0.01
    case("m")
        factor=1.0        
    case default
        factor=1.0
    END select
END FUNCTION ulen2m


! -----------------------
! Prints date and time
! -----------------------

subroutine time_now() bind(c,name='time_now')
use iso_c_binding
CALL date_and_time(DATE=date,TIME=time,ZONE=zone,VALUES=values)
tmpmonth=values(2)
tmpday=values(3)
tmpyear=values(1)
tmphour=values(5)
tmpminute=values(6)
tmpsecond=values(7)
tmphund=values(8)
IF (tmphour .GT. 12) THEN
    mer = 'p'
    tmphour = tmphour - 12
    ELSE
    mer = 'a'
END IF 

WRITE (fileout_unit, '(i2,"/",i2.2,"/",i4.4)') tmpmonth, tmpday,tmpyear
WRITE (fileout_unit, '(i2,":",i2.2,":",i2.2," ",a,"m")') tmphour, &
    tmpminute, tmpsecond,  mer

end subroutine time_now

   ! -----------------------
   ! Prints Page Header
   ! -----------------------

subroutine header() bind(c,name='header')
use iso_c_binding
WRITE(fileout_unit,'(20x,A20)') PNAME
WRITE(fileout_unit,'(A80)') exampletitle
WRITE(fileout_unit,'(80("-"))')

end subroutine header
! -----------------------
! K Assem
! -----------------------
! subroutine k_assem() bind(c,name='k_assem')
! use iso_c_binding
! implicit none
! integer :: nel
! real(c_double), dimension(ndfel,ndfel):: rot,elst
! integer(c_int), dimension(2)::con
! do nel=1,ne
!     elst=0.0
!     rot=0.0
!     con(1)=elem_prop(nel)%inc1
!     con(2) =elem_prop(nel)%inc2
!     elst=elem_stiff(nel)
!     rot=rotmatgen(nel)
!     elst=matmul(transpose(rot),matmul(elst,rot))
!     Call ELASSgen(elst,con)           
! enddo  
! end subroutine k_assem  
! ! -----------------------
! ! Band Solver
! ! -----------------------
! subroutine band_solvergen() bind(c,name='band_solvergen')
! use iso_c_binding
! implicit none
! integer :: nel
! real(kind=c_double), dimension(ndfel,ndfel):: rot,elst
! integer(kind=c_int), dimension(2)::con
! do nel=1,ne
!     elst=0.0
!     rot=0.0
!     con(1)=elem_prop(nel)%inc1
!     con(2) =elem_prop(nel)%inc2
!     elst=elem_stiff(nel)
!     rot=rotmatgen(nel)
!     elst=matmul(transpose(rot),matmul(elst,rot))
!     Call ELASSgen(elst,con)           
! enddo  
! call BOUNDgen()
! call BGAUSSgen()
! end subroutine band_solvergen

! SUBROUTINE printmatrix(sk,label) !bind(c,name='printmatrix')
! !use iso_c_binding
!     IMPLICIT NONE
!     REAL(kind=c_double), intent(in) :: sk(:,:)
!     CHARACTER (kind=c_char,len=*), intent(in) :: label
!     INTEGER :: i,j
!     !n=size(tk,1)
!     !ms=size(tk,2)
!     WRITE(fileout_unit,*) 'Matrix: ',label
!     DO i=1,n
!         WRITE(fileout_unit,'(*(g15.2))') (sk(i,j), j=1,ms)
!     END DO
! END SUBROUTINE printmatrix


! ! -----------------------
! ! Multiply Matrix * Vector
! ! -----------------------
! FUNCTION matvec (mat, v) result (r) !bind(c,name='matvec')
! !use iso_c_binding
! implicit none

! real(kind=c_double), INTENT(IN) :: mat(:,:)
! real(kind=c_double), INTENT(IN) :: v(:)
! real(kind=c_double) :: r(SIZE(mat,1))

! integer(c_int) :: i
! integer(c_int) :: m

! m = SIZE(v)

! r = 0.0       !! clear whole vector
! DO i = 1, m
!     r = r + v(i) * mat( :, i )
! END DO
! END FUNCTION

! ! -----------------------
! ! Multiply Matrix * Matrix
! ! -----------------------
! FUNCTION mmult (matA, B) result (C) !bind(c,name='mmult')
! !use iso_c_binding
! implicit none

! real(kind=c_double), INTENT(IN) :: matA(:,:)
! real(kind=c_double), INTENT(IN) :: B(:,:)
! real(kind=c_double) :: C(size(matA,1), size(B, 2))    

! integer :: M, col, i, j

! M = size(matA,1)
! col = size(B,2)
! C=0
! !C(:,:) = 0.0       

! DO i = 1, M
!     DO j = 1, col
!     C(:,i) = C(:,i) + B(j,i)*matA(:,j)
!     END DO
! END DO
! END FUNCTION
! ! -----------------------
! ! Element Stiffness Matrix
! ! -----------------------
! function elem_stiff(nel) result(kelst) !bind(c,name='elem_stiff')
! !use iso_c_binding
! implicit none

! integer(kind=c_int),INTENT(IN) :: nel
! integer(kind=c_int) :: isec,imat
! real(kind=c_double) :: EIz,EAx,d,EIy,GIx
! real(kind=c_double),dimension(ndfel,ndfel) :: kelst 

! imat=elem_prop(nel)%mat_no
! isec=elem_prop(nel)%sec_no
! d=elem_prop(nel)%elem_len

! EAx=mat_table(imat)%Emod*sec_table(isec)%ax
! EIz=mat_table(imat)%Emod*sec_table(isec)%iz
! EIy=mat_table(imat)%Emod*sec_table(isec)%iy
! GIx=mat_table(imat)%Gmod*sec_table(isec)%ix
! !print *, imat,isec,d
! !allocate(kelst(ndfel,ndfel))
! kelst=0.0
! SELECT CASE (strutype )

! case('2DTruss')
!     kelst(1, 1) = EAx / D
!     kelst(2, 1) = 0.0;
!     kelst(1, 2) = -EAx / D
!     kelst(2, 2) = 0.0
! case('3dTruss')
!     kelst(1, 1) = EAx / D;
!     kelst(4, 1) = -EAx / D;
!     kelst(1, 4) = -EAx / D;
!     kelst(4, 4) = EAx / D;
! case('2DFrame')

!     kelst(1,1) =EAx/d
!     kelst(1,4) =-kelst(1,1)
!     kelst(2,2) =12.*EIz/(d**3)
!     kelst(2,3) =6.*EIz/(d*d)
!     kelst(2,5) =-kelst(2,2)
!     kelst(2,6) =kelst(2,3)
!     kelst(3,2) =kelst(2,3)
!     kelst(3,3) =4.*EIz/d
!     kelst(3,5) =-kelst(2,3)
!     kelst(3,6) =2.*EIz/d
!     kelst(4,1) =kelst(1,4)
!     kelst(4,4) =kelst(1,1)
!     kelst(5,2) =kelst(2,5)
!     kelst(5,3) =kelst(3,5)
!     kelst(5,5) =kelst(2,2)
!     kelst(5,6) =kelst(3,5)
!     kelst(6,2) =kelst(2,6)
!     kelst(6,3) =kelst(3,6)
!     kelst(6,5) =kelst(5,6)
!     kelst(6,6) =kelst(3,3)
! case('3DFrame')
!         kelst(1, 1) = EAx / D                 !(0,0)
!         kelst(7, 1) = -EAx / D                !(6,0) 
!         kelst(2, 2) = 12.0 * EIz / D**3       !(1,1)
!         kelst(6, 2) = 6.0 * EIZ / D**2        !(5,1)
!         kelst(8, 2) = -12.0 * EIZ / D ** 3    !(7,1)
!         kelst(12, 2) = 6.0 * EIz / D ** 2     !(11,1)
!         kelst(3, 3) = 12.0 * EIy / D ** 3     !(2,2)
!         kelst(5, 3) = -6.0 * EIy / D ** 2     !(4,2)
!         kelst(9, 3) = -12.0 * EIy / D ** 3    !(8,2)
!         kelst(11, 3) = -6.0 * EIy / D ** 2    !(10,2)
!         kelst(4, 4) = GIx / D                 !(3,3)
!         kelst(10, 4) = -GIx / D               !(9,3)
!         kelst(3, 5) = -6.0 * EIy / D ** 2     !(2,4)
!         kelst(5, 5) = 4.0 * EIy / D           !(4,4)
!         kelst(9, 5) = 6.0 * EIy / D ** 2      !(8,4)
!         kelst(11, 5) = 2.0 * EIy / D          !(10,4)
!         kelst(2, 6) = 6.0 * EIz / D ** 2      !(1,5)
!         kelst(6, 6) = 4.0 * EIz / D           !(5,5)
!         kelst(8, 6) = -6.0 * EIz / D ** 2     !(7,5)
!         kelst(12, 6) = 2.0 * EIz / D          !(11,5)
!         kelst(1, 7) = -EAx / D                !(0,6)
!         kelst(7, 7) = EAx / D                 !(6,6)
!         kelst(2, 8) = -12.0 * EIz / D ** 3    !(1,7)
!         kelst(6, 8) = -6.0 * EIz / D ** 2     !(5,7)
!         kelst(8, 8) = 12.0 * EIz / D ** 3     !(7,7)
!         kelst(12, 8) = -6.0 * EIz / D ** 2    !(11,7)
!         kelst(3, 9) = -12.0 * EIy / D ** 3    !(2,8)
!         kelst(5, 9) = 6.0 * EIy / D ** 2      !(4,8)
!         kelst(9, 9) = 12.0 * EIy / D ** 3     !(8,8)
!         kelst(11, 9) = 6.0 * EIy / D ** 2     !(10,8)
!         kelst(4, 10) = -GIx / D               !(3,9)
!         kelst(10, 10) = GIx / D               !(9,9)
!         kelst(3, 11) = -6.0 * EIy / D ** 2    !(2,10)
!         kelst(5, 11) = 2.0 * EIy / D          !(4,10)
!         kelst(9, 11) = 6.0 * EIy / D ** 2     !(8,10)
!         kelst(11, 11) = 4.0 * EIy / D         !(10,10)
!         kelst(2, 12) = 6.0 * EIz / D ** 2     !(1,11)
!         kelst(6, 12) = 2.0 * EIz / D          !(5,11)
!         kelst(8, 12) = -6.0 * EIz / D ** 2    !(7,11)
!         kelst(12, 12) = 4.0 * EIz / D         !(11,11)   
! !    call printmatrix(kelst,"kelst")
! end select

! return
! end function elem_stiff
! ! -----------------------
! ! Element Rotation Matrix
! ! -----------------------
! function rotmatgen(nel) result(rot) !bind(c,name='rotmatgen')
! !use iso_c_binding
! implicit none

! REAL(kind=c_double)  :: rot(ndfel,ndfel)
! INTEGER(kind=c_int), INTENT(IN)                :: nel
! real(kind=c_double) :: dt,beta,cx,cy,cz,cxz
! integer(kind=c_int) :: i,j
! rot=0.0
! dt=elem_prop(nel)%elem_len !prop(kdsp+5)
! beta=elem_prop(nel)%beta!prop(kdsp+1)
! cx=elem_prop(nel)%dx/dt !prop(kdsp+6)/dt
! cy=elem_prop(nel)%dy/dt !prop(kdsp+7)/dt
! cz=elem_prop(nel)%dz/dt !prop(kdsp+8)/dt
! cxz=SQRT(cx*cx+cz*cz)
! SELECT CASE ( strutype )
!   CASE ('2DTruss')
!     rot(1,1)=cx
!     rot(1,2)=cy
!     rot(2,1)=-cy
!     rot(2,2)=cx
!   CASE ('3DTruss')
!     IF(cxz /= 0.) then
!       rot(1, 1) = cx;
!       rot(1, 2) = cy;
!       rot(1, 3) = cz;
!       rot(2, 1) = -cx * cy / CXZ;
!       rot(2, 2) = CXZ;
!       rot(2, 3) = -cy * cz / CXZ;
!       rot(3, 1) = -cz / CXZ;
!       rot(3, 2) = 0.0;
!       rot(3, 3) = cx / CXZ;
!     else if (CXZ == 0) then 
!       rot(1, 1) = 0.0;
!       rot(1, 2) = cy;
!       rot(1, 3) = 0.0;
!       rot(2, 1) = -cy;
!       rot(2, 2) = 0.0;
!       rot(2, 3) = 0.0;
!       rot(3, 1) = 0.0;
!       rot(3, 2) = 0.0;
!       rot(3, 3) = 1.0;      
!     end if
!   CASE ('2DFrame')
!     rot(1,1)=cx
!     rot(1,2)=cy
!     rot(2,1)=-cy
!     rot(2,2)=cx
!     rot(3,3)=1.0
!   CASE ('3DFrame')
 
!     IF(cxz /= 0.) then 
!     rot(1,1)=cx
!     rot(1,2)=cy
!     rot(1,3)=cz
!     rot(2,1)=(-cx*cy*COS(beta)-cz*SIN(beta)) /cxz
!     rot(2,2)=cxz*COS(beta)
!     rot(2,3)=(-cy*cz*COS(beta)+cx*SIN(beta))/cxz
!     rot(3,1)=(cx*cy*SIN(beta)-cz*COS(beta))/cxz
!     rot(3,2)=-cxz*SIN(beta)
!     rot(3,3)=(cy*cz*SIN(beta)+  cx*COS(beta))/cxz
!     else
!     rot(1,2)=cy
!     rot(2,1)=-cy*COS(beta)
!     rot(2,3)=SIN(beta)
!     rot(3,1)=cy*SIN(beta)
!     rot(3,3)=COS(beta)
!     end if    
! END SELECT

! DO  i=1,ndf
!   DO  j=1,ndf
!     rot(i+ndf,j+ndf)=rot(i,j)
!   END DO
! END DO

! END function rotmatgen

! ! -----------------------
! ! Assembling Global Stiffness Matrix
! ! -----------------------
! SUBROUTINE elassgen(elst,con) bind(c,name='elassgen')
! use iso_c_binding
! !use structvarsgen
! IMPLICIT NONE
! !NEL =number of the current node
! !N1 =number of the start node
! !N2 =number of the END node
! ! -----------------------------------------------------------
! !INTEGER(c_int), intent(in) :: nne,ndf
! INTEGER(kind=c_int) :: n1,n2,kc,kr,i1,i2,i,j1,j,j2,k,ic9,k2,k1, ki,l
! integer(kind=c_int),intent(in) :: con(:)
! real(kind=c_double),intent(inout) :: elst(:,:)
 
! DO  i=1,nne
!   n1 =con(i)
!   i1 =ndf*(i-1)
!   j1 =ndf*(n1-1)
!   DO  j=i,nne
!     n2 =con(j)
!     i2 =ndf*(j-1)
!     j2 =ndf*(n2-1)
!     DO  k=1,ndf
!       ki=1
!       IF(n1-n2 < 0) THEN
!         GO TO    20
!       ELSE IF (n1-n2 == 0) THEN
!         GO TO    10
!       ELSE
!         GO TO    30
!       END IF
! !---STORE A DIAGONAL SUBMATRIX
!       10     ki=k
! !---STORE AN OFF DIAGONAL SUBMATRIX
!       20    kr=j1+k
!       ic9=j2-kr+1
!       k1=i1+k
!       GO TO 40
! !---STORE THE TRANSPOSE OF AN OFF DIAGONAL MATRIX
!       30    kr=j2+k
!       ic9=j1-kr+1
!       k2=i2+k
!       40    DO  l=ki,ndf
!         kc=ic9+l
!         IF(n1-n2 > 0) THEN
!           GO TO    46
!         END IF
!         k2=i2+l
!         GO TO 51
!         46    k1=i1+l
!         51    CONTINUE
!         tk(kr,kc)=tk(kr,kc)+elst(k1,k2)
!       END DO
!     END DO
!   END DO
! END DO
! !write(fileout_unit,'("Subroutine ELASS: ",i5)')nel
! RETURN
! END SUBROUTINE elassgen

! SUBROUTINE dloadgen() bind(c,name='dloadgen')
! use iso_c_binding
! !use structvarsgen
! implicit none
! INTEGER(kind=c_int) :: klc

! integer(kind=c_int) :: mn,j,isec,imat,inc1,inc2,kdsp1,kdsp2
! real(kind=c_double) :: f(ndfel),vlocal(ndfel),vglob(ndfel),wload,ra,rma,dl
! real(kind=c_double) :: rot(ndfel,ndfel)


! totalwht=0.0
! DO  mn=1,NE
!   rot=0.0
!   fem_dload(mn)%fem=0.0
!   isec=elem_prop(mn)%sec_no
!   imat=elem_prop(mn)%mat_no
!   inc1=elem_prop(mn)%inc1
!   inc2=elem_prop(mn)%inc2
!   dl=elem_prop(mn)%elem_len 
!   wload=sec_table(isec)%ax*mat_table(imat)%matden 


! !---- COMPUTE MOMENTS VECTOR ORIENTATION (Cross Product)---
!   totalwht=totalwht+wload*dl

!   kdsp1=ndf*(inc1-1) 
!   kdsp2=ndf*(inc2-1) 
!   ra=wload*dl/2.
!   rma=wload*dl*dl/12.

!   IF(strutype == '3DFrame') then 
!     f(1)=0.0; f(2)=-1.0; f(3)=0.0 ;f(4)=0.0; f(5)=0.0 ; f(6)=0.0
!     f(7)=0.0; f(8)=-1.0; f(9)=0.0 ;f(10)=0.0; f(11)=0.0 ; f(12)=0.0 
!     rot=rotmatgen(mn)
!     vlocal=rot .mv. f
!     vlocal(1)=ra*vlocal(1)
!     vlocal(2)=ra*vlocal(2)   
!     vlocal(3)=ra*vlocal(3)
!     vlocal(4)=-rma*vlocal(4)
!     vlocal(5)=-rma*vlocal(5)
!     vlocal(6)=-rma*vlocal(6)
!     vlocal(7)=ra*vlocal(7)
!     vlocal(8)=ra*vlocal(8)
!     vlocal(9)=ra*vlocal(9)
!     vlocal(10)=rma*vlocal(10)
!     vlocal(11)=rma*vlocal(11)
!     vlocal(12)=rma*vlocal(12)
!   else if(strutype == '2DFrame') then
!     f(1)=0.0; f(2)=-1.0; f(3)=0.0 ;f(4)=0.0; f(5)=-1.00 ; f(6)=0.0! dload unit Y vector    
!     rot=rotmatgen(mn)
!     vlocal=rot .mv. f
!     vlocal(1)=ra*vlocal(1)
!     vlocal(2)=ra*vlocal(2)    
!     vlocal(3)=-rma*vlocal(3)
!     vlocal(4)=ra*vlocal(4)
!     vlocal(5)=ra*vlocal(5)
!     vlocal(6)=rma*vlocal(6)
!   end if 
  
!   vglob=transpose(rot) .mv. vlocal
!   fem_dload(mn)%fem(1:ndfel)=fem_dload(mn)%fem(1:ndfel)-vlocal(1:ndfel)
!   do klc=1,nlc
!     DO  j=1,ndf
!       al(kdsp1+j,klc)=al(kdsp1+j,klc)+vglob(j)
!       al(kdsp2+j,klc)=al(kdsp2+j,klc)+vglob(j+ndf)
!     END DO
!   end do
! END DO
! RETURN
! END SUBROUTINE dloadgen

! SUBROUTINE forcegen() bind(c,name='forcegen')
! use iso_c_binding
! !use structvarsgen
! IMPLICIT NONE

! REAL(kind=c_double) :: u(ndfel),f(ndfel),ul(ndfel),fg(ndfel)
! REAL(kind=c_double) :: rot(ndfel,ndfel),elst(ndfel,ndfel)
! INTEGER(kind=c_int) :: klc,n1,n2,lc,nel,k1,k2,j1,j2,i,i1,i2,j

! lc=size(al,2)

! reac=0.0
! DO klc=1,lc
!   DO nel=1,NE
!     rot=0.0
!     n1=elem_prop(nel)%inc1
!     n2=elem_prop(nel)%inc2

!     rot=rotmatgen(nel)
!     !call printmatrix(rot,"rot")
!     ! do i=1,ndfel
!     !   write(fileout_unit,'(*(f15.2))') (rot(i,j),j=1,ndfel)
!     ! enddo
!     k1=ndf*(n1-1)
!     k2=ndf*(n2-1)
!     DO  i=1,ndf
!       j1=k1+i
!       j2=k2+i
!       u(i)=al(j1,klc)
!       u(i+ndf)=al(j2,klc)
!     END DO

!     ul=rot .mv. u
!     ! write(fileout_unit,'(*(2Pf15.2))') (u(i),i=1,ndfel)   
!     ! write(fileout_unit,'(*(2Pf15.2))') (ul(i),i=1,ndfel)
! !-------------------------------     COMPUTE MEMBER END FORCES IN LOCAL COORDINATES
!     elst= elem_stiff(nel)
!     f=elst .mv. ul
!     !f=matmul(elst, ul)
! !--------------------------------------- STORE MEMBER END FORCES IN ARRAY FORCE
!     i1=ndfel*(nel-1)+NE*ndfel*(klc-1)
!     DO  i=1,ndfel
!       i2=i1+i
!       intforc(i2)=f(i)
!     END DO
! !-------------    ROTATE MEMBER FORCES TO THE GLOBAL REFERENCE FRAME AND STORE IN ARRAY FG
!     f(1:ndfel)=f(1:ndfel)+fem_dload(nel)%fem(1:ndfel)
    
!     fg=transpose(rot) .mv. f
!     !fg=matmul(transpose(rot), f)
!     ! write(fileout_unit,'(*(f15.2))') (fg(i),i=1,ndfel)
!     ! write(fileout_unit,'(*(f15.2))') (f(i),i=1,ndfel)
!     ! write(fileout_unit,'(80("-"))')          
! !-----------------     ADD ELEMENT CONTRIBUTION TO NODAL RESULTANTS IN ARRAY REAC
!     DO  i=1,ndf
!       j1=k1+i
!       j2=k2+i
!       reac(j1,klc)=reac(j1,klc)+fg(i)
!       reac(j2,klc)=reac(j2,klc)+fg(i+ndf)
!     END DO
!   END DO
! END DO

! RETURN
! END SUBROUTINE forcegen

! SUBROUTINE boundgen() bind(c,name='boundgen')
! use iso_c_binding
! !use structvarsgen
! IMPLICIT NONE
! !---INTRODUCTION OF THE BOUNDARY CONDITIONS
! INTEGER(kind=c_int) :: l1,no,l2,k1,i,kr,kv,j,l,icol
! nlc=size(reac,2)

! DO  l=1,nbn
  
! !---NO=NUMBER OF THE CURRENT BOUNDARY
  
!   l1=(ndf+1)*(l-1)+1
!   no=ib(l1)
!   k1=ndf*(no-1)
!   DO  i=1,ndf
!     l2=l1+i
!     IF(ib(l2) == 0) THEN
!       GO TO    10
!     ELSE
!       cycle!GO TO   100
!     END IF
    
! !---SET DIAGONAL COEFFICIENT OF TK EQUAL TO 1 AND PLACE PRESCRIBED VALUE IN AL
!     10      kr=k1+i
!     DO  j=2,ms
!       kv=kr+j-1
!       IF(n-kv < 0) THEN
!         GO TO    30
!       END IF
! !---MODIFY ROW OF TK AND CORRESPONDING ELEMENTS IN AL
!       DO  icol=1,nlc
!         al(kv,icol)=al(kv,icol)-tk(kr,j)*reac(kr,icol)
!       END DO
!       tk(kr,j)=0.
!       30      kv=kr-j+1
!       if(kv<=0) cycle
      
! !---MODIFY COLUMN IN TK AND CORRESPONDING ELEMENT IN AL
!       do icol=1,nlc
!         al(kv,icol)=al(kv,icol)-tk(kv,j)*reac(kr,icol)
!       END DO
!       tk(kv,j)=0.
!     END DO
!     tk(kr,1)=1.
!     DO  icol=1,nlc
!       al(kr,icol)=reac(kr,icol)
!     END DO
!   END DO
! END DO
! RETURN
! END SUBROUTINE boundgen

! SUBROUTINE mfem(klc,f) bind(c,name='mfem')
! use iso_c_binding
! !use structvarsgen
! implicit none

! real(kind=c_double) ::  vlocal(ndfel),vglob(ndfel),rot(ndfel,ndfel)
! real(kind=c_double) :: wa,wb,a,dl,ra,rb,rma,rmb
! integer(kind=c_int) :: n1,n2,kdsp1,kdsp2,mn,i,j
! integer(kind=c_int),INTENT(IN) :: klc
! real(kind=c_double),INTENT(IN) :: f(:)


! DO  i=1,nlmem
!   mn=trap_load(i)%mem_no 
!   wa=trap_load(i)%wa 
!   wb=trap_load(i)%wb 
!   a=trap_load(i)%a 

!   n2=elem_prop(mn)%inc2 
!   dl=elem_prop(i)%elem_len 
 
! !---- COMPUTE MOMENTS VECTOR ORIENTATION ---

!   n1=elem_prop(mn)%inc1 !nne*(mn-1)
!   kdsp1=ndf*(n1-1)
!   kdsp2=ndf*(n2-1)
!   ra=wa*((dl-a)**3)*(dl+a)/(2.*dl**3)
!   rb=((wa+wb)*(dl-a)/2.)- ra
!   rma=wa*((dl-a)**3)*(dl+3.*a)/(12.*dl*dl)
!   rmb=(ra*dl)-((wa*(dl-a)**2)/2.)-((wb-wa)*(dl-a)*(dl-a)/6.)+rma

!   IF(strutype == '3DFrame') then !GO TO 12
!     vlocal=rot .mv. f
!     vlocal(1)=ra*vlocal(1)
!     vlocal(2)=ra*vlocal(2)   
!     vlocal(3)=ra*vlocal(3)
!     vlocal(4)=-rma*vlocal(4)
!     vlocal(5)=-rma*vlocal(5)
!     vlocal(6)=-rma*vlocal(6)
!     vlocal(7)=ra*vlocal(7)
!     vlocal(8)=ra*vlocal(8)
!     vlocal(9)=ra*vlocal(9)
!     vlocal(10)=rmb*vlocal(10)
!     vlocal(11)=rmb*vlocal(11)
!     vlocal(12)=rmb*vlocal(12)
!   else if(strutype == '2DFrame') then!GO TO 41
!     rot=rotmatgen(mn)
!     vlocal=rot .mv. f
!     vlocal(1)=ra*vlocal(1)
!     vlocal(2)=ra*vlocal(2)    
!     vlocal(3)=-rma
!     vlocal(4)=ra*vlocal(4)
!     vlocal(5)=ra*vlocal(5)
!     vlocal(6)=rmb
!   end if !41    CONTINUE
!   vglob=transpose(rot) .mv. vlocal
!   fem_dload(mn)%fem(1:ndfel)=fem_dload(mn)%fem(1:ndfel)-vlocal(1:ndfel)
!   DO  j=1,ndf
!     al(kdsp1+j,klc)=al(kdsp1+j,klc)+vglob(j)
!     al(kdsp2+j,klc)=al(kdsp2+j,klc)+vglob(j+ndf)
!   END DO
! END DO
! RETURN
! END SUBROUTINE mfem

! SUBROUTINE outptgen() !bind(c,name='outptgen')
! !use iso_c_binding
! use iso_fortran_env
! !use structvarsgen, nodename => nodestring
! IMPLICIT NONE
! !---------------------------------------------------------------------
! !       PROGRAM TO OUTPUT JOINT DISPLACEMENT,NODAL REACTION
! !       AND MEMBER FORCES
! !-----------------------------------------------------------------

! INTEGER(kind=c_int) :: k,k2,k1,klc
! INTEGER(kind=c_int) :: i,j,nel,j1,l1,no,n1
! !CHARACTER*80 :: line
! CHARACTER(kind=c_char,len=8), DIMENSION(4,2) ::  dat


! nlc=size(al,2)
! ndfel=nne*ndf

! dat(1,1)="kN"
! dat(1,2)="m"

! CALL header()
! IF(kiter > 0) WRITE(fileout_unit,'("Number of Iterations ",i5)') kiter

! DO  klc=1,nlc
!   WRITE(fileout_unit,14) klc,dat(slen,1),dat(kip,2)

! 14      FORMAT('NODAL DISPLACEMENTS FOR LOADING',i3,2X,/,'ACTIVE UNITS :',2A8)
!   IF(strutype == '3DFrame') WRITE(fileout_unit,13)
! 13     FORMAT('NODE',7X,'DX',10X,'DY',10X,'DZ',10X,'ROTX',10X,'ROTY',10X,'ROTZ',/)
!   IF(strutype == '2DFrame') WRITE(fileout_unit,41)
!   41    FORMAT('NODE',15X,'DX',10X,'DY',10X,'ROTZ',/)
!   DO  i=1,nn
!     k1=ndf*(i-1)+1
!     k2=k1+ndf-1
!     DO  j=k1,k2
!       j1=j-ndf*(i-1)
!       al(j,klc)=al(j,klc)
!     END DO
!     WRITE(fileout_unit,'(a10,6g15.4)') nodestring(i),(al(j,klc),j=k1,k2)
!   END DO
! END DO

! DO  klc=1,nlc
!   WRITE(fileout_unit,3) klc,dat(kip,1),dat(slen,2)
! 3       FORMAT('NODAL REACTION FOR LOADING',i3,2X,/,'ACTIVE UNITS :',2A8,/)
!   IF(strutype == '3DFrame') WRITE(fileout_unit,42)
! 42    FORMAT('NODE',5X,'PX',10X,'PY',10X,'PZ',10X,'MX',10X,'MY',10X,'MZ',/)
!   IF(strutype == '2DFrame') WRITE(fileout_unit,43)
!   43    FORMAT('NODE',15X,'PX',10X,'PY',15X,'MZ')
!   DO  i=1,nbn
!     l1=(ndf+1)*(i-1)+1
!     no=ib(l1)
!     k1=ndf*(no-1)+1
!     k2=k1+ndf-1
!     WRITE(fileout_unit,'(a10,6F15.2)') nodestring(no),(reac(j,klc),j=k1,k2)
!   END DO
! END DO

! write(fileout_unit,'(a15,f15.2,a5)')'Total Weight: ',totalwht,' kN'

! !---OUTPUT MEMBER END FORCES
! WRITE(fileout_unit,4) dat(slen,1),dat(kip,2)
! 4       FORMAT('MEMBER END FORCES',2X,/,'ACTIVE UNITS :',2A8,/)
! IF(strutype == '3DFrame') WRITE(fileout_unit,44)
! 44    FORMAT('LC',5X,'MEMBER',2X,'NODE',5X,'FX',10X,'FY',10X,  &
!     'FZ',10X,'MX',10X,'MY',10X,'MZ')
! IF(strutype == '2DFrame') WRITE(fileout_unit,45)
! 45    FORMAT('LC',5X,'MEMBER',2X,'NODE',15X,'FX',15X,'FY',10X, 'MZ',/)
! DO  klc=1,nlc
!   DO  nel=1,NE
!     k1=ndfel*(nel-1)+1+NE*ndfel*(klc-1)
!     k2=k1+2
!     n1=nne*(nel-1)
!     WRITE(fileout_unit,'(i10,5X,a10,3F15.2)') nel,nodestring(elem_prop(nel)%inc1),(intforc(k),k=k1,k2)  
!     k1=k2+1
!     k2=k1+2
!     WRITE(fileout_unit,'(i2,13X,a10, 3F15.2)') klc,nodestring(elem_prop(nel)%inc2),(intforc(k),k=k1,k2)    
!   END DO
!   WRITE(fileout_unit,'(80("-"))')
! END DO
! CALL time_now()
! RETURN
! END SUBROUTINE outptgen

! SUBROUTINE bgaussgen() !bind(c,name='bgaussgen')
! !use iso_c_binding
! !use structvarsgen, only:printmatrix,fileout_unit,n,nlc,kerr,ms,a =>tk, b => al
! use iso_fortran_env
! use ieee_exceptions
! IMPLICIT NONE

! !---N:     ROW DIMENSION OF A AND B
! !---MS:     COLUMN DIMENSION OF A
! !---LC:     COLUMN DIMENSION OF B
! !---D:      AUXILIARY VECTOR
! REAL(kind=c_double),  allocatable :: d(:)
! REAL(kind=c_double) :: c
! INTEGER(kind=c_int) :: n1,k,l,ni,k1,k2,j,icol,i,k3
! !logical :: isnan
! allocate(d(n))

! kerr=.false.
! n1=n-1
! DO  k=1,n1
!   c=tk(k,1)
!   k1=k+1
!   IF(ABS(c)-.000001 <= 0.0) THEN 
!     WRITE(fileout_unit,'("**** SINGULARITY IN ROW",i5,1X,"****")') k
!     kerr=.true.
!     call printmatrix(tk,"Stiff")
!     return
!   END IF  
! !---DIVIDE ROW BY DIAGONAL COEFFICIENT
  
!   ni=k1+ms-2
!   l=MIN0(ni,n)
!   DO  j=2,ms
!     d(j)=tk(k,j)
!   END DO
!   DO  j=k1,l
!     k2=j-k+1
!     tk(k,k2)=tk(k,k2)/c
!   END DO
!   DO  icol=1,nlc
!     al(k,icol)=al(k,icol)/c
!   END DO
  
! !       ELIMINATE UNKNOWN X(K) FROM ROW I
  
!   DO  i=k1,l
!     k2=i-k1+2
!     c=d(k2)
!     DO  j=i,l
!       k2=j-i+1
!       k3=j-k+1
!       tk(i,k2)=tk(i,k2)-c*tk(k,k3)
!     END DO
!     DO  icol=1,nlc
!       al(i,icol)=al(i,icol)-c*al(k,icol)
!     END DO
!   END DO
! END DO

! !---COMPUTE LAST UNKNOWN

! IF(ABS(tk(n,1))-.000001 <= 0.0) THEN
!   WRITE(fileout_unit,'("**** SINGULARITY IN ROW",i5,1X,"****")') n
!   kerr=.false.
!   call printmatrix(tk,"Stiff")  
! !  call exit(1)
! END IF
! DO  icol=1,nlc
!   al(n,icol)=al(n,icol)/tk(n,1)
! END DO

! !---APPLY BACKSUBSTITUTION PROCESS TO COMPUTE REMAINING UNKNOWNS

! DO  i=1,n1
!   k=n-i
!   k1=k+1
!   ni=k1+ms-2
!   l=MIN0(ni,n)
!   DO  j=k1,l
!     k2=j-k+1
!     DO  icol=1,nlc
!       al(k,icol)=al(k,icol)-tk(k,k2)*al(j,icol)
!     END DO
!   END DO
! END DO
! kerr=.true.
! deallocate(d)

! RETURN
! END SUBROUTINE bgaussgen

end module structvarsgen
