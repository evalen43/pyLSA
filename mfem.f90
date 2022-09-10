SUBROUTINE mfem(f)
use structvarsgen
implicit none

real(kind=8) ::  vlocal(ndfel),vglob(ndfel),rot(ndfel,ndfel)
real(kind=8) :: wa,wb,a,dl,ra,rb,rma,rmb,p
integer(kind=4) :: n1,n2,kdsp1,kdsp2,mn,i,j,mltype
integer(kind=4),INTENT(IN) :: klc
real(kind=8),INTENT(IN), dimension(:) :: f


DO  i=1,nlmem
  mltype=fem_param(i,1)
  klc=fem_param(i,2)
  mn=mfem_param(i,3)!%mem_no
  if(mltype==1) then
    wa=mfem_param(i,4)!%wa 
    wb=mfem_param(i,5)!%wb 
    a=mfem_param(i,6)!%a
  elseif(mltype==2) then
    p=fem_param(i,4)
    a=mfem_param(i,5)
  endif

n1=elem_prop(mn,1)!%inc1 !nne*(mn-1)
n2=elem_prop(mn,2)!%inc2 
dl=elem_prop(i,5)!%elem_len 

!---- COMPUTE MOMENTS VECTOR ORIENTATION ---

  kdsp1=ndf*(n1-1)
  kdsp2=ndf*(n2-1)
  if(mltype==1) then
    ra=wa*((dl-a)**3)*(dl+a)/(2.*dl**3)
    rb=((wa+wb)*(dl-a)/2.)- ra
    rma=wa*((dl-a)**3)*(dl+3.*a)/(12.*dl*dl)
    rmb=(ra*dl)-((wa*(dl-a)**2)/2.)-((wb-wa)*(dl-a)*(dl-a)/6.)+rma
  elseif(mltype==2) then
    ra=0
    rb=0
    rma=0
    rmb=0
  endif  

  IF(strutype == '3DFrame') then !GO TO 12
    vlocal=rot .mv. f
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
  else if(strutype == '2DFrame') then!GO TO 41
    rot=rotmatgen(mn)
    vlocal=rot .mv. f
    vlocal(1)=ra*vlocal(1)
    vlocal(2)=ra*vlocal(2)    
    vlocal(3)=-rma
    vlocal(4)=ra*vlocal(4)
    vlocal(5)=ra*vlocal(5)
    vlocal(6)=rmb
  end if !41    CONTINUE
  mkdsp=ne*(nlc-1)
  vglob=transpose(rot) .mv. vlocal
  mfem_load(mkdsp+mn,1:ndfel)=mfem_dload(mkdsp+mn,1:ndfel)-vlocal(1:ndfel)
  DO  j=1,ndf
    al(kdsp1+j,klc)=al(kdsp1+j,klc)+vglob(j)
    al(kdsp2+j,klc)=al(kdsp2+j,klc)+vglob(j+ndf)
  END DO
END DO
RETURN
END SUBROUTINE mfem