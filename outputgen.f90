SUBROUTINE outptgen()
use iso_fortran_env
!use structvarsgen, nodename => nodestring
IMPLICIT NONE
!---------------------------------------------------------------------
!       PROGRAM TO OUTPUT JOINT DISPLACEMENT,NODAL REACTION
!       AND MEMBER FORCES
!-----------------------------------------------------------------

INTEGER(kind=4) :: k,k2,k1,klc
INTEGER(kind=4) :: i,j,nel,j1,l1,no,n1
!CHARACTER*80 :: line
CHARACTER(LEN=8), DIMENSION(4,2) ::  dat


nlc=size(al,2)
ndfel=nne*ndf

dat(1,1)="kN"
dat(1,2)="m"

CALL header()
IF(kiter > 0) WRITE(fileout_unit,'("Number of Iterations ",i5)') kiter

DO  klc=1,nlc
  WRITE(fileout_unit,14) klc,dat(slen,1),dat(kip,2)

14      FORMAT('NODAL DISPLACEMENTS FOR LOADING',i3,2X,/,'ACTIVE UNITS :',2A8)
  IF(strutype == 'Frame3D') WRITE(fileout_unit,13)
13     FORMAT('NODE',7X,'DX',10X,'DY',10X,'DZ',10X,'ROTX',10X,'ROTY',10X,'ROTZ',/)
  IF(strutype == 'Frame2D') WRITE(fileout_unit,41)
  41    FORMAT('NODE',15X,'DX',10X,'DY',10X,'ROTZ',/)
  DO  i=1,nn
    k1=ndf*(i-1)+1
    k2=k1+ndf-1
    DO  j=k1,k2
      j1=j-ndf*(i-1)
      al(j,klc)=al(j,klc)
    END DO
    WRITE(fileout_unit,'(a10,6g15.4)') nodelist(i),(al(j,klc),j=k1,k2)
  END DO
END DO

DO  klc=1,nlc
  WRITE(fileout_unit,3) klc,dat(kip,1),dat(slen,2)
3       FORMAT('NODAL REACTION FOR LOADING',i3,2X,/,'ACTIVE UNITS :',2A8,/)
  IF(strutype == 'Frame3D') WRITE(fileout_unit,42)
42    FORMAT('NODE',5X,'PX',10X,'PY',10X,'PZ',10X,'MX',10X,'MY',10X,'MZ',/)
  IF(strutype == 'Frame2D') WRITE(fileout_unit,43)
  43    FORMAT('NODE',15X,'PX',10X,'PY',15X,'MZ')
  DO  i=1,nbn
    l1=(ndf+1)*(i-1)+1
    no=ib(l1)
    k1=ndf*(no-1)+1
    k2=k1+ndf-1
    WRITE(fileout_unit,'(a10,6F15.2)') nodelist(no),(reac(j,klc),j=k1,k2)
  END DO
END DO

write(fileout_unit,'(a15,f15.2,a5)')'Total Weight: ',totalwht,' kN'

!---OUTPUT MEMBER END FORCES
WRITE(fileout_unit,4) dat(slen,1),dat(kip,2)
4       FORMAT('MEMBER END FORCES',2X,/,'ACTIVE UNITS :',2A8,/)
IF(strutype == 'Frame3D') WRITE(fileout_unit,44)
44    FORMAT('LC',5X,'MEMBER',2X,'NODE',5X,'FX',10X,'FY',10X,  &
    'FZ',10X,'MX',10X,'MY',10X,'MZ')
IF(strutype == 'Frame2D') WRITE(fileout_unit,45)
45    FORMAT('LC',5X,'MEMBER',2X,'NODE',15X,'FX',15X,'FY',10X, 'MZ',/)
DO  klc=1,nlc
  DO  nel=1,NE
    k1=ndfel*(nel-1)+1+NE*ndfel*(klc-1)
    k2=k1+2
    n1=nne*(nel-1)
    WRITE(fileout_unit,'(i10,5X,a10,3F15.2)') nel,nodelist(elem_prop(nel)%inc1),(intforc(k),k=k1,k2)  
    k1=k2+1
    k2=k1+2
    WRITE(fileout_unit,'(i2,13X,a10, 3F15.2)') klc,nodelist(elem_prop(nel)%inc2),(intforc(k),k=k1,k2)    
  END DO
  WRITE(fileout_unit,'(80("-"))')
END DO
CALL time_now()
RETURN
END SUBROUTINE outptgen

! -----------------------
! Prints date and time
! -----------------------

subroutine time_now()

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

subroutine header()
WRITE(fileout_unit,'(20x,A20)') PNAME
WRITE(fileout_unit,'(A80)') exampletitle
WRITE(fileout_unit,'(80("-"))')

end subroutine header


