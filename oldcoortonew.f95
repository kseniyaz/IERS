program oldctonew
implicit none

integer i,n,m,ier,ninp
real nl, nls, nF, nD, nOm
real(8), allocatable, dimension(:,:) ::  al 
real(8), allocatable, dimension(:) ::  P1
integer, allocatable, dimension(:) ::  l,ls,F,D,Om
integer, allocatable, dimension(:) ::  moon, sun, omega, w
data ninp / 11 /
open(unit=ninp,file='tab5.2a.txt',status='old',form='formatted')
open(12,file='tab5.2a.new.txt',status='new',form='formatted')

nl=1717915923.2178
nls=129596581.0481
nF=1739527262.8478
nD=1602961601.2090
nOm=-6962890.5431

!кол-во столбцов и строк
n=1600
m=17
allocate(moon(n), stat=ier)
if (ier/=0) stop
allocate(sun(n), stat=ier)
if (ier/=0) stop
allocate(omega(n), stat=ier)
if (ier/=0) stop
allocate(w(n), stat=ier)
if (ier/=0) stop
allocate(al(n,m), stat=ier)
if (ier/=0) stop
allocate(l(n), stat=ier)
if (ier/=0) stop
allocate(ls(n), stat=ier)
if (ier/=0) stop
allocate(F(n), stat=ier)
if (ier/=0) stop
allocate(D(n), stat=ier)
if (ier/=0) stop
allocate(Om(n), stat=ier)
if (ier/=0) stop
allocate(P1(n), stat=ier)
if (ier/=0) stop


   do i=1,n
      read(11,*) al(i,:)
!      write(*,*) al(i,:)
   enddo

l(:)= int (al(:,4))
ls(:)=int (al(:,5))
F(:)=int (al(:,6))
D(:)=int (al(:,7))
Om(:)= int (al(:,8))

do i=1,1600,1
   moon(i) = F(i)+D(i)+Om(i)
   sun(i) = -D(i)
   omega(i) = -l(i)
   w(i) = Om(i)-F(i)-l(i)
enddo


do i=1,n
   P1(i)=(3.1415926535897931*2*206265*100)/((l(i)*nl+ls(i)*nls+F(i)*nF+D(i)*nD+Om(i)*nOm))
enddo

write(12,*) 'Lambda_moon	Lambda_sun	omega   	w'
do i=1,n
  write(12,*) moon(i), sun(i), omega(i), w(i), P1(i)
enddo


end

