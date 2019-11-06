program oldctonew
implicit none

integer i,n,m,ier,ninp,var,k
real nl, nls, nF, nD, nOm, pi,q
real(8), allocatable, dimension(:,:) ::  al , b
real(8), allocatable, dimension(:) ::  P1
integer, allocatable, dimension(:) ::  l,ls,F,D,Om
integer, allocatable, dimension(:) ::  moon, sun, omega, w

pi= 4* atan(1.0)

nl=1717915923.2178
nls=129596581.0481
nF=1739527262.8478
nD=1602961601.2090
nOm=-6962890.5431

!кол-во столбцов и строк
n=1600
m=17
k=4



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
allocate(b(n,k), stat=ier)
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


write(*,*) "Введите: 1 - от старых к новым, 2 - новые к старым"
read(*,*) var


select case (var)

case(1)

open(11,file='tab5_2a.txt',status='old',form='formatted')
open(12,file='tab5_2a.new.txt', form='formatted')


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
   moon(i) = F(i)+Om(i) 
   sun(i) = F(i) + Om(i) +D(i)
   omega(i) = Om(i)
   w(i) = D(i)+F(i)-ls(i)
enddo



do i=1,n
   P1(i)=(pi*2*206265*100)/((l(i)*nl+ls(i)*nls+F(i)*nF+D(i)*nD+Om(i)*nOm))
enddo


do i=1,n
  write(12,*) moon(i), sun(i), omega(i), w(i)
enddo

case(2)

open(22,file='tab5_2a.new.txt',status= 'old', form='formatted')
open(13,file='tab5_2a.test.txt',form='formatted')


   do i=1,n,1
      read(22,*) b(i,:)
!      write(*,*) al(i,:)
   enddo

moon(:)= int (b(:,1))
sun(:)=int (b(:,2))
omega(:)=int (b(:,3))
w(:)=int (b(:,4))

do i=1,n,1
   l(i) = moon(i) - omega(i) - w(i)
   ls(i) =  sun(i) - omega(i) - w(i)
   F(i) = moon(i)-omega(i)
   D(i) = - moon(i) + sun(i)
   Om(i) = omega(i)
enddo

do i=1,n
  write(13,*) l(i), ls(i), F(i), D(i), Om(i)
enddo

end select


end

