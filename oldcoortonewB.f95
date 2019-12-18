program oldcoortonewB
implicit none

integer i,Numstr,Numcol,ier,ninp,var,k,Numpl,Numampl
real nl, nls, nF, nD, nOm, pi,q
real, allocatable, dimension(:,:) ::  al , b, ampl
real, allocatable, dimension(:) ::  P1
integer, allocatable, dimension(:,:) ::  pl
integer, allocatable, dimension(:) ::  l,ls,F,D,Om, ind
integer, allocatable, dimension(:) ::  moon, sun, omega, w_sun, w_moon

pi= 4* atan(1.0)

nl=1717915923.2178
nls=129596581.0481
nF=1739527262.8478
nD=1602961601.2090
nOm=-6962890.5431

!кол-во столбцов и строк
Numstr=1275
Numcol=17
k=5
Numampl=2
Numpl=9


allocate(ampl(Numstr,Numampl), stat=ier)
if (ier/=0) stop
allocate(pl(Numstr,Numpl), stat=ier)
if (ier/=0) stop
allocate(moon(Numstr), stat=ier)
if (ier/=0) stop
allocate(sun(Numstr), stat=ier)
if (ier/=0) stop
allocate(omega(Numstr), stat=ier)
if (ier/=0) stop
allocate(w_moon(Numstr), stat=ier)
if (ier/=0) stop
allocate(w_sun(Numstr), stat=ier)
if (ier/=0) stop
allocate(ind(Numstr), stat=ier)
if (ier/=0) stop
allocate(al(Numstr,Numcol), stat=ier)
if (ier/=0) stop
allocate(b(Numstr,(Numcol)), stat=ier)
if (ier/=0) stop


allocate(l(Numstr), stat=ier)
if (ier/=0) stop
allocate(ls(Numstr), stat=ier)
if (ier/=0) stop
allocate(F(Numstr), stat=ier)
if (ier/=0) stop
allocate(D(Numstr), stat=ier)
if (ier/=0) stop
allocate(Om(Numstr), stat=ier)
if (ier/=0) stop
allocate(P1(Numstr), stat=ier)
if (ier/=0) stop


write(*,*) "Введите: 1 - от старых к новым, 2 - новые к старым"
read(*,*) var


select case (var)

case(1)

open(11,file='tab5_2b.txt',status='old',form='formatted')
open(12,file='tab5_2b.new.txt', form='formatted')


   do i=1,Numstr
      read(11,*) al(i,:)
!      write(*,*) al(i,:)
   enddo

ind(:)= int (al(:,1))
ampl(:,:)= al(:, 2:3)
pl(:,:)=  int (al( :, 9:))
l(:)= int (al(:,4))
ls(:)=int (al(:,5))
F(:)=int (al(:,6))
D(:)=int (al(:,7))
Om(:)= int (al(:,8))

do i=1,Numstr,1
   moon(i) = F(i)+D(i)+l(i)
   sun(i) = ls(i) -D(i)
   omega(i) = Om(i) - F(i) - l(i)
   w_moon(i) = -l(i)
   w_sun(i)= -ls(i)
enddo



do i=1,Numstr
   P1(i)=(pi*2*206265*100)/((l(i)*nl+ls(i)*nls+F(i)*nF+D(i)*nD+Om(i)*nOm))
enddo


do i=1,Numstr
  write(12,55) ind(i),  ampl(i,:) , moon(i), sun(i), omega(i), w_moon(i), w_sun(i), pl(i,:)
enddo


55 format(i5,2f15.2,14i5)

case(2)

open(22,file='tab5_2b.new.txt',status= 'old', form='formatted')
open(13,file='tab5_2b.test.txt',form='formatted')


   do i=1,Numstr,1
      read(22,*) b(i,:)
!      write(*,*) al(i,:)
   enddo

moon(:)= int (b(:,4))
sun(:)=int (b(:,5))
omega(:)=int (b(:,6))
w_moon(:)=int (b(:,7))
w_sun(:)=int (b(:,8))

do i=1,Numstr,1
   l(i) = -w_moon(i)
   ls(i) =  -w_sun(i)
   F(i) = moon(i)+ sun(i) + w_moon(i) + w_sun(i)
   D(i) = -sun(i) - w_sun(i)
   Om(i) = moon(i) + sun(i) + omega(i) + w_sun(i)
enddo


do i=1,Numstr
  write(13,*) l(i), ls(i), F(i), D(i), Om(i)
enddo

end select


end

