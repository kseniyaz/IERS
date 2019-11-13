program oldctonew
implicit none

integer i,Numstr,Numcol,ier,ninp,var,k
real pi,q
real, allocatable, dimension(:,:) ::  al , b
integer, allocatable, dimension(:) ::  l,ls,F,D,Om
integer, allocatable, dimension(:) ::  moon, sun, omega, w

pi= 4* atan(1.0)


!кол-во столбцов и строк
Numstr=1600
Numcol=17
k=5




allocate(moon(Numstr), stat=ier)
if (ier/=0) stop
allocate(sun(Numstr), stat=ier)
if (ier/=0) stop
allocate(omega(Numstr), stat=ier)
if (ier/=0) stop
allocate(w(Numstr), stat=ier)
if (ier/=0) stop

allocate(al(Numstr,Numcol), stat=ier)
if (ier/=0) stop
allocate(b(Numstr,k), stat=ier)
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


write(*,*) "Введите: 1 - от старых к новым, 2 - новые к старым"
read(*,*) var


select case (var)

case(1)

open(11,file='tab5_2a.txt',status='old',form='formatted')
open(12,file='tab5_2a.new.txt', form='formatted')


   do i=1,Numstr
      read(11,*) al(i,:)
!      write(*,*) al(i,:)
   enddo

l(:)= int (al(:,4))
ls(:)=int (al(:,5))
F(:)=int (al(:,6))
D(:)=int (al(:,7))
Om(:)= int (al(:,8))

do i=1,Numstr,1
   moon(i) = F(i)+Om(i) 
   sun(i) = F(i) + Om(i) +D(i)
   omega(i) = Om(i)
   w(i) = D(i)+F(i)-ls(i)
enddo



do i=1,Numstr
  write(12,*)  moon(i), sun(i), omega(i), w(i)
enddo



case(2)

open(22,file='tab5_2a.new.txt',status= 'old', form='formatted')
open(13,file='tab5_2a.test.txt',form='formatted')


   do i=1,Numstr,1
      read(22,*) b(i,:)
!      write(*,*) al(i,:)
   enddo

moon(:)= int (b(:,1))
sun(:)=int (b(:,2))
omega(:)=int (b(:,3))
w(:)=int (b(:,4))

do i=1,Numstr,1
   l(i) = moon(i) - omega(i) - w(i)
   ls(i) =  sun(i) - omega(i) - w(i)
   F(i) = moon(i)-omega(i)
   D(i) = - moon(i) + sun(i)
   Om(i) = omega(i)
enddo

do i=1,Numstr
  write(13,*) l(i), ls(i), F(i), D(i), Om(i)
enddo

end select


end

