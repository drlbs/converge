program steff 

implicit none

real (kind=8) :: p0, p1, p2, p 
real (kind=8) :: g
real (kind=8) :: tol, comp
integer       :: iter, itermax

external g

itermax = 100000

print *, 'What is the initial value of x'
read *, p0

print *, 'What is your tolerance'
read *, tol

iter = 0

print *, iter, '  ', p0, '  ', p1

do while ( iter .lt. itermax ) 
   p1 = g(p0) 
   p2 = g(p1)
   p = p0 - (p1-p0)**2/(p2-2*p1+p0)
   if ( abs(p-p0) .le.  tol)  then 
      print *, " "
      print *, 'Root found at ', p, ' with tolerance ', tol
      print *, " "
      stop 
   endif    
   iter=iter+1
   print *, iter, '  ', p0, '  ', p1 
   p0 = p
enddo 

print *, "No root found after ", iter, " iterations."


end program steff 


real (kind=8) function g(x)
real (kind=8) :: x
! looking for root of f(x)=( 2.0 - exp(x) + x**2)/3 
g = ( 2.0D0 - exp(x) + x**2)/3.0D0 + x
return
end function g





