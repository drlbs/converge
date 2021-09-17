program picard 

    real (kind=8) :: x0, x1
    real (kind=8) :: g
    real (kind=8) :: tol
    integer       :: iter

    external g

    print *, 'What is the initial value of x'
    read *, x0

    print *, 'What is your tolerance'
    read *, tol

    x1 = g(x0)
    iter = 0

    print *, iter, '  ', x0, '  ', x1

    do while ( abs(x0 - x1) .gt. tol ) 
        x0 = x1
        x1 = g(x0)
        iter=iter+1
        print *, iter, '  ', x0, '  ', x1 
    enddo 

    print *, 'Root found at ', x1, ' with tolerance ', tol

end program picard 


real (kind=8) function g(y)
    real (kind=8) :: y
    real (kind=8) :: alpha, beta, delta 
    ! looking for root of f(x)=x**3 - x**2 - x - 1
     g = y**3 - y**2 - y - 1 + y
!     g = 0.3333333333333333D0 + 2.0D0**0.3333333333333333D0 /&
!       ( 3.0D0*(29.0D0 + 27.0D0*y + 3.0D0*Sqrt(3.0D0)*&
!       sqrt(31.0D0 + 58.0D0*y + 27.0D0*y**2.0D0)) ** 0.3333333333333333D0) +&
!       (29.0D0 + 27.0D0*y + 3.0D0*sqrt(3.0D0)*sqrt(31.0D0 + 58.0D0*y + 27.0D0*y**2.0D0))&
!       **0.3333333333333333D0/(3.0D0*2.0D0**0.3333333333333333D0)

     return
end function g






