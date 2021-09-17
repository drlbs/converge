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


real (kind=8) function g(x)
    real (kind=8) :: x
    real (kind=8) :: alpha, beta, delta 
    ! looking for root of f(x)=( 2.0 - exp(x) + x**2)/3 
    g = ( 2.0D0 - exp(x) + x**2)/3.0D0 + x
    return
end function g






