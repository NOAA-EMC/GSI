	real a(10)
        do i =1,10
         a(i)=1.0*i
        end do
    
        do i=1,10
         write(*,100) (a(j), j=1,i)
100     format(*,<i>(f5.1))
        end do

        stop
        end
