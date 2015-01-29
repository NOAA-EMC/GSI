        dimension tssti(192,94)
        dimension ssti(192,94)
c
        call assign('assign -Nieee -Ff77 u:11')
        open(11,file='/ptmp2/wd23sm/rungras1/uflx.971112',
     *  form='unformatted')
        call assign('assign -Nieee -Ff77 u:51')
        open(51,file='/ptmp2/wd23sm/rungras1/uflx.971112m',
     *  form='unformatted')
c
        nt=0
        do ihr=336,1056,12
        read(11) ssti
        nt=nt+1
        print *,nt,ihr
        do j=1,94
        do i=1,192
        tssti(i,j)=tssti(i,j)+ssti(i,j)
        enddo
        enddo
        enddo
c
        print *,' nt=',nt
        fnt=1.0/float(nt)
        do j=1,94
        do i=1,192
        tssti(i,j)=tssti(i,j)*fnt
        enddo
        enddo
c
        write(51) tssti
c
        nt=0
        do ihr=1080,1812,12
        read(11) ssti
        nt=nt+1
        print *,nt,ihr
        do j=1,94
        do i=1,192
        tssti(i,j)=tssti(i,j)+ssti(i,j)
        enddo
        enddo
        enddo
c
        print *,' nt=',nt
        fnt=1.0/float(nt)
        do j=1,94
        do i=1,192
        tssti(i,j)=tssti(i,j)*fnt
        enddo
        enddo
c
        write(51) tssti
c
        stop
        end
