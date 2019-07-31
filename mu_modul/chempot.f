        
       program chempot_solver
       implicit real*8(a-h,o-z)
       common/paspoi/pas(200),poi(200),xfs(200),wfs(200)
       
       dimension :: xkf(100), ws(100), ua(100), T(100)
       dimension :: xkf2(100), xkfv(100)
       
       dimension :: rho(100), den(100), rhoi(100)
       dimension :: break(100),break1(100),break2(100)
       dimension :: coef(4,100), coef1(4,100), coef2(4,100)
        
       open(444,file='par.don')
       open(555,file='kf.don')
       
       read(444,*) n, mat, m, nt, ti, tf, niv, j, k
       
       x0=0.d0 
       hc=197.33d0
       hc2=hc*hc
       pi=3.14159d0
       pi2=pi*pi       
       wwn=938.926d0
                 
       do i=1,n
          read(555,*) xkf(i), ws(i), ua(i)
          rhoi(i) = (4.d0/(6.d0*pi2))*(xkf(i))**3
          if(mat .EQ. 1) then
             xkf(i) = xkf(i)*(2.d0)**(1.d0/3.d0)
          end if 
          xkf2(i)=xkf(i)*xkf(i)
       end do
        
       do i=1,nt
          T(i)=(i-1.d0)*(tf)+ti
       end do
       
       if(mat .EQ. 1) then
          gam = 2.d0
       else
          gam = 4.d0
       end if
        
       ct = gam*4.d0*pi
       ct = ct/(2.d0*pi)**3
                      
c      j: reserved for the EoS loop
c      k: reserved for the Temperature loop
        
c      SERVER MAIN LOOP
       icmd = 0
       do 100 while (icmd .ne. 1)
          
          read(*,*) icmd, xmu
          
          if (icmd .ne. 1) then             
             sum=0.d0
             call lgauss(m)
             x1 = 3.d0*xkf(j)
             call papoi(x0,x1,1,m,xinf,1)
             
             do i=1,m
                r=pas(i)
                r2=r*r
                ww=poi(i)
                spex=spe(r2*hc2,xkf2(j)*hc2,ua(j),
     1                   ws(j),0.d0,0.d0,wwn,1,6,0)
                dfmx=dfd(spex,T(k),xmu)
                func=rhof(r,dfmx)*ww
                sum=sum+func
             end do          
             denp=ct*sum
             rtrn=dabs(denp-rhoi(j))
             if(T(k) .LT. 0.01d0) then
                xmup=spe(xkf2(j)*hc2,xkf2(j)*hc2,
     1               ua(j),ws(j),0.d0,0.d0,wwn,1,6,0)
                rtrn=dabs(xmup-xmu)
             end if
             write(*, *) rtrn
          endif                  
 100   continue         
       
       stop
       end
