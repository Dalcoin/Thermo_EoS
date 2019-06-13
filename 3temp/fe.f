c      fe_prep V. 2.0
         
       program fe_prep
       implicit real*8(a-h,o-z)
       integer , parameter :: itnum = 3
       
        
       dimension :: t(100), xkf(100)
       dimension :: e0(100), s0(100), p0(100), f0(100)
       
       dimension :: e(itnum,100),s(itnum,100),f(itnum,100),p(itnum,100)
       
       dimension :: e10(100), e20(100), e30(100)
       
       dimension :: s10(100), s20(100), s30(100)
       
       dimension :: f10(100), f20(100), f30(100)
       
       dimension :: p10(100), p20(100), p30(100)
       
       dimension :: ft10(100), ft20(100), ft30(100)
       
       dimension :: coef0(4,100),coef1(4,100),caser0(100),caser1(100) 
       dimension :: coef2(4,100),coef3(4,100),caser2(100),caser3(100)
       
       dimension :: rho(100), rhoi(100), ftemp(10), dfdt(100)       

       open(454,file='par.don')
       read(454,*) x1, gam2, t_start, t_inc, n, tnum, jnum
        
       do i=1,tnum
          t(i) = t_start + (i - 1.d0)*t_inc
       end do
       
       open(400,file='ex.don')
       open(500,file='sx.don')
       open(600,file='fx.don')
       open(700,file='px.don')
       
       pi=3.14159d0
       pi2=pi*pi  
       gam=4.d0
       
       nf=jnum-1
       
       do i=1,jnum
c          read(400,*) xkf(i), e05(i), e10(i), e15(i), e20(i),
c     1              e25(i), e30(i), e35(i)
c          read(500,*) xkf(i), s05(i), s10(i), s15(i), s20(i),
c     1               s25(i), s30(i), s35(i)
          read(400,*) xkf(i), e(1,i), e(2,i), e(3,i)
          read(500,*) s(1,i), s(2,i), s(3,i)
          rhoi(i)= (gam/(6.d0*pi2))*(xkf(i))**3
c          rho(i) = (4.d0/(6.d0*pi2))*(xkf(i))**3
       end do
        
c       call dcsakm(n,rho,e10,caser2,coef2)
c       call dcsakm(n,rho,e20,caser3,coef3)
       
       do k=1,tnum
          do i=1,jnum
             f(k,i) = e(k,i)-t(k)*s(k,i)
             fval = f(k,i)
             ftemp(i) = fval
             write(600,*) ftemp(i)
          end do
          write(600,*) "   "
       
          call dcsakm(jnum,rhoi,ftemp,caser0,coef0)         
       
          do i=1,jnum
             dfdt(i) = dcsder(1,rhoi(i),nf,caser0,coef0)   
             p(k,i) = rhoi(i)*rhoi(i)*dfdt(i)
             write(700,*) p(k,i)
          end do
          write(700,*) "   "
       
       end do
              
       end  
