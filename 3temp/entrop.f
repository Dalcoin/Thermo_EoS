       
c      Entropy_calc V. 2.0
       
       program entropy_calc
       implicit real*8(a-h,o-z)
       common/paspoi/pas(200),poi(200),xfs(200),wfs(200)
       data wwn/938.926d0/
       
       dimension :: xkf(100), ws(100), ua(100), mu(100)
       dimension :: xkf2(100), rhoi(100), T(100)      
       dimension :: xmu_05(100), xmu_10(100), xmu_15(100), xmu_20(100)
       dimension :: xmu_25(100), xmu_30(100), xmu_35(100)
         
c      Constants
       
       open(555,file='kf.don')
       open(444,file='par.don')
       open(111,file='loops.don')
       open(888,file='mu.don')
       open(999,file='s_out.srt')
c       open(000,file='dump.don')
       
       read(444,*) x1, gam2, t_start, t_inc, n, tnum, jnum 
       
       x0=0.d0 
       
       hc=197.33d0
       hc2=hc*hc
       
       pi=3.14159d0
       pi2=pi*pi       
        
c       wwn=938.926d0
       gam =4.0d0
       
       do i=1,jnum
          read(555,*) xkf(i),ws(i),ua(i)
          rhoi(i)= (gam/(6.d0*pi2))*(xkf(i))**3
          xkf2(i)=xkf(i)*xkf(i)
       end do
       
       do i=1,jnum
          read(888,*) xkf(i), xmu_10(i), xmu_20(i), xmu_30(i)
       end do
        
       gam=gam2
       ct = gam*4.d0*pi
       ct = ct/(2.d0*pi)**3
       
       do i=1,tnum
          T(i)=(i-1.d0)*t_inc+t_start
       end do
        
       read(111,*) j, k
c       j=5
c       k=1
       
       sum=0.d0
       call lgauss(n)      
       call papoi(x0,x1,1,n,xinf,1)
          
       do i=1,n
          r=pas(i)
          r2=r*r
          ww=poi(i)
          spex=spe(r2*hc2,xkf2(j)*hc2,ua(j),
     1       ws(j),0.d0,0.d0,wwn,1,6,0)
       
          go to (0010,0020,0030) k
            
 0010        df=dfd(spex,T(k),xmu_10(j)) 
             go to 151      
 0020        df=dfd(spex,T(k),xmu_20(j))
             go to 151
 0030        df=dfd(spex,T(k),xmu_30(j))
             go to 151
       
 151         continue
c         write(000,*) r, spex, df
          if(df .ne. 0.d0) then
             func=(df*log(df) + (1.d0 - df)*log(1.d0 - df))*r2*ww
             sum=sum+func
          end if
       end do
       
       entropy = -(ct/rhoi(j))*sum
c       write(000,*) r,df,spex,T(k),func,sum
 100   continue         
       
       write(999,*) entropy          
       
       end
