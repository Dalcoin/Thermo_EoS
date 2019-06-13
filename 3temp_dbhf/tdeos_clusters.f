
c  This program does nothing by itseft
c  A collection of the subroutines and functions
c  used in calculating temperature dependent
c  EOS. Includes legendre-gaussian integration
c  Fermi-Dirac distribtuion, single particle energy (SPE)
c

c--------------Fermi-Dirac-Function--------------c

       function dfd(spe,tt,xmu)
       implicit real*8(a-h,o-z)
       wwn=938.926d0
       dfd=1.d0/(1.d0+exp(((spe-wwn)-xmu)/tt))
       end


c--------------Single-Particle-Energy-Function--------------c


      function spe (qq,qfq,ua,ws,uc,ud,wn,iprop,ispex,ipaho)

c        single particle energy of one particle below or above
c        the fermi surface of a medium
c
c        spe is called by:
c                          matbnd,
c                          matusf,
c                          matgmt,
c                          obaa,
c                          tbibnn,
c                          tbibd,
c                          tbaann,
c                          tbainn.
c
c        ispe = 0 : same as ispe=2
c        ispe = 1 : same as ispe=2
c        ispe = 2 : continous spectrum with free energies below and
c                   above fermi surface.
c        ispe = 3 : same as ispe=2 plus constant shift of particle
c                   potential above fermi surface by uc.
c        ispe = 4 : conventional choice for sp energy:
c                   bound energy below the fermi surface and
c                   free energy above the fermi surface (i.e. gap).
c        ispe = 5 : continous spectrum with bound energy below the ferm
c                   surface and continous continuation above the fermi
c                   surface until u=0., free energies after
c        ispe = 6 : continuous for ever
c        ispe = 7 : continuous choice with k-dependent parameters
c                   according to fua in case of iprop=2;
c                   in case of iprop=1 same as ispe=6.
c
      implicit real*8 (a-h,o-z)
      data wwn/938.926d0/

      ispe=ispex
      if (ispe.lt.2) ispe=2
c
      go to (1000,2000),iprop

 1000 if (ipaho.eq.0) go to 1001
      go to (1100,1200),ipaho
 1001 if (qq.gt.qfq) go to 1200
c
c
 1100 go to (1110,1110,1110,1140,1140,1140,1170),ispe
 1110 spe=0.5d0*qq/wn             
      go to 9000
 
 1140 spe=0.5d0*qq/ws+ua
c     if (wn.ne.wwn) spe=spe+0.5d0*qq*(1.d0/wn-1.d0/wwn)+wn-wwn
      go to 9000
 1170 wsk=wn
      spe=0.5d0*qq/wn+wsk
      go to 9000
c
c
 1200 go to (1210,1210,1210,1210,1250,1140,1170),ispe
 1210 spe=0.5d0*qq/wn+wn-wwn
      go to 8000
 1250 spe1=0.5d0*qq/ws+ua
      if (wn.ne.wwn) spe1=spe1+0.5d0*qq*(1.d0/wn-1.d0/wwn)+wn-wwn
      spe2=0.5d0*qq/wn+wn-wwn
      spe=dmin1(spe1,spe2)
      go to 9000
c
c
c
c
 2000 if (ipaho.eq.0) go to 2001
      go to (2100,2200),ipaho
 2001 if (qq.gt.qfq) go to 2200
c
c
 2100 go to (2110,2110,2110,2140,2140,2140,2270),ispe
 2110 spe=dsqrt(qq+wn*wn)
      go to 9000
 2140 spe=dsqrt(qq+ws*ws)+wwn-ws+ua
c     if (wn.ne.wwn) spe=spe+dsqrt(qq+wn*wn)-dsqrt(qq+wwn*wwn)
      go to 9000
c
c
 2200 go to (2210,2210,2210,2210,2250,2140,2270),ispe
 2210 spe=dsqrt(qq+wn*wn)
      go to 8000
 2250 spe1=dsqrt(qq+ws*ws)+wwn-ws+ua
      if (wn.ne.wwn) spe1=spe1+dsqrt(qq+wn*wn)-dsqrt(qq+wwn*wwn)
      spe2=dsqrt(qq+wn*wn)
      spe=dmin1(spe1,spe2)
      go to 9000
 2270 wsk=wn
      spe=dsqrt(qq+wsk*wsk)
c      if (qq.lt.25.*qfq) go to 9000
c      spe=dsqrt(qq+wn*wn)
      go to 9000
 8000 if (mod(ispe,2).eq.1) spe=spe+uc
 9000 return
      end



c---------------------------------------------------------------------------------------------------
c                             Subroutine                                                           | 
c---------------------------------------------------------------------------------------------------





c---------------------------------------------------------------------------------------------------
c                            Numeric Integration Subroutines                                       |
c---------------------------------------------------------------------------------------------------

c-------------------------------------------------
c          Legendrian Gauss Integration          |
c-------------------------------------------------
       subroutine lgauss(n)
       implicit real*8(a-h,o-z)
       dimension z(200),wz(200)
       common/paspoi/pas(200),poi(200),xfs(200),wfs(200)
       
       if(n-1) 1,2,3
 1        return
 2        z(1)=0.d0
          wz(1)=2.d0
          return
 3        r=dfloat(n)
       g=-1.d0
       
       do 147 i=1,n
          test=-2.d0
          ic=n+1-i
          if(ic.lt.i) go to 150
 4           s=g
             t=1.d0
             u=1.d0
             v=0.d0
          do 50 k=2,n
             a=dfloat(k)
             p=((2.d0*a-1.d0)*s*g-(a-1.d0)*t)/a
             dp=((2.d0*a-1.d0)*(s+g*u)-(a-1.d0)*v)/a
             v=u
             u=dp
             t=s
 50       s=p
          if(abs((test-g)/(test+g)).lt.0.5d-09) go to 100
             sum=0.d0
          if(i.eq.1) go to 52
             do 51 j=2,i
                sum=sum+1.d0/(g-xfs(j-1))
 51          continue
 52       test=g
          g=g-p/(dp-p*sum)
          go to 4
 100      xfs(ic)=-g
          xfs(i)=g
          wfs(i)=2.d0/(r*t*dp)
          wfs(ic)=wfs(i)
 147   g=g-r*t/((r+2.d0)*g*dp+r*v-2.d0*r*t*sum)
 150   do 160 i=1,n
          z(i)=xfs(i)
          wz(i)=wfs(i)
 160   continue
       return
       end
c-------------------------------------------------
c              Steps and Weights                 |
c-------------------------------------------------
       subroutine papoi(xi,xf,ni,nf ,xinf,k)
       implicit real*8(a-h,o-z)
       common/paspoi/pas(200),poi(200),tp(200),hp(200)

       coeff=1.d0           
       go to (10,20,30) k
 10       xs=(xi+xf)/2.d0
          xd=(xf-xi)/2.d0
          do 1 i=ni,nf
             pas(i)=xs+xd*tp(i)
 1           poi(i)=xd*hp(i)
          return
 20       do 2 i=ni,nf
             pas(i)=((1.d0+tp(i))/(1.d0-tp(i)))*coeff+xinf
             poi(i)=2.d0*hp(i)/((1.d0-tp(i))**2)
             poi(i)=poi(i)*coeff
c 2   write(15,*) pas(i),poi(i),i
  2       continue
          return
 30    do 3 i=ni,nf
          pas(i)=xi*(1.d0+tp(i))/(1.d0-tp(i))+xinf
          poi(i)=2.d0*xi*hp(i)/((1.d0-tp(i))**2)
 3     continue                       
       return
       end
c-------------------------------------------------


