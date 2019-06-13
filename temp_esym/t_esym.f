
      program temp_eos
      implicit real*8 (a-h,o-z)
       
      dimension :: e010(100), e020(100), e030(100)
      dimension :: e110(100), e120(100), e130(100)
      dimension :: esym10(100), esym20(100), esym30(100)
      dimension :: xkf(100), den(100), xkf0(100), xkf1(100)
      

      open(300,file='e0.don')
      open(400,file='e1.don')
      open(500,file='esym.don')

      n=10

      do i =1,n
         read(300,*) den(i), xkf0(i), e010(i), e020(i), e030(i) 
         read(400,*) den(i), xkf1(i), e110(i), e120(i), e130(i)
      end do

      do i=1,n
         esym10(i) = e110(i) - e010(i)  
         esym20(i) = e120(i) - e020(i)
         esym30(i) = e130(i) - e030(i)
      end do

      do i=1,n
         write(500,1000) 0.6d0*xkf1(i), den(i), esym10(i), 
     1                   esym20(i),esym30(i)
      end do
 
1000  FORMAT(2x,F7.2,2x,F7.4,2x,F8.3,2x,F8.3,2x,F8.3)
    
      end
