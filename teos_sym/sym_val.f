       program etico
       implicit real*8(a-h,o-z)

       dimension :: xkf(100), den(100), t(100)

       dimension :: q105(100),q110(100),q115(100),q120(100),q125(100)
       dimension :: q130(100),q135(100)
       
       dimension :: q005(100),q010(100),q015(100),q020(100),q025(100)
       dimension :: q030(100),q035(100)

       dimension :: qs05(100),qs10(100),qs15(100),qs20(100),qs25(100)
       dimension :: qs30(100),qs35(100)
        
       
       open(200,file='par.don')
       open(400,file='q0.don')
       open(500,file='q1.don')
       open(800,file='qs.don')
       
       pi=3.14159d0
       pi2=pi*pi             
       
       read(200,*) n, jnum, itnum, lach
       
       do i=1,jnum
          read(400,*) den(i), q005(i),q010(i),q015(i),
     1                q020(i),q025(i),q030(i),q035(i)
          read(500,*) den(i), q105(i),q110(i),q115(i),
     1                q120(i),q125(i),q130(i),q135(i)
       end do
        
       do i=1,itnum
          t(i) = 5.d0 + (1-i)*5.d0  
       end do 
        
       do i=1,jnum
          qs05(i) = q105(i) - q005(i) 
          qs10(i) = q110(i) - q010(i)
          qs15(i) = q115(i) - q015(i)
          qs20(i) = q120(i) - q020(i)
          qs25(i) = q125(i) - q025(i)
          qs30(i) = q130(i) - q030(i)
          qs35(i) = q135(i) - q035(i)          
       end do
       
       do i=1,jnum
          write(800,*) den(i),qs05(i),qs10(i),qs15(i),
     1                 qs20(i),qs25(i),qs30(i),qs35(i)
       end do          
         
                     
       
       end    
