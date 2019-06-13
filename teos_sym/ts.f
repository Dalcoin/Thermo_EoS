       
       
       program tsing
       implicit real*8(a-h,o-z)

       dimension :: kf(100), den(100), t(100)

       dimension :: s05(100),s10(100),s15(100),s20(100),s25(100)
       dimension :: s30(100),s35(100)
       dimension :: ts05(100),ts10(100),ts15(100),ts20(100),ts25(100)
       dimension :: ts30(100),ts35(100)
       
       open(200,file='par.don')
       open(400,file='sx.don')
       open(800,file='ts.don')

       read(200,*) n, jnum, itnum, lach

       do i=1,itnum
          t(i) = 5.d0 + (1-i)*5.d0  
       end do 

       do i=1,jnum
          read(400,*) den(i), s05(i),s10(i),s15(i),
     1                s20(i),s25(i),s30(i),s35(i)
       end do

       do i=1,jnum
          ts05(i) = t(1)*s05(i)
          ts10(i) = t(2)*s10(i)
          ts15(i) = t(3)*s15(i)
          ts20(i) = t(4)*s20(i)
          ts25(i) = t(5)*s25(i)
          ts30(i) = t(6)*s30(i)
          ts35(i) = t(7)*s35(i)
       end do

       do i=1,jnum
          write(800,*) den(i), ts05(i),ts10(i),ts15(i),
     1                 ts20(i),ts25(i),ts30(i),ts35(i)
       end do
        
       end 
