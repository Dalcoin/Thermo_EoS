       
        
       program final_collection_script
       implicit real*8(a-h,o-z)

       integer, parameter :: itnum = 7
       integer, parameter :: ijnum = 10

       dimension :: qt_mu(itnum, ijnum), qt_ex(itnum, ijnum),
     1              qt_sx(itnum, ijnum), qt_fx(itnum, ijnum),
     1              qt_px(itnum, ijnum), qt_ke(itnum, ijnum),
     1              qt_pe(itnum, ijnum)
       dimension :: xkf(100), den(100), t(100)

       
       open(100,file='par.don')
       
       open(500,file='mu.don')
       open(550,file='ex.don')
       open(555,file='sx.don')
       open(600,file='fx.don')
       open(660,file='px.don')
       open(666,file='kx.don')
       open(700,file='ux.don')
       open(900,file='therm_par_out.srt')
       
       read(100,*) t_eval, gb, gb1, gb2, n_int, t_num, j_num 
       
       gam = 2.0
       pi = 3.14159d0
       pi2 = pi*pi 
       
       do i = 1,j_num
          read(500,*) xkf(i), qt_mu(1,i),  qt_mu(2,i), qt_mu(3,i),
     1                qt_mu(4,i), qt_mu(5,i), qt_mu(6,i), qt_mu(7,i)

          read(550,*) xkf(i), qt_ex(1,i),  qt_ex(2,i), qt_ex(3,i),
     1                qt_ex(4,i), qt_ex(5,i), qt_ex(6,i), qt_ex(7,i)

          read(555,*) xkf(i), qt_sx(1,i),  qt_sx(2,i), qt_sx(3,i),
     1                qt_sx(4,i), qt_sx(5,i), qt_sx(6,i), qt_sx(7,i)

          read(600,*) xkf(i), qt_fx(1,i),  qt_fx(2,i), qt_fx(3,i),
     1                qt_fx(4,i), qt_fx(5,i), qt_fx(6,i), qt_fx(7,i)

          read(660,*) xkf(i), qt_px(1,i),  qt_px(2,i), qt_px(3,i),
     1                qt_px(4,i), qt_px(5,i), qt_px(6,i), qt_px(7,i)

          read(666,*) xkf(i), qt_kx(1,i),  qt_kx(2,i), qt_kx(3,i),
     1                qt_kx(4,i), qt_kx(5,i), qt_kx(6,i), qt_kx(7,i)

          read(700,*) xkf(i), qt_ux(1,i),  qt_ux(2,i), qt_ux(3,i),
     1                qt_ux(4,i), qt_ux(5,i), qt_ux(6,i), qt_ux(7,i)

          den(i) = gam*(xkf(i)**3)/(3.d0*pi2)
       end do
       
       
       
       do i=1,t_num
          t(i) = 5.d0 + (i-1)*5.d0
          write(900,*) "         "
          write(900,*) "         "
          write(900,*) "T = ",t(i), " MeV"
          write(900,*) "         "
          do j=1,j_num
             write(900,*) den(i), qt_mu(j,i), qt_sx(j,i), qt_ex(j,i),
     1                    qt_fx(j,i), qt_px(j,i), qt_kx(j,i), qt_ux(j,i)
          end do
       end do

       end
 





