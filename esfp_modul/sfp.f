
       program sfp
       implicit real*8(a-h,o-z)
       common/bigs/entropy_val
       common/bigp/pr_vals(1000)
       dimension :: den(1000), xkf(1000), ea(1000)
       dimension :: xmu(1000), ua(1000), ws(1000)
       dimension :: t(1000), big_s(1000), fe(1000)      
       dimension :: rho_array(1000), f_array(1000)
       dimension :: prs_t(1000), xkf0(1000)

       pi = 3.14159265d0
       pi2 = pi*pi
       hc = 197.33d0 

       open(999,file='values_sfp.srt') 

       open(444,file='par.don')
       read(444,*) n, mat, ti, tinc, numt

       open(555,file='eos_data.don')
       do i=1,n*numt
          read(555,*) xkf0(i), ws(i), ua(i), ea(i),  xmu(i)
          if(mat .EQ. 0) then
             den(i) = 2.d0*xkf0(i)*xkf0(i)*xkf0(i)/(3.d0*pi2)
          else 
             xkf(i) = xkf0(i)*(2.d0**(1.d0/3.d0))
             den(i) = xkf(i)*xkf(i)*xkf(i)/(3.d0*pi2)
          end if
       end do
       
       do i=1,numt
          t(i) = ti+(i-1)*tinc
       end do
       
       itr = 0
       do j=1,numt
          do i=1,n
             k = i+(j-1)*n
             call entropy(xkf0(i),xmu(k),t(j),ua(i),ws(i),mat)
             big_s(k) = entropy_val
             fe(k) = free_energy(ea(k),big_s(k),t(j))
             write(999,1010) xkf(i), den(i), t(j), ea(k), big_s(k),fe(k)
          end do
          itr=itr+1
          if(itr .EQ. 1) then
             ias = 1
          else
             ias = (n)*(itr-1)+1
          end if
          iae = n*(itr)
          f_array = fe(ias:iae)                
          call pressure(den,f_array,n)    
          prs_t = pr_vals
          do i=1,n
             write(999,1111) prs_t(i)
          end do 
       end do

1010  Format(F8.4,2x,F8.4,2x,F6.2,2x,F8.4,2x,F8.4,2x,F9.4)
1111  Format(F8.3)
         
       end        

