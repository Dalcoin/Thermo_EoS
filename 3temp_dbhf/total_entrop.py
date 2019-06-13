import subprocess


reset=1
counter = 0

temp_list=[]
mu_list=[]

i_list=[1]
j_list=[1]

file_par = open('par.don', 'r')
lines_par = file_par.readlines()
file_par.close()
l_par = lines_par[0].strip("\n")
s_par = l_par.split(" ")
s_par = filter(None,s_par)

n = int(s_par[5])
m = int(s_par[6])

#n=4
#m=10
p=n*m

for j in range(1,n+1):
    for i in range(1,m+1):
        file_in = open('loops.don', 'r')
        lines = file_in.readlines()
        file_in.close()
        l = lines[0].strip("\n") 

        s = l.split(" ")

        inum=int(s[0])
        ival=inum
        jnum=int(s[1])
        jval=jnum
        
        if(counter == 0):
            i_rst = inum
            j_rst = jnum
            i_reset = str(inum) + " "
            j_reset = str(jnum) 
            input_reset = i_reset + j_reset

#   Bash
        subprocess.call("./os.sh",shell=True)
          
        file_mu = open('s_out.srt', 'r')
        lines_mu = file_mu.readlines()
        file_mu.close()
        l_mu = lines_mu[0].strip("\n")
        mu = l_mu.split(" ")
        mu = filter(None, mu)

        mu_f = float(mu[0])
        print(mu_f, i, j)  
 
        temp_list.append(mu_f)
#        print(temp_list)
#        print(ival,jval)


        ival+=1
        i_list.append(ival-1)
        chempot_redux=str(ival)+" "+str(jval)
    
        file_out = open('loops.don', 'w')
        file_out.write(chempot_redux)
        file_out.close()
        counter = counter + 1
        
    jval+=1
    ival=1
    j_list.append(jval)
    chempot_redux=str(ival)+" "+str(jval)
    file_out = open('loops.don', 'w')
    file_out.write(chempot_redux)
    file_out.close()
    mu_list.append(temp_list)
    temp_list=[]


file_muo = open('s_out_total.srt','w')
for k in range(0,n):
    chem_group=mu_list[k]
    for kk in range(0,m):
        chemp=chem_group[kk]
        chempo=str(chemp)+"\n"
        file_muo.write(chempo)
    file_muo.write(str(" \n"))
file_muo.close()
    
if(reset == 1):
    file_reset = open('loops.don', 'w')
    file_reset.write(input_reset)
    file_reset.close()

subprocess.call("rm s_out.srt", shell=True)
subprocess.call("clear", shell=True)
subprocess.call("ls", shell=True)



