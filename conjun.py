#Converts "s_out_total.srt" into the proper formatting for "sx.don"


file_par = open('par.don', 'r')
lines_par = file_par.readlines()
file_par.close()

l_par = lines_par[0].strip("\n")
s_par = l_par.split(" ")
s_par = filter(None,s_par)

m = int(s_par[5])
n = int(s_par[6])


line_val = []
group_val = []

file_s = open('s_out_total.srt', 'r')
lines_s = file_s.readlines()
file_s.close()

for k in range(0,m):
    for j in range(0,n):
        l_val = lines_s[j+(k*11)].strip("\n")
        l_val_c = l_val.split(" ")
        l_val_clean = filter(None,l_val_c)
        line_val.append(l_val_clean)
    print(line_val)
    group_val.append(line_val)
    line_val = []

out_line=""
out_lines=[]
file_sx = open('sx.don','w')
for k in range(0,n):
    for kk in range(0,m):
       sx_group=group_val[kk]
       sx_line=sx_group[k] 
       sx_line=str(sx_line[0])
       out_line = out_line + "  " + sx_line
    line_output = out_line + str(" \n")    
    file_sx.write(line_output)   
    out_line=""
file_sx.close()
