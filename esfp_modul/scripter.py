import subprocess
import time
import numpy as np
import scipy as sp
import math as mt


def list_file_grab(file_in,grab_list,repeat,formater):
    
    # list_file_grab('file.in',[6,7],False,True)    
    # file_name: input file string
    # grab_list: list of integers with the lines to be parsed and printed
    #            input [] for the entire file to be grabed.
    # repeat:    boolean, true for option to repeat; if true then the repetition
    #            value is the first value in the grab_list, the middle values are 
    #            the shifted repeated indicies, and the last value is number of cycles
    # formater:  Boolean, True if returned as scrubbed list, else a raw list is returned  
    
    assert str(type(file_in)) == "<type 'str'>" , "Error: 'file_in' is not a string"
    assert str(type(grab_list)) == "<type 'list'>" , "Error: 'grab_list' is not an list"
    assert str(type(repeat)) == "<type 'bool'>" , "Error: 'repeat' is not a boolean"
    assert str(type(formater)) == "<type 'bool'>" , "Error: 'formater' is not a boolean"

#    file_in_r = open(file_in,'r')
#    file_lines = file_in_r.readlines()
#    file_in_r.close() 
    
    with open(file_in,'r') as file_in_r:
        file_lines = file_in_r.readlines()    
    
    raw_lines = []
    form_lines = []   

    if(len(grab_list) == 0):
        n=len(file_lines)    
        if(formater == True):
            for i in range(n):
                lines = file_lines[i].strip("\n").strip("\r").split(" ")            
                lines = filter(None,lines) 
                form_lines.append(lines)
            return form_lines
        else:
            for i in range(n):
                raw_lines.append(file_lines[i])    
            return raw_lines       
        
    grab_list = [x-1 for x in grab_list]    
    n = len(grab_list)
    
    if(repeat == False):
        grab_list_check = grab_list
    else:
        grab_list_check = grab_list[1:-1]
    
    for i in range(len(grab_list_check)):
        assert str(type(grab_list_check[i])) == "<type 'int'>" , "Error: grab_list must be a list of integers"
        dup_test = dup_check(grab_list_check)[0]
        assert dup_test == False , "Error: grab_list values must be unique"
    
    if(repeat == False):
        for i in range(n):
            raw_lines.append(file_lines[grab_list[i]])
            lines = file_lines[grab_list[i]].strip("\n").strip("\r").split(" ")            
            lines = filter(None,lines) 
            form_lines.append(lines)
        if(formater == True):
            return form_lines
        else:
            return raw_lines                               
                
    if(repeat == True):  
        assert len(grab_list) > 2, "Error: grab_list must take at least three values"
        bnd = grab_list[0]+1
        saut = grab_list[1:-1]
        n = grab_list[-1]+1
        for i in range(n):
            for j in range(len(saut)):
                line_tag = saut[j]+bnd*i
                raw_lines.append(file_lines[line_tag])
                lines = file_lines[line_tag].strip("\n").strip("\r").split(" ")            
                lines = filter(None,lines)          
                form_lines.append(lines)            
        if(formater == True):
            return form_lines
        else:
            return raw_lines   


def esfp_modul_scripter():
    subprocess.call("f90 $F90FLAGS -o run -s -w sfp.f temp_eos_modules.f $LINK_FNL",shell=True)
    subprocess.call("./run",shell=True)
    subprocess.call("rm run",shell=True)
    
    lines_out = list_file_grab("values_sfp.srt",[],False,True)
    pars = list_file_grab("par.don",[],False,True)
    pars = pars[0]
    
    n = int(pars[0])
    mat = int(pars[1])
    ti = float(pars[2])
    tinc = float(pars[3])
    numt = int(pars[4])
    
    s = "  "
    ts=[]
    for i in range(numt):ts.append(float(ti+i*tinc))
    for i in range(numt):
        kf, den, tgroup, ea, sa, fa, prs= ([] for i in range(7))
        for j in range(n):             
            kf.append('%.4f' % float(lines_out[j+i*(2*n)][0]))
            den.append('%.4f' % float(lines_out[j+i*(2*n)][1]))
            tgroup.append(lines_out[j+i*(2*n)][2])
            ea.append('%.3f' % float(lines_out[j+i*(2*n)][3]))
            sa.append('%.3f' % float(lines_out[j+i*(2*n)][4]))
            fa.append('%.3f' % float(lines_out[j+i*(2*n)][5]))
            prs.append('%.3f' % float(lines_out[j+(2*i+1)*(n)][0]))
        with open('esfp_values.don','a+') as fileout:
            fileout.write("Kf      Den     EA      SA     FA      PRS\n")  
            fileout.write("\n")
            fileout.write("T = " + str(ts[i]) + "\n")        
            for k in range(n):
                      line_str = str(kf[k])+s+str(den[k])+s+str(ea[k])+s
                      line_str = line_str + str(sa[k])+s+str(fa[k])+s+str(prs[k])
                      line_str = line_str+"\n"
                      fileout.write(line_str)
            fileout.write("\n")
    subprocess.call("rm values_sfp.srt",shell=True) 
    print("Sequence Finished: look in 'esfp_values.don' for the results.")

                      
# Main Program

esfp_modul_scripter()
