import scipy.optimize as sciopt #import SciPy module with optimization routines
import subprocess               #module for calling subprocesses
import time                     #module with timing functions
  
# start the FORTRAN server process
server = subprocess.Popen("./xeb_server", stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr = subprocess.STDOUT)
function_call_counter = 0	#counts the number of energy() evaluations

# definition of function to minimize
def energy(v): # v is a list of parameters. here we just have the chemical potentail v = [mu]
    global function_call_counter
        #write the "EVALUATE ENERGY" command (icmd = 0)  and parameter values into the STDIN of the FORTRAN server
    server.stdin.writelines(["0\n" + str(v[0]) + "\n"])
        #wait for FORTRAN server responce and read the energy value from from its STDOUT 
    en = float(server.stdout.readline())	
    function_call_counter += 1	#increase the counter	
	#print out parameters and energy every n iterations
    if True: #set to False to disable printing
        n = 10
    if n == 1 or (function_call_counter % n) == 1:
        print v, en	
    return en 	#return the value of the energy
    

#----------Main Program----------


v = [-20.0] #set up the initial guess for mu

if False:            #set this to True, if you want to run the FORTRAN  routine once and quit.
    print v          #otherwise this section is ignored. Useful for testing purposes
    en = energy(v)
    print en
        
    server.communicate("1\n0\n") # issue a "STOP" command to the server, wait for it to finish

    print "YAY!! :D"
    quit()

time_0 = time.time() #start timing
                                
#call the minimization routine
res = sciopt.minimize(
    fun = energy,             #target function to be minimized. energy() in our case			
    x0 = v,                   #initial guess for solution			
    method = 'Nelder-Mead',     
    tol = 1e-6                #max relative error between consecutive iterations
    )

#And the result is:...
print res

time_1 = time.time() #timing
#print "total time:", time_1 - time_0
#print "function evaluation:", function_call_counter
#print "time per function evaluation:", (time_1 - time_0)/function_call_counter

server.communicate("1\n0\n") # issue a "STOP" command (icmd = 1) to the FORTRAN server, wait for it to finish

outputfile = open("mu.don", "w")
nma=res.x
outputfile.writelines([str(nma[0]),"\n"])
outputfile.close()

