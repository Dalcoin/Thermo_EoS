Write a python program which 

0) checks the files 'eos_data.don' and 'par.don' 
   for formatting errors; I'll leave this up to you to 
   decide how to best approach. 
1) runs 'sfp.f'
2) opens and reads the 'values_sfp.srt' file
3) produces an output file titled 'values_sfp.txt' which is
   formated as detailed below:

each line should be 

   kfs  kfa  den  ua  ws  mu  ea  s  f  p  t


where:

kfs:  fermi momentum (symmetric)
kfa:  fermi momentum (actual)
den:  density
ua:   effective potential
ws:   effective mass
mu:   chemical potential
ea:   energy-per-particle
s:    entropy-per-particle
f:    free-energy-per-particle
p:    pressure 
t:    temperture 


4) deletes 'values_sfp.srt'

I've attached the python scripting module I wrote, along with
the actualization of the module for reference. If you choose to 
use the functions I've written, don't forget to include comments
at the top of your file which list the dependencies. 

