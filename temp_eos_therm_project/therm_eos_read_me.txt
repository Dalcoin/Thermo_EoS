

1)  Place input-Eos data into 'eos_data.don', use the format:


kf  ws  ua ea  mu

where:

kf: symemtric equivalent of the fermi momentum
ws: effective mass
ua: effective potential
ea: energy-per-particle
mu: chemical potential


2) place input options in 'par.don', use the format:

n  mat  t0  tinc  tnum

where:

n: number of fermi momentum per temperature
mat: (1) for neutron matter, (0) for symemtric matter
t0: starting temperature
tinc: temperature value to increment by
tnum: number of increments after the initial temp. is calculated;
      (e.g.) if t0 = 10, and tinc = 5, and tnum = 4
       then the temperatures to be evaluated at are
       10, 15, 20, 25, 30


Default settings:

For neutron matter
10  1  10.  10.  2

For symemtric matter
10  0  10.  10.  2


 
    