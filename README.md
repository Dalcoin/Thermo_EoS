# Thermo_EoS
Programs to compute chemical potential, entropy, free-energy and pressure from a given temperature dependent EoS


Things left to still do:

1) Clean and modularize code, it works but it ain't pretty.
2) Write a script finalizing the OS (one-shot) run into multi-runs \ 
(Done 06/31/2019)





New Things left to still do:

1) Thoroughly test the modulated therm_eos program
2) Write a python script for automatic runing and output scrubbing


Finished (07/31/2019)

Two new module folders:

esfp_modul

mu_modul


Self contained moduls:

esfp_modul: contains programs which compute and format the energy-per-particle,
            entropy-per-particle, free-energy-per-particle and pressure as 
            functions of density. The input is the chemical potential, the 
            energy-per-particle as a function of fermi momentum and the 
            effective mass and effective potential as functions of fermi 
            momentum 

mu_modul: contains programs which compute and formate the chemical potential as
          functions of density. The input is the effective mass and effective 
          potential as functions of fermi momentum 









