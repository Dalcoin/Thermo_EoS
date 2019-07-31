File Directory:


Code File(s):

   chempot.f

Data File(s):

   Input:

      par.don
      kf.don

   Output:
   
      out_mu.srt

Scripting File(s):

   serv.py
   total_mu_serv.py




1) Input parameters in par.don
2) Input tabulated fermi-momentum, effective mass, and potential energy data in kf.don
2 a) If using NM parameters, set "gam = 2.d0" in the code file
2 b) If using SM parameters, set "gam = 4.d0" in the code file
3) If using the total_mu_serv.py script, input loop start in loops.don
4) Type "python tota_mu.py" to run
5) Tabulated chemcial potential values will be output in out_mu.srt
