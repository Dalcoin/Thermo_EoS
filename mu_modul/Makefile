all: chempot.f 
	f90 $$F90FLAGS -o xeb_server -s -w chempot.f temp_eos_modules.f $$LINK_FNL

exec: eb_run_box.f eb.f
	f90 $$F90FLAGS -o exec -s -w eb.f eb_run_box.f $$LINK_FNL

	
clean:
	rm -f exec xeb_server values.out values_min.out eb check
