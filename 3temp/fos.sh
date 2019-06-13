f90 $F90FLAGS -o run -s -w fe.f tdeos_clusters.f $LINK_FNL
./run
rm run
