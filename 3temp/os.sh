f90 $F90FLAGS -o run -s -w entrop.f tdeos_clusters.f $LINK_FNL
./run
rm run
