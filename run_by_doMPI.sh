(nohup mpirun --mca routed direct -hostfile ~/mpi/SSD_for_eRm/hosts -n 112 R --slave -f ~/mpi/SSD_for_eRm/ssd_for_extended_rasch.R > ~/mpi/SSD_for_eRm/log.txt 2>&1 &)
