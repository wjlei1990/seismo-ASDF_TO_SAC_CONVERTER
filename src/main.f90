program main

  use asdf_data
  use var_main

  use asdf_read_subs
  use asdf_write_subs
  use main_subs

  use mpi
  implicit none

  type(asdf_event) :: my_asdf

  integer :: nproc, comm, rank
  integer :: ierr, adios_err

  character(len=20), dimension(MAXDATA_PER_PROC) :: station, network
  character(len=20), dimension(MAXDATA_PER_PROC) :: component, receiver_id
  integer :: nrecords

  integer :: i

  call mpi_init(ierr)
  call mpi_comm_dup(mpi_comm_world, comm, ierr)
  call mpi_comm_rank(comm, rank, ierr)
  call mpi_comm_size(comm, nproc, ierr)

  if(nproc.ne.1) then
    print *, "current version does not support parallel"
    stop
  endif

  call read_main_parfile(rank, nproc, comm, ierr)
  
  call read_asdf_file(input_asdf_file, my_asdf, nrecords, &
    station, network, component, receiver_id, 0, &
    rank, nproc, comm, ierr)

  call system('mkdir -p '//trim(OUTDIR)//'')

  if(GENERATE_STATION_FILE) then
    print *, "-----------------------------------"
    print *, "Generate STATION file..."
    call write_station_file(my_asdf, OUTDIR, ierr)
  endif

  if(WRITE_SAC) then
    print *, "-----------------------------------"
    print *, "Write SAC file..."
    call write_sac_output(my_asdf, OUTDIR, ierr)
  endif

  if(WRITE_ASCII) then
    print *, "------------------------------------"
    print *, "Write ASCII file..."
    call write_ascii_output(my_asdf, OUTDIR, ierr)
  endif

end program main

