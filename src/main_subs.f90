module main_subs

  implicit none

contains

subroutine read_main_parfile(rank, nproc, comm, ierr)

  use var_main

  integer :: rank, nproc, comm, ierr

  integer :: dummy_row, i
  integer :: IIN = 11
  character(len=30) :: dummy_string

  !print *,"Read main par"
  dummy_row = 8
  
  open(UNIT=IIN,FILE="PAR_FILE_MAIN",iostat=ierr)
  if(ierr.ne.0)then
    print *,"Can't find PAR_FILE_MAIN. Stop! "
    stop
  endif

  do i=1,dummy_row
    read(IIN,*)
  enddo

  !print *,"HERE"

	read(IIN,3) dummy_string, DEBUG
	print *, "DEBUG: ", DEBUG 
	read(IIN,3) dummy_string, WRITE_SAC 
	print *, "WRITE_SAC: ", WRITE_SAC 
	read(IIN,3) dummy_string, WRITE_ASCII
	print *, "WRITE_ASCII: ", WRITE_ASCII
	read(IIN,3) dummy_string, GENERATE_STATION_FILE
  print *, "STATION_FILE:", GENERATE_STATION_FILE

  read(IIN, *)
  read(IIN, *)

	read(IIN,2) dummy_string, INPUT_ASDF_FILE
	print *, "INPUT_ASDF_FILE: ", trim(INPUT_ASDF_FILE)

  read(IIN, *)
  read(IIN, *)
	
  read(IIN,2) dummy_string, OUTDIR
	print *, "OUTDIR: ", trim(OUTDIR)

2 format(a,a)
3 format(a,l20)
!4 format(a,i)

end subroutine read_main_parfile


subroutine write_ascii_output(my_asdf, outdir, ierr)

  use asdf_data
  use ascii_rw

  type(asdf_event) :: my_asdf
  character(len=150) :: outdir
  integer :: ierr

  double precision, allocatable :: data(:)
  double precision :: b, dt
  integer :: npt

  integer :: i, j

  character(len=300) :: fn, file_prefix

  call system('mkdir -p '//trim(outdir)//'')
	!do a channel name modify here
	do i=1,my_asdf%nrecords
		my_asdf%component_array(i)(1:2)="LH"
	enddo

  do i=1,my_asdf%nrecords
    file_prefix=trim(my_asdf%receiver_name_array(i))//"."//&
          trim(my_asdf%network_array(i))//"."//&
          trim(my_asdf%component_array(i))//'.'//&
          trim(my_asdf%receiver_id_array(i))
    fn=trim(outdir)//"/"//trim(file_prefix)//".ascii"
    print *, "fn:", trim(fn)

    allocate(data(my_asdf%npoints(i)))
    data(:)=dble(my_asdf%records(i)%record)
    b=dble(my_asdf%begin_value(i))
    dt=dble(my_asdf%sample_rate(i))
    npt=my_asdf%npoints(i)
    call dwascii(fn, data, npt, b, dt)
    deallocate(data)
  enddo

end subroutine write_ascii_output

subroutine write_sac_output(my_asdf, outdir, ierr)
  
  use asdf_data
  use sac_header

  type(asdf_event) :: my_asdf
  character(len=*) :: outdir
  integer :: ierr
  integer :: i,j
  
  integer :: npts
  integer,parameter :: NDATAMAX=1000000
  real :: delta, B, E
  real :: xdata(NDATAMAX), ydata(NDATAMAX)
  integer :: nerr

  character(len=200) :: filename

  call system('mkdir -p '//trim(outdir)//'')

  do i=1, my_asdf%nrecords
    npts=my_asdf%npoints(i)
    if(npts.gt.NDATAMAX)then
      print *,"npts greater than NDATAMAX. Skip this data"
      cycle
    endif
    filename=trim(OUTDIR)//'/'//trim(my_asdf%receiver_name_array(i))//&
              '.'//trim(my_asdf%network_array(i))//&
              '.'//trim(my_asdf%component_array(i))//&
              '.'//trim(my_asdf%receiver_id_array(i))//&
              '.sac'
    print *,"SAC file name: ", trim(filename)

    B=real(my_asdf%begin_value(i))
    delta=real(my_asdf%sample_rate(i))
    E=real(B+(npts-1)*delta)
    xdata(1)=real(B)
    do j=2, npts
      xdata(j)=xdata(1)+(j-1)*delta
    enddo
    ydata(1:npts)=real(my_asdf%records(i)%record(1:npts))

    !-----------------------
    !Begin header
    call newhdr()

    !basic info
    call setnhv('npts', npts, nerr)
    call setlhv('leven', .true., nerr)
    call setfhv('b', xdata(1), nerr)
    call setfhv('e', xdata(npts), nerr)
    call setfhv('delta', delta, nerr) 

    !convert the date type from double to real
    gmt_year = my_asdf%gmt_year(i) 
    gmt_day = my_asdf%gmt_day(i) 
    gmt_hour = my_asdf%gmt_hour(i) 
    gmt_min = my_asdf%gmt_min(i) 
    gmt_sec =  my_asdf%gmt_sec(i) 
    gmt_msec = my_asdf%gmt_msec(i) 

    event_lat = real ( my_asdf%event_lat(i) )
    event_lo = real( my_asdf%event_lo(i) )
    event_dpt = real( my_asdf%event_dpt(i) )

    dist = real ( my_asdf%dist(i) )
    ev_to_sta_AZ = real ( my_asdf%ev_to_sta_AZ(i)  )
    sta_to_ev_AZ = real ( my_asdf%sta_to_ev_AZ(i) ) 
    great_circle_arc = real ( my_asdf%great_circle_arc(i) )
    
    receiver_lat = real( my_asdf%receiver_lat(i) )
    receiver_lo = real( my_asdf%receiver_lo(i) )
    receiver_el = real( my_asdf%receiver_el(i) )
    receiver_dpt = real( my_asdf%receiver_dpt(i) )

    cmp_azimuth = real( my_asdf%cmp_azimuth(i) )
    cmp_incident_ang = real( my_asdf%cmp_incident_ang(i) )

    P_pick = real( my_asdf%P_pick(i) )
    S_pick = real( my_asdf%S_pick(i) )
    scale_factor = real( my_asdf%scale_factor(i) )

    !=================================================
    !set the sac header
    !time field
    call setnhv('NZYEAR', gmt_year, nerr)
    call setnhv('NZJDAY', gmt_day, nerr)
    call setnhv('NZHOUR', gmt_hour, nerr)
    call setnhv('NZMIN', gmt_min, nerr)
    call setnhv('NZSEC', gmt_sec, nerr)
    call setnhv('NZMSEC', gmt_msec, nerr)
    
    !event field
    call setkhv('KEVNM', my_asdf%event, nerr)

    call setfhv('EVLA', event_lat, nerr)
    call setfhv('EVLO', event_lo, nerr)
    call setfhv('EVDP', event_dpt, nerr)

    call setfhv('DIST', dist, nerr)
    call setfhv('AZ', ev_to_sta_AZ, nerr)
    call setfhv('BAZ', sta_to_ev_AZ, nerr)
    call setfhv('GCARC', great_circle_arc, nerr)

    !receiver_info
    call setfhv('STLA', receiver_lat, nerr)
    call setfhv('STLO', receiver_lo, nerr)
    call setfhv('STEL', receiver_el, nerr)
    call setfhv('STDP', receiver_dpt, nerr)

    call setfhv('CMPAZ', cmp_azimuth, nerr)
    call setfhv('CMPINC', cmp_incident_ang, nerr)

    call setkhv('KSTNM', my_asdf%receiver_name_array(i), nerr)
    call setkhv('KNETWK', my_asdf%network_array(i), nerr)
    call setkhv('KCMPNM', my_asdf%component_array(i), nerr)
    call setkhv('KHOLE', my_asdf%receiver_id_array(i), nerr)

    call setfhv('t1', P_pick, nerr)
    call setfhv('t2', S_pick, nerr)
    call setfhv('SCALE', scale_factor, nerr)
    !finish header

    !-----------------------
    !write the record
    call wsac0(filename, xdata, ydata, nerr)

    if(nerr.ne.0)then
      write(*,*) "Error Writing SAC file:  ", trim(filename)
    endif

  enddo

  !print *,"========="
  !print *, "return"
  !print *,"========="

end subroutine write_sac_output

logical function sta_exist(receiver, network, sta, ntw, sta_index)

  character(len=*) :: receiver, network
  character(len=*) :: sta(:), ntw(:)
  integer :: sta_index
  integer :: i

  if(sta_index.eq.0)then
    sta_exist=.false.
    return
  endif

  do i=1, sta_index
    if( (trim(sta(i)).eq.trim(receiver)) .and. &
                            (trim(ntw(i)).eq.trim(network)) ) then
      sta_exist=.true.
      return
    endif
  enddo

  sta_exist=.false.
  return

end function sta_exist

subroutine write_station_file(my_asdf, outdir, ierr)

  use asdf_data

  type(asdf_event) :: my_asdf
  character(len=*) :: outdir
  integer :: ierr

  integer :: IIN=101
  character(len=200) :: STATION_FN

  character(len=30), allocatable :: sta(:), ntw(:)
  real, allocatable :: lat(:), lon(:), ele(:), dpt(:)
  integer :: nrecords, nstations, sta_index

  integer :: i

  nrecords=my_asdf%nrecords
  if(nrecords.le.0)then
    print *,"nrecords is not correct!"
    return
  endif
  allocate(sta(nrecords))
  allocate(ntw(nrecords))
  allocate(lat(nrecords))
  allocate(lon(nrecords))
  allocate(ele(nrecords))
  allocate(dpt(nrecords))
  !allocate((nrecords))

  STATION_FN=trim(OUTDIR)//'/STATIONS_'//trim(my_asdf%event)
  print *,"STATION filename: ",trim(STATION_FN)

  sta_index=0
  do i=1, my_asdf%nrecords
    if(sta_exist(my_asdf%receiver_name_array(i),my_asdf%network_array(i),&
        sta, ntw, sta_index)) then
      cycle
    else
      sta_index=sta_index+1
      sta(sta_index)=my_asdf%receiver_name_array(i)
      ntw(sta_index)=my_asdf%network_array(i)
      lat(sta_index)=my_asdf%receiver_lat(i)
      lon(sta_index)=my_asdf%receiver_lo(i)
      ele(sta_index)=my_asdf%receiver_el(i)
      dpt(sta_index)=my_asdf%receiver_dpt(i)
    endif
  enddo
  nstations=sta_index
  print *,"Total number of Station:", nstations

  open(unit=IIN, file=STATION_FN)
  do i=1, nstations
    write(IIN,5) sta(i), ntw(i), lat(i), lon(i), ele(i), dpt(i)
  enddo
  close(IIN)

5 format(a10,a4,2f11.4, 2f10.1)

end subroutine write_station_file

end module main_subs
