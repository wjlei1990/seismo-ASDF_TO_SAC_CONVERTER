module var_main

  logical :: DEBUG
  logical :: WRITE_SAC
  logical :: WRITE_ASCII
  logical :: GENERATE_STATION_FILE

  character(len=200) :: INPUT_ASDF_FILE, OUTDIR

end module var_main

module sac_header

  integer :: gmt_year, gmt_day, gmt_hour, gmt_min, gmt_sec, gmt_msec
  real :: event_lat, event_lo, event_dpt
  real :: dist, ev_to_sta_AZ, sta_to_ev_AZ, great_circle_arc
  real :: receiver_lat, receiver_lo, receiver_el, receiver_dpt
  real :: cmp_azimuth, cmp_incident_ang
  real :: P_pick, S_pick, scale_factor


end module sac_header
