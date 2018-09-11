!-----------------------------------------------------------------------
! Read point driving data
!-----------------------------------------------------------------------
subroutine DRIVE(itime)

#include "OPTS.h"

use CONSTANTS, only: &
  eps,               &! Ratio of molecular weights of water and dry air
  e0,                &! Saturation vapour pressure at Tm (Pa)
  Tm                  ! Melting point (K)

use DRIVING, only: &
  dt,                &! Timestep (s)
  year,              &! Year
  month,             &! Month of year
  day,               &! Day of month
  hour,              &! Hour of day
  LW,                &! Incoming longwave radiation (W/m2)
  Ps,                &! Surface pressure (Pa)
  Qa,                &! Specific humidity (kg/kg)
  Rf,                &! Rainfall rate (kg/m2/s)
  Sf,                &! Snowfall rate (kg/m2/s)
  SW,                &! Incoming shortwave radiation (W/m2)
  Ta,                &! Air temperature (K)
  Ua,                &! Wind speed (m/s)
  LWtmp,             &! Incoming longwave radiation (W/m2)
  Pstmp,             &! Surface pressure (Pa)
  Qatmp,             &! Specific humidity (kg/kg)
  Rftmp,             &! Rainfall rate (kg/m2/s)
  Sftmp,             &! Snowfall rate (kg/m2/s)
  SWtmp,             &! Incoming shortwave radiation (W/m2)
  Tatmp,             &! Air temperature (K)
  Uatmp               ! Wind speed (m/s)

use GRID, only: &
  Nx,Ny               ! Grid dimension

use DATAIO

implicit none

integer, intent(in) :: itime

integer :: i, j

real :: &
  es,                &! Saturation vapour pressure (Pa)
  Tc,                &! Temperature (C)
  time(4)             ! Temporary variable

! Read time variables

call check(nf90_get_var(SW_file%ncid, SW_file%timeid, time, start = (/1,itime/), count = (/4,1/)))

year  = time(1)
month = time(2)
day   = time(3)
hour  = time(4)

! Read meteorological variables

call check(nf90_get_var(SW_file%ncid, SW_file%varid, SWtmp, start = (/1,itime/), count = (/SW_file%nspace, 1/)))
call check(nf90_get_var(LW_file%ncid, LW_file%varid, LWtmp, start = (/1,itime/), count = (/LW_file%nspace, 1/)))
call check(nf90_get_var(Sf_file%ncid, Sf_file%varid, Sftmp, start = (/1,itime/), count = (/Sf_file%nspace, 1/)))
call check(nf90_get_var(Rf_file%ncid, Rf_file%varid, Rftmp, start = (/1,itime/), count = (/Rf_file%nspace, 1/)))
call check(nf90_get_var(Ta_file%ncid, Ta_file%varid, Tatmp, start = (/1,itime/), count = (/Ta_file%nspace, 1/)))
call check(nf90_get_var(RH_file%ncid, RH_file%varid, Qatmp, start = (/1,itime/), count = (/RH_file%nspace, 1/)))
call check(nf90_get_var(Ua_file%ncid, Ua_file%varid, Uatmp, start = (/1,itime/), count = (/Ua_file%nspace, 1/)))
call check(nf90_get_var(Ps_file%ncid, Ps_file%varid, Pstmp, start = (/1,itime/), count = (/Ps_file%nspace, 1/)))

do i = 1, Nx
  do j = 1, Ny
    SW(i,j) = SWtmp(i)
    LW(i,j) = LWtmp(i)
    Sf(i,j) = SFtmp(i)
    Rf(i,j) = RFtmp(i)
    Ta(i,j) = Tatmp(i)
    Qa(i,j) = Qatmp(i)
    Ua(i,j) = Uatmp(i)
    Ps(i,j) = Pstmp(i)
  end do
end do

Ps = Ps * 1000.0

Sf = Sf / dt
Rf = Rf / dt

Ta = Ta + Tm

Qa = max(Qa, 10.0)
LW = max(LW, 110.0)

do i = 1, Nx
  do j = 1, Ny
    Ua(i,j) = max(Ua(i,j), 0.1)
    Tc = Ta(i,j) - Tm
    es = e0*exp(17.5043*Tc/(241.3 + Tc))
    Qa(i,j) = (Qa(i,j)/100)*eps*es/Ps(i,j)
  end do
end do

end subroutine DRIVE
