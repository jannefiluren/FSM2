!-----------------------------------------------------------------------
! Set parameter values and initialize prognostic variables
!-----------------------------------------------------------------------
subroutine SETUP

#include "OPTS.h"
use CONSTANTS
use DIAGNOSTICS
use DRIVING
use GRID
use IOUNITS
use PARAMETERS
use PARAMMAPS
use SOILPARAMS 
use STATE_VARIABLES
use AVERAGED_OUTPUTS
use LANDUSE_PARAMETERS
use DATAIO

implicit none

integer :: & 
  i,j,               &! Point counters
  k                   ! Level counter

real :: &
  hcon_min            ! Thermal conductivity of soil minerals (W/m/K)

real, allocatable :: &
  fsat(:),           &! Initial moisture content of soil layers as fractions of saturation
  Tprof(:)            ! Initial soil layer temperatures (K)

character(len = 300) :: pathmet, pathres, pathparam, spatial_res

namelist /drive/ pathmet, pathres, pathparam, spatial_res
namelist /simperiod/ istart, istop

! Read drive variables

read(5, drive)

! Open netcdf with parameter data

Param_file%file_name = trim(pathparam)

call get_paraminfo(Param_file)

! Open netcdfs with forcing data

SW_file%file_name = trim(pathmet)//"/iswr_"//trim(spatial_res)//".nc"
SW_file%var_name = "iswr"

LW_file%file_name = trim(pathmet)//"/ilwr_"//trim(spatial_res)//".nc"
LW_file%var_name = "ilwr"

Sf_file%file_name = trim(pathmet)//"/snowf_"//trim(spatial_res)//".nc"
Sf_file%var_name = "snowf"

Rf_file%file_name = trim(pathmet)//"/rainf_"//trim(spatial_res)//".nc"
Rf_file%var_name = "rainf"

Ta_file%file_name = trim(pathmet)//"/tair_"//trim(spatial_res)//".nc"
Ta_file%var_name = "tair"

RH_file%file_name = trim(pathmet)//"/rhum_"//trim(spatial_res)//".nc"
RH_file%var_name = "rhum"

Ua_file%file_name = trim(pathmet)//"/wind_"//trim(spatial_res)//".nc"
Ua_file%var_name = "wind"

Ps_file%file_name = trim(pathmet)//"/pres_"//trim(spatial_res)//".nc"
Ps_file%var_name = "pres"

call get_metinfo(SW_file)
call get_metinfo(LW_file)
call get_metinfo(Sf_file)
call get_metinfo(Rf_file)
call get_metinfo(Ta_file)
call get_metinfo(RH_file)
call get_metinfo(Ua_file)
call get_metinfo(Ps_file)

! Open netcdfs for storing results

SWE_file%file_name = trim(pathres)//"/swe_"//trim(spatial_res)//".nc"
SWE_file%var_name  = "swe"

HS_file%file_name = trim(pathres)//"/snowdepth_"//trim(spatial_res)//".nc"
HS_file%var_name  = "snowdepth"

alb_file%file_name = trim(pathres)//"/alb_"//trim(spatial_res)//".nc"
alb_file%var_name  = "alb"

Gsurf_file%file_name = trim(pathres)//"/gsurf_"//trim(spatial_res)//".nc"
Gsurf_file%var_name  = "gsurf"

Hatmo_file%file_name = trim(pathres)//"/hatmo_"//trim(spatial_res)//".nc"
Hatmo_file%var_name  = "hatmo"

Latmo_file%file_name = trim(pathres)//"/latmo_"//trim(spatial_res)//".nc"
Latmo_file%var_name  = "latmo"

Melt_file%file_name = trim(pathres)//"/melt_"//trim(spatial_res)//".nc"
Melt_file%var_name  = "melt"

Rnet_file%file_name = trim(pathres)//"/rnet_"//trim(spatial_res)//".nc"
Rnet_file%var_name  = "rnet"

Rof_file%file_name = trim(pathres)//"/rof_"//trim(spatial_res)//".nc"
Rof_file%var_name  = "rof"

Tsurf_file%file_name = trim(pathres)//"/tsurf_"//trim(spatial_res)//".nc"
Tsurf_file%var_name  = "tsurf"

Tsoil_file%file_name = trim(pathres)//"/tsoil_"//trim(spatial_res)//".nc"
Tsoil_file%var_name  = "tsoil"

call create_resfile(SWE_file, Ta_file)
call create_resfile(HS_file, Ta_file)
call create_resfile(alb_file, Ta_file)
call create_resfile(Gsurf_file, Ta_file)
call create_resfile(Hatmo_file, Ta_file)
call create_resfile(Latmo_file, Ta_file)
call create_resfile(Melt_file, Ta_file)
call create_resfile(Rnet_file, Ta_file)
call create_resfile(Rof_file, Ta_file)
call create_resfile(Tsurf_file, Ta_file)
call create_resfile(Tsoil_file, Ta_file)

! Simulation period

istart = 1
istop = Ta_file%ntime

read(5, simperiod)

! Grid parameters

Nx = Ta_file%nspace
Ny = Param_file%nclasses
Nsmax = 3
Nsoil = 4

! Snow and soil layer settings

allocate(Dzsnow(Nsmax))
allocate(Dzsoil(Nsoil))
if (Nsmax == 3) Dzsnow = (/0.1, 0.2, 0.4/)
if (Nsoil == 4) Dzsoil = (/0.1, 0.2, 0.4, 0.8/)

! Driving data

dt = 10800
lat = 0
noon = 0
zT = 20
zU = 20
lat = (3.14159/180)*lat  ! convert latitude to radians

! Allocate forcing variables

allocate(LWtmp(Nx))
allocate(Pstmp(Nx))
allocate(Qatmp(Nx))
allocate(Rftmp(Nx))
allocate(Sftmp(Nx))
allocate(SWtmp(Nx))
allocate(Tatmp(Nx))
allocate(Uatmp(Nx))

allocate(LW(Nx,Ny))
allocate(Ps(Nx,Ny))
allocate(Qa(Nx,Ny))
allocate(Rf(Nx,Ny))
allocate(Sf(Nx,Ny))
allocate(SW(Nx,Ny))
allocate(Ta(Nx,Ny))
allocate(Ua(Nx,Ny))

! Defaults for numerical solution parameters

Nitr = 4

! Defaults for canopy parameters

avg0 = 0.1
avgs = 0.4
cden = 0.004
cvai = 4.4
cveg = 20
gsnf = 0
kext = 0.5
kveg = 1
rchd = 0.67
rchz = 0.1
tcnc = 240
tcnm = 2.4

! Defaults for snow parameters

asmx = 0.8
asmn = 0.5
bstb = 5
bthr = 2
eta0 = 3.7e7
etaa = 0.081
etab = 0.018
hfsn = 0.1
kfix = 0.24
rho0 = 300
rhoc = 150
rhof = 100
rcld = 300
rmlt = 500
Salb = 10
snda = 2.8e-6
sndb = 0.042
sndc = 0.046
Talb = -2
tcld = 1000
tmlt = 100
trho = 200
Wirr = 0.03
z0sn = 0.01

! Defaults for ground surface parameters

bstb = 5
gsat = 0.01
z0zh = 10

! Surface data from defaults, namelist or named map files

allocate(alb0(Nx,Ny))
allocate(canh(Nx,Ny))
allocate(fcly(Nx,Ny))
allocate(fsnd(Nx,Ny))
allocate(fsky(Nx,Ny))
allocate(fveg(Nx,Ny))
allocate(hcan(Nx,Ny))
allocate(scap(Nx,Ny))
allocate(trcn(Nx,Ny))
allocate(VAI(Nx,Ny))
allocate(z0sf(Nx,Ny))

alb0(:,:) = 0.2
canh(:,:) = -1
fcly(:,:) = 0.3
fsnd(:,:) = 0.6
fsky(:,:) = 1
fveg(:,:) = -1
hcan(:,:) = 0
scap(:,:) = -1
trcn(:,:) = -1
VAI(:,:)  = 0.0
z0sf(:,:) = 0.1

! Read landuse parameters

allocate(farea(Nx,Ny))

call check(nf90_get_var(Param_file%ncid, Param_file%frac_landuse_id, farea))
call check(nf90_get_var(Param_file%ncid, Param_file%lai_landuse_id, VAI))
call check(nf90_get_var(Param_file%ncid, Param_file%hcan_landuse_id, hcan))

!hcan(:,:) = 30
!VAI(:,:)  = 2


if (canh(1,1) < 0) canh(:,:) = 2500*VAI(:,:)
if (trcn(1,1) < 0) trcn(:,:) = exp(-kext*VAI(:,:))
if (fveg(1,1) < 0) fveg(:,:) = 1 - exp(-kveg*VAI(:,:))
if (scap(1,1) < 0) scap(:,:) = cvai*VAI(:,:)

! Derived soil parameters

allocate(b(Nx,Ny))
allocate(hcap_soil(Nx,Ny))
allocate(hcon_soil(Nx,Ny))
allocate(sathh(Nx,Ny))
allocate(Vsat(Nx,Ny))
allocate(Vcrit(Nx,Ny))

do j = 1, Ny
do i = 1, Nx
  if (fcly(i,j) + fsnd(i,j) > 1) fcly(i,j) = 1 - fsnd(i,j)
  b(i,j) = 3.1 + 15.7*fcly(i,j) - 0.3*fsnd(i,j)
  hcap_soil(i,j) = (2.128*fcly(i,j) + 2.385*fsnd(i,j))*1e6 / (fcly(i,j) + fsnd(i,j))
  sathh(i,j) = 10**(0.17 - 0.63*fcly(i,j) - 1.58*fsnd(i,j))
  Vsat(i,j) = 0.505 - 0.037*fcly(i,j) - 0.142*fsnd(i,j)
  Vcrit(i,j) = Vsat(i,j)*(sathh(i,j)/3.364)**(1/b(i,j))
  hcon_min = (hcon_clay**fcly(i,j)) * (hcon_sand**(1 - fcly(i,j)))
  hcon_soil(i,j) = (hcon_air**Vsat(i,j)) * (hcon_min**(1 - Vsat(i,j)))
end do
end do

! Convert time scales from hours to seconds

tcnc = 3600*tcnc
tcnm = 3600*tcnm
tcld = 3600*tcld
tmlt = 3600*tmlt
trho = 3600*trho

! Allocate state variables

allocate(albs(Nx,Ny))
allocate(Ds(Nsmax,Nx,Ny))
allocate(Nsnow(Nx,Ny))
allocate(Qcan(Nx,Ny))
allocate(Sice(Nsmax,Nx,Ny))
allocate(Sliq(Nsmax,Nx,Ny))
allocate(Sveg(Nx,Ny))
allocate(Tcan(Nx,Ny))
allocate(theta(Nsoil,Nx,Ny))
allocate(Tsnow(Nsmax,Nx,Ny))
allocate(Tsoil(Nsoil,Nx,Ny))
allocate(Tsrf(Nx,Ny))
allocate(Tveg(Nx,Ny))

! Default initialization of state variables

albs(:,:)    = 0.8
Ds(:,:,:)    = 0
Nsnow(:,:)   = 0
Qcan(:,:)    = 0
Sice(:,:,:)  = 0
Sliq(:,:,:)  = 0
Sveg(:,:)    = 0
Tcan(:,:)    = 285
Tsnow(:,:,:) = Tm
Tsoil(:,:,:) = 285
Tveg(:,:)    = 285

! Initial soil profiles from namelist

allocate(fsat(Nsoil))
allocate(Tprof(Nsoil))
fsat(:)  = 0.5
Tprof(:) = 285
do k = 1, Nsoil
  theta(k,:,:) = fsat(k)*Vsat(:,:)
  Tsoil(k,:,:) = Tprof(k)
end do
Tsrf(:,:) = Tsoil(1,:,:)

! Outputs

allocate(diags(Nx,Ny,Ndiags))
allocate(SWin(Nx,Ny))
allocate(SWout(Nx,Ny))
diags(:,:,:) = 0
SWin(:,:) = 0
SWout(:,:) = 0
Nave = 24

allocate(SWE_mean(Nx))
allocate(HS_mean(Nx))
allocate(alb_mean(Nx))
allocate(Gsurf_mean(Nx))
allocate(Hatmo_mean(Nx))
allocate(Latmo_mean(Nx))
allocate(Melt_mean(Nx))
allocate(Rnet_mean(Nx))
allocate(Rof_mean(Nx))
allocate(Tsurf_mean(Nx))
allocate(Tsoil_mean(Nx))

! Write short log to screen

print *, "Config: ", ALBEDO, CANMOD, CONDCT, DENSTY, EXCHNG, HYDROL
print *, "Simulation period: ", istart, istop

end subroutine SETUP
