#############################################################################
# Flexible Snow Model compilation script
#
# Richard Essery
# School of GeoSciences
# University of Edinburgh
#############################################################################
FC=gfortran
cd src

cat > OPTS.h << EOF
/* Process options */
#define ALBEDO 1   /* snow albedo: 0 - diagnostic, 1 - prognostic                          */
#define CANMOD 1   /* forest canopy: 0 - zero layer, 1 - one layer                         */
#define CONDCT 1   /* snow thermal conductivity: 0 - constant, 1 - Yen (1981)              */
#define DENSTY 1   /* snow density: 0 - constant, 1 - Verseghy (1991), 2 - Anderson (1976) */
#define EXCHNG 0   /* turbulent exchange: 0 - constant, 1 - Louis (1979)                   */
#define HYDROL 1   /* snow hydraulics: 0 - free draining, 1 - bucket                       */

/* Driving data options */
#define DRIV1D 0   /* 1D driving data format: 0 - FSM, 1 - ESM-SnowMIP                  */
#define DOWNSC 0   /* 1D driving data downscaling: 0 - no, 1 - yes                      */
#define DEMHDR 0   /* DEM header: 0 - none, 1 - ESRI format                             */
#define SWPART 0   /* SW radiation: 0 - total, 1 - direct and diffuse calculated        */
#define ZOFFST 0   /* Measurement height offset: 0 - above ground, 1 - above canopy top */
EOF

$FC -cpp -o FSM -O3 \
DATANC.F90 MODULES.F90 CANOPY.F90 CUMULATE.F90 DRIVENC.F90 DUMP.F90 EBALFOR.F90 \
EBALSRF.F90 FSMNC.F90 LUDCMP.F90 OUTPUTNC.F90 PHYSICS.F90 QSAT.F90    \
RADIATION.F90 READMAPS.F90 SETUPNC.F90 SNOW.F90 SOIL.F90 SFEXCH.F90      \
THERMAL.F90 TRIDIAG.F90 \
-I/usr/include -L/usr/lib -lnetcdff -lnetcdf
mv FSM ../FSM
rm *.mod
cd ..

