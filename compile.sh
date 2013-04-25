#!/bin/bash

NETCDF="/opt/netcdf/4.2-intel"
LIBS="-L/opt/netcdf/4.2-intel/lib -lnetcdf -lnetcdff"

cd src

ifort -w -convert big_endian -fp-model source -I$NETCDF/include -O3 -c joinwrf.f90
ifort -w -convert big_endian -fp-model source -I$NETCDF/include -O3 -c fjoinwrf.f90
ifort -w -convert big_endian -fp-model source -O3 -c timelib3d.f90

ifort -O3 -o ../joinwrfh joinwrf.o fjoinwrf.o timelib3d.o $LIBS

ls ../joinwrfh >& /dev/null
if [ $? = 0 ]; then
 echo SUCCESS
else
 echo FAILURE
fi