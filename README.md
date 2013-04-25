joinwrfh
========

Join WRF history file in netCDF format either in space (split files) or in time (files at multiple time levels into one file)..


Original code from the Advanced Regional Prediction (ARPS) v5.3.3 - http://www.caps.ou.edu/ARPS/

+ efficiency improvements from http://www.geomar.de/mitarbeiter/fb1/me/jharlass/wrf/joinwrfh/


The Advanced Regional Prediction System (ARPS), including its pre- and post-processing and data assimilation packages, resides in the public domain and may be used free of charge, and without restriction, including for-profit activities by private industry.
http://www.caps.ou.edu/ARPS/arpsdown.html



Compile script included for OMD cluster. You may edit it to fit your needs.

Dependencies : NetCDF

Compilation : ./compile.sh

Usage : ./joinwrfh < joinwrfh.input