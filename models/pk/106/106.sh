#!/bin/bash

#$ -wd /data/home/kyleb/project.mrg/az/current/models/pk/106

/opt/NONMEM/nm75/run/nmfe75 106.ctl  106.lst  -parafile=106.pnm -maxlim=2
