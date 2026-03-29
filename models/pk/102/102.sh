#!/bin/bash

#$ -wd /data/home/kyleb/project.mrg/az/current/models/pk/102

/opt/NONMEM/nm75/run/nmfe75 102.ctl  102.lst  -parafile=102.pnm -maxlim=2
