#!/bin/csh
# get the cloud measurement from Danish telescope
wget http://www.asu.cas.cz/~dk154/template.html -O pyrgeo.last
grep MEPYF pyrgeo.last | gawk -v FS="<|>" '{print $5}' >! pyrgeo.dat

