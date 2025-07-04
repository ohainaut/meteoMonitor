#----------------------------------------------------------------------
# meteomonitor makefile
#
# @(#) $Id:$
#
# who      when       what
# -------- ---------- ---------------------------------
# ohainaut 2010-01-10 updated paths, added SM recompilation
# ohainaut 2003-09-07 vlt-like structure
# ohainaut 2003-02-09 updated for losame
#----------------------------------------------------------------------

# OFFLINE DEFINITIONS
F77=gfortran -std=legacy 

XLIB =  -L /usr/X11R6/lib/ -lX11 -lnsl
DEST = /home/ohainaut/Source/Packages/meteo/meteoMonitor/src/junk/
.SUFFIXES: .o .for .f
.f.o:
	$(F77) -ff2c -c $<

#*# # ONLINE DEFINITIONS
#*# F77 = f77
#*# SCISOFT = /home/dimm/SOURCES/meteoMonitor/sm/sm2_4_2
#*# XLIB =  -lnsl -lX11 -lm -ldl
#*# DEST = $(HOME)/dimm/
#*# # RULES:
#*# .SUFFIXES: .o .for .f
#*# .f.o:
#*# 	$(F77) -c $<

#----------------------------------------------------------------------
# LIB:
SMDIR =  $(HOME)/Source/Packages/meteo/meteoMonitor/sm/sm2_4_2/src
SM = $(SMDIR)/libplotsub.a \
     $(SMDIR)/libdevices.a \
     $(SMDIR)/libutils.a 



#----------------------------------------------------------------------

FORTRAN = meteoli read_dimm

HTML = blueball.gif  greenball.gif  orangeball.gif  redball.gif  yellowball.gif dimm.html meteo.html eso-logo.gif eso.gif meteo_msg.txt

all: $(FORTRAN)	

install: 
	#
	# install compiled executable
	#
	cp $(FORTRAN) $(DEST) 
	#
	# install execs
	#
	cd ../exec ; chmod uog+rx * ; cp * $(DEST)
	#
	# install configs and html
	#
	cd ../config ; cp $(HTML) $(DEST)
	cd $(DEST) ; rm -f index.html ; ln -s meteomonitor.html index.html
	# 
	# install SM configuration files
	#
	cd ../config ; cp config.sm $(DEST)/.sm
	cd ../config ; cp graphcap.meteoli $(DEST)/.graphcap.meteoli
	cd ../config ; cp meteo.config  $(DEST) 
	#
	# install archive
	#	
	mkdir  $(DEST)/Archive
	(cd ../archive ; tar cf - * ) | (cd $(DEST)/Archive ; tar xf -)
	#
	# ... install done

clean:
	rm -f *~ ../config/*~ ../exec/*~ ../*~
	rm -f *.o
	# ...cleaning done


mrproper:
	# full cleaning, preserving lasc...
	cd $(DEST); mv -f lasc ~ ; rm -rf * ; mv -f ~/lasc .
	# ... done

update:
	# update message
	cp $(DEST)/meteo_msg.txt ../config
	# updating archive...
	(cd $(DEST)/Archive ; tar cf - * ) | ( cd ../archive ; tar xf - )
	# ... done

#----------------------------------------------------------------------

# FORTRAN sources:


meteoli: meteoli.o
	$(F77) -o meteoli meteoli.o ${SM} $(XLIB) 

meteoli_lossam: meteoli_lossam.o
	$(F77) -o meteoli_lossam meteoli_lossam.o ${SM} $(XLIB) -lm -ldl

read_dimm: read_dimm.o
	$(F77) -o read_dimm read_dimm.o  $(XLIB)


#------------------------------------------------------------------------------
# to recompile the SM libraries

smLibs:
	cd ~/SOURCES/meteoMonitor/sm/sm2_4_2/src/plotsub
	make
	cd ~/SOURCES/meteoMonitor/sm/sm2_4_2/src/devices
	make
	cd ~/SOURCES/meteoMonitor/sm/sm2_4_2/src/utils
	make


# --oOo--
