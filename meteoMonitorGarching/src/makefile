#----------------------------------------------------------------------
# .TITLE Makefile
# .PURPOSE all purpose makefile!
#----------------------------------------------------------------------
# LIB:


#== X11
#X11 =  -L /usr/X11R6/lib/ -lX11  -lnsl
#X11 =  -lX11  -lnsl
X11 =  -lX11  

#== TCL TK
TCL = /usr/lib/x86_64-linux-gnu/libtcl8.6.so /usr/lib/x86_64-linux-gnu//libtk8.6.so


#== FORTRAN
#UBUNTU
#F77=gfortran -std=legacy 
#Fedora ESO
F77 = f77

#== SMONGO
SMDIR =  $(HOME)/Source/Packages/meteo/meteoMonitor/sm/sm2_4_2/src
LIB_SM = $(SMDIR)/libplotsub.a \
     $(SMDIR)/libdevices.a \
     $(SMDIR)/libutils.a 


#----------------------------------------------------------------------
# RULES:
.SUFFIXES: .o .f
.f.o:
	$(F77) -ff2c -c $<


#----------------------------------------------------------------------


BIN = /home/ohainaut/bin
HTML = /home/ohainaut/public_html/meteo

OUT = meteoli_garching read_garching meteoDog

all: $(OUT)

install: 
	cp $(OUT) $(BIN)
	cp xearth $(BIN)
	cd ../config ; chmod uog+x meteorun_garching;  cp meteorun_garching $(BIN)
	cd ../config ; cp *gif meteo.config $(HTML)

clean:
	rm -f $(OUT) *.o

#----------------------------------------------------------------------



meteoli_garching: meteoli_garching.o
	$(F77) -o meteoli_garching meteoli_garching.o ${LIB_SM} $(X11) $(TCL) -lm

read_garching: read_garching.o
	$(F77) -o read_garching read_garching.o -lm

