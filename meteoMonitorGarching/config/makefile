#----------------------------------------------------------------------
# .TITLE Makefile
#----------------------------------------------------------------------

DEST = ../../

#----------------------------------------------------------------------

all:	scripts html config

#--scripts

SCRIPTS = meteorun xearth

scripts:
	chmod uog+rx $(SCRIPTS)
	cp $(SCRIPTS) $(DEST)


#--html
HTML = blueball.gif  greenball.gif  orangeball.gif  redball.gif  yellowball.gif

html:
	cp $(HTML) $(DEST)
	cd $(DEST) ; ln -s meteomonitor.html index.html

config:
	cp config.sm $(HOME)/.sm
	cp graphcap.meteoli $(HOME)/.graphcap.meteoli
	cp meteo.config $(DEST)
