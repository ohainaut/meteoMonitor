C----------------------------------------------------------------------
      program astronduty
C     
C.purpose
C     Converts meteo input files from
C     - Vaisala tower (meteo.last)
C     - DIMM/ASM (dimm.last)
C     - LOSSAM extinction (sky.last)
C     - NTT seeing (ntt.last)
C     - cloud measurement (pyrgeo.dat)
C     into a generic log file    dimm.dat  which is kept to 800 lines (FIFO)
C     
C     The input files are copied to the current directory by meteorun
C     The output file is converted into the MeteoMonitor web page
C     and graphs by meteoli. 
c
C     This program should be called by meteorun.
C
C.history
C     Lots of code in this  prg is archeologic, dating from MeteoMonitor v.0.1
C     ("astrOnDuty") from the early '90s
c     |Displays meteorological and seeing information, and generates
C     |warning messages when the conditons are critical.
c     |
C     |This program reads the files meteo.last and dimm.last
C     |copied from the meteo station and seeing monitor by exec show duty.
c     |
C     |SHOULD BE CALLED ONLY BE    SHOWDUTY
C     |
c     |ORH 8feb92
C
C
C.history
C     2015-07-11 oh: read cloud measurement from Danish telescope
C     2015-04-01 oh: check to reject wind > 999 and wind dir > 999
c     2014-03-31 oh: more cleaning, extends log from 800 to 950 lines
c     2010-01-10 ohainaut some cleaning
C     Tue Oct 16 23:41:01 2001 cleaned and updated
C     1999-jun-26 ohainaut added a test on the anemometer: does not take
C       into account dead ones.
C     1999-mar ohainaut added NTT seeing
C     1997-nov ohainaut upgraded for MeteoMonitor V2x
C----------------------------------------------------------------------
      implicit none

      integer iyear, imonth, iday, ih, im, is ! date read from file
      integer ihdimm, imdimm ! date read from file
      integer i, ijunk
      integer LuInMeteoLast, LuInNttLast, LuInDimmLast, LuInSkyLast
      integer LuInCloud
      integer LuInDimmDat,  LuOutDimmDat


      real*4 abs_time, DimmSeeingTime, NTTSeeingTime, meteo_time 
                                ! time of measurement for abs,
                                ! seeing and meteo  
      real*4 cloud
      real*4 xabs, xabs_rms, xabs_am ! absorption and related
      real*4 xNttSeeing, xDimmSeeing            ! seeing meas.
      real*4 tmp, atmp          ! temperature and average
      integer ihum, iahum       ! integer value of Humidity and av.
      real*4 dpt, adpt          ! dew point and av.
      real*4 phg, aphg          ! pressure (in mB, not in mmHg!) and av
      real*4 ws1, aws1          ! wind speeds (level 1, 2, 3) and av.
      real*4 ws2, aws2
      real*4 ws3, aws3
      real*4 wnd, awnd          ! mean wind speed and av.
      integer iwd1, iawd1       ! integer value of the wind direction.
      integer iwd2, iawd2       ! integer value of the wind direction.
      integer iwd3, iawd3       ! integer value of the wind direction.

c      real*4 dtd

      character*80 meteo,seeing,sky
      character line*160
      character xabs_ra*5, xabs_dec*6 ! star on which  absorption is measured


C----------------------------------------------------------------------
C     READ THE DATA
C----------------------------------------------------------------------

C     define logical units

      LuInMeteoLast = 50
      LuInNttLast = 51
      LuInDimmLast = 54
      LuInSkyLast = 52
      LuInDimmDat = 55
      LuInCloud = 56
      LuOutDimmDat = 60

      open(unit=LuInMeteoLast,file='meteo.last') ! weather
      open(unit=LuInDimmLast,file='dimm.last')   ! dimm seeing
      open(unit=LuInCloud,file='pyrgeo.dat')     ! Cz cloud
c      open(unit=LuInNttLast,file='ntt.last')    ! ntt seeing, obsolete

      open(unit=LuInSkyLast,file='photonight.lis')

      open(unit=LuInDimmDat,file='dimm.dat')     ! previous database file in
      open(unit=LuOutDimmDat,file='dimm_new.dat')! next database file out

      xabs_ra = "-----"
      xabs_dec = "------"

C-----
C     read sky.last

 1100 read(LuInSkyLast,9999,end=1200,err=1200)sky

 8589 i = index(sky,'')       ! for some reasons, there is a ^M in the
                                ! line... 
      if (i .gt. 0) then 
         sky(i:i) = ' '
      endif

      read (sky(13:14),*,err=8100,end=8100) ih    ! get time of the abs meas.
      read (sky(16:17),*,err=8100,end=8100) im
      read (sky(19:20),*,err=8100,end=8100) is
      abs_time = ih+im/60.+is/3600.
      i = index(sky,'Ave=') +4 ! find the Absorption
      read(sky(i:i+5),*,err=8100,end=8100) xabs
      i = index(sky,'Rms=') +4 
      read(sky(i:i+5),*,err=8100,end=8100) xabs_rms
      i = index(sky,'AM=') +3
      read(sky(i:i+6),*,err=8100,end=8100) xabs_am
      i = index(sky,'RA:') +3 
      xabs_ra = sky(i:i+4)
      i = index(sky,'Dec:') +4 
      xabs_dec = sky(i:i+5)


      read(sky,*,err=1100,end=8100) ijunk, abs_time, xabs, xabs_rms

      goto 1100







c
C     read ntt.last
c
 1200 continue

C2014: NTT is obsolete
c*      read(LuInNttLast,9999,err=8200,end=8200)seeing
c*      read (seeing(13:14),*,err=8200,end=8200) ih ! get time of the seeing
c*      read (seeing(16:17),*,err=8200,end=8200) im
c*      read (seeing(19:20),*,err=8200,end=8200) is
c*      NTTSeeingTime = ih+im/60.+is/3600.
c*
c*      i = index(seeing,'seeing=') +7 ! find the seeing
c*      read(seeing(i:),*,err=8200,end=8200) xNttSeeing
      NTTseeingTime = -9999.
      xNTTSeeing = -9999.



!!!

c----------------------------------------------------------------------
C     READ THE CLOUD FILE
c----------------------------------------------------------------------
      read(LuIncloud,*,err=940,end=940) cloud
      goto 950

C error:
 940  cloud = -9999


 950  continue



c------------------------------------------------------------------------------
C     read dimm.last
c------------------------------------------------------------------------------

 1210 read(LuInDimmLast,9999,err=8250,end=8250)seeing

C     | post upgrade format

C|123456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C| 1999-03-22T02:14:52:  Current seeing=0.69 arcsec at 0.5 um (Airmass 1.42)

      read (seeing(13:14),*,err=8200,end=8200) ih ! get time of the seeing
      read (seeing(16:17),*,err=8200,end=8200) im
      read (seeing(19:20),*,err=8200,end=8200) is
      DimmSeeingTime = ih+im/60.+is/3600.

      i = index(seeing,'seeing=') +7 ! find the seeing
      read(seeing(i:),*,err=8250,end=8250) xDimmSeeing



c----------------------------------------------------------------------
C     read meteo.last
c----------------------------------------------------------------------

 1300 read(LuInMeteoLast,*,err=7888,end=7888)
      read(LuInMeteoLast,9999,err=7888,end=7888)meteo !1997-11-30  20:59 LASILLA1

      read(meteo(1:4),*,err=7888,end=7888) iyear
      read(meteo(6:7),*,err=7888,end=7888) imonth
      read(meteo(9:10),*,err=7888,end=7888) iday
      read (meteo(13:14),*,err=7888,end=7888) ih
      read (meteo(16:17),*,err=7888,end=7888) im

      meteo_time = ih+im/60.

      read(LuInMeteoLast,*,err=7888,end=7888)       ! skip
                                !       INTV   INST    AVE    MAX    MIN    DEV
      read(LuInMeteoLast,9998,err=8885,end=7888)tmp,atmp ! keep 2m altitude temp
                                !T 1 C   20M   14.3   13.7   14.3   13.3    0.2
 8585 read(LuInMeteoLast,*,err=7888,end=7888) !T 2 C   20M   30.3   30.3   30.3   30.3    0.
      read(LuInMeteoLast,*,err=7888,end=7888) !T 3 C   20M   12.5   12.4   12.5   12.3    0.1
      read(LuInMeteoLast,9997,err=8884,end=7888)ihum,iahum
                                !RH  %   20M     44     45     47     43 
 8584 read(LuInMeteoLast,9998,err=8883,end=7888)dpt,adpt
                                !TD  C   20M    2.4    2.0    2.5    1.3   
 8583 read(LuInMeteoLast,9998,err=8882,end=7888)phg, aphg
                                !P   MB  20M  771.0  770.8  771.0  770.7    0.1
 8582 read(LuInMeteoLast,*,err=7888,end=7888)       ! skip
                                !QNH MB  20M 1025.9 1025.7 1025.9 1025.6    0.1
      read(LuInMeteoLast,9998,err=8881,end=7888)ws1,aws1
                                !WS1 M/S 20M    0.2    0.2    0.9    0.0    0.2
 8581 read(LuInMeteoLast,9998,err=8880,end=7888)ws2,aws2
                                !WS2 M/S 20M    5.2    5.6    9.4    0.0    1.9
 8580 read(LuInMeteoLast,9998,err=8879,end=7888) ws3,aws3
                                !WS3 M/S 20M    5.7    6.0    8.9    4.0    0.9
 8579 read(LuInMeteoLast,9997,err=8586, end=7888) iwd1,iawd1
                                !WD1 DEG 20M    203    204    174    180      6
 8586 read(LuInMeteoLast,9997,err=8587, end=7888) iwd2,iawd2
                                !WD2 DEG 20M    191    200    231    169     10
 8587 read(LuInMeteoLast,9997,err=8878,end=7888) iwd3,iawd3
                                !WD3 DEG 20M    203    207    236    180      8



 8578 continue

c     Wind       ! wind speed = average 10m and 30m + check validity
      wnd = 0
      awnd = 0
      i = 0



      if (ws2 .gt. 0 .and. ws2 .lt. 999.) then
         wnd = wnd + ws2
         awnd = awnd + aws2
         i = i+1
      endif
      if (ws3 .gt. 0 .and. ws3 .lt. 999.) then
         wnd = wnd + ws3
         awnd = awnd + aws3
         i = i+1
      endif

      if (i.gt.0) then
         wnd = wnd/i
         awnd = awnd/i
      endif

      if (iwd3.gt.999) then
         if (iwd2.gt.999) then
            if (iwd1.gt.999) then
               iwd3 = -9
               iawd2 = -9.
            else
               iwd3 = iwd1
               iawd3 = iawd1
            endif
         else
            iwd3 = iwd2
            iawd3 = iawd2
         endif
      endif

      if (iwd3.gt.360) iwd3 = -9
      if (iawd3.gt.360) iawd3 = -9


 9999 format(160a)
 9998 format(11x,f7.1,f7.1)
 9997 Format(11x,I7,I7)



C----------------------------------------------------------------------
C     READ ERRORS
C----------------------------------------------------------------------
      goto 7777                 ! skip errors

c 9990 xNttSeeing=9.999
c      goto 9991

 8100 xabs = -9.                ! no absorption data
      xabs_rms = -9.
      xabs_am = -9.
      xabs_ra = "-O-"
      xabs_dec = '-O-'
      goto 1200

c 8300 meteo= ' --- '
c      iyear = -9
c      imonth = -9
c      iday = -9
c      ih = -9
c      im = -9
c      goto 8588

 8200 xNttSeeing= -9.              ! no seeing
      ih = -9
      im = -9
      is = -9
      goto 1210

 8250 xDimmSeeing= -9.              ! no seeing
      ih = -9
      im = -9
      is = -9
      goto 1300
      
 8885 tmp= 99.999
      atmp= 99.999
      goto 8585
 8884 ihum= -99
      iahum = -99
      goto 8584
 8883 dpt= 99.999
      adpt= 90.999
      goto 8583
 8882 phg= 99.999
      aphg= 99.999
      goto 8582
 8881 ws1= 0.
      aws1= 0.
      goto 8581
 8880 ws2= ws1
      aws2= aws1
      goto 8580
 8879 ws3= ws1
      aws3= aws1
      goto 8579
 8878 iwd3= 1000
      iawd3= 1000
      goto 8578
 8555 Write(6,*) '[2J***** Sory, no data; wait next update *****'

C----------------------------------------------------------------------
C     Read and update log file.
C----------------------------------------------------------------------


 7777 write(LuOutDimmDat,9989) iyear, imonth, iday, ih, im,
     $     tmp, atmp,
     $     ihum, iahum,
     $     dpt, adpt,
     $     phg, aphg,
     $     wnd, awnd,
     $     iwd3,iawd3,
     $     xDimmSeeing,
     $     DimmSeeingTime,
     $     cloud

 9989 format(
     $     i4,1x, i2,1x, i2,1x, i2,1x,  i2,1x, !iyear,...
     $     2(f5.1,1x),           ! tmp
     $     2(i3,1x),             ! hum
     $     2(f5.1,1x),           ! dpt
     $     2(f6.1,1x),           ! phg
     $     2(f4.1,1x),           ! wnd
     $     2(i3,1x),             ! wd3
     $     f5.2,1x,              ! dimm seeing
     $     f6.3,                 !  DimmSeeingTime
     $     f7.2                  ! cloud pyrgeometer
     $ )


 7888 i = 1 ! skip here if there is no data
      do while (i.le.950)
 1234    read(LuInDimmDat,9999,err=8888,end=8888)line
         write(LuOutDimmDat,9999)line
         i = i+1
      enddo


 8888 continue
      END

