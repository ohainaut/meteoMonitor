C----------------------------------------------------------------------
      program astronduty
C     
C.purpose
C     Converts meteo input files from
C     - Vaisala tower (meteo.last)
C     - DIMM/ASM (dimm.last)
C     - LOSSAM extinction (sky.last)
C     - NTT seeing (ntt.last)
C     into a generic log file    dimm.dat  which is kept to 800 lines (FIFO)
C     
C     The input files are copied to the current directory by meteorun
C     The output file is converted into the MeteoMonitor web page
C     and graphs by meteoli. 
c
C     This program should be called by meteorun.
C
C.hisory
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
C.version Tue Oct 16 23:41:01 2001 cleaned and updated
C.history
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
      integer LuInDimmDat,  LuOutDimmDat


      real*4 abs_time, DimmSeeingTime, NTTSeeingTime, meteo_time 
                                ! time of measurement for abs,
                                ! seeing and meteo  
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
      integer iwd3, iawd3       ! integer value of the wind direction.

c      real*4 dtd

      character*80 meteo,seeing,sky
      character line*160
      character xabs_ra*5, xabs_dec*6 ! star on which  absorption is measured


C----------------------------------------------------------------------
C     READ THE DATA
C----------------------------------------------------------------------

      LuInMeteoLast = 50
      LuInNttLast = 51
      LuInDimmLast = 54
      LuInSkyLast = 52
      LuInDimmDat = 55
      LuOutDimmDat = 60

      open(unit=LuInMeteoLast,file='meteo.last')
C-DIMM
      open(unit=LuInDimmLast,file='dimm.last')
C-NTT
      open(unit=LuInNttLast,file='ntt.last')
c      open(unit=LuInSkyLast,file='sky.last')
      open(unit=LuInSkyLast,file='photonight.lis')
      open(unit=LuInDimmDat,file='dimm.dat')
      open(unit=LuOutDimmDat,file='dimm_new.dat')

      xabs_ra = "-----"
      xabs_dec = "------"


C     read sky.last

 1100 read(LuInSkyLast,9999,end=1200,err=1200)sky

 8589 i = index(sky,'')       ! for some reasons, there is a ^M in the
                                ! line... 
      if (i .gt. 0) then 
         sky(i:i) = ' '
c         goto 8589
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

C     read ntt.last

 1200 read(LuInNttLast,9999,err=8200,end=8200)seeing
      read (seeing(13:14),*,err=8200,end=8200) ih ! get time of the seeing
      read (seeing(16:17),*,err=8200,end=8200) im
      read (seeing(19:20),*,err=8200,end=8200) is
      NTTSeeingTime = ih+im/60.+is/3600.

      i = index(seeing,'seeing=') +7 ! find the seeing
      read(seeing(i:),*,err=8200,end=8200) xNttSeeing




C     read dimm.last

 1210 read(LuInDimmLast,9999,err=8250,end=8250)seeing

C old | Pre-upgrade format
C old      read (seeing(13:14),*,err=8200,end=8200) ih ! get time of the seeing
C old      read (seeing(16:17),*,err=8200,end=8200) im
C old      read (seeing(19:20),*,err=8200,end=8200) is
C old      DimmSeeingTime = ih+im/60.+is/3600.
C old
C old      i = index(seeing,'seeing=') +7 ! find the seeing
C old      read(seeing(i:),*,err=8250,end=8250) xDimmSeeing

C     | post upgrade format

C|123456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C| 1999-03-22T02:14:52:  Current seeing=0.69 arcsec at 0.5 um (Airmass 1.42)

      read (seeing(13:14),*,err=8200,end=8200) ih ! get time of the seeing
      read (seeing(16:17),*,err=8200,end=8200) im
      read (seeing(19:20),*,err=8200,end=8200) is
      DimmSeeingTime = ih+im/60.+is/3600.

      i = index(seeing,'seeing=') +7 ! find the seeing
      read(seeing(i:),*,err=8250,end=8250) xDimmSeeing


C     read meteo.last

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
 8580 read(LuInMeteoLast,9998,err=8879,end=7888)ws3,aws3
                                !WS3 M/S 20M    5.7    6.0    8.9    4.0    0.9
 8579 read(LuInMeteoLast,9997,err=7888,end=7888)    !skip
                                !WD1 DEG 20M    203    204    174    180      6
      read(LuInMeteoLast,9997,err=7888,end=7888)    !skip
                                !WD2 DEG 20M    191    200    231    169     10
      read(LuInMeteoLast,9997,err=8878,end=7888)iwd3,iawd3
                                !WD3 DEG 20M    203    207    236    180      8



 8578 continue

c     Wind       ! wind speed = average 10m and 30m + check validity
      wnd = 0
      awnd = 0
      i = 0
      if (ws2 .gt. 0) then
         wnd = wnd + ws2
         awnd = awnd + aws2
         i = i+1
      endif
      if (ws3 .gt. 0) then
         wnd = wnd + ws3
         awnd = awnd + aws3
         i = i+1
      endif

      if (i.gt.0) then
         wnd = wnd/i
         awnd = awnd/i
      endif

      if (iwd3.gt.360) iwd3 = -9
      if (iawd3.gt.360) iawd3 = -9


 9999 format(160a)
 9998 format(11x,f7.1,f7.1)
 9997 Format(11x,I7,I7)



C----------------------------------------------------------------------
c     TTy OUTPUT DBG
C----------------------------------------------------------------------
C 
C (from good old original meteomonitor)    
C 
C      write(6,9996)meteo(1:30)
C 9996 Format('[1;1H',19x,80a)
C      write(6,*)' Wind:                                    Current',
C     >  '    Average'
C      write(6,*)'      + N +        Temperature (C)  : ', tmp,atmp
C      write(6,*)'    \       /      Humidity    (%)  : ',hum,ahum
C      write(6,*)'   +         +     Dew Point   (C)  : ',dpt,adpt
C      write(6,*)'   W    +    E     Pressure    (mB) : ',phg,aphg
C      write(6,*)'   +         +     Wind        (m/s): ',wnd,awnd
C      write(6,*)'    /       \      '
C      write(6,*)'      + S +       ',seeing(11:46)
C 
C      read(seeing(27:30),*,err=9990)xNttSeeing
C      if (xNttSeeing.lt.0.01)xNttSeeing=99.99
C 9991 if(iwd3.ge.11.25 .and. iwd3.lt.33.75 )
C     $     write(6,*)'[3;11H[7m+[0m'
C      if(iwd3.ge.33.75 .and. iwd3.lt.46.25 )
C     $     write(6,*)'[4;13H[7m/[0m'
C      if(iwd3.ge.46.25 .and. iwd3.lt.78.75 )
C     $     write(6,*)'[5;14H[7m+[0m'
C      if(iwd3.ge.78.75 .and. iwd3.lt.101.25)
C     $     write(6,*)'[6;14H[7mE[0m'
C      if(iwd3.ge.101.25.and. iwd3.lt.122.75)
C     $     write(6,*)'[7;14H[7m+[0m'
C      if(iwd3.ge.122.75.and. iwd3.lt.141.25)
C     $     write(6,*)'[8;13H[7m\[0m'
C      if(iwd3.ge.141.25.and. iwd3.lt.168.75)
C     $     write(6,*)'[9;11H[7m+[0m'
C      if(iwd3.ge.168.75.and. iwd3.lt.191.25)
C     $     write(6,*)'[9;9H[7mS[0m'
C      if(iwd3.ge.191.25.and. iwd3.lt.213.75)
C     $     write(6,*)'[9;7H[7m+[0m'
C      if(iwd3.ge.213.75.and. iwd3.lt.236.25)
C     $     write(6,*)'[8;5H[7m/[0m'
C      if(iwd3.ge.236.25.and. iwd3.lt.258.75)
C     $     write(6,*)'[7;4H[7m+[0m'
C      if(iwd3.ge.258.75.and. iwd3.lt.281.25)
C     $     write(6,*)'[6;4H[7mW[0m'
C      if(iwd3.ge.281.25.and. iwd3.lt.303.75)
C     $     write(6,*)'[5;4H[7m+[0m'
C      if(iwd3.ge.303.75.and. iwd3.lt.326.25)
C     $     write(6,*)'[4;5H[7m\[0m'
C      if(iwd3.ge.326.25.and. iwd3.lt.348.75)
C     $     write(6,*)'[3;7H[7m+[0m'
C      if(iwd3.ge.348.75.and. iwd3.lt.360.  )
C     $     write(6,*)'[3;9H[7mN[0m'
C      if(iwd3.ge.0.    .and. iwd3.lt.11.25 )
C     $     write(6,*)'[3;9H[7mN[0m'
C      write(6,*)'[10;1H'
C 
C      dtd = tmp-dpt
C 
C      if(xNttSeeing.le.0.7)then
C        write(6,*) '     ********* N.T.T **********'
C        write(6,*) '        Seeing = ',xNttSeeing
C        WRITE(6,*) '        Service Observing???'
C        write(6,*) '     ********* N.T.T **********'
C        write(6,*)
C        ENDIF
C 
C      if(hum.ge.90. .and. hum.le.95.)then
C        write(6,*) '     ******** WARNING *********'
C        write(6,*) '     * Humidity: ',hum,'  *'
C        write(6,*) '     **************************'
C        Write(6,*)
C        endif
C      if(dtd.le.5 .and. dtd.gt.2.5)then
C        write(6,*) '     ******** WARNING *********'
C        write(6,*) '     * T - DewPt: ',dtd,' *'
C        write(6,*) '     **************************'
C        write(6,*)
C        endif
C      if(wnd.ge.14. .and. wnd.lt.20.)then
C        write(6,*) '     ******** WARNING *********'
C        write(6,*) '     *   Wind Speed  >14.m/s  *'
C        write(6,*) '     *   CALL THE DOMES       *'
C        write(6,*) '     **************************'
C        write(6,*)
C        endif
C      if(hum.ge.95.)then
C        write(6,*) '     [7m******** DANGER **********[0m'
C        write(6,*) '     [7m* Humidity: ',hum,'  *[0m'
C        write(6,*) '     [7m**************************[0m'
C        write(6,*)
C        endif
C 
C      if(dtd.le. 2.5)then
C        write(6,*) '     [7m******** DANGER **********[0m'
C        write(6,*) '     [7m* T - DewPt: ',dtd,' *[0m'
C        write(6,*) '     [7m**************************[0m'
C        write(6,*)
C        endif
C      if(wnd.ge.20.)then
C        write(6,*) '     [7m******** DANGER **********[0m'
C        write(6,*) '     [7m* Wind Speed > 20 m/s    *[0m'
C        write(6,*) '     [7m**************************[0m'
C        write(6,*)
C        endif
C 
C      if(dtd.le.2.5 .or. hum.ge.95. or. wnd.ge.20.)then
C        write(6,*) '[11;40H[7;5m==========================[0m'
C        write(6,*) '[12;40H[7;5m|     CLOSE THE DOMES    |[0m'
C        WRITE(6,*) '[13;40H[7;5m==========================[0m'
C        ENDIF
C 
C      write(6,*) '[22;1H'


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
     $     0,
     $     xNttSeeing,NttSeeingTime,
     $     xDimmSeeing,
     $     xabs, xabs_rms,xabs_am,DimmSeeingTime,
     $     xabs_ra, xabs_dec

      write(6,*) xabs_ra, xabs_dec
 9989 format(
     $     i4,1x, i2,1x, i2,1x, i2,1x,  i2,1x, !iyear,...
     $     2(f5.1,1x),           ! tmp
     $     2(i3,1x),             ! hum
     $     2(f5.1,1x),           ! dpt
     $     2(f6.1,1x),           ! phg
     $     2(f4.1,1x),           ! wnd
     $     2(i3,1x),             ! wd3
     $     i2,1x,                ! ijunk (for compatibility)
     $     f5.2,1x,f6.3,1x,        ! NTT seeing and time
     $     f5.2,1x,              ! dimm seeing
     $     2f7.3,1x,f5.2,1x,f6.3, ! abs, amass and DimmSeeingTime
     $     'ARA',A5,'ADE',A6)


 7888 i = 1 ! skip here if there is no data
      do while (i.le.800)
 1234    read(LuInDimmDat,9999,err=8888,end=8888)line
         write(LuOutDimmDat,9999)line
         i = i+1
      enddo


 8888 continue
      END

