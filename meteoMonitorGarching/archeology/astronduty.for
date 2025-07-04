C----------------------------------------------------------------------
      program astronduty
C     
c     Displays meteorological and seeing information, and generates
C     warning messages when the conditons are critical.
c
C     This program reads the files meteo.last and dimm.last
C     copied from the meteo station and seeing monitor by exec show duty.
c
C     SHOULD BE CALLED ONLY BE    SHOWDUTY
C
c     ORH 8feb92
C----------------------------------------------------------------------

      character*80 time,seeing
      
      open(unit=50,file='meteo.last')
      open(unit=51,file='dimm.last')

      read(50,9999,err=8887)time
 8587 read(51,9999,err=8886)seeing
 8586 read(50,*,err=8555)
      read(50,9998,err=8885)t,tav
 8585 read(50,*,err=8555)
      read(50,*,err=8555)
      read(50,9997,err=8884)irh,irhav
 8584 read(50,9998,err=8883)td,tdav
 8583 read(50,9998,err=8882)p,pav
 8582 read(50,*,err=8555)
      read(50,9998,err=8881)ws1,ws1av
 8581 read(50,9998,err=8880)ws2,ws2av
 8580 read(50,9998,err=8879)ws3,ws3av
 8579 read(50,9997,err=8555)
      read(50,9997,err=8555)
      read(50,9997,err=8878)iwd3,iwd3av

 8578 rh= irh
      rhav= irhav
      ws= (ws1+ws2+ws3)/3.
      wd= iwd3*1
      wsav= (ws1av+ws2av+ws3av)/3.
      wdav= iwd3av*3.

 9999 format(80a)
 9998 format(11x,f7.1,f7.1)
 9997 Format(11x,I7,I7)
     

      write(6,9996)time(1:30)
 9996 Format('[1;1H',19x,80a)
      write(6,*)' Wind:                                    Current',
     >  '    Average'
      write(6,*)'      + N +        Temperature (C)  : ', t,tav
      write(6,*)'    \       /      Humidity    (%)  : ',rh,rhav
      write(6,*)'   +         +     Dew Point   (C)  : ',td,tdav
      write(6,*)'   W    +    E     Pressure    (mB) : ',p,pav
      write(6,*)'   +         +     Wind        (m/s): ',ws,wsav
      write(6,*)'    /       \      '
      write(6,*)'      + S +       ',seeing(11:46)

      read(seeing(27:30),*,err=9990)xseeing
      if (xseeing.lt.0.01)xseeing=99.99
 9991 if(wd.ge.11.25 .and. wd.lt.33.75 )write(6,*)'[3;11H[7m+[0m'
      if(wd.ge.33.75 .and. wd.lt.46.25 )write(6,*)'[4;13H[7m/[0m'
      if(wd.ge.46.25 .and. wd.lt.78.75 )write(6,*)'[5;14H[7m+[0m'
      if(wd.ge.78.75 .and. wd.lt.101.25)write(6,*)'[6;14H[7mE[0m'
      if(wd.ge.101.25.and. wd.lt.122.75)write(6,*)'[7;14H[7m+[0m'
      if(wd.ge.122.75.and. wd.lt.141.25)write(6,*)'[8;13H[7m\[0m'
      if(wd.ge.141.25.and. wd.lt.168.75)write(6,*)'[9;11H[7m+[0m'
      if(wd.ge.168.75.and. wd.lt.191.25)write(6,*)'[9;9H[7mS[0m'
      if(wd.ge.191.25.and. wd.lt.213.75)write(6,*)'[9;7H[7m+[0m'
      if(wd.ge.213.75.and. wd.lt.236.25)write(6,*)'[8;5H[7m/[0m'
      if(wd.ge.236.25.and. wd.lt.258.75)write(6,*)'[7;4H[7m+[0m'
      if(wd.ge.258.75.and. wd.lt.281.25)write(6,*)'[6;4H[7mW[0m'
      if(wd.ge.281.25.and. wd.lt.303.75)write(6,*)'[5;4H[7m+[0m'
      if(wd.ge.303.75.and. wd.lt.326.25)write(6,*)'[4;5H[7m\[0m'
      if(wd.ge.326.25.and. wd.lt.348.75)write(6,*)'[3;7H[7m+[0m'
      if(wd.ge.348.75.and. wd.lt.360.  )write(6,*)'[3;9H[7mN[0m'
      if(wd.ge.0.    .and. wd.lt.11.25 )write(6,*)'[3;9H[7mN[0m'
      write(6,*)'[10;1H'
      dtd = t-td

      if(xseeing.le.0.7)then
        write(6,*) '     ********* N.T.T **********'
        write(6,*) '        Seeing = ',xseeing
        WRITE(6,*) '        Service Observing???'
        write(6,*) '     ********* N.T.T **********'
        write(6,*)
        ENDIF

      if(rh.ge.90. .and. rh.le.95.)then
        write(6,*) '     ******** WARNING *********'
        write(6,*) '     * Humidity: ',rh,'  *'
        write(6,*) '     **************************'
        Write(6,*)
        endif
      if(dtd.le.5 .and. dtd.gt.2.5)then
        write(6,*) '     ******** WARNING *********'
        write(6,*) '     * T - DewPt: ',dtd,' *'
        write(6,*) '     **************************'
        write(6,*)
        endif
      if(ws.ge.14. .and. ws.lt.20.)then
        write(6,*) '     ******** WARNING *********'
        write(6,*) '     *   Wind Speed  >14.m/s  *'
        write(6,*) '     *   CALL THE DOMES       *'
        write(6,*) '     **************************'
        write(6,*)
        endif
      if(rh.ge.95.)then
        write(6,*) '     [7m******** DANGER **********[0m'
        write(6,*) '     [7m* Humidity: ',rh,'  *[0m'
        write(6,*) '     [7m**************************[0m'
        write(6,*)
        endif

      if(dtd.le. 2.5)then
        write(6,*) '     [7m******** DANGER **********[0m'
        write(6,*) '     [7m* T - DewPt: ',dtd,' *[0m'
        write(6,*) '     [7m**************************[0m'
        write(6,*)
        endif
      if(ws.ge.20.)then
        write(6,*) '     [7m******** DANGER **********[0m'
        write(6,*) '     [7m* Wind Speed > 20 m/s    *[0m'
        write(6,*) '     [7m**************************[0m'
        write(6,*)
        endif

      if(dtd.le.2.5 .or. rh.ge.95. or. ws.ge.20.)then
        write(6,*) '[11;40H[7;5m==========================[0m'
        write(6,*) '[12;40H[7;5m|     CLOSE THE DOMES    |[0m'
        WRITE(6,*) '[13;40H[7;5m==========================[0m'
        ENDIF
 
      write(6,*) '[22;1H'
      goto 7777
 9990 xseeing=9.999
      goto 9991
 8887 time= ' --- '
      goto 8587
 8886 seeing= '             No data...'
      goto 8586
 8885 t= 99.999
      tav= 99.999
      goto 8585
 8884 irh= -99
      irhav = -99
      goto 8584
 8883 td= 99.999
      tdav= 90.999
      goto 8583
 8882 p= 99.999
      pav= 99.999
      goto 8582
 8881 ws1= 0.
      wsav= 0.
      goto 8581
 8880 ws2= ws1
      ws2av= ws1av
      goto 8580
 8879 ws3= ws1
      ws3av= ws1av
      goto 8579
 8878 iwd3= 1000
      iw3av= 1000
      goto 8578
 8555 Write(6,*) '[2J***** Sory, no data; wait next update *****'
 7777 END

