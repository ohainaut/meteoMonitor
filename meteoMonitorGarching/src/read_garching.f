C     .version 2012-05-01T11:45:30 Tue  fixed format for date
c.history
      implicit none

      integer inLu, outLu, i, LuInDimmDat, LuOutDimmDat
      integer iday, imonth, iyear, ih, im
      real*4 x(10)

      integer ihum, iahum, iwd, iawd,ipunkt
      real*4 temp, atemp, dpt, adpt, wnd, awnd, phg, aphg, xabs
      real*4 rain, raintot,sun

      character myLine*128

      inLu = 50
      open(inLu,file="doku.txt")

      LuInDimmDat = 55
      LuOutDimmDat = 60
      open(unit=LuInDimmDat,file='dimm.dat')
      open(unit=LuOutDimmDat,file='dimm_new.dat')



 1000 read(inLu,9999,err=1000,end=7000) myline
 9999 format(a128)

C     Date
      if (index(myline,"Messwert").gt.0) then
         read(inLu,9999,err=1000,end=7000) myline
         read(inLu,9999,err=1000,end=7000) myline
         ipunkt = index(myline,".")-1
         read(myLine(:ipunkt),*,err=1000,end=1000) iday
         myline = myline(ipunkt+2:)
         ipunkt = index(myline,".")-1
         read(myLine(:ipunkt),*,err=1000,end=1000) imonth
         myline = myline(ipunkt+2:)
         ipunkt = index(myline," ")-1
         read(myLine(:ipunkt),*,err=1000,end=1000) iyear
         myline = myline(ipunkt+2:)
         ipunkt = index(myline,":")-1
         read(myLine(:ipunkt),*,err=1000,end=1000) ih
         myline = myline(ipunkt+2:)
         read(myLine,*,err=1000,end=1000) im

      endif


C     temperature
      if (index(myLine,"Lufttempe").gt.0) then
         atemp = 0
         do i = 1, 9
            read(inLu,9999,err=1000,end=7000) myline
            read(myLine(5:),*,err=1000,end=1000) x(i)
            atemp = atemp + x(i)
         enddo
         temp = (x(2)+x(3))/2.
         atemp = atemp/9.
      endif


C     dewpoint
      if (index(myLine,"Taupunkt").gt.0) then
         adpt = 0
         do i = 1, 9
            read(inLu,9999,err=1000,end=7000) myline
            read(myLine(5:),*,err=1000,end=1000) x(i)
            adpt = adpt + x(i)
         enddo
         dpt = (x(2)+x(3))/2.
         adpt = adpt/9.
      endif


C     humidity
      if (index(myLine,"Feuchte").gt.0) then
         iahum = 0
         do i = 1, 9
            read(inLu,9999,err=1000,end=7000) myline
            read(myLine(5:),*,err=1000,end=1000) x(i)
            iahum = iahum + x(i)
         enddo
         ihum = (x(2)+x(3))/2
         iahum = iahum/9
      endif




C     windspeed
      if (index(myLine,"Windgeschwin").gt.0) then
         awnd = 0
         do i = 1, 9
            read(inLu,9999,err=1000,end=7000) myline
            read(myLine(5:),*,err=1000,end=1000) x(i)
            awnd = awnd + x(i)
         enddo
         wnd = (x(5)+x(6)+x(7))/3.
         awnd = awnd/9
      endif

      if (index(myLine,"Albe").gt.0) then 
  
         read(inLu,9999,err=1000,end=7000) myline
         read(inLu,9999,err=1000,end=7000) myline
         read(myLine(5:),*,err=1000,end=1000) sun
  
      endif


C     Pressure/skip/wind/skip/rain
!!  <TD><b>Luftdruck<br>477 m</b></TD>
!!  <TD><b>Luftdruck<br>NN</b></TD>
!!  <TD><b>Windrichtung<br>50 m H&ouml;he</b></TD>
!!  <TD><b>Niederschlag<br>aktuell</TD>
!!  <TD><b>Niederschlags-<br>summe ab 0 Uhr</b></TD>
!!  <TD><b>Diffusions-<br>kategorie</b></TD></TR><TR>
!!  <TD>958.1  hPa</TD>
!!  <TD>1014.2  hPa</TD>
!!  <TD>281  &deg;</TD>
!!  <TD>  0.015  mm</TD>
!!  <TD>  3.500  mm</TD>
      if (index(myLine,"Diffusions").gt.0) then
         read(inLu,9999,err=1000,end=7000) myline
         read(myLine(5:),*,err=1000,end=1000) phg
         aphg = phg

         read(inLu,9999,err=1000,end=7000) myline
         read(inLu,9999,err=1000,end=7000) myline
         read(myLine(5:),*,err=1000,end=1000) iwd
         iawd = iwd

 2       read(inLu,9999,err=1000,end=7000) myline
         read(myLine(5:),*,err=1000,end=1000) rain
         read(inLu,9999,err=1000,end=7000) myline
         read(myLine(5:),*,err=1000,end=1000) raintot


      endif
      goto 1000

 7000  continue ! end of file


      write(LuOutDimmDat,9989)               ! for the local dimm.dat (new
                                              ! format) 
     |     iyear, imonth, iday, ih, im, 
     |     temp, atemp,
     |     ihum, iahum,
     |     dpt, adpt,
     |     phg, aphg,
     |     wnd, awnd,
     |     iwd,iawd,
     |     raintot, sun, rain  !! garching format.




 9989 format(
     $     i4,1x, i2,1x, i2,1x, i2,1x,  i2,1x, !iyear,...
     $     2(f5.1,1x),           ! tmp
     $     2(i3,1x),             ! hum
     $     2(f5.1,1x),           ! dpt
     $     2(f6.1,1x),           ! phg
     $     2(f4.1,1x),           ! wnd
     $     2(i3,1x),             ! wd3
     $     f6.3,1x,f7.2,1x,f6.3) ! raintot sun rain
        

         
      do while (i.le.990)
 1234    read(LuInDimmDat,9999,err=8888,end=8888)myline
         write(LuOutDimmDat,9999)myline
         i = i+1
      enddo

 8888 end
