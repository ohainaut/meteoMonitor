*----------------------------------------------------------------------
*.TITLE  METEOMONITOR
*.FILE   meteoli.f
*.LANGUAGE F77 
*.PURPOSE
*     Generates a graph and HTML page displaying the meteo status of La Silla
*.USAGE
*     call c-shell meteorun, which 
*            - call this program, which 
*                  o reads the dimm data
*                  o produce black background graphic
*                  o produce white bcg graph
*                  o produce HTML page
*            - call mogriphy to converts black graph in GIF
*            - sleep till next update
*      This program expects the dimm data to be updated by another program
*
*.AUTHOR O.R. Hainaut
*.VERSION  2010-01-10 ohainaut maintenance and cleanup v2.9
*.HISTORY
*     Sun Feb  9 03:40:12 CLST 2003 upgraded for new LOSSAM: 2.8
*     Mon Jan 20 23:28:36 CLST 2003 minor fix of JD.
*     Thu Oct 18 04:14:51 SAT 2001 v.2.7 released
*     Tue Oct 16 23:41:24 2001 cleaned and updated ohainaut
*     Mon Mar 22 22:51:35 SAT 1999: v.2.5 released
*     Mar 1999: restaured the DIMM (upgrade of DIMM completed) and add red
*               NTT dots
*     Jan 1999: temp. modif to include the seeing from NTT. orh
*     Dec 1997: iniclude wind history, LOSSAM, zoom ORH
*     Oct 1997: better graphics and html output (O. Hainaut)
*     Mar 1992: basic graphics (Ger Van Rossum)
*     Feb 1992: v1.0 plain VT100 output (O. Hainaut)
*+++++++++++++++.

      program meteomonitor

      implicit none

      integer yr(1000), mo(1000), dy(1000), hh(1000), mm(1000)
      real*4 tmp(1000), atmp(1000), hum(1000), ahum(1000), dpt(1000)
      real*4 adpt(1000), phg(1000), aphg(1000), wnd(1000)
      real*4 awnd(1000), wdir(1000), awdir(1000), wdev(1000)
      real*4 rain(1000), sun(1000),  raintot(1000), extn(1000)
      real*4 SeeDimm(1000), sidtime,  irrad(1000), trans(1000)
      real*4  xAbs_UT(1000) !lossam
                                ! all the meteo variables read from DIMM
      real*4 xabs(1000), xabs_rms(1000)
      real*4 airm(1000), utdimm(1000)

      real*8 julday
      integer ihum, iahum, iwd, iawd ! temp. meteo variables
      integer main_plot, hardcopy, zoom
      integer LUDimm, LUHtmLoc, LULightHtmLoc, LUZommHtmLoc, LUjunk    
      integer LUUT, LUMsg, LUlos
                                ! logical units 

      integer i, j, id          ! junk
      integer idev              ! device flag (2=postscript/1=gif)
      integer npt               ! total # of meteo meas. pts
      integer nabs              ! # of lossame points
      integer ilast_jd, ijd     ! integer part of the last
                                ! measurement JDate and current  
      integer iSTh, iSTm, iSTs  ! sidereal time HMS
      integer isun              ! flag sun altitude
      integer iwin(4),iwi       ! parametesr for sm_window
      integer iut               ! flag for abscice UT label
      integer ServUTH, ServUtM, servUtHM ! www server UT
      integer iwyr, iwmo, iwdy, iwh, iwm, iws
      character ServUtHmChar*4
      real*4  ServUt            ! www server UT
      real*4  MaxUtDiff, UtDiff ! max acceptanble diff. between UTs

      real*4 y_min, y_max       ! limits for temp.plot
      real*4 y_ok, y_danger, delta_y ! limits for colors code
      real*4 hour(1000)         ! converted decimal hour
      real*4 dot(1000),odot(1000) ! smongo point type
      real*4 hour_now, hour_left ! current time and first time, limits
      real*4 windowlim          ! for disp
      real*4 x(1000),y(1000),w    ! junk used for lines, labels, etc...
      real*4 wnd1, wnd2,wnd_min,wnd_max ! wind limits
      real*4 wdir1, wdir2,wdir_min,wdir_max, wdir0 ! wind limits
      real*4 tmp1,tmp2,tmp_min,tmp_max ! temperature limits
      real*4 phg1, phg2,phg_min,phg_max ! pressure limit
      real*4 hum1, hum2,hum_min,hum_max ! humidity limits
      real*4 see_min, see_max   ! seeing limits
      real*4 sun_min, sun_max   ! sun limits
      real*4 trans_min, trans_max   ! sun limits
      real*4 rain_min, rain_max   ! sun limits
      real*4 abs_min, abs_max, abs_rms_min, abs_rms_max
      real*4 abs1, abs2, abs_rms1, abs_rms2 ! absorption limits
      real*4 label_off          ! vertical off between line and its label
      real*4 expand_def, expand_small !default and small scales for labels
      real*4 wind               ! wind speed for rose circles.
      real*4 xlabel, ylabel     ! used to position labels on plots

      real*4 pideg


C     REAL*8!!!
      real*8 last_jd, jd, day   ! Julian Day, ref and current
      real*8 dTU, dGMST, dST, dst0ut    ! for Sidereal time
      real*8 obslong, obslat    ! longitude, lat of the observatory
      real*8 junk1, junk2       ! used for subroutines
      real*8 RAsun, DecSun, HAsun, Zsun ! sun coordinates


      character line*160, label20*20 !buffer and labels
      character fileHtml*132, fileHtmLoc*132 ! URL adresses
      character lightHtml*132, lightHtmLoc*132 ! URL adresses
      character fileZoomHtmLoc*132,fileZoomHtml*132
      character filedimm*132, roothtml*132, fileinfo*132
      character filesatel*132
      character xearthGif*132, currentGif*132, currentZoomGif*132
                                ! filenames of the images
      character configFile*80   ! configuration file


      character red*32,yellow*32,green*32 ! color hexadecimal code for HTML
      character wincol*32,tmpcol*32, phgcol*32, humcol*32
      character abscol*32, abs_rmscol ! color status for html tab
      character default_col(3)*32, linecol*10 
      character nsew(4)*1

      character suncol(5)*32   ! colors for the sun altitude
      character buttoncol*6     ! color for the buttons

      character button1*30, butlink1*100 ! content of the buttons
      character button2*30, butlink2*100
      character button3*30, butlink3*100
      character button4*30, butlink4*100
      character button5*30, butlink5*100
      character button6*30, butlink6*100
      character commentlink*100

      character screen_dev(3)*132 ! device (1=interactif/gif, 2=ps)
      
      logical debug             ! local or real

      common default_col,idev
c----------------------------------------------------------------------
C--   Reads config file to get some filenames
c----------------------------------------------------------------------
      configfile = 'meteo.config'

      open(unit=50, 
     |     file=configfile, 
     |     status='old',
     |     err=8801)

      read(50,*)                ! skip header line
      read(50,*)                ! skip header line
      
      read(50,*)                ! skip comment line
      read(50,9999)  roothtml   ! URL of the directory where meteomonitor
                                ! runs 


      read(50,*)                ! skip comment line
      read(50,9999) filedimm    ! file containing the DIMM data


      read(50,*)                ! skip comment line
      read(50,9999) fileinfo    ! file containing the limit info etc



C     Defines some files

      filehtmloc = 'meteomonitor.html'
      filezoomhtmloc = 'meteomonizoom.html'
      lighthtmloc = 'meteo_light.html'
                                !  HTML output


      filehtml = roothtml(1:index(roothtml," ")-1)//
     |     filehtmloc(1:index(filehtmloc," ")-1)
      lighthtml = roothtml(1:index(roothtml," ")-1)//
     |     lighthtmloc(1:index(lighthtmloc," ")-1)
      filezoomhtml = roothtml(1:index(roothtml," ")-1)//
     |     filezoomhtmloc(1:index(filezoomhtmloc," ")-1)
                                ! URL of the meteomonitor
                                ! must point to local directory


c----------------------------------------------------------------------
C--   init; definitions and preferences live here
c----------------------------------------------------------------------
      debug =  .false.


              
      buttoncol = '00ffFF'      ! color of the buttons 
                                ! 00ffff = cyan

      button1 = 'Update'
      butlink1 = filehtml(1:index(filehtml," ")-1)//'#tab'
                                ! This button is expected to be the update

      button2 = 'PRINT GRAPH'
      butlink2 = roothtml(1:index(roothtml," ")-1)//'meteomonitor.ps'
                                ! URL of the postscript version; should
                                ! be .../meteomonitor.ps AND a local file.

      button3 = 'Help/Info'
      butlink3 = fileinfo(1:index(fileinfo," ")-1)

      button4 = 'Zoom graph'
      butlink4 = filezoomhtml(1:index(filezoomhtml," ")-1)//'#tab'

      button5 = 'Satellite Pics'
      butlink5 =  
     $     '/lasilla/dimm/meteo.html'


      button6 = 'Plot Archive'
      butlink6 =  roothtml(1:index(roothtml," ")-1)//'Archive'

      commentlink = 'mailto:ohainaut@eso.org?Subject=MeteoMonitor'


      maxUtDiff = .5            ! max difference between UT from meteo
                                ! station  and www server, in hour


      wnd1 = 14.                ! wind limits: safety
      wnd2 = 20.
      wnd_max = 24.99           ! plot; they will be automatically
      wnd_min = -1.             ! changed if needed

      wdir1 = 99999.
      wdir2 = 99999.
      wdir_min = -200.
      wdir_max = 200.

      tmp1 = 5.                 ! temp-dew limits
      tmp2 = 2.
      tmp_min = 0.
      tmp_max = 20.

      phg1 = 770.               ! pressure limits
      phg2 = 768.               ! (values from Manfred Mornhingweg)

!     Average pressure for Garching: 969.3mbar
      phg1 = 957.               ! pressure limits
      phg2 = 950.               ! (values from Manfred Mornhingweg)
      phg_min = 945.
      phg_max = 975.

      hum1 = 85.                ! humidity limits
      hum2 = 90.
      hum_min = 0.1
      hum_max = 100.

      see_min = 0.01            ! seeing limits
      see_max = 1.49

      sun_min = -10.            ! irrad limits
      sun_max = 1000.

      trans_min = -.1            ! irrad limits
      trans_max = 1.1


      rain_min = -.1            ! rain limits
      rain_max = 1.


      abs_min = 0.              ! absorption limits
      abs_max = .7
      abs1 = .2   
      abs2 = .5

      abs_rms_min = 0.
      abs_rms_max = .07
      abs_rms1 = .02
      abs_rms2 = .05

      obslong = 11.65  ! Garching
      obslat = 48.14

c      screen_dev(1) = 'x11 -bg black -fg white -geometry 1200x740'
                                ! for interactive debug
      screen_dev(1) = 'ps_meteoli current.ps'
      screen_dev(2) = 'postlandencap meteomonitor.ps'
      screen_dev(3) = 'ps_meteoli current_zoom.ps'

      default_col(1) = 'white'
      default_col(2) = 'black'
      default_col(3) = 'white'

      linecol = 'cyan'
      linecol = "blue"

      label_off = 0.03          ! vertical offset
      expand_def = 0.8          ! normal size for fonts and symbols
      expand_small = 0.5        ! size for small labels


      if (debug) write(6,*)"DBG: init done"

c----------------------------------------------------------------------
C More Inits and defs.
C no user serviceable parts beyond here

      pideg = acos(-1.)/180.

      red = 'redball.gif'
      yellow = 'yellowball.gif'
      green = 'greenball.gif'


      nsew(1) = 'N'
      nsew(2) = 'E'
      nsew(3) = 'S'
      nsew(4) = 'W'

      suncol(1) = 'blueball.gif'      ! >0, colors for the sun altitude
      suncol(2) = red           ! civil, red
      suncol(3) = 'orangeball.gif'      ! nautical, orange
      suncol(4) = yellow        ! astronomical, yellow
      suncol(5) = green         ! night, green

      iwin(1) = 3               ! parameters for sm_win
      iwin(2) = 3              ! 3 windows in X, not touching, 
                                ! 3 in Y, touching eachother

      LUDimm = 55
      LUMsg = 56
      LUUT = 57
      LUlos = 58
      LUHtmLoc = 66
      LULightHtmLoc = 67
      LUZommHtmLoc = 68


      LUJunk = 65


      if (debug) write(6,*)"LUdimm, filedimm", LUDimm,filedimm
      open(unit=LUDimm,         ! all the meteo data
     |     file=filedimm,
     |     status='old',
     |     err=8803)
           

      open(unit=LUHtmLoc,
     |     file=filehtmloc)
      open(unit=LULightHtmLoc,
     |     file=lighthtmloc)
      open(unit=LUZommHtmLoc,
     |     file=filezoomhtmloc)

      open(unit=LUJunk, file='.meteojunk')

      open(unit=LUlos, file='longexp.log')  ! lossam

c


      do i = 1, 1000            ! size and shape of points
         dot(i) = 30.08
         odot(i) = 800.+ (30.-i)/30
      enddo
      dot(1) = 83.7
      call sm_ptype(dot,npt)

      if (debug) write(6,*)"DBG: init2 done"

C----------------------------------------------------------------------
C     READ THE METEO SERVER UT
C----------------------------------------------------------------------
      ServUtHmChar = '9999'

      open(unit=LUUT,           ! the www server ut, just to check that 
     |     file='ut.last',      ! the meteo server is alive
     |     status='old',
     |     err=8804)


      read(LUUT,*, err=900, end=900) ServUTHM ! format hhmm
      servUTH = int(ServUTHM/100.)
      servUTM = ServUtHM - servUTH*100
      servUT = ServUTH + servUTM/60.
      write(ServUtHmChar,'(i4.4)') servUtHm


      goto 920

 900  continue                  ! in case of pb reading UT
      servUT = -99999.

 920  continue

      close(LUUT)

      
      xearthGif = roothtml(1:index(roothtml," ")-1)//
     |     'xearth_'//ServUtHmChar//'.gif'
      currentGif = roothtml(1:index(roothtml," ")-1)//
     |     'current_'//ServUtHmChar//'.gif'
      currentZoomGif = roothtml(1:index(roothtml," ")-1)//
     |     'current_zoom_'//ServUtHmChar//'.gif'



      if (debug) write(6,*)"DBG: meteo done"



c----------------------------------------------------------------------
c     READ INPUT FILE
c----------------------------------------------------------------------
C     the first line in the file is the last entry

      i = 1
      do while(.true.)          ! read dimm.dat
 1000    read( LUDimm, 9999, err=1000, end=2000) line

 1010    id = index(line,'')  ! for some reasons, there is a ^M in the
                                ! line... 
         if (id .gt. 1) then
            line(id:id) = ' '
c            goto 1010
         endif

 1020    id = index(line,'/') 
         if (id .gt. 0) then
            line(id:id) = '0'
            goto 1020
         endif

         read(line,*,err=1000,end=1000) ! for the local dimm.dat (new
                                              ! format) 
     |        yr(i), mo(i), dy(i), hh(i), mm(i), 
     |        tmp(i), atmp(i),
     |        ihum, iahum,
     |        dpt(i), adpt(i),
     |        phg(i), aphg(i),
     |        wnd(i), awnd(i),
     |        iwd, iawd,
     |        raintot(i), !! Garching format
     |        sun(i),
     |        rain(i)


         hum(i) = float(ihum)
         ahum(i) = float(iahum)
         wdir(i) = float(iwd)
         awdir(i) = float(iawd)
         rain(i) = rain(i)*100

         i = i+1
      enddo
      
 2000 npt = i-1

      if (debug) write(6,*)"DBG: npt:",npt
      if (debug) write(6,*)"DBG: current UT:",yr(1)
     $     ,mo(1),dy(1),hh(1),mm(1)
      

c
c--   CONVERSION
c
      call JDATE(yr(1),mo(1),1.d0*dy(1), last_jd)



      ilast_jd = int(last_jd -0.5d0)
      wdir0 = wdir(1)

      do i = 1, npt             !!! 1 is the LAST datapoint
         
         call JDATE(yr(i),mo(i),1.d0*dy(i), jd)
         ijd = int(jd -0.5d0) 

         hour(i) = hh(i) + mm(i)/60. - 24.*(ilast_jd-ijd)
         utdimm(i) = utdimm(i) - 24.*(ilast_jd-ijd)
                                ! setting negative values for yesturday



*     *Sidereal time at 0h UT
      day = dy(1)*1.d0          
      call JDATE(yr(1),mo(1),day, jd) ! JD at 0hUT

      dTU = (jd-2451545.0d0)/36525.d0
      dGMST = 24110.54841d0 + 8640184.812866d0*dTU
     >     + 0.093104d0*dTU*dTU - 6.2d-6*dTu*dTu*dTu
                                ! Greenwich Mean Sid time at 0hUT, in sec;
                                ! Algorithm from Astron. Almanach

      dGMST = dGMST/3600.d0     ! in hours

      dST0ut = dGMST+obslong/15.d0  ! compensation for observatory longitude
                                 ! = local sid. time at 0hUT



C---  model the sun irradiance for the past points
      dST = dST0ut + (hh(i) - hh(1) +
     |     servUTh + mm(i)/60.d0 )/.9972696d0
                               ! local sid time at point (i)
                               ! we might get it wrong by 1h around the
                               !     hour change
      do while(dST.lt.0.d0)
         dST = dST+24.d0
      enddo
      do while(dST.ge.24.d0)
         dST = dST-24.d0
      enddo

     
C--   Actual Julian Day for point(i)
      day = dy(i)+ (hh(i) + mm(i)/60. )/24.
      call JDATE(yr(1),mo(1),day, julday)

C--   coordinates of the sun

      call solcoor(julday, junk1, junk2, RAsun, DecSun)
      
      RAsun = RAsun/15.d0       ! RA in hours
      HAsun = dST - RAsun          ! Sun hour angle

      call h_dist(obslat, HAsun, DecSun, Zsun)
      ! zSun is the altitude

C--   estimate the irradiance
      if (zsun.gt.0) then
         irrad(i) = 1250. * sin(zSun*pideg) !! 1150W/m2 
         trans(i) = sun(i)/irrad(i)
         extn(i) = -2.5*log10(trans(i))*sin(Zsun*pideg)
      else
         irrad(i) = 0.
         trans(i) = 999.
         extn(i) = 9999.
      endif
      if (debug) write(6,*) "[dbg] Irrad:", i,hh(i),mm(i),zsun,
     |     irrad(i),trans(i),
     |     extn(i)

      





         if (wnd_min.gt.wnd(i)) wnd_min = wnd(i) ! centering the wind plot to
         if (wnd_max.lt.wnd(i)) wnd_max = wnd(i) ! avoid too many jumps.

         wdir(i) = wdir(i)-wdir0
         if (wdir(i).ge.180.) wdir(i) = wdir(i)-360.
         if (wdir(i).le.-180.) wdir(i) = wdir(i)+360.

         if (tmp_min.gt.tmp(i) .and. tmp(i).ge.-50.)
     $        tmp_min = tmp(i)  ! find the plot limits
         if (tmp_max.lt.tmp(i) .and. tmp(i) .le. 50.)
     $        tmp_max = tmp(i)
         if (tmp_min.gt.dpt(i) .and. dpt(i).ge.-50.)
     $        tmp_min = dpt(i)
         if (tmp_max.lt.dpt(i) .and. dpt(i) .le. 50.)
     $        tmp_max = dpt(i)
         if (phg_min.gt.phg(i).and.phg(i).gt.900.) phg_min = phg(i)     
         if (phg_max.lt.phg(i).and.phg(i).lt.1100.)
     $        phg_max = phg(i)+0.5
         if (hum_min.gt.hum(i).and. hum(i).gt. -5.)
     $        hum_min = hum(i)
         if (hum_max.lt.hum(i) .and. hum(i).le.105.)
     $        hum_max = hum(i)
         if (see_min.gt.SeeDimm(i).and.SeeDimm(i).gt.0.)
     $        see_min = SeeDimm(i)
         if (see_max.lt.SeeDimm(i)) see_max = SeeDimm(i)

         if (sun_min.gt.sun(i).and.sun(i).gt.-1.)
     $        sun_min = sun(i)
         if (sun_max.lt.sun(i)) sun_max = sun(i)
         if (sun_max.lt.irrad(i)) sun_max = irrad(i)

          
         if (rain_max.lt.rain(i)) rain_max = rain(i)
         if (rain_max.lt.raintot(i)) rain_max = raintot(i)


         if (      abs(xabs(i)-9.99).le.0.01
     $        .or. abs(xabs_rms(i)-9.99).le.0.01
     $        .or. xabs(i).le.0. .or. xabs_rms(i).le.0.) then
            xabs(i) = -9999.    ! no seeing monitor
            xabs_rms(i) = -9999.
         else
            xabs(i) = xabs(i)/airm(i)+0.1 ! airmass correction for the
                                ! absorption.  
            xabs_rms(i) = xabs_rms(i)/airm(i) ! these are from Marc
                                ! M Sarazin's IDL routine - He must know what
                                ! he is doing!   
            if (abs_min.gt.xabs(i)) abs_min = xabs(i)
            if (abs_max.lt.xabs(i) .and. hour(1)-hour(i).le. 2.)
     $           abs_max = xabs(i) ! takes into account only the last 2h

            if (abs_rms_min.gt.xabs_rms(i)) abs_rms_min = xabs_rms(i)
            if (abs_rms_max.lt.xabs_rms(i) .and. hour(1)-hour(i).le. 2.)
     $           abs_rms_max = xabs_rms(i) ! takes into account only the last 2h
         endif

      enddo


      if (debug) write(6,*)"DBG: convert done"


c------------------------------------------------------------------------------
c     LOSSAM INPUT
c------------------------------------------------------------------------------

      i = 1

 3000 read(LUlos, 9999, err=3000, end=3500) line

      iwyr = 0
      iwmo = 0
      iwdy = 0
      iwh = 0
      iwm = 0
      iws = 0

      read(line(1:4),*, err=3010, end=3010) iwyr
 3010 read(line(6:7),*, err=3020, end=3020) iwmo
 3020 read(line(9:10),*, err=3030, end=3030) iwdy
 3030 read(line(12:13),*, err=3040, end=3040) iwh
 3040 read(line(15:16), *,err=3050, end=3050) iwm
 3050 read(line(18:19),*, err=3060, end=3060) iws
 3060 continue

      xabs_Ut(i) = iwh + iwm/60. + iws/3600.
      xabs_rms(i) = -999.
      
      id = index(line,'RFlrms')
      if (id.gt.0) then
         id = id +6             ! to skip "RFlrms"
         read(line(id:),*, err=3070,end=3070) xabs_rms(i)
      endif
 3070 continue
      i = i+1
      goto 3000                 ! read loop
      
 3500 nabs = i-1                ! nr of points


C----------------------------------------------------------------------
C   GRAPH. OUTPUT
C----------------------------------------------------------------------
      
      main_plot = 1
      hardcopy = 2
      zoom = 3


      do idev = 1, 3            ! we do it trice: 1= color for screen,
                                ! 2=b&w postscript for hardcopies
                                ! 3= zoom of last hour

         if (debug) write(6,*)"DBG: idev:",idev
         if (debug) write(6,*)">"//screen_dev(idev)

         if (idev .eq. zoom) then ! zoom on last hour
            hour_now = hour(1)+.1
            hour_left = hour(1)-2.1

         else                   ! normal display
            hour_now = hour(1)+1.
!            if (hour_now.le.10)then
!               hour_left = hour_now-16. ! high resolution night
!            else
!               hour_left = hour_now-25. ! low resolution day
!            endif
            hour_left = hour_now -25.
         endif


         call sm_device(screen_dev(idev))


         if (debug) write(6,*) "DBG: dev: "//screen_dev(idev)

         if (idev .eq. hardcopy) then
            call sm_location(1000,31760,4500,29260)
         else
            call sm_location(1000,31760,2000,30760)
         endif
         
         call sm_expand(expand_def)    
         call sm_defvar('TeX_strings','1')
         call sm_graphics
         call sm_erase

         call sm_limits(0.,0.,1.,1.)

c         call sm_box(0,1,0,1)
         
      

C--   pressure

      if (debug) write(6,*)"DBG: start pressure"
      call sm_expand(expand_def)    
      call oli_window(iwin(1),iwin(2),1,1) 
      call oli_window(3,3,1,1) 


      iut = 1

      y_min = phg_min
      y_max = phg_max
      y_ok = phg1
      y_danger = phg2

      delta_y = wnd(1)

      call sm_limits(hour_left,hour_now,y_min,y_max)
      call sm_ticksize(1.,3.,1.,10.)
      call sm_expand(.7)

      call sm_ctype(default_col(idev))
      call sm_box(0,1,0,0)
      if (debug) write(6,*)"DBG: box"

      x(1) = hour_left
      x(2) = hour_now

      y(1) = 773.               ! Average pressure at La Silla
      y(1) = 962
      y(2) = y(1)

      if (debug) write(6,*)"DBG: col:", linecol
      call sm_ctype(linecol)
      if (debug) write(6,*)"DBG: ctype"
      if (debug) write(6,*)"DBG: xy", x(1),x(2), y(1),y(2)
      call sm_conn(x,y,2)
      if (debug) write(6,*)"DBG:conn"

      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(9,' \t Mean Pressure')
      if (debug) write(6,*)"DBG: lab"

      y(1) = 770.              ! "Clouds"
      y(1) = 957.              ! "Clouds"
      y(2) = y(1)
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(9,' \t Clouds')

      y(1) = 768.               ! "Bad"
      y(1) = 950.
      y(2) = y(1)
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(9,' \t Bad')

      y(1) = 765.05               ! "Storm"
      y(1) = 940.
      y(2) = y(1)
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(9,' \t Storm')
      
      call sm_ctype(default_col(idev))
      call sm_ptype(dot,npt)
      call sm_expand(2.)
      call sm_points(hour,phg,npt)

      if (phg(1) .gt. y_ok) then
         call sm_ctype('green')
         phgcol = green
      else if (phg(1) .gt. y_danger)  then
         call sm_ctype('yellow')
         phgcol = yellow
      else
         call sm_ctype('red')
         phgcol = red
      endif


      call sm_points(hour,phg,1)
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      label20 = 'Atm. Pressure (hPa)'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    
      


C--   TEMPERATURE
 1234 continue

      if (debug) write(6,*)"DBG: start temp"

      call oli_window(iwin(1),iwin(2),1,2) 
      iut = 0

      y_min = tmp_min-2.
      y_max = tmp_max+2.
      y_ok = tmp1
      y_danger = tmp2

      delta_y = tmp(1) - dpt(1)

      call sm_limits(hour_left,hour_now,y_min,y_max)
      call sm_ticksize(1.,3.,1.,10.)
      call sm_expand(.7)
      call sm_box(0,1,0,1)
      

      call sm_ptype(dot,npt)   ! main curve
      call sm_expand(2.)
      call sm_points(hour,tmp,npt)
      call sm_ctype('cyan')
      call sm_points(hour,dpt,npt) ! dew point curve

      if (delta_y .gt. y_ok) then ! final big colored dot
         call sm_ctype('green')
         tmpcol = green
      else if (delta_y .gt. y_danger)  then
         call sm_ctype('yellow')
         tmpcol = yellow
      else
         call sm_ctype('red')
         tmpcol = red
      endif

      call sm_points(hour,tmp,1)
      call sm_points(hour,dpt,1)

      call sm_expand(.65)
c      call sm_ctype(default_col(idev))
c      call sm_relocate(hour_left,20.)
c      call sm_putlabel(6,'  \t Temperature')


      call sm_ctype('cyan')
      call sm_expand(.65)
      call sm_relocate(hour_left,-10.)
      call sm_putlabel(6,'  \t Dew Point')
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      label20 = 'Temperature (^oC)'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    




C--   WIND SPEED
      if (debug) write(6,*)"DBG: start wind"
      call oli_window(iwin(1),iwin(2),2,2) 
      iut = -1

      y_min = wnd_min
      y_max = wnd_max
      y_ok = wnd1
      y_danger = wnd2

      delta_y = wnd(1)

      call sm_limits(hour_left,hour_now,y_min,y_max)
      call sm_ticksize(1.,3.,0.,0.)
      call sm_expand(.7)
      call sm_box(0,1,0,1)
      
      x(1) = hour_left
      x(2) = hour_now

      y(1) = wnd1
      y(2) = wnd1
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
      call sm_relocate(x(1),y(1)-(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(3,' \t Pointing Limit')

      y(1) = wnd2
      y(2) = wnd2
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
      call sm_relocate(x(1),y(1)-(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(3,' \t Close Dome')

      call sm_ctype(default_col(idev))
      call sm_expand(2.)
      call sm_points(hour,wnd,npt)
      
c      call sm_ctype('cyan')
c      call sm_conn(hour,awnd,npt)

      if (wnd(1) .lt. wnd1) then
         call sm_ctype('green')
         wincol = green
      else if (wnd(1) .lt. wnd2)  then
         call sm_ctype('yellow')
         wincol = yellow
      else
         call sm_ctype('red')
         wincol = red
      endif



      call sm_points(hour,wnd,1)
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      label20 = 'Wind Speed (m/s)'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    


C--   Wind direction
      if (debug) write(6,*)"DBG: start wind dir"
      call oli_window(iwin(1),iwin(2),2,1) 
      iut = 1

      y_min = wdir_min
      y_max = wdir_max
      y_ok = wdir1
      y_danger = wdir2

      delta_y = wdir(1)

      call sm_limits(hour_left,hour_now,y_min+wdir0,y_max+wdir0)
      call sm_ticksize(1.,3.,30.,90.)
      call sm_expand(.7)
      call sm_box(0,0,0,0)
      call sm_angle(90.)

      do i = -360, 360, 90      ! label the Y axis
         if (i.ge.y_min+wdir0 .and. i.le.y_max+wdir0) then
            iwi = mod(i/90,4)+1
            do while(iwi.le. 0)
               iwi = iwi+4
            enddo
            call sm_relocate
     |           (hour_left-(hour_now-hour_left)*.05,i*1.)
            call sm_putlabel(8,'\t '//nsew(iwi))
            iwi = (iwi-1)*90
            write(line,*) iwi
            call sm_relocate
     |           (hour_now+(hour_now-hour_left)*.05,i*1.)
            call sm_putlabel(2,'\t '//line(2:4))
         endif         
      enddo

      call sm_angle(0.)

      call sm_limits(hour_left,hour_now,y_min,y_max)      
      x(1) = hour_left
      x(2) = hour_now

c      y(1) = wdir1
c      y(2) = wdir1
c      call sm_ctype(linecol)
c      call sm_conn(x,y,2)
c      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
c      call sm_expand(expand_small)
c      call sm_putlabel(9,' \t Pointing Limit')

      call sm_ctype(default_col(idev))
      call sm_expand(2.)
      call sm_points(hour,wdir,npt)


C     not needed for direction
C      if (wdir(1) .lt. wdir1) then
C         call sm_ctype('green')
C         wincol = green
C      else if (wdir(1) .lt. wdir2)  then
C         call sm_ctype('yellow')
C         wincol = yellow
C      else
C         call sm_ctype('red')
C         wincol = red
C      endif


      call sm_points(hour,wdir,1)
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      label20 = 'Wind Direction (^o)'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    


C--   Humidity
      if (debug) write(6,*)"DBG: start hum"
      call oli_window(iwin(1),iwin(2),1,3) 
      iut = -1

      y_min = hum_min
      y_max = hum_max
      y_ok = hum1
      y_danger = hum2

      delta_y = hum(1)

      call sm_limits(hour_left,hour_now,y_min,y_max)
      call sm_ticksize(1.,3.,10.,20.)
      call sm_expand(.7)
      call sm_box(0,1,0,1)
            
      x(1) = hour_left
      x(2) = hour_now

c      y(1) = hum1
c      y(2) = hum1
c      call sm_ctype(linecol)
c      call sm_conn(x,y,2)

c      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
c      call sm_expand(expand_small)
c      call sm_putlabel(9,' \t Reopen Dome')


      x(1) = (hour_left+hour_now)/2.
      y(1) = hum2
      y(2) = hum2
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
c      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
c      call sm_expand(expand_small)
c      call sm_putlabel(9,' \t Close Dome')


      call sm_ctype(default_col(idev))
      call sm_ptype(dot,npt)
      call sm_expand(2.)
      call sm_points(hour,hum,npt)

      if (delta_y .lt. y_ok) then
         call sm_ctype('green')
         humcol = green
      else if (delta_y .lt. y_danger)  then
         call sm_ctype('yellow')
         humcol = yellow
      else
         call sm_ctype('red')
         humcol = red
      endif

      call sm_points(hour,hum,1)
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      label20 = 'Humidity (%)'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    


C!CC--   seeing
C!C      if (debug) write(6,*)"DBG: start seeing"
C!C      call oli_window(iwin(1),iwin(2),3,1) 
C!C      iut = 1
C!C
C!C      y_min = see_min
C!C      y_max = see_max
C!C      y_ok = 99.
C!C      y_danger = 99.
C!C
C!C      delta_y = SeeDimm(1)
C!C
C!C      call sm_limits(hour_left,hour_now,y_min,y_max)
C!C      call sm_ticksize(1.,3.,0.,0.)
C!C      call sm_expand(.7)
C!C      call sm_box(0,1,0,1)
C!C      
C!C      call sm_ptype(dot,npt)   ! dimm points
C!C      call sm_expand(2.)
C!C      call sm_points(utdimm,seeDimm,npt)
C!C
C!C
C!C     
C!C      call sm_ctype('green')   ! big green dimm point
C!C      dot(1) = 83.7
C!C      call sm_ptype(dot,npt) 
C!C      call sm_expand(2.)
C!C      call sm_points(utdimm,seeDimm,1)
C!C      call sm_ctype(default_col(idev))
C!C      call sm_expand(1.001)
C!C
C!C      if (SeeDimm(1).le.0.0001) then
C!C         call sm_ctype('yellow')
C!C         call sm_expand(expand_small)    
C!C         call sm_relocate(hour_now,0.1)
C!C         call sm_putlabel(7,'\t Currently not available ')
C!C         call sm_ctype(default_col(idev))
C!C         call sm_expand(expand_def)    
C!C      endif
C!C
C!C      
C!C      label20 = 'Seeing (")'    ! title and all normal labels
C!C      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
C!C      call sm_expand(expand_def)    
C!C
C!C
C!C
C!C      call sm_ctype(default_col(idev)) ! reset 
C!C      call sm_expand(expand_def)    
C!C
C!C      y_min = see_min
C!C      y_max = see_max
C!C      y_ok = 99.
C!C      y_danger = 99.
C!C
C!C      delta_y = SeeDimm(1)
C!C
C!C      call sm_limits(hour_left,hour_now,y_min,y_max)
C!C      call sm_ticksize(1.,3.,0.,0.)
C!C      call sm_expand(.7)
C!C      call sm_box(0,1,0,1)
C!C      
C!C      call sm_ptype(dot,npt)   ! dimm points
C!C      call sm_expand(2.)
C!C      call sm_points(utdimm,seeDimm,npt)
C!C
C!C
C!C     
C!C      call sm_ctype('green')   ! big green dimm point
C!C      dot(1) = 83.7
C!C      call sm_ptype(dot,npt) 
C!C      call sm_expand(2.)
C!C      call sm_points(utdimm,seeDimm,1)
C!C      call sm_ctype(default_col(idev))
C!C      call sm_expand(1.001)
C!C
C!C      if (SeeDimm(1).le.0.0001) then
C!C         call sm_ctype('yellow')
C!C         call sm_expand(expand_small)    
C!C         call sm_relocate(hour_now,0.1)
C!C         call sm_putlabel(7,'\t Currently not available ')
C!C         call sm_ctype(default_col(idev))
C!C         call sm_expand(expand_def)    
C!C      endif
C!C
C!C      
C!C      label20 = 'Seeing (")'    ! title and all normal labels
C!C      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
C!C      call sm_expand(expand_def)    
C!C
C!C
C!C
C!C      call sm_ctype(default_col(idev)) ! reset 
C!C      call sm_expand(expand_def)    
C!C

C--   sun
      if (debug) write(6,*)"DBG: start sun"
      call oli_window(iwin(1),iwin(2),3,3) 
      iut = -1

      y_min = sun_min
      y_max = sun_max
      y_ok = 1.
      y_danger = 9999.

      delta_y = sun(1)

      call sm_limits(hour_left,hour_now,y_min,y_max)
      call sm_ticksize(1.,3.,50.,200.)
      call sm_expand(.7)
      call sm_box(0,1,0,1)

      call sm_ctype('cyan')
      call sm_points(hour,irrad,npt)
      call sm_relocate(hour_left,50.)
      call sm_expand(.65)
      call sm_putlabel(6,'  \t Max')
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)
      label20 = 'Solar irrad. (W/m^2)'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    

      

      call sm_ptype(dot,npt)   ! main curve
      call sm_expand(2.)
      call sm_points(hour,sun,npt)

      if (delta_y .gt. y_ok) then ! final big colored dot
         call sm_ctype('green')
         tmpcol = green
      else if (delta_y .gt. y_danger)  then
         call sm_ctype('yellow')
         tmpcol = yellow
      else
         call sm_ctype('red')
         tmpcol = red
      endif

      call sm_points(hour,sun,1)
      call sm_ctype(default_col(idev))



C--   Transmission
      if (debug) write(6,*)"DBG: start TRANS"
      call oli_window(iwin(1),iwin(2),3,2) 
      iut = 0

      y_min = trans_min
      y_max = trans_max
      y_ok = 99.
      y_danger = 99.
      delta_y = trans(1)




      call sm_limits(hour_left,hour_now,y_min,y_max)
      call sm_ticksize(1.,3.,.05,.2)
      call sm_expand(.7)
      call sm_box(0,1,0,1)


      call sm_ctype('cyan')
      call sm_points(hour,extn,npt)
      call sm_relocate(hour_left,0.)
      call sm_expand(.65)
      call sm_putlabel(6,'  \t Extn (mag/airm)')
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      

      call sm_ptype(dot,npt)   ! main curve
      call sm_expand(2.)
      call sm_points(hour,trans,npt)
      call sm_ctype('green')
      tmpcol = green




      call sm_points(hour,trans,1)

      call sm_expand(.65)

      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      label20 = 'Atmos. Transmission'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    

      x(1) = hour_left
      x(2) = hour_now

      y(1) = .7
      y(2) = y(1)
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(9,' \t Haze')

      y(1) = .4
      y(2) = y(1)
      call sm_ctype(linecol)
      call sm_conn(x,y,2)
      call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
      call sm_expand(expand_small)
      call sm_putlabel(9,' \t Clouds')
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)


C-- RAIN
      if (debug) write(6,*)"DBG: start rain"
      call oli_window(iwin(1),iwin(2),3,1) 

      iut = 1

      y_min = rain_min
      y_max = rain_max*1.05
      y_ok = -100.
      y_danger = -100.

      delta_y = rain(1)

      call sm_limits(hour_left,hour_now,y_min,y_max)
      if (y_max.le.10.) then
         call sm_ticksize(1.,3.,.2,1.)
      else
         call sm_ticksize(1.,3.,1.,5.)
      endif
      call sm_expand(.7)
      call sm_box(0,1,0,1)
      

      call sm_ptype(dot,npt)   ! main curve
      call sm_expand(2.)
      call sm_points(hour,rain,npt)

      call sm_ctype('cyan')
      call sm_points(hour,raintot,npt) ! integrated curve

      if (rain(1) .gt. 0.) then ! final big colored dot
         call sm_ctype('yellow')
         tmpcol = yellow
         if (rain(1) .gt. 0.1) then ! final big colored dot
            call sm_ctype('red')
            tmpcol = red
         endif
      endif

      call sm_points(hour,rain,1)

      call sm_expand(.65)

      call sm_ctype(default_col(idev))
      call sm_expand(1.001)

      label20 = 'Rain (0.1 mm)'
      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
      call sm_expand(expand_def)    

      call sm_ctype('cyan')
      call sm_relocate(hour_left,y_max*.8)
      call sm_putlabel(6,'  \t Rain total since 0h (mm)')

      call sm_ctype(default_col(idev))
      call sm_expand(1.001)




C!C
C!CC--   Transparency
C!C      if (debug) write(6,*)"DBG: start transp"
C!C      call oli_window(iwin(1),iwin(2),3,3) 
C!C
C!C      iut = -1
C!C
C!C      y_min = abs_min
C!C      y_max = abs_max
C!C      x(1) = hour_left
C!C      x(2) = hour_now
C!C
C!C      call sm_limits(hour_left,hour_now,y_min,y_max)
C!C      call sm_ticksize(1.,3.,0.,0.)
C!C      call sm_box(0,1,0,1)
C!C
C!C      if (y_max.gt.abs1) then
C!C         y(1) = abs1            ! abs warning line
C!C         y(2) = abs1
C!C
C!C         call sm_ctype(linecol) 
C!C         call sm_conn(x,y,2)
C!C         call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
C!C         call sm_expand(expand_small)
C!C         call sm_putlabel(9,' \t Clouds?')
C!C         call sm_ltype(0)      ! solid
C!C      endif
C!C
C!C
C!C      if (y_max.gt.abs2) then
C!C         x(1) = hour_left       ! abs warning line
C!C         x(2) = hour_now
C!C         y(1) = abs2
C!C         y(2) = abs2
C!C
C!C         call sm_ctype(linecol) 
C!C         call sm_conn(x,y,2)
C!C         call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
C!C         call sm_expand(expand_small)
C!C         call sm_putlabel(9,' \t Clouds!')
C!C      endif
C!C
C!C
C!C      call sm_ctype(default_col(idev)) 
C!C      call sm_ptype(dot,npt)   ! plot the curve
C!C      call sm_expand(2.)
C!C      call sm_points(hour,xabs,npt)
C!C
C!C
C!C      if (xabs(1) .lt. abs1 ) then ! chose the color of the big dot
C!C         call sm_ctype('green')
C!C         abscol = green
C!C      else if (xabs(1).lt. abs2)  then
C!C         call sm_ctype('yellow')
C!C         abscol = yellow
C!C      else
C!C         call sm_ctype('red')
C!C         abscol = red
C!C      endif
C!C      call sm_points(hour(1),xabs(1),1)
C!C      call sm_ctype(default_col(idev))
C!C      call sm_expand(1.001)
C!C
C!C
C!C      label20 = 'Extinction (mg/airm)'
C!C      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
C!C      call sm_expand(expand_def)    
C!C
C!C      if (xabs(1).le.0.) then
C!C         call sm_ctype('yellow')
C!C         call sm_relocate(hour_now,0.03)
C!C         call sm_expand(expand_small)    
C!C         call sm_putlabel(7,'\t Currently not available ')
C!C         call sm_ctype(default_col(idev))
C!C         call sm_expand(expand_def)    
C!C      endif
C!C
C!CC--   Transparency RMS
C!C      if (debug) write(6,*)"DBG:   start rms"
C!C      
C!C      iut = 0
C!C
C!C      call oli_window(iwin(1),iwin(2),3,2) 
C!C
C!C      y_min = abs_rms_min
C!C      y_max = abs_rms_max
C!C
C!C      x(1) = hour_left
C!C      x(2) = hour_now
C!C
C!C      call sm_limits(hour_left,hour_now,y_min,y_max)
C!C      call sm_ticksize(1.,3.,0.,0.)
C!C      call sm_box(0,1,0,1)
C!C
C!C      if (y_max.gt.abs_rms1) then
C!C         y(1) = abs_rms1            !  warning line
C!C         y(2) = abs_rms1
C!C         call sm_ctype(linecol) 
C!C         call sm_conn(x,y,2)
C!C         call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
C!C         call sm_expand(expand_small)
C!C         call sm_putlabel(9,' \t Clouds?')
C!C      endif
C!C
C!C      if (y_max.gt.abs_rms2) then
C!C         x(1) = hour_left       ! abs warning line
C!C         x(2) = hour_now
C!C         y(1) = abs_rms2
C!C         y(2) = abs_rms2
C!C
C!Cc         call sm_ltype(1)      ! dots
C!C         call sm_ctype(linecol) 
C!C         call sm_conn(x,y,2)
C!C         call sm_relocate(x(1),y(1)+(y_max-y_min)*label_off)
C!C         call sm_expand(expand_small)
C!C         call sm_putlabel(9,' \t Clouds!')
C!C         call sm_ltype(0)      ! solid
C!C      endif
C!C
C!C      call sm_ctype(default_col(idev)) 
C!C      call sm_ptype(dot,npt)   ! plot the curve
C!C      call sm_expand(2.)
C!C      call sm_ctype(default_col(idev))
C!C      call sm_points(xabs_ut,xabs_rms,nabs)
C!C
C!C      if (xabs_rms(nabs) .lt. abs_rms1 ) then ! chose the color of the big dot
C!C         call sm_ctype('green')
C!C         abs_rmscol = green
C!C      else if (xabs_rms(nabs).lt. abs_rms2)  then
C!C         call sm_ctype('yellow')
C!C         abs_rmscol = yellow
C!C      else
C!C         call sm_ctype('red')
C!C         abs_rmscol = red
C!C      endif
C!C      call sm_points(xabs_ut(nabs),xabs_rms(nabs),1)
C!C
C!C      call sm_ctype(default_col(idev))
C!C      call sm_expand(1.001)
C!C
C!C      label20 = 'Extinction RMS'
C!C      call labeline(iut,hour_now,hour_left,y_min,y_max,label20)
C!C      call sm_expand(expand_def)    
C!C
C!CC      if (xabs(1).le.0..or.
C!CC     |    xabs_rms(1).le.0.) then
C!CC         call sm_ctype('yellow')
C!CC         call sm_relocate(hour_now,0.003)
C!CC         call sm_expand(expand_small)    
C!CC         call sm_putlabel(7,'\t Currently not available ')
C!CC         call sm_ctype(default_col(idev))
C!CC         call sm_expand(expand_def)    
C!CC      endif
C!C
C!C
C!C
C--   Wind rose
      if (debug) write(6,*)"DBG: start rose"
      call sm_ctype(default_col(idev)) ! select color for current
                                        ! device 

      call oli_window(iwin(1),iwin(2),2,3)  ! position graph in page



      windowlim = 28.
      call sm_limits
     |     (-1.26*windowlim,1.25*windowlim,-windowlim-9,windowlim-2.5)
                                ! magic parameters to make it round

      do j = 15, 25, 5          ! main circles
         wind = j*1.
         if (j.eq.15.) wind = 14.
         do i = 1, 180
            x(i) = cos(pideg*(90.-i*2.))*wind
            y(i) = sin(pideg*(90.-i*2.))*wind
         enddo
         x(181) = x(1)          ! close the circle
         y(181) = y(1)

         call sm_conn(x,Y,181) ! draw the circle


         write(line,*) wind     ! label the circle
         call sm_relocate(0.,wind-1.)
         call sm_expand(expand_small)
         call sm_putlabel(2,'\t '//line(2:3))
         call sm_expand(1.001)
      enddo

      do i = 0, 360, 10         ! little ticks on the circles
         j = mod(i,30)
         if (j .eq. 0) then
            w = 5.
         else
            w = 2.
         endif
         x(1) = cos(pideg*(i*1.))*23.
         y(1) = Sin(pideg*(i*1.))*23.
         x(2) = cos(pideg*(i*1.))*(23.+w)
         y(2) = sin(pideg*(i*1.))*(23.+w)
         call sm_conn(x,y,2)
      enddo



C      j = 0                     ! history of the wind dir
C      do i = 1, npt
C         j = j+1
C         x(j) = cos(pideg*(90.-awdir(j)))*awnd(j)
C         y(j) = sin(pideg*(90.-awdir(j)))*awnd(j)
C      enddo
C      jpt = j
C      call sm_ctype(default_col(idev))
C      call sm_ptype(odot,jpt)
C      call sm_conn(x,y,jpt)
      


      x(2) = 0.                 ! wind vector
      y(2) = 0.
      x(1) = cos(pideg*(90.-wdir0))*wnd(1)
      y(1) = sin(pideg*(90.-wdir0))*wnd(1)
      call sm_conn(x,y,2)



      if (wnd(1) .lt. wnd1) then
         call sm_ctype('green') ! smongo color
         wincol = green         ! HTML hexa color
      else if (wnd(1) .lt. wnd2)  then
         call sm_ctype('yellow')
         wincol = yellow
      else
         call sm_ctype('red')
         wincol = red
      endif

      call sm_expand(2.)
      call sm_ptype(dot,1)
      call sm_points(x,y,1)
      call sm_ctype(default_col(idev))
      call sm_expand(1.001)



c      x(1) = cos(pideg*(90.-awdir(1)))*awnd(1) ! aver. wind vector
c      y(1) = sin(pideg*(90.-awdir(1)))*awnd(1)
c      call sm_ctype(linecol)
c      call sm_conn(x,y,2)
c      call sm_ctype(default_col(idev))
      
      call sm_expand(1.01)      ! label the wind rose
      call sm_relocate(0.,windowlim)
      call sm_putlabel(8,'N')
      call sm_relocate(0.,-windowlim)
      call sm_putlabel(2,'S')
      call sm_relocate(windowlim,0.)
      call sm_putlabel(6,'E')
      call sm_relocate(-windowlim,0.)
      call sm_putlabel(4,'W')


      if (wincol .eq. yellow) then ! mark in red forbidden region
         do i = 1, 180            
            x(i) = cos(pideg*(90.-(wdir0-90.+i)))*23.
            y(i) = sin(pideg*(90.-(wdir0-90.+i)))*23.           
            odot(i) = 53.5
            call sm_ctype('red')
         enddo
         call sm_ptype(odot,180)
         call sm_conn(x,y,180)
         call sm_ctype(default_col(idev))
      endif


!-- bottom labels 

      if (idev .eq. 2) then
         call oli_window(1,1,1,1)
         call sm_limits(0.,1.,0.,1.)
         call sm_relocate(1.02,-.04)
         call sm_expand(expand_small)
         call sm_putlabel
     |        (1,'\t Meteomonitor 2.8; comments: ohainaut@eso.org')
         
         call sm_alpha
         write(LUJunk,2020) yr(1),mo(1),dy(1),hh(1),mm(1)
 2020    format(
     |        '\t La Silla, UT ',I4,'-',i2.2,'-',i2.2,' ',i2.2,':',i2.2/
     |        )
         rewind(LUJunk)             ! for some reasons, it does not want to
                                ! write directly to line... 
         read(LUJunk,9999)line
         close(LUJunk)
         call sm_relocate(0.00,-.03)
         call sm_putlabel(3,line)
      endif         
      
C--   finish the plot
      if (debug) write(6,*)"DBG: finishing"
      call sm_expand(1.001)
      call sm_gflush
      call sm_gflush
      call sm_hardcopy
      if (debug) write(6,*)"[enter] to continue"

      if (debug) read(5,9999) line

      enddo                     ! loop on the devices: 1 screen, 2 postcript

      call sm_alpha

C----------------------------------------------------------------------
C    HTML
C----------------------------------------------------------------------
      if (debug) write(6,*)"DBG: start html"

*     *Sidereal time
      day = dy(1)*1.d0          
      call JDATE(yr(1),mo(1),day, jd) ! JD at 0hUT

      dTU = (jd-2451545.0d0)/36525.d0
      dGMST = 24110.54841d0 + 8640184.812866d0*dTU
     >     + 0.093104d0*dTU*dTU - 6.2d-6*dTu*dTu*dTu
                                ! Greenwich Mean Sid time at 0hUT, in sec;
                                ! Algorithm from Astron. Almanach

      dGMST = dGMST/3600.d0     ! in hours

      dST0ut = dGMST+obslong/15.d0  ! compensation for observatory longitude
                                 ! = local sid. time at 0hUT

c      dST = dST0ut + (hh(1) + mm(1)/60.d0 )/.9972696d0
      dST = dST0ut + (servUTh + servUTm/60.d0 )/.9972696d0
                                ! current local sid time

      do while(dST.lt.0.d0)
         dST = dST+24.d0
      enddo
      do while(dST.ge.24.d0)
         dST = dST-24.d0
      enddo

      iSTh = int(dST)
      iSTm = int((dST-iSTh)*60.d0)
      iSTs = int((dST-iSTh-iSTm/60.d0)*3600.d0)

      
C--   Actual curent Julian Day
c      day = dy(1)+ (hh(1) + mm(1)/60. )/24.
      day = dy(1)+ (servUTh + servUTm/60. )/24.
      call JDATE(yr(1),mo(1),day, jd)

C--   coordinates of the sun

      call solcoor(jd, junk1, junk2, RAsun, DecSun)
      
      RAsun = RAsun/15.d0       ! RA in hours
      HAsun = dST - RAsun          ! Sun hour angle

      call h_dist(obslat, HAsun, DecSun, Zsun)

      if (Zsun .ge. 0.d0 ) then ! Day
         isun = 1               ! for color
         write(line,5999)
 5999    format('&deg; ')
      else if (Zsun .ge. -6.d0) then ! civil twilight
         isun = 2
         write(line,5998)
 5998    format('&deg; (Civil Twilight)')
      else if (Zsun .ge. -12.d0) then ! nautical 
         isun = 3
         write(line,5997)
 5997    format('&deg;  (Nautical Twilight)')
      else if (Zsun .ge. -18.d0) then ! astrono.
         isun = 4
         write(line,5996) 
 5996    format('&deg;  (Astronomical Twilight)')
      else 
         isun = 5
         write(line,5995) 
 5995    format('&deg;')
      endif

c-- print the header      

      write(LUHtmLoc,5010)butlink1,lighthtml
      write(LUZommHtmLoc,5010)butlink1,lighthtml
 5010 format(
     | '<html>'/
     | '<head>'/
     | '<META HTTP-EQUIV=REFRESH '/
     | 'CONTENT="120; URL=',a132,'">',
     | '</head>'/
     | '<TITLE> MeteoMonitor </TITLE>'/
     | '<body bgcolor="101010" '/
     | '      text="ffffff"'/
     | '      link="ccffff"'/
     | '      vlink="ccccff"> '/
     | ' '/
     | '<font color="FF0000"><b>Hints:</b></font>'/
     | '<ul><li>Resize the window to FULL SCREEN. '/
     | '    <li>To <BLINK>PRINT</BLINK>,'/
     | '        use the "PRINT GRAPH" button, NOT Netscape`s'/
     | '    <li>From <blink>outside La Silla</blink>, pls use '/
     | '        <a href="'/
     | a132/
     | ,'       ">this lighter page</a>.'/
     | '    <li>This page is self-updating every 2 min.'/
     | '</ul>'/
     | '<hr>'/
     | '<p><CENTER>'/
     |)
      
      write(LUHtmLoc,5012)
      write(LUZommHtmLoc,5012)
 5012 format(
     | '</head>'/
     | '<TITLE> MeteoMonitor </TITLE>'/
     | '<body bgcolor="101010" '/
     | '      text="ffffff"'/
     | '      link="ddffff"'/
     | '      vlink="ddddff"> '/
     | ' '/
     | '<a name="tab"><p><h1>Garching - MeteoMonitor</h1>'/
     | '<!---=====================================================--->'/
     | '<!---====   table     ====================================--->'/
     | '<!---=====================================================--->'/
     | '<p><table border=0>            <!---outer tab             --->'/
     | '<tr><td>                       <!---1st cell = small tab  --->'/
     | '<table border=0>               <!---Small Inner table ------->'/
     | '<!--row 1 = header ------------------------------------------>'/
     | ' <tr><th></th>'/                     ! Name    1
     | '     <th>Current</th>'/              ! Current 2
     | '     <th width=2></th>'/
     | '     <th>Average</th>'/              ! Aver    3
     | '     <th width=2></th>'/                     ! Units   4
     | ' </tr>'/
     |)

      write(LULightHtmLoc,5013)
 5013 format(
     | '</head>'/
     | '<TITLE> MeteoMonitor </TITLE>'/
     | '<body bgcolor="101010" '/
     | '      text="ffffff"'/
     | '      link="eeffff"'/
     | '      vlink="ddddff"> '/
     | ' '/
     | '<p><h1>Garching - MeteoMonitor</h1>'/
     | '<!---=====================================================--->'/
     | '<!---====   table     ====================================--->'/
     | '<!---=====================================================--->'/
     | '<p>'/
     | '<table border=0>               <!---Small Inner table ------->'/
     | '<!--row 1 = header ------------------------------------------>'/
     | ' <tr><th></th>'/                     ! Name    1
     | '     <th>Current</th>'/              ! Current 2
     | '     <th> </th>'/                     ! color ball b
     | '     <th>Average</th>'/              ! Aver    3
     | '     <th></th>'/                     ! Units   4
     | ' </tr>'/
     |)


c--   fill the table
      write(LUHtmLoc,5030)  hum(1),humcol,ahum(1)
      write(LULightHtmLoc,5030)  hum(1),humcol,ahum(1)
      write(LUZommHtmLoc,5030)  hum(1),humcol,ahum(1)
 5030 format(
     | '<!--row 2 = Humidity   ------------------------------------->'/
     | '   <tr><td align=right>Humidity:</td>'/   ! 1
     | '     <td align=center><font color="FFFFFF">&#032;',f5.1/
     | '        </font></td>'/        ! 2
     | '     <td><img src="',a32,'" width=20 height=20></td>'/ ! b
     | '     <td align=center>&#032;',f5.1,'</td>'/! 3
     | '     <td>%</td>'/             ! 4
     | ' </tr>'/
     |     )


      write(LUHtmLoc,5020)  tmp(1),tmpcol,atmp(1)
      write(LULightHtmLoc,5020)  tmp(1),tmpcol,atmp(1)
      write(LUZommHtmLoc,5020)  tmp(1),tmpcol,atmp(1)
 5020 format(
     | '<!--row 3 = Temperature ------------------------------------->'/
     |     ' <tr><td align=right>Temperature:</td>'/    ! 1
     |     '     <td align=center><font color="ffffff">&#032;',f5.1/
     |     '        </font></td>'/                      ! 2
     | '     <td><img src="',a32,'" width=20 height=20></td>'/ ! b
     |     '     <td align=center>&#032;',f5.1,'</td>'/ ! 3
     |     '     <td>&deg;C</td>'/                      ! 4
     | ' </tr>'/
     | )


      write(LUHtmLoc,5040)dpt(1),tmpcol,adpt(1)
      write(LULightHtmLoc,5040)dpt(1),tmpcol,adpt(1)
      write(LUZommHtmLoc,5040)dpt(1),tmpcol,adpt(1)
 5040 format(
     | '<!--row 4 = Dew Point   ------------------------------------->'/
     |     ' <tr><td align=right>Dew Point</td>'/ ! 1 
     |     '     <td align=center><font color="ffffff">&#032;',f5.1/
     |     '        </font></td>'/                ! 2
     |     '     <td><img src="',a32,'" width=20 height=20></td>'/ ! b
     |     '     <td align=center>&#032;',f5.1,'</td>'/ ! 3
     |     '     <td>&deg;C</td>'/                ! 4
     |)

      write(LUHtmLoc,5050)wnd(1),wincol,awnd(1)
      write(LULightHtmLoc,5050)wnd(1),wincol,awnd(1)
      write(LUZommHtmLoc,5050)wnd(1),wincol,awnd(1)
 5050 format(
     | '<!--row 5 = Wind Speed  ------------------------------------->'/
     | ' <tr><td align=right>Wind Speed:</td>'/    ! 1
     | '     <td align=center><font color="ffffff">&#032;',f4.1/
     | '        </font></td>'/         ! 2
     |     '     <td><img src="',a32,'" width=20 height=20></td>'/ ! b
     | '     <td align=center>&#032;',f4.1,'</td>'/ ! 3
     | '     <td>m/s</td>'/            ! 4
     | ' </tr>'/
     |)


      write(LUHtmLoc,5060)phg(1),phgcol,aphg(1)
      write(LULightHtmLoc,5060)phg(1),phgcol,aphg(1)
      write(LUZommHtmLoc,5060)phg(1),phgcol,aphg(1)
 5060 format(
     | '<!--row 6 = Pressure    ------------------------------------->'/
     | ' <tr><td align=right>Atm. Pressure:</td>'/  ! 1
     | '     <td align=center><font color="ffffff">&#032;',f6.1/
     | '        </font></td>'/          ! 2
     |     '     <td><img src="',a32,'" width=20 height=20></td>'/ ! b
     | '     <td align=center>&#032;',f6.1,'</td>'/  ! 3
     | '     <td>hPa</td>'/             ! 4
     | ' </tr>'/
     |)

      write(LUHtmLoc,5062) Zsun,suncol(isun),line
      write(LULightHtmLoc,5062) Zsun,suncol(isun),line
      write(LUZommHtmLoc,5062) Zsun,suncol(isun),line
 5062 format(
     | '<!--row 7 = Sun Altitude ------------------------------------>'/
     | ' <tr><td align=right>Sun Altitude:</td>'/  ! 1
     | '     <td align=center><font color="ffffff">&#032;',f4.0/
     | '       </font>   </td>'/       ! 2
     |     '     <td><img src="',a32,'" width=20 height=20></td>'/ ! b
     | '     <td>        </td>'/       ! 3
     | '     <td>',a132,'</td>'/       ! 4
     | ' </tr>'/
     |)

CC      write(LUHtmLoc,5070)SeeDimm(1)
CC      write(LULightHtmLoc,5070)SeeDimm(1)
CC      write(LUZommHtmLoc,5070)SeeDimm(1)
CC 5070 format(
CC     | '<!--row 8 = Seeing       ------------------------------------>'/
CC     | ' <tr><td align=right>DIMM Seeing:</td>'/
CC     | '     <td align=center>',f4.2,'</td>'/   
CC     | '     <td>        </td>'/
CC     | '     <td ALIGN=left COLSPAN=2>arcsec</td>'/
CC     | ' </tr>  '/
CC     |)
      write(LUHtmLoc,5071)sun(1)
      write(LULightHtmLoc,5071)sun(1)
      write(LUZommHtmLoc,5071)sun(1)
 5071 format(
     | '<!--row 8 = sun       ------------------------------------>'/
     | ' <tr><td align=right>Solar irrad:</td>'/
     | '     <td align=center>',f4.0,'</td>'/   
     | '     <td>        </td>'/
     | '     <td>        </td>'/
     | '     <td ALIGN=left COLSPAN=2>W/m2</td>'/
     | ' </tr>  '/
     |)



      write(LUHtmLoc,5070)raintot(1)
      write(LULightHtmLoc,5070)raintot(1)
      write(LUZommHtmLoc,5070)raintot(1)
 5070 format(
     | '<!--row 8 = rain       ------------------------------------>'/
     | ' <tr><td align=right>Rain since 0h:</td>'/
     | '     <td align=center>',f6.2,'</td>'/   
     | '     <td>        </td>'/
     | '     <td>        </td>'/
     | '     <td ALIGN=left COLSPAN=2>mm</td>'/
     | ' </tr>  '/
     |)



cLOSSAM      write(LUHtmLoc,5072) xabs(1)
cLOSSAM      write(LULightHtmLoc,5072) xabs(1)
cLOSSAM      write(LUZommHtmLoc,5072) xabs(1)
 5072 format(
     | '<!--row 9 = Extinction   ------------------------------------>'/
     | ' <tr><td align=right>LOSSAM Extinction:</td>'/   !1
     | '     <td align=center>',f5.3,'</td>'/        ! 2
     | '     <td>        </td>'/
     | '     <td ALIGN=left COLSPAN=2>Mag/airmass</td>'/     ! 4
     | ' </tr>  '/
     |)

cLOSSAM      write(LUHtmLoc,5073) xabs_rms(1)
cLOSSAM      write(LULightHtmLoc,5073) xabs_rms(1)
cLOSSAM      write(LUZommHtmLoc,5073) xabs_rms(1)
 5073 format(
     | '<!--row 9 = Extinction RMS----------------------------------->'/
     | ' <tr><td align=right>Extinction RMS</td>'/   !1
     | '     <td align=center>',f5.3,'</td>'/        ! 2
     | '     <td>        </td>'/        ! 3
     | '     <td ALIGN=left COLSPAN=2>Mag/airmass</td>'/     ! 4
     | ' </tr>  '/
     |)

      write(LUHtmLoc,5074)
      write(LULightHtmLoc,5074)
      write(LUZommHtmLoc,5074)
 5074 format(
     | '<!--row 10 = SKIP         ----------------------------------->'/
     | ' <tr><td><font color="000000">.</font></td>'/   !1
     | ' </tr>  '/
     | '<!--row 11 = TITLE        ----------------------------------->'/
     | ' <tr><td ALIGN=center colspan=5>'/
     | '     <b>Last Update</b> </td>'/
     | ' </tr>'/
     |)

      write(LUHtmLoc,5080) yr(1),mo(1),dy(1),hh(1),mm(1)
      write(LULightHtmLoc,5080) yr(1),mo(1),dy(1),hh(1),mm(1)
      write(LUZommHtmLoc,5080) yr(1),mo(1),dy(1),hh(1),mm(1)
 5080 format(
     | '<!--row 12 = UT           ----------------------------------->'/
     | ' <tr><td align=right>Local time:</td>'/
     | '     <td ALIGN=center colspan=2>'/
     |          I4,'-',i2.2,'-',i2.2/
     | '     </td><td>'/
     | '       <font color="00ffff">',i2.2,':',i2.2/
     | '       </font>'/
     | '     </td>'/               
     | ' </tr>'/
     |)

      write(LUHtmLoc,5090) jd
      write(LULightHtmLoc,5090) jd
      write(LUZommHtmLoc,5090) jd
 5090 format(
     | '<!--row 13 = JD           ----------------------------------->'/
     | ' <tr><td align=right>JD:</td>'/
     | '     <td ALIGN=center colspan=2>'/
     |        f11.3,'</td>'/               
     | ' </tr>'/
     |)

      write(LUHtmLoc,3100) isth, istm
      write(LULightHtmLoc,3100) isth, istm
      write(LUZommHtmLoc,3100) isth, istm
 3100 format(
     | '<!--row 14 = ST           ----------------------------------->'/
     | ' <tr><td align=right>ST:</td>'/
     | '     <td ALIGN=center colspan=2>'/
     |         i2.2,':',i2.2/ 
     | '     </td>'/               
     | ' </tr>'/
     |)


C     Check UT consistency and print message
      
c      if (servUT.ge.0.) then ! valid time from server
c         UTdiff = abs(servUT - hour(1))
c         if (UtDiff .gt. maxUtDiff)then
c            write(LUHtmLoc,3102) servUtH,servUtM
c            write(LULightHtmLoc,3102) servUtH,servUtM
c            write(LUZommHtmLoc,3102) servUtH,servUtM
c         endif
c      endif

 3102 format(
     | '<!--row 15 = UT Warning  ----------------------------------->'/
     | ' <tr><td ALIGN=center colspan=5>'/
     | ' <font color="red">WARNING</font> Large difference between'/
     | ' WWW UT (',i2.2,':',i2.2,') and MeteoServer UT'/
     | ' (MeteoServer might be stopped)'/
     | '     </td>'/               
     | ' </tr>'/
     |)

C     Check UT variation and print message
      
      UTdiff = abs(hour(1) - Hour(4) )
      if (UtDiff .lt. 0.001)then
         write(LUHtmLoc,3103) 
         write(LULightHtmLoc,3103) 
         write(LUZommHtmLoc,3103) 
      endif

 3103 format(
     | '<!--row 15b = UT stopped?------------------------------->'/
     | ' <tr><td ALIGN=center colspan=5>'/
     | ' <font color="red">WARNING</font> '/
     | ' MeteoServer UT seems constant'/
     | ' (MeteoServer might be stopped)'/
     | '     </td>'/               
     | ' </tr>'/
     |)



      write(LUHtmLoc,3110) xearthGif
      write(LUZommHtmLoc,3110) xearthGif
 3110 format(
     | '<!--row 10 = SKIP         ----------------------------------->'/
     | ' <tr><td><font color="000000">.</font></td>'/   !1
     | ' </tr>  '/
     | '<!--row 11 = XEARTH       ----------------------------------->'/
     | ' <tr><td ALIGN=center colspan=4><img src="',a132/
     | '" alt="map of Xearth; press RELOAD is you see this"></td>'/
     | ' </tr>  '/
     | '<!--row 12 = SKIP         ----------------------------------->'/
     | ' <tr><td><font color="000000">.</font></td>'/   !1
     | ' </tr>  '/
     |)

      write(LUHtmLoc,5025) buttoncol,butlink1,button1
      write(LUHtmLoc,5026) buttoncol,butlink2,button2
      write(LUHtmLoc,5027) buttoncol,butlink3,button3
      write(LUHtmLoc,5026) buttoncol,butlink4,button4
      write(LUHtmLoc,5027) buttoncol,butlink5,button5
      write(LUHtmLoc,5026) buttoncol,butlink6,button6

      write(LULightHtmLoc,5025) buttoncol,lighthtml,button1
      write(LULightHtmLoc,5026) buttoncol,butlink2,button2
      write(LULightHtmLoc,5027) buttoncol,butlink3,button3
      write(LULightHtmLoc,5026) buttoncol,butlink4,button4
      write(LULightHtmLoc,5027) buttoncol,butlink5,button5
      write(LULightHtmLoc,5026) buttoncol,butlink6,button6


      write(LUZommHtmLoc,5025) buttoncol,butlink1,button1
      write(LUZommHtmLoc,5026) buttoncol,butlink2,button2
      write(LUZommHtmLoc,5027) buttoncol,butlink3,button3
      write(LUZommHtmLoc,5026) buttoncol,butlink4,button4
      write(LUZommHtmLoc,5027) buttoncol,butlink5,button5
      write(LUZommHtmLoc,5026) buttoncol,butlink6,button6


 5025 format(                   ! left button
     | ' <tr><td ALIGN=center colspan=2 >'/ 
     | '         <table border=2>'/
     | '         <tr>'/
     | '         <td bgcolor="',a6,'" width=150 align=center>'/
     | '         <a href="',a100,'">'/
     | '         <font color="444444">'/
     | a50/
     | '         </font></a></td></tr></table>'/ ! 12
     | ' </td>'/
     | )
 5026 format(                   ! RIGHT button
     | ' <td ALIGN=center colspan=2 >'/ 
     | '         <table border=2>'/
     | '         <tr>'/
     | '         <td bgcolor="',a6,'" width=150 align=center>'/
     | '         <a href="',a100,'">'/
     | '         <font color="444444">'/
     | a30/
     | '         </font></a></td></tr></table>'/ ! 12
     | ' </td>'/
     | )
 5027 format(                   ! left button with new window
     | ' <tr><td ALIGN=center colspan=2 >'/ 
     | '         <table border=2>'/
     | '         <tr>'/
     | '         <td bgcolor="',a6,'" width=150 align=center>'/
     | '         <a href="',a100,'" target="New">'/
     | '         <font color="444444">'/
     | a30/
     | '         </font></a></td></tr></table>'/ ! 12
     | ' </td>'/
     | )



      write(LUHtmLoc,3120) currentGif
 3120 format(
     | ' </table>         <!--------- END of small table --------->'/
     | ' </td>            <!--- end of 1st cell of outer table---->'//
     | ' <td>             <!--- right cell of outer table--------->'/
     | ' <!-----  GRAPH ------------------------------------------>'/ 
     | ' <img src="',a132/
     | '" alt="graphs; press reload if you see this">',
     | ' </td></tr>       <!--- end of 2d cell of outer table----->'/
     | '</table>'/
     | '</center>'/
     |)


      write(LULightHtmLoc,3122)
 3122 format(
     | ' </table>         <!--------- END of small table --------->'/
     | ' </td>            <!--- end of 1st cell of outer table---->'//
     | '</center>'/
     |)

      write(LUZommHtmLoc,3124) currentZoomGif
 3124 format(
     | ' </table>         <!--------- END of small table --------->'/
     | ' </td>            <!--- end of 1st cell of outer table---->'//
     | ' <td>             <!--- right cell of outer table--------->'/
     | ' <!-----  GRAPH ------------------------------------------>'/ 
     | ' <img src="',a132,'" alt="zoomed graphs">',
     | ' </td></tr>       <!--- end of 2d cell of outer table----->'/
     | '</table>'/
     | '</center>'/
     |)

 3150 open( unit=LUMsg, file='meteo_msg.txt' )
      read( LUMsg, 9999, err=3200, end=3200) line
      write(LUHtmLoc, 9999) line
      write(LULightHtmLoc, 9999) line
      write(LUZommHtmLoc, 9999) line
      goto 3150
 3200 continue

c--- end of file
      write(LUHtmLoc,5300)
     |     butlink1,lighthtml,butlink2,butlink3,commentlink
      write(LULightHtmLoc,5300)
     |     lighthtml,lighthtml,butlink2,butlink3,commentlink
 5300 format(
     | '<!---=====================================================--->'/
     | '<!---====   Buttons and links ============================--->'/
     | '<!---=====================================================--->'/
     | '<hr>'/
     | '<font color="red"><b>Buttons: </b></font> ['/
     | '<a href="',a100,'">Update</a>|'/
     | '<a href="',a100,'">No graphic</a>|'/
     | '<a href="',a100,'">Print</a>|'/
     | '<a href="',a100,'">Info</a>|'/
     | '<a href="',a100,'">Bugs/Comments/Suggestions</a>]'/
c     | '<hr>'/
     | '<br><font color="red"><b>Links: </b></font> ['/
     | '<a href="http://www.ls.eso.org/lasilla/dimm">'/
     | 'La Silla meteoMonitor</a>|'/
     | '<a href="http://www.meteo.physik.uni-muenchen.de/'//
     | 'dokuwiki/doku.php?id=wetter:garching:neu">'/
     | 'LMU Garching Weather station</a> (from which the data come)'/
     | '] '/
     | '<!---=====================================================--->'/
     | '<!---====   Copyright ====================================--->'/
     | '<!---=====================================================--->'/
     | '<hr>'/
     | '<font color="red">MeteoMonitor</font> v2.10 (2011-Apr-1) --'/
     | 'Copyright&#169 (1997) '/
     | '<a href="http://www.eso.org">'/
     | 'European Southern Observatory</a>/'/
     | '<a href="http://www.eso.org/~ohainaut">'/
     | 'O.Hainaut</a>.<br>'/
     |)

      write(LUHtmLoc,5310)
      write(LUZommHtmLoc,5310)
 5310 format(
     | '<!---=====================================================--->'/
     | '<!---====   END   ========================================--->'/
     | '<!---=====================================================--->'/
     | '<font  color="000000">'/
     | '<p>.<p>.<p>.<p>.<p>.<p>.<p>.<!--- to hide header on reload--->'/
     | '</body>'/
     | '</html>'/
     |)


      close(66)
      close(67)
      close(68)
      goto 8888

C----------------------------------------------------------------------     
c---  ERRORS
C----------------------------------------------------------------------     

 8801 write(6,*) 'ERROR8801: meteo.config not found'
      write(6,*) '       this file contains the default URLs etc...'
      goto 8888

 8802 write(6,*) 'ERROR8801 reading meteo.config'
      write(6,*) '      not enough lines'
      goto 8888

 8803 write(6,*) 'ERROR8803 reading dimm.dat'
      goto 8888

 8804 write(6,*) 'ERROR8804 reading ut.last'
      goto 8888


 8888 continue
      
 9999 format(132a)

      end





c----------------------------------------------------------------------
c----------------------------------------------------------------------
c----------------------------------------------------------------------
c----------------------------------------------------------------------
c----------------------------------------------------------------------

      subroutine labeline(iut,hour_now,hour_left,y_min,y_max,label20 )

C     draw all the labels of a box

      implicit none
      real*4 x(10),y(10)              ! junk used for lines, labels, etc...
      real*4 ylabel1,ylabel2   ! where the  labels should go
      real*4 y_min, y_max, xlabel
      real*4 hour_now, hour_left ! current time and first time, limits
      integer i, j, iut, idev
      character line*80, label20*20
      character default_col(3)*32
      common default_col,idev

      if (iut.eq.1) then 
         ylabel1 = y_min - (y_max-y_min)/20.
      else if (iut.eq.-1) then
         ylabel1 = y_max + (y_max-y_min)/20.
      else
         ylabel1 = -9999.
      endif
      
      ylabel2 = y_max - (y_max-y_min)/10.
  

C     --- label the X axis with hours in [0,23]
      if (iut.ne. 0) then
         call sm_expand(0.7)


         do i = int(hour_now), int(hour_left),-1
            x(1) = i         

            j = i
            do while(j.lt.0)
               j = j+24
            enddo
            write(line,2999) j
 2999       format(i2.2)
            
            if (mod(i,3).eq.0) then
               call sm_relocate(x(1),ylabel1)
               call sm_putlabel(5,'\t '//line(1:2))
            endif
         enddo
         
c         call sm_relocate(hour_now,ylabel1)
c         call sm_putlabel(3,' \t UT')

      endif
      
      
      call sm_expand(1.01)
      
      x(1) = 0.                 ! midnight line
      x(2) = 0.
      y(1) = y_min
      y(2) = y_min + (y_max-y_min)/10.
      call sm_ctype('red')
      call sm_conn(x,y,2)
      call sm_ctype(default_col(idev))
      
c     call sm_relocate(0.,ylabel2)
c      call sm_putlabel(6,' Today')
c      call sm_relocate(0.,ylabel2)
c      call sm_putlabel(4,'Yesterday ')


c     --- title
c      xlabel = (hour_left+hour_now)/2.
      xlabel = hour_left+(hour_now-hour_left)/20.
      call sm_relocate(xlabel,ylabel2)
      call sm_expand(0.8)
      call sm_putlabel(6,'\t '//label20)


      return
      end


c------------------------------------------------------------------------------
      SUBROUTINE JDATE(IYR,IMO,DAY,DAYJUL)
C     .NAME      JDATE.DOR
C     .VERSION
C          original coding:  D. Gillet   ESO - Garching 870102
C          variables renamed and restructured:   D. Baade   ST-ECF, Garching
C          R.M.West, transfered from VAX and adapted to PC, 25.1.1991
C          Transfer to Sun/Unix, O Hainaut, Feb1991
C     .ALGORITHM
C          adapted from MEEUS J.,1980, ASTRONOMICAL FORMULAE FOR CALCULATORS
C     .REMARKS
C          Input:
C              IYR   : Civil Year  (Integer*4)
C              IMO   : Civil Month (Integer*4)
C              DAY   : Civil Day (with UT fraction) (Real*8)
C          Output:
C              DAYJUL: Julian Date (Real*8)
c------------------------------------------------------------------------------
      implicit real*8 (A-H,O-Z), integer*4 (I-N)
      dimension DATE(3)
C

      DATE(1) = dfloat(IYR)
      DATE(2) = dfloat(IMO)
      DATE(3) = dint(DAY)
      UT = DAY - DATE(3)
C
      if (DATE(2).gt.2) then
	 YP = DATE(1)
         P = DATE(2)
      else
         YP = DATE(1)-1
         P = DATE(2)+12
      endif

      C = DATE(1) + DATE(2)*1.d-2 + DATE(3)*1.d-4 + UT*1.d-6
      if (C.ge.1582.1015) then
         IA = idint(YP/100.d0)
         A = dfloat(IA)
         IB = 2-IA+idint(A/4.d0)
      else
         IB = 0
      endif

      DAYJUL = dint(365.25d0*YP)+dint(30.6001d0*(P+1.d0))+DATE(3)+UT
     |     +dfloat(IB)+1720994.5d0



      return
      end

*------------------------------------------------------------------------------
      subroutine solcoor(dayjul,tsol,xm,ra,dec)
* .PURPOSE    Compute the *APPROX* solar coordinates
c$$$c     seems to work within a couple of deg.
c.ALGORITHM   From J. Meeus' ASTRONOMICAL FORMULAE FOR CALCULATORS 1980
*.REMARKS  
*   Input: 
*           dayjul Julian Date   
*   Output:
*           tsol: sun's true long
*           ra, dec: sun's RA and Dec (RA in DEGREES, dec in degrees)
*.VERSION  2001-10-18 passed to radians, ok within few arcmin. ohainaut   
*.HISTORY
c     Mon Jan 29 18:18:47 1996
C     ORHainaut Aug92
*------------------------------------------------------------------------------


      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)

      pideg = acos(-1.)/180.

c     julian centuries since 1900 Jan 0.5 ET:
      T = (dayjul - 2415020.d0)/ 36525.d0    

C     Geometric mean long of the sun:
      XL = 279.69668d0 + 36000.76892d0*T + 0.0003025d0*T*T

c     Sun's mean anomaly
      XM = 358.47583d0 + 35999.04975d0*T - 0.0001500d0*T*T
     >     - 0.0000033d0*T*T*T

c     Eccentricity of earth's orbit
      e = 0.01675104d0 - 0.0000418d0*T - 0.000000126d0*T*T

C     sun's equation of the center:
      C = (1.919460d0 -0.004789*T -0.000014*T*T) * dsin(pideg*XM)
      C = C+ (0.020094d0 - 0.000100d0*T) * dsin(pideg* 2.d0*XM )
      C = C+ 0.000293d0 * dsin(pideg* 3.d0*XM )
     
C     Sun's true longitude
      Tsol = XL+C
      Tsol = dmod(tsol,360.d0)
     
C     c     sun's true anomaly
C     Tnu = XM + C
C     
C     C     sun's radius vector:
C     R = 1.0000002d0* (1.d0 -e*e)
C     R = R/(1.d0+ e* dcos(pideg*Tnu))


      Omega = 259.18d0-1934.142d0*T

c     ecliptic obliquity
      epsilon = 23.453394d0 -0.0130125d0*T
      epsilon = epsilon - 0.00000164d0 *T*T
      epsilon = epsilon + 0.000000503d0*T*T*T

c     correction for apparent position:
      epsilon = epsilon+ 0.00256d0* dcos(pideg*Omega)      

c     conversion to RA Dec:
      tg_ra = dcos(pideg*epsilon) * dsin(pideg*tsol)/dcos(pideg*tsol)
      sin_dec = dsin(pideg*epsilon) * dsin(pideg*tsol)


      ra = datan(tg_ra)/pideg

      if (dcos(pideg*Tsol).lt.0.d0) ra = ra+180.d0
                                ! ra should be in the same quadrant as Tsol 

      do while (ra .lt. 0.d0) 
         ra = ra+360.d0
      enddo
      do while (ra .ge. 360.d0) 
         ra = ra-360.d0
      enddo
         
      dec = dasin(sin_dec)/pideg
      do while(dec .le. -90.)
         dec = dec+360.d0
      enddo
      do while(dec .gt. 90.)
         dec = dec-360.d0
      enddo


      return
      end

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine h_dist(dlat,ha,dec,dh)
c.Purpose        compute the zenithal distance
c.input          everything in deg., except ha in hours
c.Version        Sat Jul 22 16:44:34 1995
c.author         O Hainaut
c----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z), INTEGER*4 (I-N)
      
      pideg = dacos(-1.d0)/180.

      dha = ha*15.d0
      sinh = dsin(pideg*dlat)*dsin(pideg*dec) 
      sinh = sinh+ dcos(pideg*dlat)*dcos(pideg*dec)*dcos(pideg*dha)

      dh = dasin(sinh)/pideg

      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine oli_window(nx,ny,ix,iy)
c     input nx, ny: number of windows in x and y
c           ix, iy: which window, starts at 1 to n*
C     Note: the sm window command should do this. For some 
c     reasons, it crashes.

      ni = 30000

      idx = ni/nx
      idy = ni/ny

      ix2 = ix*idx +1000 - idx/6
      ix1 = ix2-idx +idx/6

      iy2 = iy*idy +1000
      iy1 = iy2-idy

      call sm_location(ix1,ix2, iy1, iy2)
      return 
      end

c __oOo__
