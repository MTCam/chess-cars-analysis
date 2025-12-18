      SUBROUTINE CRVLAB (LEGSTR, X, Y, CHRSIZ)
C
C_______________________________________________________________________
C
C   prints legend if both theoretical and data arrays are used
C     inputs:
C       legstr  : legend label character string
C       x, y    : location of legend label
C       chrsiz  : character size
C_______________________________________________________________________
C
      CHARACTER*(*) LEGSTR
      COMMON /PLTCLP/ X1PL, X2PL, Y1PL, Y2PL
C
      XZ = X*(X2PL-X1PL)+X1PL        
      YZ = Y*(Y2PL-Y1PL)+Y1PL
      CALL SCALE2 (XZ, YZ, VX, VY)
      CALL GSMOVE (VX-2.5*CHRSIZ, VY)
      CALL GSDRAW (VX+2.5*CHRSIZ, VY)
      CALL GSMOVE (VX, VY)
      CALL GSLTYP(1)
      CALL GSMOVE (VX+3*CHRSIZ, VY-0.5*CHRSIZ)
      CALL GSPSTR (LEGSTR)
C
      RETURN
      END
      SUBROUTINE DONEPL
C
C_______________________________________________________________________
C
C   dummy subroutine to satisfy potential call from subroutine QUITS 
C   in DISSPLA version
C_______________________________________________________________________
C
      RETURN
      END

      SUBROUTINE PLTCHI_JANK (X1, Y1, X2, Y2, NP, XLAB, YLAB, TITL,
     1   LG1, LG2, IPL, FPLT)
C
C_______________________________________________________________________
C
C   plots theoretical and data spectra using DIGLIB graphics library
C     input variables:
C        x1, y1     : theoretical spectrum array
C        x2, y2     : data spectrum (if used)          
C        np         : number of points in spectra
C        xlab, ylab : plot labels
C        titl       : title of data array
C        lg1, lg2   : labels for legend
C        ipl        : number of spectra to plot
C        fplt       : logical flag to call device driver menu only once
C_______________________________________________________________________
C
	save devnam, idev1

      PARAMETER (ND = 5000)
      REAL X1(*), X2(*), Y1(*), Y2(*)
      CHARACTER*(*) XLAB, YLAB, TITL, LG1, LG2
      LOGICAL FPLT
      COMMON /PLTCLP/ X1PL, X2PL, Y1PL, Y2PL
      COMMON /GCDSEL/ IDEV
C
C---  local variables:
C
      DIMENSION YDIFF(ND), XPLZER(2), PLTZER(2), PLTOFF(2)
      CHARACTER ANS*1, namdev*8, devnam*8, CHR
	 logical nottrm

      DATA IDEV1 / 0 /
C      DATA I4014, I125, ILN03 / 1, 5, 10 /
      DATA NULL, IESC, IBELL / 22, 27, 7 /
      DATA XPLZER(1), PLTZER / 0., 0., 0. /, devnam /'        '/
	 external namdev

c	 Function to determine if device is a terminal or graphics board

	 nottrm() =
     + ((devnam .eq. 'QMSLASER') .or.
     +  (devnam .eq. 'LASERWRI') .or.
     +  (devnam .eq. 'DECLN03 ') .or.
     +  (devnam .eq. 'HPPLOTTR') .or.
     +  (devnam .eq. 'HPLSJIII'))
C
C---  determine plot limits
C
      CALL MINMAX (X1, NP, XMIN1, XMAX1)
      IF (IPL .EQ. 2) CALL MINMAX (X2, NP, XMIN2, XMAX2)
      CALL MINMAX (Y1, NP, YMIN1, YMAX1)
      IF (IPL .EQ. 2) CALL MINMAX (Y2, NP, YMIN2, YMAX2)
C
      IF (IPL .EQ. 2) THEN
         XMIN = AMIN1 (XMIN1, XMIN2)
         XMAX = AMAX1 (XMAX1, XMAX2)
         YMAX = AMAX1 (YMAX1, YMAX2)
         YMIN = -0.25*YMAX
         XPLZER(2) = 2.*XMAX
         PLTOFF(1) = YMIN/2.
         PLTOFF(2) = PLTOFF(1)
C
         DO 200 I = 1, NP
            YDIFF(I) = Y2(I)-Y1(I)+PLTOFF(1)
200      CONTINUE
C
      ELSE
         XMIN = XMIN1
         XMAX = XMAX1
         YMIN = YMIN1
         YMAX = YMAX1
         IF (YMIN .GT. 0.) YMIN = 0.
      ENDIF
C
C---  set up device driver
C
      IF (FPLT .OR. nottrm()) THEN
	   CALL CLEAR_TEXT ()
         CALL SELDEV (40)
         IDEV1 = IDEV
      ELSE
         CALL DEVSEL (IDEV1, 40, IERR)
      ENDIF
	 devnam = namdev()			!Get graphics device name & save it
      FPLT = .FALSE.
C
C---  set up plot
C
      CALL BGNPLT
      IF (devnam .eq. 'TEK4014 ') THEN
         CHRSIZ = 0.9*GOODCS(0.5)
	 else if (devnam .eq. 'DECLN03 ') then
	    chrsiz = 0.35
      else
         chrsiz = 0.22
      ENDIF
	 call gscolr (16, ierr)		!Bright white for axes
      CALL MAPSET (0., 100., 0., 100., CHRSIZ, 0.7*CHRSIZ, .FALSE.)
      CALL MAPIT (XMIN, XMAX, YMIN, YMAX, XLAB, YLAB, TITL, 64+128)
C
C---  plot curves
C
      CALL GSLTYP(1)
      IF (IPL .EQ. 2) THEN
         CALL CURVE (XPLZER, PLTZER, 2, 0, 1., 1)
         CALL CURVE (XPLZER, PLTOFF, 2, 0, 1., 1)
	    call gscolr (14, ierr)		!Bright magenta for residuals
         CALL CURVE (X1, YDIFF, NP, 0, 1., 1)
	    call gscolr (10, ierr)		!Bright red for data
         CALL GSLTYP (3)
         CALL CURVE (X2, Y2, NP, 0, 1., 1)
      ENDIF

	 call gscolr (11, ierr)			!Bright green for theory
	 call gsltyp (1)
      CALL CURVE (X1, Y1, NP, 0, 1., 1)
C
C---  print legend if using data array
C
      IF (LG1(1:1) .NE. ' ') THEN
	    call gscolr (11, ierr)			!Bright green
         CALL CRVLAB (LG1, 0.1, 0.95, CHRSIZ)
         IF (IPL .EQ. 2) THEN
	       call gscolr (10, ierr)			!Bright red
            CALL GSLTYP(3)
            CALL CRVLAB (LG2, 0.1, 0.87, CHRSIZ)
         CALL GSLTYP(1)
         ENDIF
      ENDIF
	 call gscolr (16, ierr)				!Back to bright white
C
C---  end plot and release terminal
C
      CALL ENDPLT
C
C---  hardcopy of plot on vt125/vt240/vt241 terminal
C
	  if ((devnam .eq. 'DECVT125') .or. (devnam .eq. 'DECVT240')
     +    .or. (devnam .eq. 'DECVT241')) then
C
C---  the '+' carriage control character is not recognized by some
C---  compilers.  if so, the '+' should be replaced by 1x.  for some
C---  compilers that do not recognize '+', carriage return/line
C---  feed can be suppressed by putting a $ at the end of the format
C---  statement.
C
         WRITE (*, '(4A)') '+', IESC, CHAR(92), IBELL
         ANS = 'N'
         CALL YESNO ( 'Copy to printer' , ANS)
         IF (ANS .EQ. 'Y') THEN
            WRITE (*, '(5A)') '+', IESC, 'PpS(H)', IESC, CHAR(92)
            PRINT *, 'Enter <RETURN> when done'
         ELSE
            PRINT *, 'Enter <RETURN> to continue'
         ENDIF
      ELSEIF (devnam (1:3) .eq. 'TEK') THEN
	    WRITE(*, '(2A)') '+', IBELL
      ELSEIF (devnam (1:3) .eq. 'VGA') THEN
         CALL BEEP ()
       ENDIF
C
C      IF (ITERM .EQ. ILN03) THEN
C         WRITE (*, '(3A)') '+', IESC, CHAR(12)
C      ELSE
C50       READ (5, '(A1)', ERR = 50) PAUSE
C      ENDIF

C50	 if (.not. nottrm()) read (*, '(a)', err = 50) pause  

50	if (.not. nottrm()) CALL GET_CHAR (CHR)

      if (devnam .ne. 'HPPLOTTR') CALL BGNPLT

      IF (devnam .EQ. 'TEK4014 ') THEN
         DO I = 1, 50
            WRITE (*, '(2A)') '+', NULL
         END DO
         WRITE (*, '(3A)') '+', IESC, '2' 
      ENDIF
      CALL RLSDEV
C
      RETURN
      END

C==== chiplt_csv.f  — replacement for CHIPLT/PLTCHI (CSV writer)
C     Writes:  pltchi_0001.csv  (and .meta), then _0002, ...
C     CSV columns:
C       if IPL=1: x1,y1
C       if IPL=2: x1,y1,x2,y2,yresid   (yresid = y2 - y1)

      SUBROUTINE PLTCHI (X1, Y1, X2, Y2, NP, XLAB, YLAB, TITL,
     1   LG1, LG2, IPL, FPLT)
C -- signature copied from your code
      INTEGER NP, IPL
      REAL X1(*), Y1(*), X2(*), Y2(*)
      CHARACTER*(*) XLAB, YLAB, TITL, LG1, LG2
      LOGICAL FPLT, PLOTTER_AVAILABLE
      EXTERNAL PLOTTER_AVAILABLE

      INTEGER I, IOS
      INTEGER LUNCSV, LUNMETA
      PARAMETER (LUNCSV=41, LUNMETA=42)

      CHARACTER*64 FCSV, FMETA
      INTEGER ICALL
      REAL YRES

      SAVE ICALL
      DATA ICALL /0/

C -- unique filenames: pltchi_0001.csv, pltchi_0001.meta, ...
      ICALL = ICALL + 1
      WRITE(FCSV, '( "pltchi_", I4.4, ".csv" )')  ICALL
      WRITE(FMETA,'( "pltchi_", I4.4, ".meta")')  ICALL

C -- CSV
      OPEN(UNIT=LUNCSV, FILE=FCSV, STATUS='UNKNOWN', IOSTAT=IOS)
      IF (IOS .NE. 0) RETURN

      IF (IPL .EQ. 2) THEN
         WRITE(LUNCSV,'(A)') 'x1,y1,x2,y2,yresid'
      ELSE
         WRITE(LUNCSV,'(A)') 'x1,y1'
      ENDIF

      DO 10 I = 1, NP
         IF (IPL .EQ. 2) THEN
            YRES = Y2(I) - Y1(I)
            WRITE(LUNCSV,
     &        '(ES20.10,1X,'','',1X,ES20.10,1X,'','',1X,ES20.10,1X,'','',
     &          1X,ES20.10,1X,'','',1X,ES20.10)')
     &          X1(I), Y1(I), X2(I), Y2(I), YRES
         ELSE
            WRITE(LUNCSV,'(ES20.10,1X,'','',1X,ES20.10)') X1(I), Y1(I)
         ENDIF
   10 CONTINUE
      CLOSE(LUNCSV)

C -- tiny sidecar with labels/title/flags (optional but handy)
      OPEN(UNIT=LUNMETA, FILE=FMETA, STATUS='UNKNOWN', IOSTAT=IOS)
      IF (IOS .EQ. 0) THEN
         WRITE(LUNMETA,'(A)')     'title: '  // TITL
         WRITE(LUNMETA,'(A)')     'xlabel: ' // XLAB
         WRITE(LUNMETA,'(A)')     'ylabel: ' // YLAB
         WRITE(LUNMETA,'(A)')     'legend1: '// LG1
         WRITE(LUNMETA,'(A)')     'legend2: '// LG2
         WRITE(LUNMETA,'(A,I0)')  'ipl: ', IPL
         CLOSE(LUNMETA)
      ENDIF

      WRITE(*,'(A)') 'Plot data written to: ' // TRIM(FCSV)
     & // ', ' // TRIM(FMETA)

C -- original code set this during device/menu setup; keep behavior
      FPLT = .FALSE.
 
      IF (plotter_available()) THEN
         CALL spawn_plotter(FCSV)
      ELSE
         WRITE(*,*) 'Plotter utility not available, skipping display.'
      ENDIF

      RETURN
      END
      
      logical function is_windows()
      implicit none
      character*64 os
      integer len

      os = ' '
      call get_environment_variable('OS', os, length=len)
      if (len .ge. 10) then
         is_windows = (os(1:10) .eq. 'Windows_NT')
      else
         is_windows = .false.
      endif
      return
      end
 
      logical function plotter_available()
      implicit none
      integer st
      character*256 cmd
      logical is_windows
      external is_windows
      integer cmdstat
      character*256 cmdmsg
      if (is_windows()) then
C        Windows: where plot-carsfit (works for .exe/.bat/.cmd on PATH)
         cmd = 'cmd /c where plot-carsfit >nul 2>&1'
      else
C        macOS/Linux: command -v carsfit-plot
         cmd = 'sh -c "command -v plot-carsfit >/dev/null 2>&1"'
      endif
C      write(*,*) 'plotter_available cmd = [', trim(cmd), ']'
C      call flush(0)

      call execute_command_line(trim(cmd), wait=.true., exitstat=st,
     &                          cmdstat=cmdstat, cmdmsg=cmdmsg)

      if (cmdstat .ne. 0) then
C         write(0,'(A,I0,2A)') 'plotter_available: cmdstat=',
C     &                        cmdstat, ' msg=', trim(cmdmsg)
C         call flush(0)
         plotter_available = .false.
         return
      endif
C      call execute_command_line(trim(cmd), wait=.true., exitstat=st)
      plotter_available = (st .eq. 0)
      return
      end

      subroutine spawn_plotter(csvname)
      implicit none
      character*(*) csvname
      character*1024 cmd
      logical is_windows
      external is_windows

      if (is_windows()) then
C        Windows: start launches without blocking. /B = no new window.
C        Quotes: start requires a window title argument (""), then the command.
         cmd = 'cmd /c start "" /B plot-carsfit "'//trim(csvname)//'"'
      else
C        macOS/Linux: background with &, silence output
         cmd = 'sh -c "plot-carsfit "'//trim(csvname)//'"
     & >/dev/null 2>&1 &"'
      endif

      call execute_command_line(cmd)
      return
      end

      subroutine spawn_plot_py(python, script, csv)
      implicit none
      character(len=*), intent(in) :: python, script, csv
      character(len=:), allocatable :: cmd
      
      cmd = trim(python) // ' "' // trim(script) // '" "' // trim(csv) // '" '

      write(*,'(A)') 'Executing command:' // cmd

      call execute_command_line(cmd, wait=.false.)
      end subroutine

      SUBROUTINE PLRES (CHI, DAT, DATNAM, IPLTYP, ITASK, ITITLE,
     1    NP, PRBFIL, VARFIT, VARRUN, W)
C
C___________________________________________________________________
C
C  plots a spectrum
C    input parameters:
C      chi  - theory array for ordinate
C      dat  - data array, if comparing theory to data
C      datnam - name of data file, if any
C      ipltyp - 1 = convolved spectrum, 2 = theoretical chi,
C               3 = reference spectrum
C      itask - task being done (see main program)
C      ititle - header line from data file, if any
C      np - number points
C      prbfil - name of probe instrument function file
C      varfit, varrun - fitting and run control variables
C      w - wavenumber array (abscissa)
C___________________________________________________________________
C
      DIMENSION CHI(*), DAT(*), VARFIT(*), VARRUN(*), W(*)
      CHARACTER* (*)DATNAM, ITITLE, PRBFIL
C
C---  local variables
C
      CHARACTER ANS1*1, ANS2*1, ANS3*1
      LOGICAL FPLT, frstim
      SAVE ANS1, ANS2, ANS3
      DATA ANS1, ANS2, ANS3/ 'Y' , 'N' , 'N' /
      DATA FPLT/ .TRUE. /, frstim / .true. /
C
C:::::::::::::::::::::::::::::::::::::::::: end declarations ::::::::
C
	 if (frstim) then
	   write(*,*) 'When prompted for a plot, typing "m" and <RETURN>'
	   write(*,*) 'will allow a new plot device to be selected.'
	   frstim = .false.
	 endif

      IF (IPLTYP .EQ. 1) THEN
         IF (ITASK .NE. 1 .AND. ITASK .NE. 7) THEN
            CALL YESNOM ( 'Plot convolved spectrum' , ANS1)
            IF ((ANS1 .NE. 'Y') .and. (ans1 .ne. 'M')) then
		    RETURN
	       else if (ans1 .eq. 'M') then
		    fplt = .true.
		  endif
            CALL PLTCHI (W, CHI, W, DAT, NP, 'Raman Shift (cm-1)', 
     1	       'SQRT(Intensity)', ITITLE, 'Theory', 'Data', 2, FPLT)
         ELSE
            CALL YESNOM ( 'Plot convolved spectrum' , ANS1)
            IF ((ANS1 .NE. 'Y') .and. (ans1 .ne. 'M')) then
		    RETURN
	       else if (ans1 .eq. 'M') then
		    fplt = .true.
		  endif
            CALL PLTCHI (W, CHI, W, DAT, NP, 'Raman Shift (cm-1)', 
     1	       'SQRT(Intensity)', ' ', ' ', ' ', 1, FPLT)
         ENDIF
      ELSEIF (IPLTYP .EQ. 2) THEN
         CALL YESNOM ( 'Plot theoretical susceptibility' , ANS2)
         IF ((ANS2 .NE. 'Y') .and. (ans2 .ne. 'M')) then
		 RETURN
	    else if (ans2 .eq. 'M') then
		 fplt = .true.
	    endif
         CALL PLTCHI (W, CHI, W, CHI, NP, 'Raman Shift (cm-1)', 
     1	    'Theoretical Susceptibility', ' ', ' ', ' ', 1, FPLT)
      ELSEIF (IPLTYP .EQ. 3) THEN
         CALL YESNOM ( 'Plot convolved reference spectrum ' , ANS3)
         IF ((ANS3 .NE. 'Y') .and. (ans3 .ne. 'M')) then
		 RETURN
	    else if (ans3 .eq. 'M') then
		 fplt = .true.
	    endif
         CALL PLTCHI (W, CHI, W, CHI, NP, 'Raman Shift (cm-1)', 
     1	    'Reference Spectrum', ' ', ' ', ' ', 1, FPLT)
      ENDIF
C
      END


      SUBROUTINE YESNOM (PROMPT, YORN)
C
C-----------------------------------------------------------------
C    Reads a character until it has obtained either a 'y', an
C    'n', 'm', or a blank.  Handles either upper or lower case,
C    returns upper case.
C     if user response to prompt is <cr> only, yorn is returned
C     as the upper case of the input value, unless the input was
C	 'm', when 'Y' is returned.
C-----------------------------------------------------------------
C
      CHARACTER* (*)PROMPT, YORN
      CHARACTER*1 ANS
C
c	 Don't give 'm' as default choice

	 if ((yorn .eq. 'm') .or. (yorn .eq. 'M')) yorn = 'Y'

100   CONTINUE
      PRINT '(1X,A,A2,A1,A4)' , PROMPT, ' <' , YORN, '> ? '
      READ '(A1)' , ANS
C
      IF (ANS .EQ. ' ') THEN
C
C            . . . make sure have upper case
C
         IF (YORN .EQ. 'n') YORN = 'N'
         IF (YORN .EQ. 'y') YORN = 'Y'
         RETURN
      ENDIF
C
      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
         YORN = 'Y'
      ELSEIF (ANS .EQ. 'N' .OR. ANS .EQ. 'n') THEN
         YORN = 'N'
	 elseif (ans .eq. 'M' .or. ans .eq. 'm') then
	    yorn = 'M'
      ELSE
         PRINT *,
     1   ' !!! ERROR . . ACCEPTABLE RESPONSES ARE: YES, NO,'
         PRINT *,
     1   ' Y, N (UPPER OR lower CASE), OR A CARRIAGE RETURN.'
         GO TO 100
      ENDIF
C
      PRINT *, ' '
C
      END
