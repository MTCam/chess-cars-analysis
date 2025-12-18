      PROGRAM CARSFT
C
C______________________________________________________________________
C
C    Program for calculation of CARS (coherent anti-Stokes Raman
C    spectroscopy) spectra.  See accompanying file cars.doc for
C    help and documentation.
C
C    This is the VAX version, with input and output to the
C    terminal.  The graphics package used in this version is DISSPLA,
C    but another version has been modified to use DIGLIB, a public
C    domain graphics package.  To convert the graphics package from
C    DISSPLA to DIGLIB, link with the external subroutine CHIPLT
C    and the DIGLIB library, instead of linking with the external
C    subroutine CHIDIS and the DISSPLA library.
C
C    This program was issued by Sandia Laboratories, a prime contractor
C    to the United States Department of Energy.  Neither the United
C    States, nor the United States Department of Energy, nor any of
C    their employees, nor any other their contractors, subcontractors,
C    nor their employees, make any warranty, express or implied, or
C    assumes any legal liability or responsibility for the accuracy,
C    completeness, or usefulness of any information, apparatus,
C    product or process disclosed, or represents that its use would
C    not infringe privately owned rights.
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17, NP = 30, NPP = 31)
      DIMENSION VARRUN(NR)
      COMMON /PARAMS/ VARFIT(NF, 4)
      COMMON /CSTEP/ XVAR(NP), XMAX(NP), XMIN(NP), DELTX(NP),
     1   DELMN(NP), ERR(NP, NPP), FOBJ, NFS, NTRAC, MATRX,
     2   MASK(NP), NFMAX, NFLAT, JVARY, NXTRA, KFLAG, NOREP,
     3   KERFL, KW
C
C   newref : indicates that a new reference array is to be
C   computed and written to the data file.  it is a flag to insure
C   the file is read in task23, since it will have same name as
C   the file perhaps already opened
C
      LOGICAL BACKUP, NEWREF, EXTPRG
      CHARACTER DATFIL*40, PROMP(5)*80, RUNFIL*40, ANS*1,
     1   HAVRUN*1, CONT*1, PRBFIL*20, PSTFIL*40
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  initialize console (text) window and state menu
C
	CALL INIT_TEXT ()
      CALL INIT_MENU ()
C
C---  print header with version date
C
      CALL LOGO
C
C---  read and process molecular variables,
C
      CALL MOLPAR
C
C---  set defaults and initialization
C
      CALL SETDEF (DATFIL, PRBFIL, VARFIT, NF, VARRUN)
      DATA RUNFIL, PSTFIL / 'cars.par' , 'postmor.' /
C
C---  open postmortem file
C
C      CALL OPFILE (PSTFIL, 33, 40, 'Name of postmortem file ?',
C      CALL OPFILE (PSTFIL, 33, 40, ' ',
C     1   'UNKNOWN')
      PRINT *, ' '
C
C---  beginning of main loop
C
100   CONTINUE
      BACKUP = .FALSE.
      EXTPRG = .FALSE.
      DATA HAVRUN/ 'Y' /
C      CALL YESNO ( 'Read run parameters from a file' , HAVRUN)
C
      IF (HAVRUN .EQ. 'Y') THEN
         CALL OPFILE (RUNFIL, 22, 40,
     1      'Name of run parameter file ?' , 'OLD' )
         HAVRUN = 'N'
         CALL VARIN (DATFIL, PRBFIL, VARRUN, VARFIT)
      ENDIF
C
120   CONTINUE
      CALL DISRUN (PRBFIL, VARRUN, BACKUP, .FALSE.)
      IF (BACKUP) GO TO 100
      ITASK = VARRUN(1)
C
C---  do task:
C
      IF (ITASK .EQ. 1) THEN
         CALL TASK1 (PRBFIL, VARFIT, VARRUN, BACKUP, EXTPRG)
C
      ELSEIF (ITASK .EQ. 2 .OR. ITASK .EQ. 3) THEN
140      CONTINUE
         CALL TASK23 (DATFIL, NEWREF, PRBFIL, VARFIT, VARRUN,
     1      BACKUP, EXTPRG)
C
         IF (BACKUP) THEN
            GO TO 120
         ELSEIF (EXTPRG) THEN
            GO TO 150
         ENDIF
C
         DATA ICONT/1/
         PROMP(1) = 'Do same task with this data'
         PROMP(2) = 'Go on to another task with same data'
         PROMP(3) = 'Go on to another task with new data'
         PROMP(4) = 'Exit'
		CALL CLEAR_TEXT ()
         CALL MENU (1, 4, ICONT, PROMP)
         IF (ICONT .EQ. 1) GO TO 140
      ELSEIF (ITASK .EQ. 4) THEN
         CALL TASK4 (VARFIT, VARRUN, BACKUP, EXTPRG)
      ELSEIF (ITASK .EQ. 5) THEN
         NEWREF   = .TRUE.
         CALL TASK5 (DATFIL, PRBFIL, VARFIT, VARRUN, BACKUP,
     1      EXTPRG)
      ELSEIF (ITASK .EQ. 6) THEN
         CALL TASK6 (PRBFIL, VARFIT, VARRUN, BACKUP, EXTPRG)
      ELSEIF (ITASK .EQ. 7) THEN
         CALL TASK7 (PRBFIL, VARFIT, VARRUN, BACKUP, EXTPRG)
      ENDIF
C
      IF (BACKUP) GO TO 120
C
C---  close output files
C
      CLOSE (25)
      CLOSE (26)
C
      IF (ITASK .EQ. 2 .OR. ITASK .EQ. 3) THEN
         IF (ICONT .EQ. 2) THEN
            GO TO 100
         ELSEIF (ICONT .EQ. 3) THEN
            CLOSE (10)
            GO TO 100
         ELSEIF (ICONT .EQ. 4) THEN
            GO TO 160
         ENDIF
      ENDIF
C
150   CONTINUE
      DATA CONT/ 'Y' /
      CALL YESNO ( 'Another problem' , CONT)
      IF (CONT .EQ. 'Y') GO TO 100
C
160   CONTINUE
      DATA ANS/ 'Y' /
      CALL YESNO ( 'Save variables on file for future use' ,
     1   ANS)
      IF (ANS .EQ. 'Y') CALL VAROUT (DATFIL, 22, PRBFIL,
     1   VARRUN, ITASK, RUNFIL, VARFIT)
C
      CALL QUITS (-1, 'Normal termination in CARSFT' )
      END
      BLOCKDATA
C
C______________________________________________________________________
C
      PARAMETER (NF = 30, NM = 4, NR = 17)
C
      COMMON /VERZUN/ UPDATE, CLOK, CDAT, CRAY
      CHARACTER*8 UPDATE, CLOK, CDAT, CRAY
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
      COMMON /NAMTSK/ NAMTSK(7)
      CHARACTER NAMTSK*50
      COMMON /PLTINI/ PLTINI
      LOGICAL PLTINI
C
      DATA UPDATE / '11-19-99' /
C
C   cex = 1/(k/hc) = 1/0.6952
C
      DATA AVAG/6.022E23/, CEX/1.4384/, PI/3.141592653/, VSTP/
     1   22414./, SQTPI/1.772453851/
C
      DATA NAMFIT/ 'Horizontal shift' , 'Vertical shift' ,
     1   'Wavenumber expansion' , 'Intensity expansion' ,
     2   'Probe line width (FWHM)' , 'Pump line width (FWHM)' ,
     3   'Temperature' , 'Line width multiplier' ,
     4   'Polarization angle' , '2nd-pump freq offset' , NM*
     5   ' ' , 'Delv4(1)' , 'Delv4(2)' , 'Delv4(3)' ,
     6   'Delv5(1)' , 'Delv5(2)' , 'Delv5(3)' ,
     7   'Exponential gap param. 1' , 'Exponential gap param. 2' ,
     8   'Exponential gap param. 3' , 'Exponential gap param. 4' ,
     9   'Pressure' , 'Narrowing parameter', '2nd-pump linewidth',
     1   'Spare', 'Spare', 'Spare' /
C
C   lengths of names in namfit
C   lengths of 11 through 10+nm are for 'xxxxx mole fraction'
C   in which xxxxx is the gas name.  these will be set in molpar
C
      DATA LNFIT / 16, 14, 20, 19, 23, 22, 11, 21, 18, 20, NM*22,
     1    6*8, 4*24, 8, 22, 19, 3*5/
C
      DATA NAMRUN/ 'Task number' , 'Beginning wavenumber' ,
     1   'Ending wavenumber' , 'Wavenumber range extension' ,
     2   'Buffer background susceptibility' , 'Lineshape model' ,
     3   'Angle between pump 1 and probe' , 'Ratioed spectrum' ,
     4   'Branch subtraction' , 'Reject gas' ,
     5   'Gain factor ratio (ref./res.)' , 'Convolution method' ,
     6   ' ' , 'Degree of saturation' ,
     7   'Angle between pumps 1 and 2' , 'Adaptive gridding' ,
     8   'Variance model' /
C
      DATA LNRUN / 11, 20, 17, 26, 32, 15, 30, 16, 18, 10, 29, 18,
     1   1, 20, 27, 17, 14 /
C
      DATA NAMTSK/ 'Convolved spectrum, no data' ,
     1   'Compare theory to data' ,
     2   'Least-squares fit of theory to data' ,
     3   'Theoretical susceptibility only' ,
     4   'Reference spectrum calculation for data file' ,
     5   'Reference spectrum, no data file' ,
     6   'Generate quickfitter libraries' /
C
C   pltini is true if plot package initialization has been done
C   (disspla version).
C
      DATA PLTINI/ .FALSE. /
C
      END
      SUBROUTINE ADDXOF (NTHE, CCONV2, CONV2, CHIOFF, ILIB)
C
C______________________________________________________________________
C
C   adds vertical offset to convolved spectrum.  adds square of offset
C   to square of spectrum, keeping sign of offset.  no offset is added
C   for task7 to generate library spectra for quickfitter
C______________________________________________________________________
C
      DIMENSION CCONV2(*), CONV2(*)
C
C---  don't add vertical shift if doing real part of susceptibility
C---  for task7.
C
      IF (ILIB .EQ. 2) RETURN
C
CSPON replace the first line inside the do loop with:
CSPON conv2(i) = cconv2(i)+chioff
CSPON remove the second line inside the do loop.
C
      DO 100 I = 1, NTHE
         CONV2(I) = CCONV2(I)+CHIOFF*ABS(CHIOFF)
         IF (CONV2(I) .LT. 0.) CONV2(I) = 0.
100   CONTINUE
C
      END
      SUBROUTINE AMPTST (AMPL, IDGAS, NT, NTHE, NSPEC, NW,
     1   PARPR, WAVEN, WTRAN, GAMMA, WIDMUL, AMPEPS, TWOPMP, FOFF)
C
C______________________________________________________________________
C
C  set criteria for keeping line or rejecting it as insignificant
C  criteria is (partial pressure * amplitude of line) >
C  eps * (maximum amplitude)
C______________________________________________________________________
C
      PARAMETER (NM =4)
      save eps

      DIMENSION AMPL(NW, NM, *), IDGAS(*), NT(NM, *), PARPR(*),
     1   WAVEN(*), WTRAN(NW, NM, 2), GAMMA(NW, NM, *)
      DOUBLE PRECISION WAVEN
      LOGICAL TWOPMP
      DATA EPS / 0.01 /
C
      AMPMAX = 0.
C
      DO 200 M = 1, NSPEC
         DO 200 IPMP = 1, 2
         DO 200 K = 1, NT(M, IPMP)
            IF (TWOPMP .AND. (IPMP .EQ. 2)) THEN
               WT = WTRAN(K, M, IPMP) + FOFF
            ELSE
               WT = WTRAN(K, M, IPMP)
            ENDIF
            IF ((WT .GE. WAVEN(1)) .AND. (WT .LE. WAVEN(NTHE))) THEN
               TES = ABS(AMPL(K, M, IPMP)*PARPR(IDGAS(M))/(WIDMUL*
     1               GAMMA(K, M, IPMP)))
               IF (TES .GT. AMPMAX) AMPMAX = TES
            ENDIF
200   CONTINUE
C
      AMPEPS = AMPMAX*EPS
      END
      SUBROUTINE BADREF (VARRUN, BACKUP)
C
C______________________________________________________________________
C
C  gets reference conditions if data file has errors in second
C  record
C    output parameters:
C      backup : flag to perform new calculation
C      varrun(1) : itask = 5 calculates new reference spectrum
C      varrun(8) : if -1, do unreferenced calculation
C______________________________________________________________________
C
      DIMENSION VARRUN(*)
      LOGICAL BACKUP
      CHARACTER PROMP(3)*80
C
      PRINT *,
     1' !! ERROR - REFERENCE TEMPERATURE OR PRESSURE READ FROM'
      PRINT *, '  FILE IS 0 !!'
      PROMP(1) = 'Calculate new reference spectrum for data file'
      PROMP(2) = 'Do unreferenced calculation'
      PROMP(3) = 'Stop'
      DATA INEXT / 1 /
      CALL MENU (1, 3, INEXT, PROMP)
C
      IF (INEXT .EQ. 1) THEN
         VARRUN(1) = 5
         BACKUP = .TRUE.
      ELSEIF (INEXT .EQ. 2) THEN
         VARRUN(8) = -1.
         BACKUP = .TRUE.
      ELSE
         CALL QUITS (-1, '!! ERROR EXIT FROM CARSFT !!' )
      ENDIF
      END
      SUBROUTINE CALC3 (CHIREF, IBEG, IDGAS, GAUSPR, NDAT, NPL,
     1   NSPEC, PRBFIL, REFTEM, REFP, DATPL, PRESS, TEMP, VARFIT,
     2   VARRUN, WDAT, WPL, WMIN, WMAX, CHNORM, CHIMAX, CHIT2,
     3   NTHE, WAVEN, DATFIL, GAMPR, GAMPU, FOFF, TOOMNY, SPCPRB,
     4   ENADPT, SHOTST, PARPR)
C
C______________________________________________________________________
C
C   does calculations for itask = 3 (least-squares fit of convolved
C   spectrum with data)
C     output variables:
C        chnorm : convolved spectrum, normalized to maximum, or
C                  to nonresonant array
C        chit2   : unconvolved spectrum (squared)
C        chimax  : max of chnorm
C        waven   : wavenumbers corresponding to chi arrays
C        nthe    : number points in waven
C______________________________________________________________________
C
      PARAMETER (NJ = 130, NF = 30, NV = 6, NM = 4, NTM =
     1   32*16384, NW = 1000, NV45 = 10, NP = 30, NPP = 31,
     2   NTM1 = NTM/2)
      DIMENSION DATPL(*), VARFIT(NF, *), CHNORM(*), IDGAS(*),
     1   WPL(*), CHIREF(*), CHIT2(*), VARRUN(*), WAVEN(*),
     2   WDAT(*), PARPR(*)
      DOUBLE PRECISION WPL, WAVEN, WDAT
      LOGICAL GAUSPR, TOOMNY, SPCPRB, ENADPT, SHOTST
      CHARACTER DATFIL*(*), PRBFIL*(*)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C---  local variables:
C
      DIMENSION AMPL(NW, NM, 2), GAMMA(NW, NM, 2), MAXQ(2, NM),
     1   MAXV5(0:NV45), WTRAN(NW, NM, 2), NQ(NW, 4, NM, 2), NT(NM, 2),
     2   POP(0:NV, 0:NJ, NM), POPN(0:NV, 0:NJ, NM),
     3   POPV45(0:NV45, 0:NV45), T11(NW, 2, NM, 2)
      INTEGER XMODEL(NW, NM, 2)
      CHARACTER*1 TTYPE(NW, NM, 2)
      CHARACTER ANS*1
      LOGICAL LOFFQ(2), DBLCON, SKIP, USEREF, LSRCNV, TWOPMP, ADAPTV,
     1   FORBRD, DOPPLR
      LOGICAL MENU_STOPFIT, STOPMSG
	COMMON /QWMENU/ MENU_STOPFIT, STOPMSG
      COMMON /CSTEP/ XVAR(NP), XMAX(NP), XMIN(NP), DELTX(NP),
     1   DELMN(NP), ERR(NP, NPP), FOBJ, NFS, NTRAC, MATRX,
     2   MASK(NP), NFMAX, NFLAT, JVARY, NXTRA, KFLAG, NOREP,
     3   KERFL, KW
      COMMON /PRTCHI/ ANS
      COMMON /TINSTR/ TINSTR(2*NTM)
      COMMON /RSTWV/ SKIP, FORBRD
      COMMON /WAVE/ WAVTMP(NTM1)
      DOUBLE PRECISION WAVTMP
      EXTERNAL FITRES
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
C
C---  variables calculated in inical are very weak function
C---  of temp, the only other fitting variable involved, and will
C---  be considered constant during fit iterations.
C
      CALL INICAL (IDGAS, METHPN, NSPEC, PRESS, TEMP, WMIN,
     1   WMAX, WTRAN, GAMMA, LOFFQ, NQ, MAXQ, MAXV4, MAXV5, NT,
     2   POP, POPN, POPV45, TTYPE, XMODEL, T11, VARFIT, WADD,
     3   WLIL, WBIG, FOFF, APSI, TWOPMP, PARPR)
C
      CALL CONWAV (GAMMA, NSPEC, NT, NTM, WMIN, WMAX, NTHE,
     1   WAVEN, GAMPR, GAMPU, GAUSPR, WLIL, WBIG, WLO, WHI,
     2   LSRCNV, TOOMNY, DBLCON, IDGAS, TEMP, DOPPLR, GAMMN)
      IF (TOOMNY) RETURN

      ADAPTV = .FALSE.
      IF (NTHE .GT. NTM1/2) CALL CHKADP (ENADPT, NT, NSPEC,
     1   NTHE1, WHI, WLO, WAVTMP, WTRAN, GAMMA, ADAPTV, DOPPLR,
     2   GAMMN, TWOPMP, FOFF)
      IF ((.NOT. GAUSPR) .AND. (.NOT. SPCPRB)) CALL PROBE
     1   (PRBFIL, NTHE, WLO, WHI, TINSTR)
      PRINT '(A/)' , ' . . . Entering least-squares fit'
C
C---  create common to communicate with function called
C---  by fitting routine
C
      CALL MAKCOM (ATHETA, CHIREF, CHNRPM, DBLCON, GAUSPR,
     1   WTRAN, IBEG, IDGAS, LOFFQ, NQ, NDAT, NPL, NSPEC, NTHE,
     2   NT, REFTEM, REFP, T11, TTYPE, USEREF, WDAT, XMODEL, DEGSAT,
     3   WADD, WLIL, WBIG, WLO, WHI, LSRCNV, APSI, TWOPMP, SPCPRB,
     4   ADAPTV, NTHE1, MAXQ, SHOTST)
C
C---  reset flag for calling CHIAMP and WAVMOD at least once in FITCAL
C
      SKIP = .FALSE.
C
C---  must recalculate resonant susceptibility for exponential gap
C---  model for n2 when partial pressure varies because foreign gas
C---  broadening is included
C
      FORBRD = .FALSE.
      IF (METHPN .EQ. 3 .AND. GASNAM(1) .EQ. 'N2') THEN
         DO 120 M = 1, NSPEC
            IF (GASNAM(IDGAS(M)) .EQ. 'CO2' .OR. GASNAM(IDGAS(M))
     1         .EQ. 'H2O') FORBRD = .TRUE.
120      CONTINUE
      ENDIF
C
C---  set up fit min., max. values and step sizes for fit variables
C
      CALL FITDEL (VARFIT, XVAR, XMIN, XMAX, DELTX, DELMN, MASK)
C
C---  use preset values for parameters in STEPIT
C---  for more statistical output (e.g. approximate standard
C---  deviations) in postmor., set matrx between 100 and 110 (try 102)
C
      NTRAC = 0
      MATRX = 0
      NFMAX = 1000
      NFLAT = 1
      KW = 33
      ANS = 'Y'
      CALL YESNO ( 'Write intermediate fit output to screen' , ANS)
C
C---  least-squares minimizer routine from ref 1.
C
	MENU_STOPFIT = .FALSE.
	STOPMSG = .TRUE.
      NFS = NF
      CALL STEPIT (FITRES)
C233   PRINT *, ' Enter number of fit variable for FIDO'
C      READ (5, '(I2)', ERR = 233) JX
C      CALL FIDO (FITRES, JX, 1., .05, SQRT(ERR(JX, JX)), 2, FDINT)
C
C  call fitcal again to get all variables as function of values
C  determined for varfit - set skip =.false. so wpl will be calculated
C  unconditionally
C
      SKIP   = .FALSE.
      CALL FITCAL (AMPL, ATHETA, CHIREF, CHNRPM, DBLCON, GAUSPR,
     1   WTRAN, IBEG, LOFFQ, NQ, IDGAS, NDAT, NSPEC, NTHE, NPL,
     2   NT, PRESS, USEREF, REFTEM, REFP, -1, T11, TINSTR, TTYPE,
     3   VARFIT, WAVEN, WDAT, XMODEL, CHNORM, CHIMAX, CHIT2,
     4   WPL, DEGSAT, WLIL, WBIG, WLO, WHI, LSRCNV, APSI, TWOPMP,
     5   SPCPRB, ADAPTV, WAVTMP, NTHE1, MAXQ)
C
      CALL FITPRN (VARFIT, KFLAG, NOREP, FOBJ, .FALSE., SHOTST)
C
C---  write transition parameters to output file if requested
C
      CALL RITPAR (IRIT1, DATFIL, PRBFIL, VARRUN, VARFIT)
      IF (IRIT1 .EQ. 1) CALL RRPAR (AMPL, GAMMA, IDGAS,
     1   NSPEC, NQ, NT, POP, POPN, TTYPE, WTRAN, VARFIT)
C
      END
      SUBROUTINE CALCON (APHI, ATHETA, CHIOFF, CHNRPM, DBLCON,
     1   DELV4, DELV5, GAMPR, GAMPU, GAMPU2, GAUSPR, HAVPRB, IDGAS,
     2   METHPN, NSPEC, PARKOS, PARPR, PRBFIL, PRESS, WIDMUL, TEMP,
     3   TOTNUM, WMIN, WMAX, FOFF, CCONV2, CHIT2, NTHE, WAVEN,
     4   DATFIL, VARFIT, VARRUN, DEGSAT, WADD, BETA, APSI, TOOMNY,
     5   SPCPRB, ILIB, ENADPT)
C
C______________________________________________________________________
C
C  does basic calculation of convolved and theoretical spectra
C    output parameters:
C
C        chit2  : theoretical spectrum before convolution (squared)
C        cconv2 : convolved spectrum (squared)
C        nthe   : number points in arrays
C        waven  : wavenumbers associated with chi arrays
C______________________________________________________________________
C
      PARAMETER (NJ = 130, NM = 4, NTM = 32*16384, NV = 6, NW =
     1   1000, NV45 = 10, NR = 17, NF = 30, NTM1 = NTM/2)

      save nt, wtran, gamma, nq, maxq, maxv4, maxv5, wbig,
     1   wlil, wlo, whi, pop, popn, popv45, ttype, xmodel, t11, loffq,
     2   twopmp, lsrcnv, dopplr, gammn

      DIMENSION DELV4(0:*), DELV5(0:*), PARKOS(*), PARPR(*),
     1   CCONV2(*), CHIT2(*), VARFIT(NF, *), VARRUN(*), WAVEN(*),
     2   IDGAS(*)
      LOGICAL DBLCON, GAUSPR, HAVPRB, TOOMNY, SPCPRB, ENADPT
      CHARACTER DATFIL*(*), PRBFIL*(*)
      COMMON /TINSTR/ TINSTR(2*NTM)
      DOUBLE PRECISION WAVEN
C
C---  local variables:
C
      DIMENSION AMPL(NW, NM, 2), WTRAN(NW, NM, 2),
     1   GAMMA(NW, NM, 2), NQ(NW, 4, NM, 2), MAXQ(2, NM),
     2   MAXV5(0:NV45), NT(NM, 2), POP(0:NV, 0:NJ, NM), POPN(0:NV,
     3   0:NJ, NM), POPV45(0:NV45, 0:NV45), T11(NW, 2, NM, 2),
     4   WAVTMP(NTM1)
      DOUBLE PRECISION WAVTMP
C
C---  common shared with subroutines fitcal and probe
C
      COMMON /COMM3/ CHIRL(NTM, 2), CHIIM(NTM, 2), CHTMPR(NTM1, 2),
     1   CHTMPI(NTM1, 2), CHTMPSQ(NTM1)
      COMPLEX  AMPKOS(NW)
      INTEGER XMODEL(NW, NM, 2)
      CHARACTER*1 TTYPE(NW, NM, 2)
      LOGICAL LOFFQ(2), LSRCNV, TWOPMP, ADAPTV, DOPPLR
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      IF(.NOT. HAVPRB) THEN
C
C             . . . havprb functioning in this case to indicate
C                   a reference calculation for task 1. no need
C                   to call inical again in this case, since
C                   resonant calculation results are still good
C
         CALL INICAL (IDGAS, METHPN, NSPEC, PRESS, TEMP, WMIN,
     1      WMAX, WTRAN, GAMMA, LOFFQ, NQ, MAXQ, MAXV4, MAXV5,
     2      NT, POP, POPN, POPV45, TTYPE, XMODEL, T11, VARFIT,
     3      WADD, WLIL, WBIG, FOFF, APSI, TWOPMP, PARPR)
      ENDIF
C
      CALL CHIAMP (APHI, ATHETA, NT, IDGAS, LOFFQ, NQ, NSPEC,
     1   POP, T11, TEMP, TOTNUM, XMODEL, APSI, TWOPMP, AMPKOS,
     2   MAXQ, AMPL)
C
      ADAPTV = .FALSE.

      IF (.NOT. HAVPRB) THEN
         CALL CONWAV (GAMMA, NSPEC, NT, NTM, WMIN, WMAX, NTHE,
     1      WAVEN, GAMPR, GAMPU, GAUSPR, WLIL, WBIG, WLO, WHI,
     2      LSRCNV, TOOMNY, DBLCON, IDGAS, TEMP, DOPPLR, GAMMN)
         IF (TOOMNY) RETURN
         IF (NTHE .GT. NTM1/2) CALL CHKADP (ENADPT, NT, NSPEC,
     1      NTHE1, WHI, WLO, WAVTMP, WTRAN, GAMMA, ADAPTV,
     2      DOPPLR, GAMMN, TWOPMP, FOFF)
         IF ((.NOT. GAUSPR) .AND. (.NOT. SPCPRB)) CALL PROBE
     1      (PRBFIL, NTHE, WLO, WHI, TINSTR)
      ENDIF
C
      IF (ADAPTV) THEN
C
C---  generate chit2 on adaptive grid first
C
         CALL CHICAL (AMPKOS, AMPL, APHI, ATHETA, CHNRPM, DELV4,
     1      DELV5, GAMMA, IDGAS, .TRUE., NT, NQ, MAXQ, MAXV4, MAXV5,
     2      NSPEC, NTHE1, PARKOS, PARPR, POP, POPN, POPV45, PRESS,
     3      TEMP, TOTNUM, WAVTMP, WIDMUL, WMIN, WTRAN, XMODEL,
     4      FOFF, DEGSAT, BETA, APSI, TWOPMP, CHTMPI,
     5      CHTMPR, CHTMPSQ, NTM1, ILIB)
C
C---  interpolate to wavenumber array waven
C
         CALL SETWVE (CHTMPI, CHTMPR, CHTMPSQ, NTHE, NTHE1, WAVTMP,
     1   WAVEN, CHIIM, CHIRL, CHIT2)
      ELSE
         CALL CHICAL (AMPKOS, AMPL, APHI, ATHETA, CHNRPM, DELV4,
     1      DELV5, GAMMA, IDGAS, .TRUE., NT, NQ, MAXQ, MAXV4, MAXV5,
     2      NSPEC, NTHE, PARKOS, PARPR, POP, POPN, POPV45, PRESS,
     3      TEMP, TOTNUM, WAVEN, WIDMUL, WMIN, WTRAN, XMODEL,
     4      FOFF, DEGSAT, BETA, APSI, TWOPMP, CHIIM, CHIRL, CHIT2,
     5      NTM, ILIB)

      ENDIF
C---  cpu timing marker
C
C      CALL CPUTIM (TIME, 'CPU time since login after chical is: ')
C
      CALL CNV2PMP (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG,
     1   WLO, WHI, GAMPR, GAMPU, GAMPU2, GAUSPR, NTHE, TINSTR, LSRCNV,
     2   SPCPRB, CCONV2, ILIB, TWOPMP)
           
      CALL ADDXOF (NTHE, CCONV2, CCONV2, CHIOFF, ILIB)
C
C---  cpu timing marker
C
C      CALL CPUTIM (TIME, 'CPU time since login after addxof is: ')
C
C---  write transition parameters to output file if requested
C
      IF (.NOT. HAVPRB) THEN
         CALL RITPAR (IRIT1, DATFIL, PRBFIL, VARRUN, VARFIT)
         IF (IRIT1 .EQ. 1) CALL RRPAR (AMPL, GAMMA, IDGAS,
     1      NSPEC, NQ, NT, POP, POPN, TTYPE, WTRAN, VARFIT)
      ENDIF
C
      END
      SUBROUTINE CGAUSS (WLO, WHI, GAM, NTHE, FINSTR)
C
C______________________________________________________________________
C
C  creates transformed laser convolution curve (gaussian) with
C  right half at beginning of interval, and left half at end.
C    input parameters:
C      wlo, whi : limits of wavenumber calculation range
C      delw : total width of wavenumber interval
C      gam : width of gaussian before transform (fwhm)
C      nthe : number points in curve of interest (finstr will
C             be of length 2*nthe)
C    output parameters:
C      finstr : convolution curve
C______________________________________________________________________
C
	 SAVE EPS
      DIMENSION FINSTR(*)
      DOUBLE PRECISION DELW, EXPNT
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
C
C    the gamma used in the gaussian function is half width
C    at height 1/e, which is 0.6005612 * (full width at half
C    max, the input gamma)
C
      DELW = WHI-WLO
      GAM1OE = 0.6005612*GAM
      EXPNT  = GAM1OE*PI*(NTHE-2)/(2*(NTHE-1)*DELW)
      DATA EPS/1.E-10/
C
      FINSTR(1) = 1.0
C
      DO 160 I = 2, NTHE
         FINSTR(I) = EXP(-((I-1)*EXPNT)**2)
         IRIGHT = 2*NTHE-I+2
         FINSTR(IRIGHT) = FINSTR(I)
         IF (FINSTR(I) .LT. EPS) GO TO 164
160   CONTINUE
C
C      . . . gaussian spanned the interval - only need to set
C            one point to zero
C
      FINSTR(NTHE+1) = 0.
C
      RETURN
164   CONTINUE
C
C     . . . fill remaining interval with zeroes
C
      IEXIT = I
C
      DO 165 I = IEXIT+1, IRIGHT-1
         FINSTR(I) = 0.
165   CONTINUE
C
      END
      SUBROUTINE CHIAMP (APHI, ATHETA, NT, IDGAS, LOFFQ, NQ,
     1   NSPEC, POP, T11, TEMP, TOTNUM, XMODEL, APSI, TWOPMP,
     2   AMPKOS, MAXQ, AMPL)
C
C______________________________________________________________________
C
C  calculates susc. peak amplitudes for all the lines included
C  in the fit. (ampl)
C______________________________________________________________________
C
      PARAMETER (NJ = 130, NM = 4, NV = 6, NW = 1000)
      DIMENSION AMPL(NW, NM, *), IDGAS(*), NT(NM, 2), NQ(NW, 4, NM, 2),
     1   POP(0:NV, 0:NJ, *), T11(NW, 2, NM, 2), MAXQ(2, *)
      COMPLEX AMPKOS(*)
      INTEGER XMODEL(NW, NM, 2)
      LOGICAL LOFFQ(2), TWOPMP
C
      COMMON /G0V/ G0V(0:NV, NM)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C  local variables:
C
      INTEGER V, VG, VU
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C.... susceptibility in units of 1.0e-18.
C
      IF (TWOPMP) THEN
         COS1 = COS(APHI)
         COS2 = COS(APSI-ATHETA)
         COS3 = COS(APSI-APHI)
         COS4 = COS(ATHETA)
         COS5 = COS(APHI-ATHETA)
         COS6 = COS(APSI)
      ELSE
         SINTH = SIN(ATHETA)
         COSTH = COS(ATHETA)
         SINPHI = SIN(APHI)
         COSPHI = COS(APHI)
      ENDIF
C
      DO 200 M = 1, NSPEC
      DO 200 IPMP = 1, 2
         IDG = IDGAS(M)
         IF (GASNAM(IDG) .EQ. 'CO2') THEN
            DPOP = 1.
            SIG = 1.
C
            DO 50 K = 1, NT(M, IPMP)
               CALL GETCHI (TWOPMP, COS1, COS2, COS3, COS4, COS5,
     1            COS6, COSTH, SINTH, COSPHI, SINPHI, DPOP, SIG,
     2            T11, K, M, IPMP, AMPL(K, M, IPMP))
C
C---  comment out following line to remove co2 routine
C
               CALL CO2AMP (TOTNUM, K, MAXQ, NT, IPMP, AMPL)
50          CONTINUE
C
         ELSEIF (GASNAM(IDG) .EQ. 'H2O') THEN
            DPOP = 1.
            SIG = 1.
C
            DO 60 K = 1, NT(M, IPMP)
               CALL GETCHI (TWOPMP, COS1, COS2, COS3, COS4, COS5,
     1            COS6, COSTH, SINTH, COSPHI, SINPHI, DPOP, SIG,
     2            T11, K, M, IPMP, AMPL(K, M, IPMP))
C
C---  comment out following line to remove h2o routine
C
C               CALL H2OAMP (TOTNUM, K, AMPL)
60          CONTINUE
C
         ELSE
            KLIM = NT(M, IPMP)
            IF (LOFFQ(IPMP) .AND. GASNAM(IDG) .EQ. 'N2') KLIM =
     1         NT(M, IPMP)-1
C
            DO 100 K = 1, KLIM
               VG = NQ(K, 1, M, IPMP)
               VU = NQ(K, 2, M, IPMP)
               JG = NQ(K, 3, M, IPMP)
               JU = NQ(K, 4, M, IPMP)
C
CSPON replace the following line with:
CSPON dpop = pop(vg, jg, m)*float(2*jg+1)*totnum
CSPON if co2 subroutines are included, make same change in
CSPON subroutine co2amp in co2lib library.
C
               DPOP = (POP(VG, JG, M)-POP(VU, JU, M))*FLOAT(2*JG+
     1                1)*TOTNUM
               SIG  = VG*ABS(VU-VG)+1.
C
               CALL GETCHI (TWOPMP, COS1, COS2, COS3, COS4, COS5,
     1            COS6, COSTH, SINTH, COSPHI, SINPHI, DPOP, SIG,
     2            T11, K, M, IPMP, AMPL(K, M, IPMP))
C
               IF ((XMODEL(K, M, IPMP) .EQ. 4) .OR.
     1         (XMODEL(K, M, IPMP) .EQ. 5))
     1            THEN
C
C                . . . calculate amplitude vector for exponential
C                      gap model.
C
                  IF (JU .NE. JG .OR. DPOP .EQ. 0.) THEN
                     AMPKOS(K) = CMPLX(0., 0.)
                  ELSE
                     IF (AMPL(K, M, IPMP) .GE. 0.) AMPKOS(K) =
     1                  CMPLX(SQRT(AMPL(K, M, IPMP)/DPOP), 0.)
                     IF (AMPL(K, M, IPMP) .LT. 0.) AMPKOS(K) = CMPLX(0.,
     1                  SQRT(ABS(AMPL(K, M, IPMP)/DPOP)))
                  ENDIF
               ENDIF
100         CONTINUE
         ENDIF
C
         IF (LOFFQ(IPMP) .AND. GASNAM(IDG) .EQ. 'N2') THEN
C
C---  last transition represents off-resonant nitrogen q-branch:
C---  calculate population of ground-state band only.
C
            K = NT(M, IPMP)
            QVV = 0.
C
            DO 150 V = 0, 6
               T = EXP(-G0V(V, 1)*CEX/TEMP)
               QVV = QVV+T
150         CONTINUE
C
            DPOP = (1./QVV)*EXP(-G0V(0, 1)*CEX/TEMP)*TOTNUM
            SIG = 1.
            CALL GETCHI (TWOPMP, COS1, COS2, COS3, COS4, COS5,
     1         COS6, COSTH, SINTH, COSPHI, SINPHI, DPOP, SIG,
     2         T11, K, M, IPMP, AMPL(K, M, IPMP))
         ENDIF
200   CONTINUE
C
      END
      SUBROUTINE CHICAL (AMPKOS, AMPL, APHI, ATHETA, CHNRPM,
     1   DELV4, DELV5, GAMMA, IDGAS, NEWCHI, NT, NQ, MAXQ, MAXV4,
     2   MAXV5, NSPEC, NTHE, PARKOS, PARPR, POP, POPN, POPV45,
     3   PRESS, TEMP, TOTNUM, WAVEN, WIDMUL, WMIN, WTRAN,
     4   XMODEL, FOFF, DEGSAT, BETA, APSI, TWOPMP, CHIIM, CHIRL,
     5   CHIT2, ND, ILIB)
C
C______________________________________________________________________
C
C  calculate theoretical susceptibilities at wavenumbers waven
C    output variables:
C       chirl  :  real part of susceptibility
C       chiim  :  imaginary part of susceptibility
C       chit2  :  absolute value of susceptibility squared
C______________________________________________________________________
C
      PARAMETER (NJ = 130, NM = 4, NV = 6, NW = 1000, NV45 =
     1   10, NTM=32*16384)
      DIMENSION AMPL(NW, NM, *), DELV4(0:*), DELV5(0:*), GAMMA(NW, NM,
     1   *), IDGAS(*), NT(NM, *), NQ(NW, 4, NM, *), MAXQ(2, *),
     2   MAXV5(0:NV45), PARKOS(*), PARPR(*), POP(0:NV, 0:NJ,
     3   *), POPN(0:NV, 0:NJ, *), POPV45(0:NV45, 0:*),
     4   WAVEN(*), WTRAN(NW, NM, *), CHIRL(ND, *), CHIIM(ND, *),
     5   CHIT2(*)
      DOUBLE PRECISION WAVEN
      COMPLEX AMPKOS(*)
      INTEGER XMODEL(NW, NM, *)
      LOGICAL NEWCHI, TWOPMP
      COMMON /PARMOL/ PARMOL(12, NM)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      COMMON /CHINR/ CHINR(NM)
      COMMON /DFSPHI/ DFSPHI(NM)
      COMMON /FJ/ FJ(0:NV, 0:NJ, NM)
      COMMON /TERMM/ TERMM(0:NV, 0:NJ, NM)
C
C---  local variables:
C
      DIMENSION GAMMAQ(0:NJ, NM)
      COMMON /COMM4/ XRLSAV(NTM, NM, 2), XIMSAV(NTM, NM, 2)
      INTEGER V4, V5
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      IF (ILIB .EQ. 2 .OR. ILIB .EQ. 3) THEN
         YRN = 0.
      ELSE
         CALL NONRES (APHI, ATHETA, CHINR, CHNRPM, IDGAS, NSPEC,
     1      PARPR, TOTNUM, APSI, TWOPMP, YRN)
      ENDIF
C
      CALL AMPTST (AMPL, IDGAS, NT, NTHE, NSPEC, NW, PARPR,
     1   WAVEN, WTRAN, GAMMA, WIDMUL, AMPEPS, TWOPMP, FOFF)
C
C---  if doing rotational diffusion for any species, must form
C---  vector of q-branch line widths for all j:

C
      DO 80 M = 1, NSPEC
      DO 80 IPMP = 1, 2
         DO 70 K = 1, NT(M, IPMP)
            IF ((XMODEL(K, M, IPMP) .EQ. 2) .OR.
     1      (XMODEL(K, M, IPMP) .EQ. 3))
     1         THEN
               DO 60 J = 0, MAXQ(2, M)
                  CALL LINWID (J, PRESS, TEMP, 'Q' ,
     1               MAXQ, M, K, IDGAS, PARPR, NSPEC, GAMMAQ(J, M))
60             CONTINUE
               GO TO 80
            ENDIF
70       CONTINUE
80    CONTINUE
C
C---  loop on wavenumbers :
C---  wdf is the raman shift, waven(pump)-waven(probe).
C
      DO 900 IPMP = 1, 2
      DO 900 I = 1, NTHE
         WDF = WAVEN(I)-WAVEN(1)
         YRI = 0.
         YII = 0.
C
         DO 500 M = 1, NSPEC
            IDG = IDGAS(M)
            APPR = ABS(PARPR(IDG))
C
C---  calculate new resonant terms only for new phi, temperature,
C---  linewidth multiplier, or exponential gap parameters
C
            IF (NEWCHI) THEN
               XRLSAV(I, M, IPMP) = 0.
               XIMSAV(I, M, IPMP) = 0.
C
               DO 450 K = 1, NT(M, IPMP)
                  WJ = WTRAN(K, M, IPMP) - WAVEN(1)
                  IF ((TWOPMP) .AND. (IPMP .EQ. 2)) WJ = WJ + FOFF
                  GAMJ = WIDMUL*GAMMA(K, M, IPMP)
C
C---  use computed go to for speed
C
                  GO TO (421, 422, 422, 424, 424, 426, 427, 428,
     1               429, 430, 431, 432), XMODEL(K, M, IPMP)
C
421               CONTINUE
C
C                   . . . nomimal susceptibility model
C
                     C11 = WJ-WDF
                     C12 = AMPL(K, M, IPMP)/(4.*C11*C11+GAMJ*GAMJ)
                     XR = 2.*C11*C12
                     XI = GAMJ*C12
                     GO TO 440
422               CONTINUE
C
C                   . . . rotational diffusion pressure narrowing
C
                     CALL ROTDIF (AMPL(K, M, IPMP), DFSPHI(IDG),
     1                  GAMMAQ(0, M), GAMJ, K, NQ(1, 1, M, IPMP),
     2                  MAXQ(2, M), NV, POPN(0, 0, M), WIDMUL,
     3                  TERMM(0, 0, IDG), WDF, WJ, WAVEN,
     4                  XMODEL(K, M, IPMP), XI, XR)
                     GO TO 440
424               CONTINUE
C
C                   . . . exponential gap model of pressure narrowing
C
                     CALL KOSCHI (AMPKOS, FJ(0, 0, IDG), I, K,
     1                  MAXQ(1, M), NQ(1, 1, M, IPMP), PARKOS, POP(0, 0,
     2                  M), PRESS, TEMP, TOTNUM, WDF, WAVEN, FOFF,
     3                  WIDMUL, WTRAN(1, M, IPMP), XMODEL(K, M, IPMP),
     4                  TWOPMP, IPMP, IDGAS, PARPR, NSPEC, XI, XR)
                     GO TO 440
426               CONTINUE
C
C                   . . . voigt profile model
C
                     CMASS = PARMOL(10, IDG)
                     CALL VGTPFL (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS,
     1                  AMPL(K, M, IPMP), XR, XI)
                     GO TO 440
427               CONTINUE
C
C                   . . . galatry profile model
C
                     CMASS = PARMOL(10, IDG)
                     CALL GTYPFL (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS,
     1                  BETA, PRESS, AMPL(K, M, IPMP), XR, XI)
                     GO TO 440
428               CONTINUE
C
C                   . . . hard collision model
C
                     CMASS = PARMOL(10, IDG)
                     CALL HDCLML (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS,
     1                  BETA, PRESS, AMPL(K, M, IPMP), XR, XI)
                     GO TO 440
429               CONTINUE
C
C                   . . . cw saturated line model
C
                     DENOM = 4.*(WJ-WDF)*(WJ-WDF)+GAMJ*GAMJ*
     1                       (1.+DEGSAT)
                     XR = AMPL(K, M, IPMP)*2.*(WJ-WDF)/DENOM
                     XI = AMPL(K, M, IPMP)*GAMJ/DENOM
                     GO TO 440
430               CONTINUE
C
C                   . . . acetylene model
C
                     XR = 0.
                     XI = 0.
                     WDF = WDF+WAVEN(1)
                     WJ = WJ+WAVEN(1)
                     WDF2  = WDF**2
                     C0 = GAMJ*WDF
                     C02 = C0**2
C
                     DO 100 V4 = 0, MAXV4
C
                        DO 100 V5 = 0, MAXV5(V4)
                           WJC2 = WJ+V4*DELV4(V4)+V5*DELV5(V5)
                           T1 = WJC2**2-WDF2
                           C1 = POPV45(V4, V5)*AMPL(K, M, IPMP)*WJC2/
     1                          (T1**2+C02)
                           XR = XR+C1*T1
                           XI = XI+C1*C0
100                  CONTINUE
                     GO TO 440
431               CONTINUE
C
C                   . . . correlated hard collision model
C
                     CMASS = PARMOL(10, IDG)
                     CALL CRHCML (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS,
     1                  BETA, PRESS, PARKOS(1),
     2                  AMPL(K, M, IPMP), XR, XI)
                     GO TO 440
432               CONTINUE
C
C                   . . . co2 isotropic q-branch model
C
C---  comment out following line to remove co2 routine
C
                     CALL CO2SPC (DFSPHI(IDG), GAMJ, K, MAXQ,
     1                  WIDMUL, WDF, WAVEN, IPMP, XR, XI)
C
440               XRLSAV(I, M, IPMP) = XRLSAV(I, M, IPMP) + XR
                  XIMSAV(I, M, IPMP) = XIMSAV(I, M, IPMP) + XI
450               CONTINUE
C
            ENDIF
C
C---  partial pressure dependence of raman chi:
C
            YRI = YRI + XRLSAV(I, M, IPMP)*APPR
            YII = YII + XIMSAV(I, M, IPMP)*APPR
500         CONTINUE
C
C---  add background to real and imaginary parts, then get
C---  square of magnitude.  ilib = 1, 2 is for generating
C---  library spectra (see task7).
C
         IF (ILIB. EQ. 1) THEN
C
CSPON replace the following three lines with:
CSPON chirl(i) = 0.
CSPON chiim(i) = yii
CSPON chit2(i) = abs(chiim(i))
C
            CHIRL(I, IPMP) = YRI+YRN
            CHIIM(I, IPMP) = YII
C            CHIT2(I) = CHIRL(I)*CHIRL(I)+CHIIM(I)*CHIIM(I)
         ELSEIF (ILIB .EQ. 2) THEN
C
CSPON replace the following three lines with:
CSPON chirl(i) = 0.
CSPON chiim(i) = 0.
CSPON chit2(i) = 0.
C
            CHIRL(I, IPMP) = YRI
            CHIIM(I, IPMP) = 0.
C            CHIT2(I) = CHIRL(I)
         ELSEIF (ILIB .EQ. 3) THEN
C
CSPON replace the following three lines with:
CSPON chirl(i) = 0.
CSPON chiim(i) = yii
CSPON chit2(i) = abs(chiim(i))
C
            CHIRL(I, IPMP) = YRI
            CHIIM(I, IPMP) = YII
C            CHIT2(I) = CHIRL(I)*CHIRL(I)+CHIIM(I)*CHIIM(I)
         ENDIF
900   CONTINUE

C     Calculation of CHIT2 moved to CALCON.
     
C      OPEN(12, FILE='2PMP.SUS', STATUS='UNKNOWN')                         !TEST
C      DO 2000 I = 1, NTHE
C2000  WRITE(12,'(F12.4,1P,3G12.4)')
C     1   WAVEN(I), CHIRL(I, 1)**2 + CHIIM(I, 1)**2,
C     2   CHIRL(I, 2)**2 + CHIIM(I, 2)**2, CHIT2(I)
C      CLOSE(12)

      END
      SUBROUTINE CHKADP (ENADPT, NT, NSPEC, NTHE1, WHI, WLO, WAVTMP,
     1   WTRAN, GAMMA, ADAPTV, DOPPLR, GAMMN, TWOPMP, FOFF)
C
C______________________________________________________________________
C
C   checks to see if adaptive gridding is worthwhile, and generates
C   new wavenumber array if it is worthwhile
C     input parameters:
C       nt - number of transitions for each species
C       nspec - number of species
C       whi, wlo - wavenumber calculation limits
C       wtran - transition wavenumbers
C       gamma - transition line widths
C       gammn - minimum linewidth or Doppler width
C       dopplr - logical flag indicating if Doppler width was used
C                 for gammn
C       foff - 2nd pump frequency offset
C     output parameters:
C       wavtmp - wavenumber array from which ultimate wavenumber
C                array is interpolated
C       nthe1 - number of entries in wavtmp
C       adaptv - logical flag to do adaptive gridding
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NM = 4, NSTART = 2000, NTM = 32*16384,
     1   NTM1 = NTM/2)
      DIMENSION NT(NM, 2), WAVTMP(*), WTRAN(NW, NM, *), GAMMA(NW, NM, *)
      DOUBLE PRECISION WAVTMP
      LOGICAL ENADPT, ADAPTV, DOPPLR, TWOPMP
C
C  local variables:
C
      DIMENSION WAVE(NTM1)
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      IF (.NOT. ENADPT) THEN
         ADAPTV = .FALSE.
         RETURN
      ELSE
         ADAPTV = .TRUE.
      ENDIF
C
C---  generate evenly spaced points across calculation range.
C
      DW = (WHI-WLO)/FLOAT(NSTART-1)
C
      DO 200 I = 1, NSTART-1
         WAVE(I) = WLO+FLOAT(I-1)*DW
200   CONTINUE
C
      WAVE(NSTART) = WHI
C
C---  Add twenty-five points for each transition, out 3 linewidths.
C     Use collision width or Doppler width as appropriate.
C
      NTHE1 = NSTART
      DO 300 M = 1, NSPEC
      DO 300 IPMP = 1, 2
         IF (TWOPMP .AND. (IPMP .EQ. 2)) THEN
            WOFF = FOFF
         ELSE
            WOFF = 0.0
         ENDIF
         DO 300 K = 1, NT(M, IPMP)
            IF (DOPPLR) THEN
               WID = GAMMN
            ELSE
               WID = GAMMA(K, M, IPMP)
            ENDIF
            DO 300 I = -12, 12
               IF (WTRAN(K, M, IPMP) + WOFF .LT. WLO .OR.
     1            WTRAN(K, M, IPMP) + WOFF .GT. WHI) GO TO 300
               NTHE1 = NTHE1+1
               IF (NTHE1 .GT. NTM1) THEN
                  PRINT*,
     1            'Too many grid points for adaptive gridding: ', NTHE1
                  ADAPTV = .FALSE.
                  RETURN
               ENDIF
               WAVE(NTHE1) = WTRAN(K, M, IPMP)+ (FLOAT(I)/4.)*WID + WOFF
300   CONTINUE
C
      WRITE (*, '(A, I6, A)')
     1   ' . . . Using adaptive grid with ', NTHE1, ' points'
C
C---  sort wave
C
      CALL SSORT (WAVE, DUM, NTHE1, 1)
C
C---  convert to double precision
C
      DO 400 I = 1, NTHE1
         WAVTMP(I) = WAVE(I)
400   CONTINUE
C
      END
      
      SUBROUTINE CNV2PMP (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG,
     1   WLO, WHI, GAMPR, GAMPU, GAMPU2, GAUSPR, NTHE, TINSTR, LSRCNV,
     2   SPCPRB, CCONV2, ILIB, TWOPMP)
C
C______________________________________________________________________
C
C     handles convolutions for single or dual pump frequencies.
C     
      PARAMETER (NTM = 32*16384)

      DIMENSION CHIIM(NTM, *), CHIRL(NTM, *), CHIT2(*), CCONV2(*),
     1   TINSTR(*)
      LOGICAL DBLCON, GAUSPR, LSRCNV, SPCPRB, TWOPMP
      
C     local variables

      DIMENSION CNV1(2*NTM), CNV2(2*NTM)
      
      DO 100 I = 1, NTHE
100   CHIT2(I) = (CHIRL(I, 1))**2 + (CHIIM(I, 1))**2
 
      IF (.NOT. TWOPMP) THEN
      
C     One pump.
      
         CALL CNVOLV (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG, WLO,
     1   WHI, GAMPR, GAMPU, GAUSPR, NTHE, TINSTR, LSRCNV, SPCPRB,
     2   CCONV2, ILIB)
      ELSE
      
         print*, 'Starting convolution.'
      
C     Dual pumps.      

C     Make sure double convolution not performed.

         DBLCON = .FALSE.
         
C     Initialize scratch arrays.
         
         DO 200 I = 1, NTHE
C         CCONV2(I) = 0.0
         CNV1(I) = 0.0
200      CNV2(I) = 0.0
         
C     Do (probe/detector + pump 1) convolution for square of pump 1
C     susceptibility. Pump 1 is assumed single mode in the
C     cross-term convolution below, so use zero in place of
C     GAMPU when calling CNVOLV. Note that the square of pump 1
C     susceptibility is already in CHIT2 from above.
 
         CALL CNVOLV (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG, WLO,
     1   WHI, GAMPR, 0.0, GAUSPR, NTHE, TINSTR, LSRCNV, SPCPRB,
     2   CCONV2, ILIB)
     
C     Do (probe/detector + pump 2) convolution for square of pump 2
C     susceptibility.

         DO 300 I = 1, NTHE
300      CHIT2(I) = (CHIRL(I, 2))**2 + (CHIIM(I, 2))**2

         CALL CNVOLV (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG, WLO,
     1   WHI, GAMPR, GAMPU2, GAUSPR, NTHE, TINSTR, LSRCNV, SPCPRB,
     2   CNV1, ILIB)
      
         DO 400 I = 1, NTHE
400      CCONV2(I) = CCONV2(I) + CNV1(I)

C     Now do convolutions for cross term.
C     Begin with terms for pump 2.  We want only the pump 2 convolution
C     performed and not the probe/detector, so use GAUSPR = T and
C     GAMPR = 0.0

         DO 500 I = 1, NTHE         
500      CHIT2(I) = CHIRL(I, 2)         

         CALL CNVOLV (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG, WLO,
     1   WHI, 0.0, GAMPU2, .TRUE., NTHE, TINSTR, LSRCNV, SPCPRB,
     2   CNV1, ILIB)

         DO 600 I = 1, NTHE
600      CHIT2(I) = CHIIM(I, 2)         

         CALL CNVOLV (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG, WLO,
     1   WHI, 0.0, GAMPU2, .TRUE., NTHE, TINSTR, LSRCNV, SPCPRB,
     2   CNV2, ILIB)
     
C     Multiply these results by the corresponding pump 1 susceptibility
C     terms and perform probe/detector convolution. Use GAMPU = 0.0.

         DO 700 I = 1, NTHE
700      CHIT2(I) = CNV1(I) * CHIRL(I, 1) + CNV2(I) * CHIIM(I, 1)
               
         CALL CNVOLV (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG, WLO,
     1   WHI, GAMPR, 0.0, GAUSPR, NTHE, TINSTR, LSRCNV, SPCPRB,
     2   CNV1, ILIB)
     
         DO 800 I = 1, NTHE
800      CCONV2(I) = CCONV2(I) + 2.0 * CNV1(I)         

         print*, 'Ending convolution.'
      
         DO 900 I = 1, NTHE
900      CHIT2(I) = (CHIRL(I, 1) + CHIRL(I, 2))**2 +
     1       (CHIIM(I, 1) + CHIIM(I, 2))**2 

      ENDIF  

      RETURN
      END

      SUBROUTINE CNVOLV (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG,
     1   WLO, WHI, GAMPR, GAMPU, GAUSPR, NTHE, TINSTR, LSRCNV, SPCPRB,
     2   CCONV2, ILIB)
C
C______________________________________________________________________
C
C   performs convolution, either single with combined pump and
C   probe gaussian, or double, doing pump and probe lasers separately
C    input parameters:
C      chiim, chirl - arrays of imaginary, real parts of susceptibility
C      chit2  - imag**2 + real**2
C      dblcon - flag indicating whether to do double convolution
C      wlo, whi - limits of wavenumber calculation range
C      gampr, gampu  - fwhm of probe, pump gaussians
C      gauspr - (logical) true means use gaussian instrument function
C         for probe laser.  false means use input instrument function
C         tinstr
C      lsrcnv - (logical) true means convolve pump laser width with
C         probe instrument function (see subroutine conwav)
C      spcprb - (logical) true means convolve pump laser width with
C         probe instrument function calculated in subroutine makprb
C      nthe - number points in chi arrays
C      tinstr - input instrument function - assummed to be already
C         hartley transformed.  only used if gauspr is false
C    output parameters:
C      cconv2 - square of convolved chi
C______________________________________________________________________
C
      PARAMETER (NTM = 32*16384)

      DIMENSION CHIIM(NTM, *), CHIRL(NTM, *), CHIT2(*), CCONV2(*),
     1   TINSTR(*)
      LOGICAL DBLCON, GAUSPR, LSRCNV, SPCPRB
C
C---  local variables:
C
C     con1 and con2 are dummies used in calls to transform routine.
C     they take on several meanings, to save storage.
C
C---  common shared with subroutine chical in cray version
C
      COMMON /COMM2/ CON1(2*NTM), CON2(2*NTM), TPUMP(2*NTM),
     1   TPROBE(2*NTM)

C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C  (1) convolve square of theoretical chi with laser instrument
C       function to produce square of convolved spectrum cconv2
C
      IF (GAUSPR) THEN
C
C         . . . generate gaussian of combined pump and probe
C               for instrument function
C
         GAMC = SQRT(GAMPR**2+GAMPU**2)
         CALL CGAUSS (WLO, WHI, GAMC, NTHE, TPUMP)
         CALL CONFHT (CHIT2, .TRUE., .TRUE., NTHE, TPUMP, CCONV2)
         IF ( .NOT. DBLCON) RETURN
      ELSE
C
C         . . . use input instrument function read from file
C
         IF (LSRCNV) THEN
            CALL CGAUSS (WLO, WHI, GAMPU, NTHE, TPUMP)
C
C---  subroutine makprb is used for non-gaussian, but analytic
C---  probe instrument function
C
            IF (SPCPRB) CALL MAKPRB (GAMPR, NTHE, WLO, WHI, TINSTR)
            CALL CONFHT (TINSTR, .FALSE., .FALSE., NTHE, TPUMP,
     1         TPROBE)
            CALL CONFHT (CHIT2, .TRUE., .FALSE., NTHE, TPROBE,
     1         CCONV2)
         ELSE
            IF (SPCPRB) CALL MAKPRB (GAMPR, NTHE, WLO, WHI, TINSTR)
            CALL CONFHT (CHIT2, .TRUE., .FALSE., NTHE, TINSTR,
     1         CCONV2)
         ENDIF
         IF ( .NOT. DBLCON) RETURN
      ENDIF
C
C       . . . double convolution:
C
C  (2) convolve COMPLEX chi with pump gaussian (do real and
C       imaginary parts separately, then calculate absolute
C       value).  This is done only for single pump, so we use only the
C       IPMP = 1 parts of the CHIRL, CHIIM arrays.
C
      IF (GAUSPR) CALL CGAUSS (WLO, WHI, GAMPU, NTHE, TPUMP)
      IF (LSRCNV) THEN
         CALL CONFHT (CHIRL(1, 1), .TRUE., GAUSPR, NTHE, TPUMP, CON1)
         IF (ILIB .NE. 2) THEN
            CALL CONFHT (CHIIM(1, 1), .TRUE., GAUSPR, NTHE, TPUMP, CON2)
C
            DO 200 I = 1, NTHE
               CON1(I) = CON1(I)**2 + CON2(I)**2
200         CONTINUE
C
         ENDIF
      ELSE
         IF (ILIB .NE. 2) THEN
C
            DO 400 I = 1, NTHE
               CON1(I) = CHIRL(I, 1)**2 + CHIIM(I, 1)**2
400         CONTINUE
C
         ELSE
C
            DO 700 I = 1, NTHE
               CON1(I) = CHIRL(I, 1)
700         CONTINUE
C
         ENDIF
      ENDIF
C
C  (3) convolve result of step (2) with probe gaussian
C
      IF (GAUSPR) CALL CGAUSS (WLO, WHI, GAMPR, NTHE, TPROBE)
      IF (LSRCNV) THEN
         CALL CONFHT (CON1, .TRUE., GAUSPR, NTHE, TPROBE, CON2)
      ELSE
         CALL CONFHT (CON1, .TRUE., GAUSPR, NTHE, TINSTR, CON2)
      ENDIF
C
C  (4) average result of step (1) and result of step (3)
C      to produce square of convolved spectrum
C
      DO 300 I = 1, NTHE
         CCONV2(I) = 0.5*(CCONV2(I)+CON2(I))
300   CONTINUE
C
      END
      SUBROUTINE CONFHT (CHIT, TRFORM, EVENPR, NTHE, TINSTR, CCONV)
C
C______________________________________________________________________
C
C   performs convolution of theoretical susceptibility with
C   instrument line shape function
C      input parameters:
C        chit    : theoretical susceptibility to be convolved
C        trform   : logical flag to transform chit initially
C        nthe    : number points in chit, and in resulting convolved
C                  spectrum cconv
C        tinstr : the transformed instrument function
C        evenpr : do simplified convolution if probe instrument
C                 function even
C      output parameters:
C        cconv   : convolved spectrum - note:
C                  (must be dimensioned in calling program to 2*nthe,
C                  since is used as work space for double-length array)
C______________________________________________________________________
C
      DIMENSION CHIT(*), CCONV(*), TINSTR(*)
      LOGICAL TRFORM, EVENPR
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      NTRAN = NTHE*2
C
C---  initialize dummy array as double length of input array,
C---  with 0.'s as second half.
C
      IF (TRFORM) THEN
         DO 120 I = 1, NTHE
            CCONV(I) = CHIT(I)
120      CONTINUE
C
         DO 180 I = NTHE+1, NTRAN
            CCONV(I) = 0.
180      CONTINUE
C
C---  transform cconv before convolving.
C
         CALL FHT (CCONV, NTRAN)
      ELSE
C
         DO 100 I = 1, NTRAN
            CCONV(I) = CHIT(I)
100      CONTINUE
C
      ENDIF
C
C---  take product with transformed gaussian and transform
C---  again.  Hartley transform routine from refs 3 and 4.
C---  if probe instrument function is even, as with gaussian
C---  probe, do simplified convolution (see ref. 4).
C
      IF (EVENPR) THEN
C
         DO 200 I = 1, NTRAN
            CCONV(I) = CCONV(I)*TINSTR(I)
200      CONTINUE
C
      ELSE
         CCONV(1) = CCONV(1)*TINSTR(1)
C
         DO 300 I = 2, NTHE
            N2 = NTRAN+2-I
            TE = 0.5*(TINSTR(I)+TINSTR(N2))
            TO = 0.5*(TINSTR(I)-TINSTR(N2))
            CL = CCONV(I)
            CR = CCONV(N2)
            CCONV(I) = CL*TE+CR*TO
            CCONV(N2) = CR*TE-CL*TO
300      CONTINUE
C
         CCONV(NTHE+1) = CCONV(NTHE+1)*TINSTR(NTHE+1)
      ENDIF
C
C---  if trform is not true, want transformed convolution returned.
C---  otherwise, return convolution in frequency space.
C
      IF (TRFORM) CALL FHT (CCONV, NTRAN)
C
      END
      SUBROUTINE CONWAV (GAMMA, NSPEC, NT, NTM, WMIN, WMAX,
     1   NTHE, WAVEN, GAMPR, GAMPU, GAUSPR, WLIL, WBIG, WLO, WHI,
     2   LSRCNV, TOOMNY, DBLCON, IDGAS, TEMP, DOPPLR, GAMMN)
C
C______________________________________________________________________
C
C   prepares wavenumber array to be used in convolution
C    - called if itask = 1, 2, 3 or 7
C   input parameters:
C     ntm   : maximum number of grid points to be allowed
C     wmin   : lower limit of wavenumber which may be used for
C              interpolation later, so must be included in
C              theoretical calculation
C     wmax   : upper limit of wavenumber
C   output parameters:
C     nthe   : number points in waven
C     waven  : evenly spaced wavenumbers
C     wlo, whi  : limits of wavenumber calculation range
C     gammn  : minimum linewidth used to set wavenumber grid size
C     dopplr : logical flag indicating Doppler width used for gammn
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NM = 4)
      DIMENSION GAMMA(NW, NM, *), NT(NM, *), WAVEN(*), IDGAS(*)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      DOUBLE PRECISION WAVEN, DW
      LOGICAL GAUSPR, LSRCNV, TOOMNY, DBLCON, CO2CHK, DOPPLR
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL MINGAM (GAMMA, NSPEC, NT, TEMP, WLIL, WBIG,
     1   IDGAS, DOPPLR, GAMMN)

      IF (GAUSPR .AND. (.NOT. DBLCON)) THEN
         GAML = SQRT(GAMPR*GAMPR+GAMPU*GAMPU)
C
C---  gammin is smaller of minimum Raman width and laser width
C---  gammax is larger of minimum Raman width and laser width
C
         GAMMIN = MIN(GAMMN, GAML)
         GAMMAX = MAX(GAMMN, GAML)
      ELSE
         GAMMIN = MIN(GAMMN, GAMPU, GAMPR)
         GAMMAX = MAX(GAMMN, GAMPU, GAMPR)
      ENDIF
C
C---  increase range of waven by the larger of 2*gammax or 1/100 of
C---  the wavenumber range, on either side of user-requested
C---  values, up to wlil and wbig, to eliminate end effects on
C---  convolution
C
      DW1 = 2.*GAMMAX
      DW2 = 0.01*(WMAX-WMIN)
      DWA = MAX(DW1, DW2)
C
      WLO = MAX(WMIN-DWA, WLIL)
      WHI = MIN(WMAX+DWA, WBIG)
      NWAV = 5.*(WHI-WLO)/GAMMIN
C
C---  if pump laser width is much narrower than Raman width and also
C---  too narrow for calculation range, don't convolve pump with
C---  probe and use Raman width to determine grid size
C
CSPON for spontaneous raman spectroscopy, pump is not used, so make
CSPON pump laser width small enough to make lsrcnv true below and
CSPON convolve pump and probe together in a single convolution.
CSPON probe width should reflect overall system instrument function,
CSPON including laser linewidth and spectrometer response.  it is
CSPON essential that pump and probe NOT be convolved separately for
CSPON spontaneous raman spectroscopy.
C
      IF ((NWAV .GT. NTM) .AND. (GAMPU .LT. (GAMMN/5.))) THEN
         NWAV = 5.*(WHI-WLO)/MIN(GAMMN, GAMPR)
         LSRCNV = .FALSE.
         WRITE (*, '(1X/1X, A)')
     1   '!! PUMP WIDTH TOO SMALL - NOT INCLUDED IN CONVOLUTION !!'
      ELSE
         LSRCNV = .TRUE.
      ENDIF
C
C---  if co2 is one of the resonant species, give option to
C---  override nwav.
C
      CO2CHK = .FALSE.
      DO 100 M = 1, NSPEC
         IF (GASNAM(IDGAS(M)) .EQ. 'CO2') CO2CHK = .TRUE.
100   CONTINUE
C
150   CONTINUE
      IF (CO2CHK) THEN
         WRITE (*, '(1X/A, I10, 2A/2A/2A)')
     1      ' Presently, ', NWAV, ' grid points are called for. ',
     2      ' However, CO2', ' may require a finer grid',
     3      ' due to strong collisional narrowing.',
     4      ' Enter the number (>99) of grid points desired.',
     5      ' <unchanged>'
         READ (5, '(I10)', ERR = 150) NWAV1
         IF (NWAV1 .EQ. 0) THEN
            CONTINUE
         ELSEIF (NWAV1 .GE. 100) THEN
            NWAV = NWAV1
         ELSE
            GO TO 150
         ENDIF
      ENDIF
C
C---  get number of points for convolution based on nwav.  this will
C---  be next power of two above that resulting from using interval
C---  of 1/5 minimum line width.  then establish waven array
C
      IF (NWAV .GT. NTM) THEN
         PRINT '(/2A, I10, A)',
     1      ' !! SPECTRUM REQUIRES TOO MANY WAVENUMBER ',
     2      'GRID POINTS: ', NWAV, ' !!'
         TOOMNY = .TRUE.
         CALL PRESS_ENTER()
         RETURN
      ELSE
	 CALL NCONPT (NWAV, NTHE)
         PRINT '(/A, T54, I6/)' ,
     1      ' . . . Number of points to be used in convolution ='
     2      , NTHE
         TOOMNY = .FALSE.
      ENDIF
C
      DW = (WHI-WLO)/FLOAT(NTHE-1)
C
      DO 230 I = 1, NTHE
         WAVEN(I) = WLO+FLOAT(I-1)*DW
230   CONTINUE
C
      END
      SUBROUTINE CPF (X, Y, WR, WI)
C
C______________________________________________________________________
C
C  routine computes the real (wr) and imaginary (wi) parts of the
C  complex probability function w(z) = exp(-z**2)*erfc(-i*z) in the
C  upper half-plane z = x+i*y (i.e. for y >= 0).  maximum relative
C  error of wr is <2*10(-6), that of wi <5*10(-6).
C    input parameters:
C      x  : real part of z
C      y  : imaginary part of z
C    output parameters:
C      wr : real part of w(z)
C      wi : imaginary part of w(z)
C______________________________________________________________________
C
C  local variables:
C
      DIMENSION T(6),C(6),S(6)
      DATA (T(I),I=1,6)/.314240376,.947788391,1.59768264,2.27950708,
     1   3.02063703,3.8897249/,(C(I),I=1,6)/1.01172805,-.75197147,
     2   1.2557727E-2,1.00220082E-2,-2.42068135E-4,5.00848061E-7/,
     3   (S(I),I=1,6)/1.393237,.231152406,-.155351466,6.21836624E-3,
     4   9.19082986E-5,-6.27525958E-7/
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WR = 0.
      WI = 0.
      Y1 = Y+1.5
      Y2 = Y1*Y1
C
C---  if high relative accuracy in region II is not required, the
C---  following 19 lines may be deleted.
C
      IF ((Y.GT.0.85).OR.(ABS(X).LT.18.1*Y+1.65)) GOTO 2
C
C.... region II
C
      IF (ABS(X).LT.12.) WR = EXP(-X*X)
      Y3 = Y+3.
      DO 1 I = 1, 6
         R = X-T(I)
         R2 = R*R
         D = 1./(R2+Y2)
         D1 = Y1*D
         D2 = R*D
         WR = WR+Y*(C(I)*(R*D2-1.5*D1)+S(I)*Y3*D2)/(R2+2.25)
         R = X+T(I)
         R2 = R*R
         D = 1./(R2+Y2)
         D3 = Y1*D
         D4 = R*D
         WR = WR+Y*(C(I)*(R*D4-1.5*D3)-S(I)*Y3*D4)/(R2+2.25)
1        WI = WI+C(I)*(D2+D4)+S(I)*(D1-D3)
      RETURN
C
C---  region I
C
2     DO 3 I = 1, 6
         R = X-T(I)
         D = 1./(R*R+Y2)
         D1 = Y1*D
         D2 = R*D
         R = X+T(I)
         D = 1./(R*R+Y2)
         D3 = Y1*D
         D4 = R*D
         WR = WR+C(I)*(D1+D3)-S(I)*(D2-D4)
3        WI = WI+C(I)*(D2+D4)+S(I)*(D1-D3)
C
      RETURN
      END
      SUBROUTINE CRHCML (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS, BETA,
     1   PRESS, SHIFT, AMPL, XREAL, XIMAG)
C
C______________________________________________________________________
C
C  calculates hard collision model for susceptibility.
C  this model assumes dephasing and velocity-changling collisions
C  are correlated.
C   output parameters:
C      xreal : real part of susceptibility
C      ximag : imaginary part of susceptibility
C______________________________________________________________________
C
      DOUBLE PRECISION WAVEN(*)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WJ0 = WJ+WAVEN(1)
C
C---  constant = sqrt(2k/m0)/c.
C---  beta is total collision frequency (omega) at 1 atm.
C
      ALPHAD = 4.3014E-7*WJ0*SQRT(TEMP/CMASS)
      XH = -(WDF-WJ)/ALPHAD
      YH = GAMJ/(2.*ALPHAD)
      Z1 = PRESS*BETA/ALPHAD
C
      SH = PRESS*SHIFT/ALPHAD
C
C---  zh must be greater than both yh and sh.
C
      Z2 = MAX(YH, SH)
      IF (Z1 .GT. Z2) THEN
         ZH = Z1
      ELSE
         ZH = Z2
      ENDIF
C
      CALL CPF (XH+SH, ZH, WR, WI)
C
      W1 = 1.-SQTPI*((ZH-YH)*WR+SH*WI)
      W2 = SQTPI*((YH-ZH)*WI+SH*WR)
      DENOM = W1*W1+W2*W2
      HR = (WR*W1+WI*W2)/DENOM
      HI = (WI*W1-WR*W2)/DENOM
C
C---  lineshape function is not normalized to unit area.  area of
C---  lorentzian lineshape is pi/2, so correlated hard collision
C---  profile is corrected accordingly.
C
      AC = SQTPI/(2.*ALPHAD)
      XREAL = AMPL*HI*AC
      XIMAG = AMPL*HR*AC
C
      END
      SUBROUTINE DATG23 (DATFIL, NEWREF, USEREF, CHIDAT, CHIREF,
     1   ITITLE, NDAT, WDAT, REFTEM, REFP, VARRUN, BACKUP)
C
C______________________________________________________________________
C
C  reads data file for tasks 2 (convolved spectrum, comparison
C  with data) or 3 (least-squares fit of same)
C    input parameters:
C      datfil: name of data file
C      newref : indicates that new ref array has been computed,
C         and thus must read data file, even if same file
C      useref : if true, doing referenced calculation - read
C         reference array from data file.
C    output parameters:
C      chidat : intensity data
C      chiref : reference spectrum data
C      ititle : header line on data file
C      ndat   : number points in arrays
C      refp, reftem : reference pressure and temperature
C      wdat   : wavenumber data
C______________________________________________________________________
C
      PARAMETER (ND = 5000)
      DIMENSION WDAT(*), CHIDAT(*), CHIREF(*), VARRUN(*)
      DOUBLE PRECISION WDAT
      CHARACTER DATFIL*(*), ITITLE*(*)
      LOGICAL NEWREF, BACKUP, USEREF
C
C  local variables:
C
      DIMENSION DAT(10)
      CHARACTER REC2*80
      LOGICAL SAMFIL, USESAV
      SAVE USESAV
      DATA USESAV / .FALSE. /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL OPDATF (DATFIL, REC2, SAMFIL, ITITLE)
C
C  conditions which allow skipping read of data in file: (all
C  must exist) . .
C   (1) data file name is same as last read (samfil=.t.)
C   (2) chiref was read last time (usesav=.t.), or is not
C         needed (useref = .f.)
C   (3) chiref has not changed due to calculation in task5
C         (newref = .f.)
C
      IF (USEREF) THEN
C
C       . . . interpret 2nd record (rec2) for ref. temp and press
C
         IST = INDEX(REC2, '=' )+1
         IF (IST .EQ. 0) THEN
            CALL BADREF (VARRUN, BACKUP)
         ELSE
            ILEN = LENTH(REC2)
            CALL INTERP (0, REC2(IST:ILEN), 2, NFOUND, DAT,
     1      IER)
            IF (NFOUND .NE. 2) THEN
               CALL BADREF (VARRUN, BACKUP)
            ELSE
               REFTEM = DAT(1)
               REFP   = DAT(2)
               IF (REFTEM .EQ. 0. .OR. REFP .EQ. 0.) CALL
     1         BADREF (VARRUN, BACKUP)
            ENDIF
         ENDIF
      ENDIF
C
      IF (SAMFIL .AND. (USESAV .OR. .NOT.USEREF) .AND. (.NOT.
     1   NEWREF)) RETURN
      USESAV = USEREF
C
      IF ((USEREF) .AND. (.NOT. BACKUP)) THEN
C
C         . . . read wdat, chidat, and chiref, since doing
C               referenced calculation, and read reftem and
C               refp successfully
C
         DO 100 N = 1, ND+1
            READ (10, *, END = 350) WDAT(N), CHIDAT(N),
     1      CHIREF(N)
100      CONTINUE
         GO TO 400
      ELSE
C
C         . . . don't read chiref, since will calculate new
C               reference array or do unreferenced calculation
C
         DO 90 N = 1, ND+1
            READ (10, *, END = 350) WDAT(N), CHIDAT(N)
90       CONTINUE
         GO TO 400
      ENDIF
C
350   CONTINUE
      NDAT = N-1
      PRINT '(/A, I5, A/)' , ' . . . File contained' , NDAT,
     1   ' data points.'
      RETURN
400   CONTINUE
      PRINT '(A, I5)' ,
     1   ' !! END OF DATA FILE NOT REACHED - CODE DIMENSIONED FOR' ,
     2   ND, ' POINTS ONLY.  WILL PROCEED !!'
      NDAT = ND
      RETURN
C
      END
      SUBROUTINE DATG5 (DATFIL, CHIDAT, ITITLE, NDAT, WDAT)
C
C______________________________________________________________________
C
C  reads data file for itask= 5 (reference susceptibility calculation)
C    output parameters:
C      chidat : intensity data
C      chiref : new reference array for data file
C      ititle : header line on data file
C      ndat   : number points in arrays
C      wdat   : wavenumber data
C______________________________________________________________________
C
      PARAMETER (ND = 5000)
      DIMENSION WDAT(*), CHIDAT(*)
      DOUBLE PRECISION WDAT
      CHARACTER DATFIL*(*), ITITLE*(*)
C
C  local variables:
C
      LOGICAL SAMFIL
      CHARACTER REC2*80
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL OPDATF (DATFIL, REC2, SAMFIL, ITITLE)
C
C---  don't attempt to save time by avoiding file read for task5,
C---  since it clouds the logic, and saves little time
C
      DO 340 N = 1, ND+1
         READ (10, *, END = 350) WDAT(N), CHIDAT(N)
340   CONTINUE
      GO TO 400
C
350   CONTINUE
      NDAT = N-1
      PRINT '(/A, I5, A/)' , ' . . . File contained' , NDAT,
     1   ' data points.'
      RETURN
400   CONTINUE
      PRINT '(A, I5)' ,
     1' !! END OF DATA FILE NOT REACHED - CODE DIMENSIONED FOR'
     2, ND, ' POINTS ONLY.  WILL PROCEED !!'
      NDAT = ND
      RETURN
C
      END
      SUBROUTINE DATPR (CHIDAT, CHIREF, GAINF, NDAT, USEREF,
     1   WDAT, DATPL)
C
C______________________________________________________________________
C
C  processes input experimental data
C    output parameters:
C      datpl : intensity data with gain factor applied in referenced
C              case, or normalized to peak of 1.0 in unreferenced case.
C              also reversed according to wdat, if necessary
C      wdat  : wavenumber data reversed to increasing order, if
C              necessary
C      chiref : reference array data reversed, if necessary
C______________________________________________________________________
C
      DIMENSION CHIDAT(*), CHIREF(*), WDAT(*), DATPL(*)
      DOUBLE PRECISION WDAT
      LOGICAL USEREF
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C  insure that arrays are monotonically increasing in wavenumber
C
      IF (WDAT(1) .GT. WDAT(NDAT)) THEN
         CALL REVARD (WDAT, NDAT)
         CALL REVARR (CHIDAT, NDAT)
         IF (USEREF) CALL REVARR (CHIREF, NDAT)
      ENDIF
C
      IF (USEREF) THEN
         GFR = 1./GAINF
C
         DO 100 I = 1, NDAT
            DATPL(I) = CHIDAT(I)*GFR
100      CONTINUE
      ELSE
C
C          . . . normalize data to 1 if using unreferenced data.
C                after fitting, data will be renormalized to max.
C                of theoretical calculation
C
         CALL MNMAX (CHIDAT, 2, NDAT, 1, DUM, DATMAX)
         CALL DIVZER (DATMAX, 1, 'DATPR' , 'DATMAX' )
C
         DMR = 1./DATMAX
         DO 140 I = 1, NDAT
            DATPL(I) = CHIDAT(I)*DMR
140      CONTINUE
      ENDIF
C
      END
      SUBROUTINE DISFIT (GAUSPR, METHPN, VARFIT, ITASK, BACKUP,
     1   EXTPRG, FLPRNT)
C
C______________________________________________________________________
C
C  displays fitting variables
C  in VAX version, user can modify them.
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      DIMENSION VARFIT(*)
      LOGICAL BACKUP, GAUSPR, EXTPRG, FLPRNT
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
C
C  local variables:
C
      LOGICAL MODS, LPRIN
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      BACKUP = .FALSE.
      EXTPRG = .FALSE.
      CALL SWITCH (METHPN)
C
      IF (ITASK .EQ. 3) THEN
         CALL DISFT3 (GAUSPR, METHPN, BACKUP, EXTPRG, FLPRNT)
         RETURN
      ENDIF
C
100   CONTINUE
		CALL CLEAR_TEXT ()
         PRINT '(/12A1, A, 13A1)' , ( '_' , I = 1, 12),
     1      ' FITTING VARIABLES ', ( '_' , I = 1, 13)
      IF (.NOT. FLPRNT) THEN
         PRINT *, '(-1) Exit program'
         PRINT *, '( 0) Go back to previous menu'
      ENDIF
C
      DO 520 N = 1, NF
         IF (LPRIN(GAUSPR, METHPN, N, ITASK, VARFIT)) THEN
            IF (VARFIT(N) .NE. 0.) PRINT 1020, N, NAMFIT(N),
     1      VARFIT(N)
            IF (VARFIT(N) .EQ. 0.) PRINT 1030, N, NAMFIT(N),
     1      VARFIT(N)
         ENDIF
520   CONTINUE
C
      IF (.NOT. FLPRNT) PRINT '(44A1)' , ( '_' , I = 1, 44)
      IF (FLPRNT) RETURN
      CALL UMODF (GAUSPR, ITASK, METHPN, BACKUP, EXTPRG,
     1   MODS, VARFIT)
      IF (BACKUP .OR. EXTPRG) RETURN
      IF (MODS) GO TO 100
1020  FORMAT(' (' , I2, ') ' , A26, G12.6)
1030  FORMAT(' (' , I2, ') ' , A26, F8.0)
      END
      SUBROUTINE DISFT3 (GAUSPR, METHPN, BACKUP, EXTPRG, FLPRNT)
C
C______________________________________________________________________
C
C  displays fitting variables;
C  VAX version allows user to modify fitting variables
C  for task 3, least-squares fit
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      LOGICAL BACKUP, GAUSPR, EXTPRG, FLPRNT
      COMMON /PARAMS/ VARFIT(NF, 4)
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
C
C  local variables:
C
      CHARACTER AFIX*5
      LOGICAL MODS, LPRIN
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL SWITCH (METHPN)
C
100   CONTINUE
      CALL CLEAR_TEXT ()
      PRINT '(/30A1, A, 30A1)' , ( '_' , I = 1, 30),
     1   ' FITTING VARIABLES ' , ( '_' , I = 1, 30)
      PRINT '(32X, A)' ,
     1   '(A) NOMINAL  (B) MIN.   (C) MAX.   (D) FIX/FREE'
      IF (.NOT. FLPRNT) THEN
         PRINT *, '(-1) Exit program'
         PRINT *, '( 0) Go back to previous menu'
      ENDIF
      DO 520 N = 1, NF
         IF (LPRIN(GAUSPR, METHPN, N, 3, VARFIT)) THEN
            IF (VARFIT(N, 4) .LT. 0.) AFIX = 'FREE '
            IF (VARFIT(N, 4) .GT. 0.) AFIX = 'FIXED'
            PRINT 1020, N, NAMFIT(N), (VARFIT(N, I), I = 1, 3),
     1         AFIX
         ENDIF
520   CONTINUE
C
      IF (.NOT. FLPRNT) PRINT '(79A1)' , ( '_' , I = 1, 79)
      IF (FLPRNT) RETURN
      CALL UMODF3 (BACKUP, EXTPRG, GAUSPR, METHPN, MODS,
     1   VARFIT)
      IF (BACKUP .OR. EXTPRG) RETURN
      IF (MODS) GO TO 100
C
1020  FORMAT(' (' , I2, ') ' , A26, G12.6, 2G12.4, 2X, A5)
      END
      SUBROUTINE DISRUN (PRBFIL, VARRUN, BACKUP, FLPRNT)
C
C______________________________________________________________________
C
C  displays run control variables,
C  allows user to modify them in VAX version
C______________________________________________________________________
C
      PARAMETER (NM = 4, NF = 30, NR = 17)
      DIMENSION VARRUN(*)
      LOGICAL BACKUP, MODS, FLPRNT
      CHARACTER* (*)PRBFIL
C
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
      COMMON /NAMTSK/ NAMTSK(7)
      CHARACTER NAMTSK*50
C
C---  local variables:
C
      LOGICAL DBLCON, USEREF, GAUSPR, SPCPRB, ENADPT, SHOTST
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
50    CONTINUE
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
      BACKUP = .FALSE.
      IF (.NOT. FLPRNT) THEN
		CALL CLEAR_TEXT ()
         PRINT '(/2A)' ,
     1      ' _____________________ RUN CONTROL VARIABLES',
     2      ' _____________________'
         PRINT *, '( 0) Go back to previous menu'
      ENDIF
      PRINT '(1X, A, A)' , '( 1) Task : ' , NAMTSK(ITASK)
C
      DO 80 N = 2, 3
         IF (VARRUN(N) .NE. 0.) PRINT 1000, N, NAMRUN(N),
     1      VARRUN(N)
         IF (VARRUN(N) .EQ. 0.) PRINT 1005, N, NAMRUN(N),
     1      VARRUN(N)
80    CONTINUE
C
      N = 4
         IF (VARRUN(N) .NE. 0.) PRINT 1000, N, NAMRUN(N),
     1      VARRUN(N)
         IF (VARRUN(N) .EQ. 0.) PRINT 1010, N, NAMRUN(N),
     1      'default'
C
      N = 5
         IF (VARRUN(N) .NE. 0.) PRINT 1000, N, NAMRUN(N),
     1      VARRUN(N)
         IF (VARRUN(N) .EQ. 0.) PRINT 1005, N, NAMRUN(N),
     1      VARRUN(N)
C
      N = 6
      IF (METHPN .EQ. 2) THEN
         PRINT 1015, N,
     1   'Rotational diffusion model for collisional narrowing'
      ELSEIF (METHPN .EQ. 3) THEN
         PRINT 1015, N,
     1   'Exponential gap model for collisional narrowing'
      ELSEIF (METHPN .EQ. 4) THEN
         PRINT 1015, N,
     1   'Voigt profile for Doppler broadening'
      ELSEIF (METHPN .EQ. 5) THEN
         PRINT 1015, N,
     1   'CW saturated line model'
      ELSEIF (METHPN .EQ. 6) THEN
         PRINT 1015, N,
     1   'Galatry profile for Doppler broadening'
      ELSEIF (METHPN .EQ. 7) THEN
         PRINT 1015, N,
     1   'Hard collision profile for Doppler broadening'
      ELSEIF (METHPN .EQ. 8) THEN
         PRINT 1015, N,
     1   'Correlated hard collision profile for Doppler broadening'
      ELSE
         PRINT 1015, N, 'Isolated line model'
      ENDIF
C
      N = 7
      IF (ATHETA .NE. 0.) PRINT 1000, N, NAMRUN(N), VARRUN(N)
      IF (ATHETA .EQ. 0.) PRINT 1005, N, NAMRUN(N), VARRUN(N)
C
      IF (ITASK .LE. 3 .OR. ITASK .EQ. 7) THEN
         N = 8
         IF (USEREF) PRINT 1015, N,
     1   'Ratio to non-resonant spectrum'
         IF ( .NOT. USEREF) PRINT 1015, N,
     1   'Non-ratioed spectrum'
      ENDIF
C
      IF ((USEREF .AND. ((ITASK .EQ. 1) .OR. (ITASK .EQ. 7))) .OR.
     1   ITASK .EQ. 5 .OR. ITASK .EQ. 6) THEN
         N = 9
         IF (NBRNCH .EQ. 1) THEN
            PRINT 1015, N, 'O-branch subtraction'
         ELSEIF (NBRNCH .EQ. 2) THEN
            PRINT 1015, N, 'Q-branch subtraction'
            PRINT '(A, A, A)' , ' (10) Reject ' , GASNAM(NREJEC),
     1         ' in ref. array calc.'
         ELSE
            PRINT 1015, N, 'Neither O nor Q-branch subtraction'
         ENDIF
      ENDIF
C
C---  gain factor (only with data)
C
      IF (ITASK .EQ. 2 .OR. ITASK .EQ. 3) THEN
         N = 11
         PRINT 1000, N, NAMRUN(N), GAINF
      ENDIF
C
      IF (ITASK .NE. 4) THEN
         N = 12
         IF ( .NOT. DBLCON) PRINT 1015, N,
     1      'Single convolution with combined pump & probe'
         IF (DBLCON) PRINT 1015, N,
     1      'Convolve pump & probe separately'
         N = 13
         PRINT 1011, N, 'Probe instrument function file  ' ,
     1   PRBFIL
      ENDIF
C
      IF (METHPN .EQ. 5) THEN
         N = 14
         IF (VARRUN(N) .NE. 0.) PRINT 1000, N, NAMRUN(N),
     1      VARRUN(N)
         IF (VARRUN(N) .EQ. 0.) PRINT 1005, N, NAMRUN(N),
     1      VARRUN(N)
      ENDIF
C
      N = 15
         IF (VARRUN(N) .NE. 0.) PRINT 1000, N, NAMRUN(N),
     1      VARRUN(N)
         IF (VARRUN(N) .EQ. 0.) PRINT 1005, N, NAMRUN(N),
     1      VARRUN(N)
C
      N = 16
         IF (ENADPT) PRINT 1015, N,
     1      'Adaptive gridding enabled'
         IF ( .NOT. ENADPT) PRINT 1015, N,
     1      'Adaptive gridding disabled'
C
      N = 17
          IF (ITASK .EQ. 3) THEN
             IF (SHOTST) PRINT 1015, N,
     1          'Variance equals unity-normalized data in chi-squared'
             IF (.NOT. SHOTST) PRINT 1015, N,
     1          'No statistical weighting in chi-squared'
          ENDIF
      IF (FLPRNT) RETURN
      PRINT *,
     1   '_________________________________',
     2   '________________________________'
      PRINT *, ' '
      CALL UMODR (ITASK, BACKUP, MODS, PRBFIL, VARRUN)
      IF (BACKUP) RETURN
      IF (MODS) GO TO 50
C---  diagnostics
C
      IF (VARRUN(2) .GE. VARRUN(3)) THEN
         PRINT *,
     1   ' !! ERROR - ENDING WAVENUMBER MUST BE > BEGINNING !!'
         GO TO 50
      ENDIF
C
      IF (((VARRUN(1) .EQ. 5 .OR. VARRUN(1) .EQ. 6) .OR. USEREF)
     1   .AND. (VARRUN(7) .EQ. 0.)) THEN
         PRINT *,
     1   ' !! ERROR - ANGLE BETWEEN PUMP AND PROBE CANNOT = O'
         PRINT *,
     1   ' FOR CALCULATING REFERENCE OR RATIOED SPECTRUM !!'
         GO TO 50
      ENDIF
C
      IF (VARRUN(14) .LT. 0.) THEN
         PRINT *,
     1   ' !! ERROR - DEGREE OF SATURATION MUST BE >= 0 !!'
         GO TO 50
      ENDIF
C
      IF (VARRUN(1) .EQ. 7 .AND. USEREF) THEN
         PRINT *,
     1   ' !! ERROR - CANNOT RATIO TO NONRESONANT BACKGROUND WHEN'
         PRINT *, ' GENERATING QUICKFITTER LIBRARY SPECTRA !!'
         GO TO 50
       ENDIF
C
1000  FORMAT(' (' , I2, ') ' , A, T50, G14.6)
1005  FORMAT(' (' , I2, ') ' , A, T50, F14.0)
1010  FORMAT(' (' , I2, ') ' , A, T50, A)
1011  FORMAT(' (' , I2, ') ' , A, T40, A)
1015  FORMAT(' (' , I2, ') ' , A)
      END
      SUBROUTINE DIVZER (VAR, IERR, ROUTIN, VARBLE)
C
C______________________________________________________________________
C
C   diagnoses potential divisions by zero, and terminates code
C     input parameters:
C       var    : variable to be checked (error if =0.)
C       ierr   : flag indicating diagnostic to be issued
C       routin : name of routine from which called
C       varble : name of variable
C     output parameters: (none)
C______________________________________________________________________
C
      CHARACTER ROUTIN*(*), VARBLE*(*)
C
      CHARACTER*80 MESSAG(3)
      DATA MESSAG/ ' data may be all 0.' ,
     1   ' minimum line width = 0.' ,
     2   ' calculated susceptibility all .le. 0.' /
C
      IF (VAR .GT. 0.) RETURN
      PRINT *, ' !! ERROR - IMMINENT DIVISION BY ZERO !!'
      PRINT '(4A)' , '   VARIABLE ' , VARBLE,
     1   ' IN ROUTINE ' , ROUTIN
      CALL QUITS (-1, MESSAG(IERR))
      END
      SUBROUTINE FITCAL (AMPL, ATHETA, CHIREF, CHNRPM, DBLCON, GAUSPR,
     1   WTRAN, IBEG, LOFFQ, NQ, IDGAS, NDAT, NSPEC, NTHE, NPL, NT,
     2   PRESS, USEREF, REFTEM, REFP, JVARY, T11, TINSTR, TTYPE,
     3   VARFIT, WAVEN, WDAT, XMODEL, CHNORM, CHIMAX, CHIT2, WPL,
     4   DEGSAT, WLIL, WBIG, WLO, WHI, LSRCNV, APSI, TWOPMP, SPCPRB,
     5   ADAPTV, WAVTMP, NTHE1, MAXQ)
C
C______________________________________________________________________
C
C  main calculational module used to generate calculated susceptibility
C  to be compared to data if doing least-squares fit
C    output variables:
C    ------ ----------
C   (from norml) : chnorm, chimax
C   (from wavmod) :  wpl . . wdat with offset and expansion
C   chit2 : theoretical susceptibility (squared), in case wanted
C            for output
C   waven : wavenumbers corresponding to chi arrays
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NF = 30, NM = 4)
      DIMENSION AMPL(NW, NM, *), CHNORM(*), WTRAN(NW, NM, *), NT(NM, *),
     1   NQ(NW, 4, NM, *), CHIREF(*), T11(NW, 2, NM, *), TINSTR(*),
     2   VARFIT(NF, *), WAVEN(*), WDAT(*), WPL(*), CHIT2(*),
     3   WAVTMP(*), IDGAS(*), MAXQ(2, *)
      DOUBLE PRECISION WAVEN, WDAT, WPL, WAVTMP
      LOGICAL DBLCON, GAUSPR, LOFFQ(2), USEREF, LSRCNV, TWOPMP, SPCPRB,
     1   ADAPTV
      INTEGER XMODEL(NW, NM, *)
      CHARACTER*(*) TTYPE(NW, NM, *)
C
C  local variables: (save all local variables to save time
C  on subsequent calls when fitting)
C
      SAVE
      PARAMETER (NV = 6, NJ = 130, NTM = 32*16384, NV45 = 10,
     1   NTM1 = NTM/2)
      DIMENSION DELV4(0:NV45), DELV5(0:NV45), CHTMPR(NTM1, 2),
     1   CHTMPI(NTM1, 2), CHTMPSQ(NTM1), GAMMA(NW, NM, 2),
     2   MAXV5(0:NV45), PARKOS(4), PARPR(NM), POP(0:NV, 0:NJ, NM),
     3   POPN(0:NV, 0:NJ, NM), POPV45(0:NV45, 0:NV45), PARSAV(NM)
      COMMON /DARR1/ CCONV2(2*NTM), CONV2(NTM)
      COMMON /DARR2/ CHIIM(NTM, 2), CHIRL(NTM, 2)
      COMMON /RSTWV/ SKIP, FORBRD
C
C---  common shared with subroutines calcon and probe
C
      COMMON /COMM3/ CHICON(2*NTM), CHISV(2*NTM), CHRFSV(5*NTM1)
      REAL KOSSAV(4)
      COMPLEX AMPKOS(NW)
      LOGICAL NEWTEM, NEWPHI, NEWWOF, NEWWEX, NEWWML, NEWFOF,
     1   NEWPAR, NEWKOS, NEWCHI, SKIP, NEWPRS, NEWBET, FORBRD
      DATA APHISV, TEMSAV, WEXPSV, WOFSAV, WMLDAV, FOFSAV,
     1   PARSAV, KOSSAV, PRSSAV, BETSAV / 16*-1.E30 /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL, TEMP,
     2   TOTNUM, WEXPND, WOFFS, FOFF, BETA)
C
C---  set logical variables to decide which routines need be
C---  called again.  Don't want to skip wavmod if skip is false
C---  since wpl is not a local variable,
C---  and could be left undefined on call from calc3
C
      NEWTEM = (TEMP .NE. TEMSAV)
      NEWWOF = (WOFFS .NE. WOFSAV)
      NEWWEX = (WEXPND .NE. WEXPSV)
      NEWPHI = (APHI .NE. APHISV)
      NEWWML = (WIDMUL .NE. WMLSAV)
      NEWFOF = (FOFF .NE. FOFSAV)
      NEWPAR = .FALSE.
      DO 600 M = 1, NSPEC
         IDG = IDGAS(M)
         NEWPAR = (NEWPAR .OR. (PARPR(IDG) .NE. PARSAV(IDG)))
600   CONTINUE
      NEWKOS = ((PARKOS(1) .NE. KOSSAV(1)) .OR. (PARKOS(2)
     1   .NE. KOSSAV(2)) .OR. (PARKOS(3) .NE. KOSSAV(3))
     2   .OR. (PARKOS(4) .NE. KOSSAV(4)))
      NEWPRS = (PRESS .NE. PRSSAV)
      NEWBET = (BETA .NE. BETSAV)
C
      IF (NEWTEM .OR. NEWPRS .OR. (.NOT. SKIP)) THEN
         CALL POPLAT (IDGAS, NSPEC, TEMP, MAXQ, MAXV4, MAXV5,
     1      NT, NQ, POP, POPN, POPV45)
         DO 100 M = 1, NSPEC
         DO 100 IPMP = 1, 2
            DO 100 K = 1, NT(M, IPMP)
               CALL LINWID (NQ(K, 3, M, IPMP), PRESS, TEMP,
     1            TTYPE(K, M, IPMP), MAXQ, M, K, IDGAS, PARPR, NSPEC,
     2            GAMMA(K, M, IPMP))
100      CONTINUE
         TEMSAV = TEMP
         PRSSAV = PRESS
      ENDIF
C
      IF ((JVARY .NE. 1) .AND. (JVARY .NE. 3)) THEN
         WOFSAV = WOFFS
         WEXPSV = WEXPND
      ELSE
         WOFDEL = WOFFS-WOFSAV
         WEXDEL = WEXPND-WEXPSV
      ENDIF
C
      IF (NEWWOF .OR. NEWWEX .OR. (.NOT. SKIP)) CALL
     1   WAVMOD (IBEG, NPL, WDAT, WEXPND, WOFFS, WPL, WMIN, WMAX)
C
      IF (NEWPHI .OR. NEWTEM .OR. (.NOT. SKIP)) THEN
         CALL CHIAMP (APHI, ATHETA, NT, IDGAS, LOFFQ, NQ,
     1      NSPEC, POP, T11, TEMP, TOTNUM, XMODEL, APSI, TWOPMP,
     2      AMPKOS, MAXQ, AMPL)
         APHISV = APHI
      ENDIF
C
      IF (NEWPHI .OR. NEWTEM .OR. NEWPAR .OR. NEWKOS .OR.
     1   NEWWML .OR. NEWFOF .OR. NEWPRS .OR. NEWBET .OR. (.NOT.
     2   SKIP)) THEN
         NEWCHI = (NEWPHI .OR. NEWTEM .OR. (FORBRD .AND. NEWPAR) .OR.
     1            NEWKOS .OR. NEWWML .OR. NEWFOF .OR. NEWPRS .OR.
     2            NEWBET .OR. (.NOT. SKIP))
      IF (ADAPTV) THEN
C
C---  generate chit2 on adaptive grid first
C
            CALL CHICAL (AMPKOS, AMPL, APHI, ATHETA, CHNRPM, DELV4,
     1      DELV5, GAMMA, IDGAS, NEWCHI, NT, NQ, MAXQ, MAXV4, MAXV5,
     2      NSPEC, NTHE1, PARKOS, PARPR, POP, POPN, POPV45, PRESS,
     3      TEMP, TOTNUM, WAVTMP, WIDMUL, WMIN, WTRAN, XMODEL,
     4      FOFF, DEGSAT, BETA, APSI, TWOPMP, CHTMPI, CHTMPR,
     5      CHTMPSQ, NTM1, 1)
C
C---  interpolate to wavenumber array waven
C
            CALL SETWVE (CHTMPI, CHTMPR, CHTMPSQ, NTHE, NTHE1, WAVTMP,
     1      WAVEN, CHIIM, CHIRL, CHIT2)
         ELSE
            CALL CHICAL (AMPKOS, AMPL, APHI, ATHETA, CHNRPM, DELV4,
     1      DELV5, GAMMA, IDGAS, NEWCHI, NT, NQ, MAXQ, MAXV4, MAXV5,
     2      NSPEC, NTHE, PARKOS, PARPR, POP, POPN, POPV45, PRESS,
     3      TEMP, TOTNUM, WAVEN, WIDMUL, WMIN, WTRAN, XMODEL,
     4      FOFF, DEGSAT, BETA, APSI, TWOPMP, CHIIM, CHIRL, CHIT2,
     5      NTM, 1)
         ENDIF
C
         WMLSAV = WIDMUL
         FOFSAV = FOFF
         BETSAV = BETA
         DO 800 I = 1, 4
            KOSSAV(I) = PARKOS(I)
800      CONTINUE
         DO 900 M = 1, NSPEC
            IDG = IDGAS(M)
            PARSAV(IDG) = PARPR(IDG)
900      CONTINUE
      ENDIF
C
C---  subroutine wavoff calculates change in chicon when
C---  woffs or wexpnd is perturbed
C
      IF (((JVARY .EQ. 1) .OR. (JVARY .EQ. 3)) .AND. SKIP) THEN
         CALL WAVOFF (CCONV2, NPL, NTHE, WAVEN, WPL, CHICON,
     1      CHIEXP, CHIREF, IBEG, NDAT, USEREF, REFTEM, TEMP,
     2      REFP, PRESS, WDAT, CHNORM, CHIMAX, WOFDEL, WEXDEL,
     3      CHISV, CHRFSV, JVARY)
      ELSE
C
C---  perform convolution, add offset, interpolate at wpl, and normalize
C
         IF (JVARY .EQ. 2) THEN
            CALL ADDXOF (NTHE, CCONV2, CONV2, CHIOFF, 1)
            CALL SQTRPL (CONV2, NPL, NTHE, WAVEN, WPL, CHICON)
         ELSEIF (JVARY .EQ. 4) THEN
            CONTINUE
         ELSE
            CALL CNV2PMP (CHIIM, CHIRL, CHIT2, DBLCON, WLIL, WBIG,
     1   WLO, WHI, GAMPR, GAMPU, GAMPU2, GAUSPR, NTHE, TINSTR, LSRCNV,
     2   SPCPRB, CCONV2, 1, TWOPMP)
            CALL ADDXOF (NTHE, CCONV2, CONV2, CHIOFF, 1)
            CALL SQTRPL (CONV2, NPL, NTHE, WAVEN, WPL, CHICON)
         ENDIF
         CALL NORML (CHIEXP, CHICON, CHIREF, IBEG, NDAT, NPL,
     1      USEREF, REFTEM, TEMP, REFP, PRESS, WDAT, WPL, CHNORM,
     2      CHIMAX)
C
C---  save chicon and chiref for unperturbed variables for
C---  calculating residual when woffs or wexpnd is perturbed
C
         DO 300 I = 1, NPL
            CHISV(I) = CHICON(I)
            IF (USEREF) CHRFSV(I) = CHIREF(I)
300      CONTINUE
      ENDIF
C
C---  reset flag to call WAVMOD and CHIAMP first time through
C---  FITCAL during a fit
C
      SKIP = .TRUE.
C
      END
      SUBROUTINE FITCHK (ITASK, METHPN, ERROR)
C
C______________________________________________________________________
C
C  looks for inconsistencies in values of fit variables
C     output parameter:
C       error - logical variable indicating whether error was found
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17, N0 = 6)
      PARAMETER (NW = 1000, NM = 4, ND = 5000)

      DIMENSION NOTZER(N0)
      LOGICAL ERROR, LPRIN, GAUSPR
      CHARACTER NAMFIT*40, NAMRUN*40
C
      COMMON /PARAMS/ VARFIT(NF, 4)
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
      COMMON /CDR/ ATHETA, CHIREF(ND), CHNRPM, DBLCON, GAUSPR,
     1     WTRAN(NW, NM, 2), IBEG, IDGAS(NM), LOFFQ, NQ(NW, 4, NM, 2),
     2     NDAT, NSPEC, NTHE, NPL, NT(NM, 2), USEREF, REFTEM, REFP,
     3     T11(NW, 2, NM, 2), DEGSAT, WADD, WLIL, WBIG, WLO, WHI,
     4     LSRCNV, APSI, TWOPMP, SPCPRB, ADAPTV, NTHE1, MAXQ(2, NM),
     5     SHOTST
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      ERROR = .FALSE.
C
      IF (ITASK .EQ. 3) THEN
         DO 220 N = 1, NF
            IF (VARFIT(N, 1) .LT. VARFIT(N, 2)) THEN
               PRINT '(A, I3, A)' ,
     1         ' !! ERROR - NOMINAL VALUE OF VARIABLE' , N,
     2         ' LESS THAN MINIMUM !!'
               ERROR = .TRUE.
            ENDIF
            IF (VARFIT(N, 1) .GT. VARFIT(N, 3)) THEN
               PRINT '(A, I3, A)' ,
     1         ' !! ERROR - NOMINAL VALUE OF VARIABLE' , N,
     2         ' GREATER THAN MAXIMUM !!'
               ERROR = .TRUE.
            ENDIF
220      CONTINUE
      ENDIF
C
      DATA NOTZER/3, 4, 5, 7, 8, 24/
C
      DO 250 N = 1, N0
         INZ = NOTZER(N)
         IF (VARFIT(INZ, 1) .EQ. 0. .AND. LPRIN(GAUSPR, METHPN,
     1    INZ, ITASK, VARFIT)) THEN
            PRINT '(A, A, A)' , ' !! ERROR - ' ,
     1      NAMFIT(INZ)(1:LNFIT(INZ)), ' CANNOT BE 0 !!'
            ERROR = .TRUE.
         ENDIF
250   CONTINUE
C
      IF (ERROR) CALL PRESS_ENTER()
      END
      SUBROUTINE FITDEL (VARFIT, XVAR, XMIN, XMAX, DELTX, DELMN,
     1   MASK)
C
C______________________________________________________________________
C
C  establishes minimum and maximum values, and initial and final step
C  sizes for fit variables.
C______________________________________________________________________
C
      PARAMETER (NF = 30)
      DIMENSION VARFIT(NF, *), XVAR(*), XMIN(*), XMAX(*), DELTX(*),
     1   DELMN(*), MASK(*)
C
      DO 100 N = 1, NF
         XVAR(N) = VARFIT(N, 1)
         XMIN(N) = VARFIT(N, 2)
         XMAX(N) = VARFIT(N, 3)
         DELTX(N) = VARFIT(N, 1)/200.
         DELMN(N) = DELTX(N)/20.
         IF (VARFIT(N, 4) .GT. 0.) THEN
            MASK(N) = 1
         ELSEIF (VARFIT(N, 4) .LT. 0) THEN
            MASK(N) = 0
         ENDIF
100   CONTINUE
C
C---  use better step sizes for selected variables
C
      DELTX(3) = 1.E-3*VARFIT(3, 1)
      DELMN(3) = DELTX(3)/5.
      DELTX(7) = 1.E1
      DELMN(7) = 1.E0
      DELTX(9) = 1.E-1
      DELMN(9) = 1.E-3
      DELTX(21) = 1.E-3
      DELMN(21) = 1.E-5
      DELTX(22) = 1.E-2
      DELMN(22) = 1.E-4
      DELTX(23) = 1.E-2
      DELMN(23) = 1.E-4
      DELTX(24) = 1.E-3
      DELMN(24) = 1.E-5
C
      END
      SUBROUTINE FITPRN (VARFIT, KFLAG, NOREP, FOBJ, FLPRNT, SHOTST)
C
C______________________________________________________________________
C
C  prints answers for fitting - values of the variables for which
C  fit was done.
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      DIMENSION VARFIT(NF, *)
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      LOGICAL FLPRNT, SHOTST
C
      PRINT *, ' '
      IF (KFLAG .GT. 0) THEN
         PRINT *, '    Fit converged normally.'
         PRINT '(/2A, G12.6)', '     Final value of unnormalized ',
     1      'chi**2 = ', FOBJ
         IF (SHOTST) THEN
            PRINT *, '     with variance equal to unity-normalized ',
     1         'data'
         ELSE
            PRINT *, '     with variance equal to 1'
         ENDIF
      ELSEIF (KFLAG .EQ. -2) THEN
         PRINT *, '    Abnormal termination:  exceeded maximum ',
     1      'number of computations (1000).'
      ELSEIF (KFLAG .EQ. -3) THEN
         PRINT *, '    Abnormal termination:  terminated via sense ',
     1      'switch.'
      ENDIF
      IF (FLPRNT) RETURN
      PRINT '(//A/)' , '     Values of free variables after fit:'
      PRINT *,
     1   'variable                                     value'
      PRINT *,
     1'--------                                 ---------------'
C
      DO 100 N = 1, NF
         IF (VARFIT(N, 4) .LT. 0.) PRINT '(1X, A30, G26.5)' ,
     1   NAMFIT(N), VARFIT(N, 1)
100   CONTINUE
C
      PRINT *, ' '
      END
      SUBROUTINE FITRES
C
C______________________________________________________________________
C
C  defines chi-square for least-squares minimizer STEPIT
C  named FUNK in STEPIT documentation
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NM = 4, ND = 5000, NF = 30, NTM =
     1   32*16384, NP = 30, NPP = 31, NTM1 = NTM/2)
      SAVE AMPL, CHNORM, WPL, CHIT2
      LOGICAL DBLCON, GAUSPR, LOFFQ(2), USEREF, LSRCNV, TWOPMP, SPCPRB,
     1   ADAPTV, SHOTST
      LOGICAL MENU_STOPFIT, STOPMSG
      INTEGER XMODEL
      CHARACTER TTYPE*1, ANS*1
C
	COMMON /QWMENU/ MENU_STOPFIT, STOPMSG
      COMMON /PARAMS/ VARFIT(NF, 4)
      COMMON /CSTEP/ XVAR(NP), XMAX(NP), XMIN(NP), DELTX(NP),
     1   DELMN(NP), ERR(NP, NPP), FOBJ, NFS, NTRAC, MATRX,
     2   MASK(NP), NFMAX, NFLAT, JVARY, NXTRA, KFLAG, NOREP,
     3   KERFL, KW
      COMMON /PRTCHI/ ANS
      COMMON /CDR/ ATHETA, CHIREF(ND), CHNRPM, DBLCON, GAUSPR,
     1   WTRAN(NW, NM, 2), IBEG, IDGAS(NM), LOFFQ, NQ(NW, 4, NM, 2),
     2   NDAT, NSPEC, NTHE, NPL, NT(NM, 2), USEREF, REFTEM,
     3   REFP, T11(NW, 2, NM, 2), DEGSAT, WADD, WLIL, WBIG,
     4   WLO, WHI, LSRCNV, APSI, TWOPMP, SPCPRB, ADAPTV, NTHE1,
     5   MAXQ(2, NM), SHOTST
      COMMON /CDR1/ WDAT(ND)
      COMMON /FITDAT/ DATPL(ND)
      COMMON /SARR2/ WAVEN(NTM)
      COMMON /TINSTR/ TINSTR(2*NTM)
      COMMON /XMOD/ XMODEL(NW, NM, 2)
      COMMON /TTYP/ TTYPE(NW, NM, 2)
      COMMON /WAVE/ WAVTMP(NTM1)
      DOUBLE PRECISION WDAT, WAVEN, WAVTMP
C
C  local variables:
C
      DIMENSION AMPL(NW, NM, 2), CHNORM(ND), WPL(ND), CHIT2(NTM)
      DOUBLE PRECISION WPL

C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C     Check if user request to stop fit.
C
      IF (MENU_STOPFIT) THEN
	   IF (STOPMSG) PRINT*,' Terminating fit--- please wait.'
	   STOPMSG = .FALSE.
	   RETURN
	ENDIF

C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  transfer xvar from stepit to varfit
C
      DO 100 N = 1, NF
         VARFIT(N, 1) = XVAR(N)
100   CONTINUE
C
      CALL FITCAL (AMPL, ATHETA, CHIREF, CHNRPM, DBLCON, GAUSPR,
     1   WTRAN, IBEG, LOFFQ, NQ, IDGAS, NDAT, NSPEC, NTHE, NPL,
     2   NT, PRESS, USEREF, REFTEM, REFP, JVARY, T11, TINSTR,
     3   TTYPE, VARFIT, WAVEN, WDAT, XMODEL, CHNORM, CHIMAX,
     4   CHIT2, WPL, DEGSAT, WLIL, WBIG, WLO, WHI, LSRCNV, APSI,
     5   TWOPMP, SPCPRB, ADAPTV, WAVTMP, NTHE1, MAXQ)
C
      FOBJ = 0.
C
C---  it is assumed that the weighting factor in the chi-square is
C---  the reciprocal of the variance, which is assumed proportional
C---  to the signal level [see d. r. snelling, g. j. smallwood,
C---  r. a. sawchuk, and t. parameswaran, appl. opt. 26, 99 (1987)].
C---  clearly, negative data does not obey shot-noise statistics,
C---  so the absolute value is taken to at least add to the chi-
C---  squared instead of subtracting.
C
      IF (SHOTST) THEN
         DO 200 K = 1, NPL
            FO = CHNORM(K)-DATPL(K)
            FOBJ = FOBJ+FO*FO/ABS(DATPL(K))
200      CONTINUE
      ELSE
         DO 300 K = 1, NPL
            FO = CHNORM(K)-DATPL(K)
            IF (DATPL(K) .NE. 0.0) FOBJ = FOBJ+FO*FO
300      CONTINUE
      ENDIF
C
      IF (JVARY .EQ. 0) THEN
         WRITE (KW, '(1X, A, E12.6)')
     1      'Varying all free parameters, X2 = ', FOBJ
         IF (ANS .EQ. 'Y') WRITE (*, '(1X, A, E12.6)')
     1      'Varying all free parameters, X2 = ', FOBJ
      ELSE
         WRITE (KW, '(1X, A, I3, A, E12.6, A, E12.6)')
     1      'Varying parameter #', JVARY, ', value = ',
     2      XVAR(JVARY), ', X2 = ',FOBJ
         IF (ANS .EQ. 'Y') WRITE (*, '(1X, A, I3, A, E12.6,
     1      A, E12.6)')
     2      'Varying parameter #', JVARY, ', value = ',
     3      XVAR(JVARY), ', X2 = ',FOBJ
      ENDIF
C
      END
      SUBROUTINE FTERMS (IDGAS, MAXQ, NSPEC, PRESS, TEMP, WMIN,
     1   WMAX, METHPN, WTRAN, LOFFQ, NQ, NT, VARFIT, WADD, WLIL,
     2   WBIG, PARPR, TWOPMP)
C
C______________________________________________________________________
C
C   determines which transitions are to be used in calculation of
C   susceptibility.
C   output variables:
C     wtran : wavenumbers at which transitions take place
C     loffq : flag indicating that off-resonant n2 q-branch
C             contribution has been included
C     nq    : quantum numbers of transitions: indices are
C             (k,l,m), where k=transition index; l=1 for ground
C              vibrational state, l=2 for upper v state,
C              l=3 for ground rotational state, l=4 for upper
C              j state;  m is species index
C     nt    : number of transitions for each species
C______________________________________________________________________
C
      PARAMETER (NV = 6, NM = 4, NJ = 130, NW = 1000, NF = 30)
      DIMENSION WTRAN(NW, NM, *), IDGAS(*), NT(NM, *), MAXQ(2, *),
     1   NQ(NW, 4, NM, *), VARFIT(NF, *), PARPR(*)
      LOGICAL LOFFQ(2), TWOPMP
      COMMON /TERMM/ TERMM(0:NV, 0:NJ, NM)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C  local variables:
C
	 save vdl, jdl
      INTEGER V, VDL(2), JDL(3), VI
      CHARACTER DUMSTR
      DATA VDL/0, 1/, JDL/-2, 0, 2/
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      LOFFQ(1) = .FALSE.
      LOFFQ(2) = .FALSE.
C
C     loop through all species.
C
      DO 200 M = 1, NSPEC
         IDG = IDGAS(M)
C
C---  do co2 and h2o elsewhere
C
         IF (GASNAM(IDG) .EQ. 'CO2') THEN
            CONTINUE
         ELSEIF (GASNAM(IDG) .EQ. 'H2O') THEN
            CONTINUE
         ELSE
C
C---  get first transition to establish limits over which to keep
C---  transitions in spectrum.
C---  assume for purposes of getting gam0 that first
C---  transition is q-branch,
C---  since it is not known yet, but is relatively insensitive
C
            CALL LINWID (0, PRESS, TEMP, 'Q' , MAXQ, M, 1,
     1         IDGAS, PARPR, NSPEC, GAM0)
            CALL WLIMS (GAM0, PRESS, WMIN, WMAX, WADD, WBIG, WLIL)
C
C---  loop through selection rules for v, then j
CC
            NT(M, 1) = 0
            NT(M, 2) = 0
            DO 300 VI = 1, 2
               DO 300 JI = 1, 3
                  DO 300 V = 0, MAXQ(1, M)
                     IV2 = V+VDL(VI)
                     DO 300 J = 0, MAXQ(2, M)
                     DO 300 IPMP = 1, 2
                        IJ2 = J+JDL(JI)
                        IF (IJ2 .LT. 0) GO TO 300
                        TD = TERMM(IV2, IJ2, IDG) - TERMM(V, J, IDG)
                        IF (TWOPMP .AND. (IPMP .EQ. 2)) THEN
                           TD1 = TD + VARFIT(10, 1)
                        ELSE
                           TD1 = TD
                        ENDIF
C
C---  keep entire Q-branch for exponential gap calculation
C
                        IF (((METHPN .EQ. 3) .AND. (VI .EQ. 2)
     1                     .AND. (JI .EQ. 2)) .OR.
     2                  ((TD1 .GT. WLIL) .AND. (TD1 .LT. WBIG)))
     3                  THEN
                           NT(M, IPMP) = NT(M, IPMP) + 1
C
C---  form array of quantum numbers for an included transition
C
                           NQ(NT(M, IPMP), 1, M, IPMP) = V
                           NQ(NT(M, IPMP), 2, M, IPMP) = IV2
                           NQ(NT(M, IPMP), 3, M, IPMP) = J
                           NQ(NT(M, IPMP), 4, M, IPMP) = IJ2
                           WTRAN(NT(M, IPMP), M, IPMP) = TD
                        ENDIF
                        IF (NT(M, IPMP) .GT. NW) THEN
                           WRITE (*, '(1X, 3A)')
     1                     '!! TOO MANY TRANSITIONS FOR SPECIES ',
     2                     GASNAM(IDG), ' !!'
                           WRITE (*, '(1X, A)')
     1               '!! REDUCE WAVENUMBER RANGE OR REDIMENSION CODE !!'
                        CALL QUITS (0, DUMSTR)
                        ENDIF
300         CONTINUE
         ENDIF
200   CONTINUE
C
        IF (VARFIT(11, 1) .GT. 0.0) CALL
     1   N2BAND (IDGAS(1), LOFFQ, NQ, NT, TEMP, WBIG, WLIL, WTRAN,
     1   VARFIT(10, 1))
     
      END
      
      SUBROUTINE GASID (NM, PARPR, IDGAS, NSPEC)
C
C______________________________________________________________________
C
C  determines identities and number of gases
C    output parameters:
C      idgas : identities of gases
C      nspec : number of species
C______________________________________________________________________
C
      DIMENSION PARPR(*), IDGAS(*)
C
      NSPEC = 0
C
      DO 100 M = 1, NM
         IF (PARPR(M) .GT. 0.) THEN
            NSPEC = NSPEC+1
            IDGAS(NSPEC) = M
         ENDIF
100   CONTINUE
C
      END
      SUBROUTINE GENM (FJ, K, MAXJ, PARKOS, PRESS, WIDMUL,
     1   TEMP, VG, WTRAN, WAVEN, FOFF, TWOPMP, IPMP, IDGAS, PARPR,
     2   NSPEC, AMTX)
C
C______________________________________________________________________
C
C  generates m matrix = wj+i*totnum*<v*sigma> for use in exponential
C  gap model of pressure narrowing. (see ref. 10)
C  normally, totnum*<v*sigma> = -gamma/2 along diagonal.
C______________________________________________________________________
C
      PARAMETER (NV = 6, NJ = 130, NM = 4)
      DIMENSION FJ(0:NV, 0:*), PARKOS(*), WTRAN(*), WAVEN(*),
     1   IDGAS(*), PARPR(*)
      DOUBLE PRECISION WAVEN
      COMPLEX AMTX(*)
      INTEGER VG
      LOGICAL TWOPMP
C
      DIMENSION CSUM(0:NJ)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      NPOS(IR, IC, NORD) = IC*NORD+IR+1
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  establish as one-dim array, so as to pack elements for calls
C---  to matrix manipulation routines
C---  first, initialize amtx, since it will be half-sparse
C
      NORDER = MAXJ+1
      LEN = NORDER*NORDER
C
      DO 150 J = 1, LEN
         AMTX(J) = CMPLX(0., 0.)
150   CONTINUE
C
C---  initialize column sums
C
      DO 160 J = 0, MAXJ
         CSUM(J) = 0.
160   CONTINUE
C
C---  generate off-diagonal terms.
C---  amtx(j2,j1) describes up transitions, amtx(j1,j2) down transitions
C---  a factor of 2 is included in a1 and a2 since fwhm widths are used.
C
      AKOS = PARKOS(1)
      BKOS = PARKOS(2)
      CKOS = PARKOS(3)
      DKOS = PARKOS(4)
C
      CEXOT = CEX/TEMP
C
C---  jdel = 2 for homonuclear diatomics, 1 otherwise.
C---  only first species can be treated with exponential gap.
C---  presently only homonuclear for which parameters are
C---  available is nitrogen.
C
      IF (GASNAM(1) .EQ. 'N2') THEN
         JDEL = 2
         CGAP = 1.5
C
C---  include broadening effects due to co2 and h2o for n2
C
         PN2CO2 = 0.
         PN2H2O = 0.
C
         DO 120 M1 = 1, NSPEC
            IDG1 = IDGAS(M1)
            IF (GASNAM(IDG1) .EQ. 'CO2') PN2CO2 = PARPR(IDG1)
            IF (GASNAM(IDG1) .EQ. 'H2O') PN2H2O = PARPR(IDG1)
120      CONTINUE
C
         PN2N2 = MAX(0., 1.-PN2CO2-PN2H2O)
         DATA AN2CO2, EN2CO2, AN2H2O, EN2H2O / 0.036345, 1.36,
     1      0.042305, 1.417 /
         CN2 = 2.*WIDMUL*PRESS
         CN2N2 = CN2*PN2N2*AKOS*SQRT(295./TEMP)*
     1           (1.-EXP(-DKOS))/(1.-EXP(-DKOS*TEMP/295.))
         CN2CO2 = CN2*PN2CO2*AN2CO2*(295./TEMP)**EN2CO2
         CN2H2O = CN2*PN2H2O*AN2H2O*(295./TEMP)**EN2H2O
      ELSE
         JDEL = 1
         CGAP = 2.
         CONS = 2.*WIDMUL*AKOS*PRESS*SQRT(295./TEMP)*
     1           (1.-EXP(-DKOS))/(1.-EXP(-DKOS*TEMP/295.))
      ENDIF
C
      DO 180 J1 = 0, MAXJ-JDEL
         R2JP1 = 2*J1+1
         AD = (CGAP*FJ(VG, J1)*CEXOT)
         RJFAC = ((1.+AD/(CKOS))/(1.+AD))**2
         IF (GASNAM(1) .EQ. 'N2') THEN
            DATA DN2CO2, DN2H2O / 1.466, 0.779 /
            RJFACC = ((1.+AD/(DN2CO2))/(1.+AD))**2
            RJFACH = ((1.+AD/(DN2H2O))/(1.+AD))**2
         ENDIF
C
         DO 180 J2 = J1+JDEL, MAXJ, JDEL
            EX = (FJ(VG, J2)-FJ(VG, J1))*CEXOT
C
C--- aup represents up transitions, and will be placed in
C--- row j2, column j1 (below diagonal)
C--- adn represents down transitions, and is placed in mirror
C--- location across diagonal: row j1, column j2 (above diagonal)
C
            IF (GASNAM(1) .EQ. 'N2') THEN
               DATA BN2CO2, BN2H2O / 1.752, 1.932 /
               AUP = CN2N2*EXP(-BKOS*EX)*RJFAC+CN2CO2*EXP(-BN2CO2*
     1               EX)*RJFACC+CN2H2O*EXP(-BN2H2O*EX)*RJFACH
            ELSE
               AUP = CONS*EXP(-BKOS*EX)*RJFAC
            ENDIF
            ADN = AUP*R2JP1/(2*J2+1)*EXP(EX)
            AMTX(NPOS(J2, J1, NORDER)) = CMPLX(0., AUP)
            AMTX(NPOS(J1, J2, NORDER)) = CMPLX(0., ADN)
C
C---  sum aup into column sum for j1, and adn into column j2
C
            CSUM(J1) = CSUM(J1)+AUP
            CSUM(J2) = CSUM(J2)+ADN
180   CONTINUE
C
C---  generate diagonal terms.
C
      DO 200 J1 = 0, MAXJ
         IF (TWOPMP .AND. (IPMP .EQ. 2)) THEN 
            FK = WTRAN(K+J1) - WAVEN(1) + FOFF
         ELSE
            FK = WTRAN(K+J1) - WAVEN(1)
         ENDIF
C
C---  GK is 2 * gamma(s,t) in Bob Hall's papers (e.g., ref. 11)
C
         GK = CSUM(J1)
         NDIAG = NPOS(J1, J1, NORDER)
         AMTX(NDIAG) = CMPLX(2.*FK, -GK)
200   CONTINUE
C
      END
      SUBROUTINE GETCHI (TWOPMP, COS1, COS2, COS3, COS4, COS5, COS6,
     1   COSTH, SINTH, COSPHI, SINPHI, DPOP, SIG, T11, K, M, IPMP, AMP)
C
C______________________________________________________________________
C
C  calculates amplitude of resonant suceptibility for two-color and
C  three-color cars
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NM = 4)
      DIMENSION T11(NW, 2, NM, *)
      LOGICAL TWOPMP
C
      IF (TWOPMP) THEN
C
C---  three-color cars
C
         IF (IPMP .EQ. 2) THEN
            CH1122 = 0.5*T11(K, 1, M, IPMP)-T11(K, 2, M, IPMP)
            CH1221 = 0.5*T11(K, 2, M, IPMP)
            CH1212 = CH1221
         ELSE
            CH1122 = 0.5*T11(K, 2, M, IPMP)
            CH1221 = CH1122
            CH1212 = 0.5*T11(K, 1, M, IPMP)-T11(K, 2, M, IPMP)
         ENDIF
         AMP = 3.*DPOP*SIG*(CH1122*COS1*COS2+
     1         CH1212*COS3*COS4+CH1221*COS5*COS6)
      ELSE
C
C---  two-color cars
C
         CH1111 = T11(K, 1, M, IPMP)
         CH1221 = T11(K, 2, M, IPMP)
         AMP = 3.*DPOP*SIG*(CH1221*SINTH*SINPHI+
     1         CH1111*COSTH*COSPHI)
      ENDIF
C
      END
      SUBROUTINE GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF,
     1   DELV4, DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL,
     2   TEMP, TOTNUM, WEXPND, WOFFS, FOFF, BETA)
C
C______________________________________________________________________
C
C  translates varfit array, used by STEPIT, into working variables
C______________________________________________________________________
C
      PARAMETER (NV45 = 10)
      DIMENSION PARPR(*), DELV4(0:*), DELV5(0:*), VARFIT(*),
     1   PARKOS(*)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      DATA T0/273.15/
C
      NF = 1
      WOFFS = VARFIT(NF)
      NF    = NF+1
      CHIOFF = VARFIT(NF)
      NF     = NF+1
      WEXPND = VARFIT(NF)
      NF     = NF+1
      CHIEXP = VARFIT(NF)
      NF     = NF+1
      GAMPR  = VARFIT(NF)
      NF     = NF+1
      GAMPU  = VARFIT(NF)
      NF     = NF+1
      TEMP   = VARFIT(NF)
      NF     = NF+1
      WIDMUL = VARFIT(NF)
      NF     = NF+1
      PHI    = VARFIT(NF)
      NF     = NF+1
      FOFF   = VARFIT(NF)
      NF     = NF+1
C
      DO 100 M = 1, NM
         PARPR(M) = VARFIT(NF)
         NF       = NF+1
100   CONTINUE
C
      IF (VARFIT(NF-1) .GT. 0.) THEN
C
C                 . . . establish c2h2 shift parameters
C
         DELV4(0) = 0.
         DELV5(0) = 0.
         DO 120 K = 1, 3
            DELV4(K) = VARFIT(NF)
            NF       = NF+1
120      CONTINUE
         DO 130 K = 1, 3
            DELV5(K) = VARFIT(NF)
            NF       = NF+1
130      CONTINUE
C
C---  set delv for higher levels same as first
C
         DV41 = DELV4(1)
         DV51 = DELV5(1)
         DO 140 K = 4, NV45
            DELV4(K) = DV41
            DELV5(K) = DV51
140      CONTINUE
C
      ELSE
         NF = NF+6
      ENDIF
C
C   exponential gap model adjustable parameters
C
      DO 150 K = 1, 4
         PARKOS(K) = VARFIT(NF)
         NF = NF+1
150   CONTINUE
C
      PRESS = VARFIT(NF)
      NF = NF+1
      BETA = VARFIT(NF)
      NF = NF+1
      GAMPU2 = VARFIT(NF)
C
      APHI = PHI*PI/180.
      TOTNUM = AVAG*(T0/TEMP)*(PRESS/VSTP)
C
      END
      SUBROUTINE GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
C
C______________________________________________________________________
C
C   interprets varrun array, to return values for run control
C   variables
C______________________________________________________________________
C
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      DIMENSION VARRUN(*)
      LOGICAL DBLCON, GAUSPR, USEREF, SPCPRB, ENADPT, SHOTST
      CHARACTER* (*)PRBFIL
      CHARACTER*20 SUB
C
      GAUSPR = .TRUE.
      SPCPRB = .FALSE.
      IFC = IFIRSC(PRBFIL)
      IF (IFC .NE. 0) THEN
         LEN = LENTH(PRBFIL)
         SUB = PRBFIL(IFC:LEN)
         IF (SUB .NE. 'NONE' .AND. SUB .NE. 'None' .AND. SUB
     1      .NE. 'none') GAUSPR = .FALSE.
C
C---  if probe file name = "special" use line shape function
C---  in subroutine makprb
C
         IF (SUB .EQ. 'SPECIAL' .OR. SUB .EQ. 'Special' .OR.
     1      SUB .EQ. 'special') SPCPRB = .TRUE.
      ENDIF
C
      ITASK = VARRUN(1)
      WBEG  = VARRUN(2)
      WEND  = VARRUN(3)
      WADD  = VARRUN(4)
      CHNR  = VARRUN(5)
C
C     varrun(6)    methpn     meaning
C        1.         1       isolated line model
C        2.         2       rotational diffusion model
C        3.         3       exponential gap model
C        4.         4       Voigt profile
C        5.         5       Saturated line model
C        6.         6       Galatry profile
C        7.         7       Hard collision profile
C        8.         8       Correlated hard collision profile
C
      METHPN = VARRUN(6)
      THETA  = VARRUN(7)
C
C     varrun(8)       meaning
C        +1.       ratioed calculation (to non-resonant spectrum)
C        -1.       non-ratioed calculation
C
      IF (VARRUN(8) .GT. 0.) USEREF = .TRUE.
      IF (VARRUN(8) .LT. 0.) USEREF = .FALSE.
C
C     varrun(9)      meaning
C         1.      subtract o-branch
C         2.      subtract q-branch
C         0.      neither of above
C
      NBRNCH = VARRUN(9)
      NREJEC = VARRUN(10)
      GAINF  = VARRUN(11)
C
C     varrun(12)        meaning
C        +1.       single convolution with combined pump & probe
C        -1.       convolve pump and probe separately
C
      DBLCON = .FALSE.
      IF (VARRUN(12) .LT. 0.) DBLCON = .TRUE.
C
      DEGSAT = VARRUN(14)
      PSI = VARRUN(15)
C
C     varrun(16)       meaning
C        +1.       enable adaptive gridding
C        -1.       disable adaptive gridding
C
      IF (VARRUN(16) .GT. 0.) ENADPT = .TRUE.
      IF (VARRUN(16) .LT. 0.) ENADPT = .FALSE.
C
C     varrun(17)       meaning
C        +1.       variance = unity-normalized data for fitting
C        -1.       variance = 1 for fitting
C
      IF (VARRUN(17) .GT. 0.) SHOTST = .TRUE.
      IF (VARRUN(17) .LT. 0.) SHOTST = .FALSE.
C
C   chnrpm is nonresonant susc. per molecule of other gases
C
      CHNRPM = CHNR/(AVAG/VSTP)
C
      ATHETA = THETA*PI/180.
      APSI = PSI*PI/180.
C
      END
      SUBROUTINE GTYPFL (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS, BETA,
     1   PRESS, AMPL, XREAL, XIMAG)
C
C______________________________________________________________________
C
C  calculates galatry profile for susceptibility
C   output parameters:
C      xreal : real part of susceptibility
C      ximag : imaginary part of susceptibility
C______________________________________________________________________
C
      DOUBLE PRECISION WAVEN(*)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WJ0 = WJ+WAVEN(1)
C
C---  constant = sqrt(2k/m0)/c.
C---  beta is velocity-changing collision frequency kT/mD at 1 atm.
C
      ALPHAD = 4.3014E-7*WJ0*SQRT(TEMP/CMASS)
      XG = -(WDF-WJ)/ALPHAD
      YG = GAMJ/(2.*ALPHAD)
      ZG = PRESS*BETA/ALPHAD
C
      CALL VGTRY (XG, YG, ZG, GR, GI)
C
C---  lineshape function is not normalized to unit area.  area of
C---  lorentzian lineshape is pi/2, so galatry profile is
C---  corrected accordingly.
C
      AC = SQTPI/(2.*ALPHAD)
      XREAL = AMPL*GI*AC
      XIMAG = AMPL*GR*AC
C
      END
      SUBROUTINE HDCLML (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS, BETA,
     1   PRESS, AMPL, XREAL, XIMAG)
C
C______________________________________________________________________
C
C  calculates hard collision model for susceptibility.
C  this model assumes dephasing and velocity-changling collisions
C  are uncorrelated.
C   output parameters:
C      xreal : real part of susceptibility
C      ximag : imaginary part of susceptibility
C______________________________________________________________________
C
      DOUBLE PRECISION WAVEN(*)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WJ0 = WJ+WAVEN(1)
C
C---  constant = sqrt(2k/m0)/c.
C---  beta is total collision frequency (omega) at 1 atm.
C
      ALPHAD = 4.3014E-7*WJ0*SQRT(TEMP/CMASS)
      XH = -(WDF-WJ)/ALPHAD
      YH = GAMJ/(2.*ALPHAD)
      ZH = PRESS*BETA/ALPHAD
C
      CALL CPF (XH, YH+ZH, WR, WI)
C
      W1 = 1.-SQTPI*ZH*WR
      W2 = SQTPI*ZH*WI
      DENOM = W1*W1+W2*W2
      HR = (WR*W1-WI*W2)/DENOM
      HI = (WI*W1+WR*W2)/DENOM
C
C---  lineshape function is not normalized to unit area.  area of
C---  lorentzian lineshape is pi/2, so hard collision profile is
C---  corrected accordingly.
C
      AC = SQTPI/(2.*ALPHAD)
      XREAL = AMPL*HI*AC
      XIMAG = AMPL*HR*AC
C
      END
      SUBROUTINE INICAL (IDGAS, METHPN, NSPEC, PRESS, TEMP,
     1   WMIN, WMAX, WTRAN, GAMMA, LOFFQ, NQ, MAXQ, MAXV4, MAXV5, NT,
     2   POP, POPN, POPV45, TTYPE, XMODEL, T11, VARFIT, WADD, WLIL,
     3   WBIG, FOFF, APSI, TWOPMP, PARPR)
C
C______________________________________________________________________
C
C  does initial calculations which are common to all tasks.
C______________________________________________________________________
C
      PARAMETER (NM = 4, NW = 1000, NJ = 130, NV = 6, NV45 =
     1   10, NF = 30)
      DIMENSION IDGAS(NM), MAXQ(2, NM), MAXV5(0:NV45),
     1   WTRAN(NW, NM, *), GAMMA(NW, NM, *), NQ(NW, 4, NM, *),
     2   NT(NM, *), POP(0:NV, 0:NJ, NM), POPN(0:NV, 0:NJ, NM),
     3   POPV45(0:NV45, 0:NV45), T11(NW, 2, NM, *), VARFIT(NF, *),
     4   PARPR(*)
      INTEGER XMODEL(NW, NM, 2)
      CHARACTER*(*) TTYPE(NW, NM, *)
      LOGICAL LOFFQ(2), TWOPMP
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  if foff nonzero or pump beam polarizations not parallel,
C---  doing three-color cars (two pumps)
C
      IF ((FOFF .NE. 0.) .OR. (APSI .NE.
     1   0.)) THEN
         TWOPMP = .TRUE.
      ELSE
         TWOPMP = .FALSE.
      ENDIF
C
C---  must calculate co2 and h2o transitions before calculating
C---  populations
C
      DO 50 M = 1, NSPEC
         IF (GASNAM(IDGAS(M)) .EQ. 'CO2') THEN
            CALL LINWID (0, PRESS, TEMP, 'Q' , MAXQ, M,
     1         1, IDGAS, PARPR, NSPEC, GAM0)
            CALL WLIMS (GAM0, PRESS, WMIN, WMAX, WADD, WBIG, WLIL)
C
C---  comment out following line to remove co2 routine
C
            CALL CO2TRN (NSPEC, M, NT, TEMP, WLIL, WBIG, MAXQ, NQ,
     1         WTRAN, FOFF)
         ELSEIF (GASNAM(IDGAS(M)) .EQ. 'H2O') THEN
            CALL LINWID (0, PRESS, TEMP, 'W' , MAXQ, M,
     1         1, IDGAS, PARPR, NSPEC, GAM0)
            CALL WLIMS (GAM0, PRESS, WMIN, WMAX, WADD, WBIG, WLIL)
C
C---  comment out following line to remove h2o routine
C
C            CALL H2OTRN (NSPEC, IDGAS, NT, TEMP, WLIL, WBIG, MAXQ,
C     1         NQ, WTRAN)
         ENDIF
50    CONTINUE
C
      CALL POPLAT (IDGAS, NSPEC, TEMP, MAXQ, MAXV4, MAXV5, NT, NQ,
     1    POP, POPN, POPV45)
      CALL FTERMS (IDGAS, MAXQ, NSPEC, PRESS, TEMP, WMIN, WMAX,
     1    METHPN, WTRAN, LOFFQ, NQ, NT, VARFIT, WADD, WLIL, WBIG,
     2    PARPR, TWOPMP)
      CALL TRNCOD (NQ, IDGAS, LOFFQ, NSPEC, NT, METHPN, TTYPE,
     1   XMODEL)
C
      DO 60 M = 1, NSPEC
      DO 60 IPMP = 1, 2
         DO 60 K = 1, NT(M, IPMP)
            CALL LINWID (NQ(K, 3, M, IPMP), PRESS, TEMP,
     1         TTYPE(K, M, IPMP), MAXQ, M, K, IDGAS, PARPR, NSPEC,
     2         GAMMA(K, M, IPMP))
60    CONTINUE
C
      NSUM   = 0
C
      DO 100 M = 1, NSPEC
         IF (NSPEC .GT. 1) PRINT '(T18, 3A)' , '------  For '
     1    , GASNAM(IDGAS(M)), ' ------'
         PRINT 1000,
     1   ' . . . Highest vibrational quantum number retained ='
     2    , MAXQ(1, M)
         PRINT 1000,
     1   ' . . . Highest rotational quantum number retained ='
     2   , MAXQ(2, M)
         IF ((LOFFQ(1).OR.LOFFQ(2)) .AND. (GASNAM(IDGAS(M)) .EQ. 'N2'))
     1   PRINT *, '. . . Adding transition simulating nitrogen Q-branch'
         IF (GASNAM(IDGAS(M)) .EQ. 'C2H2') THEN
            PRINT 1000,
     1      ' . . . Highest V4 quantum number retained =' ,
     2      MAXV4
         ENDIF
         PRINT 1000,
     1   ' . . . Number of transitions included in spectrum ='
     2   , NT(M, 1) + NT(M, 2)
         NSUM = NSUM + NT(M, 1) + NT(M, 2)
100   CONTINUE
C
      IF (NSUM .EQ. 0) CALL QUITS (-1,
     1   '!! FROM INICAL - NO TRANSITIONS WITHIN PLOT RANGE !!')
C
      CALL TTERMS (IDGAS, NT, NQ, NW, NSPEC, TTYPE, T11)
C
1000  FORMAT(A, T54, I6)
      END
      SUBROUTINE INPFIT (KEY, NLINE, INFO, VARFIT)
C
C______________________________________________________________________
C
C   searches for match of input keyword with fit variable names,
C   and sets varfit when found
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      DIMENSION VARFIT(NF, *)
      CHARACTER* (*)KEY, INFO
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
      DIMENSION DAT(10)
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DO 100 K = 1, NF
         IF (KEY(1:LNFIT(K)) .EQ. NAMFIT(K)(1:LNFIT(K))) THEN
            CALL INTERP (NLINE, INFO, -4, NFOUND, DAT, IER)
            IF (IER .NE. 0) RETURN
            IF (NFOUND .EQ. 1) THEN
               VARFIT(K, 1) = DAT(1)
C
C                       . . . set new default constraints,in
C                             case fitting is chosen
C
               IF (VARFIT(K, 1) .LE. 0.) THEN
                  VARFIT(K, 2) = VARFIT(K, 1)*2.0
                  VARFIT(K, 3) = VARFIT(K, 1)*0.5
               ELSE
                  VARFIT(K, 2) = VARFIT(K, 1)*0.5
                  VARFIT(K, 3) = VARFIT(K, 1)*2.0
               ENDIF
            ELSEIF (NFOUND .EQ. 4) THEN
               DO 80 NJ = 1, 4
                  VARFIT(K, NJ) = DAT(NJ)
80             CONTINUE
            ELSE
               CALL VARERR ( 'MUST HAVE EITHER 1 OR 4 VALUES' ,
     1          NLINE)
            ENDIF
            RETURN
         ENDIF
100   CONTINUE
C
C---  exit at bottom of loop means search through all run
C---  and fit variables names has failed.
C
      CALL VARERR ( 'KEYWORD NOT IN VOCABULARY' , NLINE)
      END
      SUBROUTINE INPRUN (KEY, NLINE, INFO, VARRUN, LFOUND)
C
C______________________________________________________________________
C
C   looks for a match of input line to run variable, and sets
C   variable if found
C______________________________________________________________________
C
      PARAMETER (NF = 30, NM = 4, NR = 17)
      DIMENSION VARRUN(*)
      LOGICAL LFOUND
      CHARACTER* (*)KEY, INFO
C
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
C
C---  local variables:
C
      DIMENSION DAT(10)
      CHARACTER*1 CH1
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      LFOUND = .TRUE.
C
      DO 200 K = 1, NR
         IF (KEY(1:LNRUN(K)) .EQ. NAMRUN(K)(1:LNRUN(K))) THEN
C
C---  look for match to variable with string input first - use
C---  first significant  character after equal sign for comparison
C
            CH1 = INFO(1:1)
            IF (K .EQ. 6) THEN
C
C                  . . . lineshape model
C
               IF (CH1 .EQ. 'i' .OR. CH1 .EQ. 'I') THEN
                  VARRUN(K) = 1.
               ELSEIF (CH1 .EQ. 'r' .OR. CH1 .EQ. 'R') THEN
                  VARRUN(K) = 2.
               ELSEIF (CH1 .EQ. 'e' .OR. CH1 .EQ. 'E') THEN
                  VARRUN(K) = 3.
               ELSEIF (CH1 .EQ. 'v' .OR. CH1 .EQ. 'V') THEN
                  VARRUN(K) = 4.
               ELSEIF (CH1 .EQ. 's' .OR. CH1 .EQ. 'S') THEN
                  VARRUN(K) = 5.
               ELSEIF (CH1 .EQ. 'g' .OR. CH1 .EQ. 'G') THEN
                  VARRUN(K) = 6.
               ELSEIF (CH1 .EQ. 'h' .OR. CH1 .EQ. 'H') THEN
                  VARRUN(K) = 7.
               ELSEIF (CH1 .EQ. 'c' .OR. CH1 .EQ. 'C') THEN
                  VARRUN(K) = 8.
               ELSE
                  CALL VARERR ( 'SYNTAX ERROR AFTER EQUAL SIGN'
     1             , NLINE)
               ENDIF
            ELSEIF (K .EQ. 8) THEN
C
C                    . . . ratioed calculation or not
C
               IF (CH1 .EQ. 'y' .OR. CH1 .EQ. 'Y') THEN
                  VARRUN(K) = 1.
               ELSEIF (CH1 .EQ. 'n' .OR. CH1 .EQ. 'N') THEN
                  VARRUN(K) = -1.
               ELSE
                  CALL VARERR ( 'SYNTAX ERROR AFTER EQUAL SIGN'
     1             , NLINE)
               ENDIF
            ELSEIF (K .EQ. 9) THEN
C
C                    . . . branch to be rejected
C
               IF (CH1 .EQ. 'O' .OR. CH1 .EQ. 'o') THEN
                  VARRUN(K) = 1.
               ELSEIF (CH1 .EQ. 'q' .OR. CH1 .EQ. 'Q') THEN
                  VARRUN(K) = 2.
               ELSEIF (CH1 .EQ. 'n' .OR. CH1 .EQ. 'N') THEN
                  VARRUN(K) = 0.
               ELSE
                  CALL VARERR ( 'SYNTAX ERROR AFTER EQUAL SIGN'
     1             , NLINE)
               ENDIF
            ELSEIF (K .EQ. 10) THEN
C
C                     . . . gas to be rejected in ref. array
C
               DO 50 M = 1, NM
                  IF (CH1 .EQ. GASNAM(M)(1:1)) THEN
                     VARRUN(K) = M
                     RETURN
                  ENDIF
50             CONTINUE
               CALL VARERR ( 'SYNTAX ERROR AFTER EQUAL SIGN' ,
     1         NLINE)
            ELSEIF (K .EQ. 12) THEN
C
C                   . . . convolution method - single or double
C
               IF (CH1 .EQ. 'S' .OR. CH1 .EQ. 's') THEN
                  VARRUN(K) = 1.
               ELSEIF (CH1 .EQ. 'd' .OR. CH1 .EQ. 'D') THEN
                  VARRUN(K) = -1.
               ELSE
                  CALL VARERR ( 'SYNTAX ERROR AFTER EQUAL SIGN'
     1             , NLINE)
               ENDIF
            ELSEIF (K .EQ. 16) THEN
C
C                    . . . adaptive gridding or not
C
               IF (CH1 .EQ. 'e' .OR. CH1 .EQ. 'E') THEN
                  VARRUN(K) = 1.
               ELSEIF (CH1 .EQ. 'd' .OR. CH1 .EQ. 'D') THEN
                  VARRUN(K) = -1.
               ELSE
                  CALL VARERR ( 'SYNTAX ERROR AFTER EQUAL SIGN'
     1             , NLINE)
               ENDIF
            ELSEIF (K .EQ. 17) THEN
C
C                    . . . shot-noise statistics or none
C
               IF (CH1 .EQ. 's' .OR. CH1 .EQ. 'S') THEN
                  VARRUN(K) = 1.
               ELSEIF (CH1 .EQ. 'n' .OR. CH1 .EQ. 'N') THEN
                  VARRUN(K) = -1.
               ELSE
                  CALL VARERR ( 'SYNTAX ERROR AFTER EQUAL SIGN'
     1             , NLINE)
               ENDIF
            ELSE
C
C            . . . numerical input expected
C
               CALL INTERP (NLINE, INFO, 1, NFOUND, DAT, IER)
               IF (NFOUND .EQ. 1) VARRUN(K) = DAT(1)
            ENDIF
            RETURN
         ENDIF
200   CONTINUE
C
      LFOUND = .FALSE.
      END
      SUBROUTINE KOSCHI (AMPKOS, FJ, I, K, MAXQ, NQ, PARKOS,
     1   POP, PRESS, TEMP, TOTNUM, WDF, WAVEN, FOFF, WIDMUL, WTRAN,
     2   XMODEL, TWOPMP, IPMP, IDGAS, PARPR, NSPEC, XIMAG, XREAL)
C
C______________________________________________________________________
C
C  calculates susceptibility for a single wavenumber and a single
C  transition using exponential gap model (ref 10) for treating
C  pressure narrowing.
C
C    input parameters:
C      ampkos : COMPLEX  amplitude array
C      i   : index of point in spectrum
C      k   : transition number
C      maxq : quantum number limits (vibrational, rotational)
C      nq  : quantum numbers associated with transition
C      parkos : adjustable parameters for exponential gap model
C      wdf : wavenumber
C      waven(1) : wavenumber subtracted from transition
C         frequencies and raman shift
C      wtran : wavenumbers of transitions
C
C    output parameters:
C      xreal : real part of susceptibility
C      ximag : imaginary part of susceptibility
C______________________________________________________________________
C
      PARAMETER (NV = 6, NJ = 130, NW = 1000)
      DIMENSION FJ(0:NV, 0:*), PARKOS(*), NQ(NW, *), POP(0:NV,
     1    0:*), MAXQ(*), WTRAN(*), WAVEN(*), IDGAS(*), PARPR(*)
      DOUBLE PRECISION WAVEN
      COMPLEX AMPKOS(*)
      INTEGER XMODEL
      LOGICAL TWOPMP
C
C  local variables:
C
      DIMENSION IPVT(NJ), WORK(3*NJ)
      COMPLEX AMTX(NJ*NJ), AMTXEG(NJ*NJ), EIG(0:NV, 0:NJ),
     1   EIGV(0:NJ)
      COMPLEX VCT1(0:NV, 0:NJ), VCT2(0:NV, 0:NJ), CPXCHI
      INTEGER VG
      SAVE VCT1, VCT2, EIG
C
C   function npos determines position in the packed 1-d equivalent
C   array to the matrix of order nord of row and column indices
C   j1,j2, each starting from 0.
C
      NPOS(IR, IC, NORD) = IC*NORD+IR+1
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      VG = NQ(K, 1)
      JG = NQ(K, 3)
      MAXJ = MAXQ(2)
      NORDER = MAXJ+1
C
C   only want to do full analysis if doing first point, and then only
C   for first transition of a vibrational manifold. all other calls
C   will use stored results.
C
      IF (I .NE. 1 .OR. XMODEL .NE. 4) GO TO 800
C
      CALL GENM (FJ, K, MAXJ, PARKOS, PRESS, WIDMUL, TEMP, VG,
     1    WTRAN, WAVEN, FOFF, TWOPMP, IPMP, IDGAS, PARPR, NSPEC, AMTX)
C
C  find eigenvalues and eigenvectors of amtx matrix.
C  all matrix manipulation routines are from eispack (ref. 12)
C
C---  cpu timing marker
C
C      CALL CPUTIM (TIME, 'CPU time since login before cgeev is: ')
C
      DATA IJOB/1/
      CALL CGEEV (AMTX, NORDER, NORDER, EIGV, AMTXEG, NORDER,
     1   WORK, IJOB, IER)
      IF (IER .NE. 0) CALL QUITS (IER,
     1   'FLAG RETURNED FROM CGEEV, CALLED FROM KOSCHI' )
C
      DO 300 J = 0, MAXJ
         EIG(VG, J) = EIGV(J)
300   CONTINUE
C
C  invert eigenvector matrix.
C  create dummy, since inversion routine will destroy original,
C  and will need it below
C
      DO 320 NP = 1, NORDER*NORDER
         AMTX(NP) = AMTXEG(NP)
320   CONTINUE
C
C---  cpu timing marker
C
C      CALL CPUTIM (TIME, 'CPU time since login after cgeev is: ')
C
      CALL CGEFA (AMTX, NORDER, NORDER, IPVT, IER)
      IF (IER .NE. 0) CALL QUITS (IER,
     1   'FLAG RETURNED FROM CGEFA, CALLED FROM KOSCHI' )
      DATA JOB/1/
      CALL CGEDI (AMTX, NORDER, NORDER, IPVT, DUM, WORK, JOB)
C
C---  cpu timing marker
C
C      CALL CPUTIM (TIME, 'CPU time since login after cgedi is: ')
C
C  take matrix products.
C
      DO 400 J2 = 0, MAXJ
C
C          initialize sums:
C
         VCT1(VG, J2) = CMPLX(0., 0.)
         VCT2(VG, J2) = CMPLX(0., 0.)
         DO 400 J1 = 0, MAXJ
            NP1 = NPOS(J1, J2, NORDER)
            VCT1(VG, J2) = VCT1(VG, J2)+AMPKOS(K+
     1         J1)*AMTXEG(NP1)
            NP2 = NPOS(J2, J1, NORDER)
            VCT2(VG, J2) = VCT2(VG, J2)+AMTX(NP2)*(POP(VG,
     1         J1)-POP(VG+1, J1))*(2*J1+1)*AMPKOS(K+J1)
400   CONTINUE
C
800   CONTINUE
C
C  use results previously calculated for entire vibrational
C  manifold.
C
      CPXCHI = VCT1(VG, JG)*VCT2(VG, JG)/(-2.*WDF+EIG(VG, JG))
      XREAL  = TOTNUM*REAL(CPXCHI)
      XIMAG  = TOTNUM*AIMAG(CPXCHI)
      END
      LOGICAL FUNCTION LPRIN (GAUSPR, METHPN, N, ITASK, VARFIT)
C
C______________________________________________________________________
C
C   whether to print a given fit variable on menu, for task being
C   done.
C______________________________________________________________________
C
      PARAMETER (NF = 30, NM = 4)
      DIMENSION LPR(NF, 7), VARFIT(*)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      LOGICAL GAUSPR
C
C   order of subscripts is variable #, task #
C
C        task 1, theoretical convolved spectrum:
C
      DATA (LPR(I, 1), I = 1, NF)/0, 1, 0, 0, 1, 1, 1, 1, 19*1, 3*0/
C
C        task 2, compare calculated convolved spectrum with
C        experimental data file:
C
      DATA (LPR(I, 2), I = 1, NF)/27*1, 3*0/
C
C        task 3, least-squares comparison of theory and data
C
      DATA (LPR(I, 3), I = 1, NF)/27*1, 3*0/
C
C        task 4, theoretical susceptibilty calculation
C
      DATA (LPR(I, 4), I = 1, NF)/0, 1, 0, 0, 0, 0, 1, 1, 19*1, 3*0/
C
C        task 5, reference array calculation
C
      DATA (LPR(I, 5), I = 1, NF)/0, 0, 0, 0, 1, 1, 1, 1, 0,
     1   18*1, 3*0/
C
C        task 6, reference array calculation, no data file
C
      DATA (LPR(I, 6), I = 1, NF)/0, 0, 0, 0, 1, 1, 1, 1, 0,
     1   18*1, 3*0/
C
C        task 7, quickfitter library spectra:
C
      DATA (LPR(I, 7), I = 1, NF)/0, 0, 0, 0, 1, 1, 1, 1, 19*1, 3*0/
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      LPRIN  = .FALSE.
C
C      IF (N .EQ. 5) THEN
C
C         . . .  don't print gaussian probe width if gauspr is
C                false (means using instrument function file)
C
C         IF (GAUSPR .AND. (LPR(N, ITASK) .EQ. 1)) LPRIN = .TRUE.
      IF (N .GE. 15 .AND. N .LE. 20) THEN
C
C         . . .   only print delv parameters if mole fraction
C                 of c2h2 is .gt. 0.
C
         DO 100 M = 1, 4
            IF (((GASNAM(M) .EQ. 'C2H2') .AND. (VARFIT(10+M) .GT.
     1         0.)) .AND. (LPR(N, ITASK) .EQ. 1)) LPRIN = .TRUE.
100      CONTINUE
      ELSEIF (N .GE. 21 .AND. N .LE. 24) THEN
C
C         . . . only print exponential gap parameters if methpn=3.
C               use first exponential gap parameter for line
C               shift coefficient if methpn=8.
C
         IF ((METHPN .EQ. 3 .AND. LPR(N, ITASK) .EQ. 1) .OR.
     1      (METHPN .EQ. 8 .AND. LPR(N, ITASK) .EQ. 1 .AND.
     2      N .EQ. 21)) LPRIN = .TRUE.
      ELSEIF (N. EQ. 26) THEN
C
C         . . . only print beta if using galatry model or hard
C               collision model
C
         IF (((METHPN .EQ. 6 ) .OR. (METHPN .EQ. 7) .OR.
     1      (METHPN .EQ. 8)) .AND. LPR(N, ITASK) .EQ. 1)
     2      LPRIN = .TRUE.
      ELSE
         IF (LPR(N, ITASK) .EQ. 1) LPRIN = .TRUE.
      ENDIF
C
      END
      SUBROUTINE LINWID (JG, PRESS, TEMP, TTYPE, MAXQ, M, K,
     1   IDGAS, PARPR, NSPEC, GAMMA)
C
C______________________________________________________________________
C
C  calculates a line width
C  output variable :
C    gamma : line width (fwhm)
C______________________________________________________________________
C
      PARAMETER (NM = 4, NV = 6, NJ = 130)
      DIMENSION MAXQ(2, *), IDGAS(*), PARPR(*)
      CHARACTER*(*) TTYPE
      COMMON /GASNAM/ GASNAM(NM)
      COMMON /FJ/ FJ(0:NV, 0:NJ, NM)
      CHARACTER*5 GASNAM
C
C  local variables:
C
      DIMENSION PN2(0:4), AN2(0:NJ, 0:NJ)
      CHARACTER DUMSTR
      SAVE AN2, A1
C
C  polynomial fit of high temperature data for n2
C  data is in rahn, ref. 13
C
      DATA (PN2(I), I = 0, 4)/0.359068E-1, -0.126174E-2,
     1   0.95990E-4, -0.30278E-5, 0.300934E-7/
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      character(len=:), allocatable :: gas
      gas = trim(adjustl(GASNAM(IDGAS(M))))

      IF (GASNAM(IDGAS(M)) .EQ. 'N2') THEN
         MAXJN2 = MAXQ(2, M)
C
         IF ((JG .EQ. 0) .OR. (K .EQ. 1)) THEN
            A1 = 2.*0.0231*PRESS*SQRT(295./TEMP)*
     1           (1.-EXP(-0.1487))/(1.-EXP(-0.1487*TEMP/295.))
C
            DO 400 I = 0, MAXJN2-2
               EI = FJ(0, I, IDGAS(M))
C
               DO 500 J = I+2, MAXJN2, 2
                  EJ = FJ(0, J, IDGAS(M))
                  EXPON = (EJ-EI)*1.4384/TEMP
                  AD = (EI*1.5*1.4384/TEMP)
                  AN2(J, I) = EXP(-1.67*EXPON)*((1.+AD/
     1                        (1.21))/(1.+AD))**2
                  AN2(I, J) = AN2(J, I)*(2.*FLOAT(I)+1.)/
     1                        (2.*FLOAT(J)+1.)*EXP(EXPON)
500            CONTINUE
C
400         CONTINUE
C
         ENDIF
C
C---  generate linewidth (FWHM).
C
         GAMMA = 0.
C
         DO 700 I = MOD(JG,2), MAXJN2, 2
            IF (I .NE. JG) GAMMA = GAMMA+AN2(I, JG)
700      CONTINUE
C
         GAMMA = A1*GAMMA
      ELSEIF (GASNAM(IDGAS(M)) .EQ. 'H2') THEN
C
         DATA H2ALPH, H2BETA, H2DELT, H2TEXP, H2YOFF, H2EGAP
     1      / .012677, 1.8096, 0.51335, 0.47312, 0.0012903, 2.67 /
         MAXJN2 = MAXQ(2, M)
C
         IF ((JG .EQ. 0) .OR. (K .EQ. 1)) THEN
            A1 = PRESS*SQRT(295./TEMP)*(1.-EXP(-H2TEXP))/
     1           (1.-EXP(-H2TEXP*TEMP/295.))
C
            DO 100 I = 0, MAXJN2-2
               EI = FJ(0, I, IDGAS(M))
C
               DO 200 J = I+2, MAXJN2, 2
                  EJ = FJ(0, J, IDGAS(M))
                  EXPON = (EJ-EI)*1.4384/TEMP
                  AN2(J, I) = EXP(-H2BETA*EXPON)
                  R1 = H2EGAP*EI*1.4384/TEMP
                  AN2(J, I) = AN2(J, I)*(1.+R1/(H2DELT*TEMP/
     1                        295.))**2/(1.+R1)**2
                  AN2(I, J) = AN2(J, I)*(2.*FLOAT(I)+1.)/
     1                        (2.*FLOAT(J)+1.)*EXP(EXPON)
200            CONTINUE
C
100         CONTINUE
C
         ENDIF
C
C---  generate linewidth (FWHM).
C
         GAMMA = 0.
C
         DO 300 I = MOD(JG,2), MAXJN2, 2
            IF (I .NE. JG) GAMMA = GAMMA+AN2(I, JG)
300      CONTINUE
C
         GAMMA = A1*2.*H2ALPH*GAMMA+PRESS*H2YOFF
C
      ELSEIF (GASNAM(IDGAS(M)) .EQ. 'O2') THEN
C
C          . . use n2 q-branch linewidths for now
C
         IF (TEMP .GE. 600.) THEN
            CONS = PRESS*52.512/SQRT(TEMP)
            TJ   = JG
            IF (TJ .GT. 40) TJ = 40
            GAMMA = CONS*(PN2(0)+TJ*(PN2(1)+TJ*(PN2(2)+
     1      TJ*(PN2(3)+TJ*PN2(4)))))
         ELSE
            C1    = PRESS*760./255.
            C2    = SQRT(290./TEMP)
            CONS  = C1*C2/1000.
            DATA WM/-0.915/, WB/39.8/
            TJ    = JG
            IF (TJ .GT. 30.) TJ = 30.
            GAMMA = (WM*TJ+WB)*CONS
         ENDIF
      ELSEIF (GASNAM(IDGAS(M)) .EQ. 'CO') THEN
         MAXJN2 = MAXQ(2, M)
C
         IF ((JG .EQ. 0) .OR. (K .EQ. 1)) THEN
            A1 = 2.*0.01337*PRESS*SQRT(295./TEMP)*
     1           (1.-EXP(-0.185))/(1.-EXP(-0.185*TEMP/295.))
C
            DO 900 I = 0, MAXJN2-1
               EI = FJ(0, I, IDGAS(M))
C
               DO 800 J = I+1, MAXJN2
                  EJ = FJ(0, J, IDGAS(M))
                  EXPON = (EJ-EI)*1.4384/TEMP
                  AD = (EI*2.1*1.4384/TEMP)
                  AN2(J, I) = EXP(-1.507*EXPON)*((1.+AD/
     1                        (1.205))/(1.+AD))**2
                  AN2(I, J) = AN2(J, I)*(2.*FLOAT(I)+1.)/
     1                        (2.*FLOAT(J)+1.)*EXP(EXPON)
800            CONTINUE
C
900         CONTINUE
C
         ENDIF
C
C---  generate linewidth (FWHM).
C
         GAMMA = 0.
C
         DO 600 I = 0, MAXJN2
            IF (I .NE. JG) GAMMA = GAMMA+AN2(I, JG)
600      CONTINUE
C
         GAMMA = A1*GAMMA
      ELSEIF (GASNAM(IDGAS(M)) .EQ. 'C2H2') THEN
C
C---  acetylene line widths from ref. 16
C
         GAMMA = SQRT(293./TEMP)*(0.21*PRESS+0.01)
      ELSEIF (gas == 'CO2') THEN
C
C---  co2 line widths from ref. 31
C
         GAMMA = 0.24*PRESS*(300./TEMP)**0.75
      ELSEIF (GASNAM(IDGAS(M)) .EQ. 'H2O') THEN
C
C---  approximate water vapor linewidth
C
         GAMMA = 0.372*(295./TEMP)**0.8
C
C---  comment out following line to remove h2o routine
C
C         CALL H2OGAM (JG, PRESS, TEMP, K, MAXQ, GAMMA)
      ELSEIF (GASNAM(IDGAS(M)) .EQ. 'HCN') THEN
C
C---  hcn line widths from m. maroncelli, g. a. hopkins, and j. w.
C---  nibler, j. chem. phys. 83, 2129 (1985).
C
         GAMMA = 1.8*PRESS
      ELSE
         PRINT '(//2A)', ' Linewidths NOT available for ',
     1      GASNAM(IDGAS(M))
         DO i = 1, LEN(GASNAM(IDGAS(M)))
            PRINT '(I4,1X,A1)', IACHAR(GASNAM(IDGAS(M))(i:i)),
     1      GASNAM(IDGAS(M))(i:i)
         END DO
         CALL QUITS (0, DUMSTR)
      ENDIF
C
      END
      SUBROUTINE LOGO
C
C______________________________________________________________________
C
C   prints logo
C______________________________________________________________________
C
      COMMON /VERZUN/ UPDATE, CLOK, CDAT, CRAY
      CHARACTER*8 UPDATE, CLOK, CDAT, CRAY
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      PRINT *, ' '
      PRINT *, ' '
      PRINT *, ' '
      PRINT *,
     1'    CCCCC     AAAA     RRRRRR     SSSS    FFFFFFF IIII  TTTTTTTT'
      PRINT *,
     1'   CC    C   AA  AA    RR   RR   SS   S   FF       II      TT'
      PRINT *,
     1'  CC        AA    AA   RR   RR   SS       FF       II      TT'
      PRINT *,
     1'  CC        AAAAAAAA   RRRRRR     SSSS    FFFF     II      TT'
      PRINT *,
     1'  CC        AA    AA   RR  RR        SS   FF       II      TT'
      PRINT *,
     1'   CC    C  AA    AA   RR   RR   S   SS   FF       II      TT'
      PRINT *,
     1'    CCCCC   AA    AA   RR    RR   SSSS    FF      IIII     TT'
      PRINT *, ' '
      PRINT '(T22, A, A)' , 'Version ' , UPDATE
      PRINT *,
     1'================================================================'
      PRINT *, ' '
      END
      SUBROUTINE MAKCOM (ATHETP, CREFP, CHNRPP, DBLCOP, GAUSPP,
     1   WTRANP, IBEGP, IDGASP, LOFFQP, NQP, NDATP, NPLP,
     2   NSPECP, NTHEP, NTP, REFTEP, REFPR, T11P, TTYPP, USEREP,
     3   WDATP, XMODP, DEGSAP, WADDP, WLILP, WBIGP, WLOP, WHIP,
     4   LSCNVP, APSIP, TWOPP, SPCPRP, ADAPTP, NTHE1P, MAXQP,
     5   SHOTP)
C
C______________________________________________________________________
C
C  creates common for communication to fitres, the residual routine
C______________________________________________________________________
C
      PARAMETER (NM = 4, NW = 1000, ND = 5000)
C
      DIMENSION CREFP(*), WTRANP(NW, NM, *), IDGASP(*), NQP(NW, 4, NM,
     1   *), NTP(NM, *), T11P(NW, 2, NM, 2), WDATP(*), MAXQP(2, *)
      DOUBLE PRECISION WDATP
      LOGICAL DBLCOP, LOFFQP(2), USEREP, GAUSPP, LSCNVP, TWOPP, SPCPRP,
     1   ADAPTP, SHOTP
      INTEGER XMODP(NW, NM, *)
      CHARACTER*(*) TTYPP(NW, NM, 2)
C
      COMMON /XMOD/ XMODEL(NW, NM, 2)
      COMMON /TTYP/ TTYPE(NW, NM, 2)
      COMMON /CDR/ ATHETA, CHIREF(ND), CHNRPM, DBLCON, GAUSPR,
     1   WTRAN(NW, NM, 2), IBEG, IDGAS(NM), LOFFQ, NQ(NW, 4, NM, 2),
     2   NDAT, NSPEC, NTHE, NPL, NT(NM, 2), USEREF,
     3   REFTEM, REFP, T11(NW, 2, NM, 2), DEGSAT, WADD,
     4   WLIL, WBIG, WLO, WHI, LSRCNV, APSI, TWOPMP, SPCPRB,
     5   ADAPTV, NTHE1, MAXQ(2, NM), SHOTST
      COMMON /CDR1/ WDAT(ND)
      DOUBLE PRECISION WDAT
      LOGICAL DBLCON, GAUSPR, LOFFQ(2), USEREF, LSRCNV, TWOPMP,
     1   SPCPRB, ADAPTV, SHOTST
      INTEGER XMODEL
      CHARACTER TTYPE*1
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      ATHETA = ATHETP
      CHNRPM = CHNRPP
      DBLCON = DBLCOP
      GAUSPR = GAUSPP
      IBEG   = IBEGP
C      LOFFQ  = LOFFQP
      NDAT   = NDATP
      NPL    = NPLP
      NSPEC  = NSPECP
      NTHE   = NTHEP
      USEREF = USEREP
      REFTEM = REFTEP
      REFP   = REFPR
      DEGSAT = DEGSAP
      WADD   = WADDP
      WLIL   = WLILP
      WBIG   = WBIGP
      WLO    = WLOP
      WHI    = WHIP
      LSRCNV = LSCNVP
      APSI   = APSIP
      TWOPMP = TWOPP
      SPCPRB = SPCPRP
      ADAPTV = ADAPTP
      NTHE1  = NTHE1P
      SHOTST = SHOTP
C
C  following arrays are stacked, so need fill to only nspec
C
      DO 250 M = 1, NSPEC
         IDGAS(M) = IDGASP(M)
         MAXQ(1, M) = MAXQP(1, M)
         MAXQ(2, M) = MAXQP(2, M)
      DO 250 IPMP = 1, 2
         LOFFQ(IPMP) = LOFFQP(IPMP)
         NT(M, IPMP) = NTP(M, IPMP)
         DO 100 N = 1, 2
            DO 100 K = 1, NT(M, IPMP)
               T11(K, N, M, IPMP) = T11P(K, N, M, IPMP)
100      CONTINUE
         DO 110 K = 1, NT(M, IPMP)
            WTRAN(K, M, IPMP) = WTRANP(K, M, IPMP)
            XMODEL(K, M, IPMP) = XMODP(K, M, IPMP)
            TTYPE(K, M, IPMP)  = TTYPP(K, M, IPMP)
110      CONTINUE
         DO 115 L = 1, 4
            DO 115 K = 1, NT(M, IPMP)
               NQ(K, L, M, IPMP) = NQP(K, L, M, IPMP)
115      CONTINUE
250   CONTINUE
C
      DO 300 I = 1, NDAT
         CHIREF(I) = CREFP(I)
         WDAT(I) = WDATP(I)
300   CONTINUE
C
      END
      SUBROUTINE MAKPRB (GAMPR, NTHE, WLO, WHI, TINSTR)
C
C______________________________________________________________________
C
C   calculates non-gaussian probe instrument function,
C   normalizes and transforms it
C     input parameters:
C       gampr - width of probe instrument function
C       nthe - number points in range
C       wlo, whi - limits of wavenumber calculation range
C     output parameters:
C       tinstr - hartley transformed instrument function at uniformly
C       spaced wavenumbers over double interval (2*nthe points)
C______________________________________________________________________
C
      PARAMETER (NTM = 32*16384, NTM1 = NTM/2)
      DIMENSION TINSTR(*)
C
C  local variables
C
C---  common shared with subroutines calcon and fitcal
C
      COMMON /COMM3/ PEVEN(4*NTM+5*NTM1)
      DIMENSION WPR(1000)
      DOUBLE PRECISION WINT, DW, WPR
C
C---  generate probe instrument function with nprb points
C
      DATA NPRB / 1000 /
      CALL PRBFNC (NPRB, GAMPR, WPR, TINSTR)
      CALL NRMPRB (NPRB, WPR, TINSTR)
C
C---  produce interpolated curve at evenly
C---  spaced points.  call interpolated probe intensity peven.
C
      DW = (WHI-WLO)/FLOAT(NTHE-1)
C
C---  add 1 to neven if closer to next integer
C
      NEVEN = INT((WPR(NPRB)-WPR(1))/DW+1)
      IF (NEVEN .GT. NTM) THEN
	   PRINT*, NEVEN,' NEEDED, ',NTM,' AVAILABLE.'
	   CALL QUITS (-1,
     1'!! ERROR - MUST INCREASE DIMENSIONS OF PEVEN, ROUTINE MAKPRB !!')
	 ENDIF
      PEVEN(1) = TINSTR(1)
      WINT = WPR(1)
C
      DO 200 N = 2, NEVEN
         WINT = WINT+DW
         CALL TERPLD (NPRB, WPR, TINSTR, WINT, PEVEN(N), IER)
200   CONTINUE
C
C---  locate maximum of curve, so can be located at index 1
C
      PMAX = 0.
C
      DO 240 N = 1, NEVEN
         IF (PEVEN(N) .GT. PMAX) THEN
            PMAX = PEVEN(N)
            NMAX = N
         ENDIF
240   CONTINUE
C
C---  form instrument function to be transformed . . .
C---  first part of array is all points including and to right of
C---  peak
C
      NRIGHT = NEVEN-NMAX+1
      IND    = NMAX
C
      DO 260 N = 1, NRIGHT
         TINSTR(N) = PEVEN(IND)
         IND       = IND+1
260   CONTINUE
C
      NEXT = NRIGHT+1
C
C---  fill array with zeroes until indnex, where will begin left
C---  part of curve. nleft is number points in left part
C
      NLEFT = NEVEN-NRIGHT
      NDBL  = 2*NTHE
      INDNEX = NDBL-NLEFT+1
C
      DO 280 N = NEXT, INDNEX-1
         TINSTR(N) = 0.
280   CONTINUE
C
C---  fill rest of array with left part, beginning at index indnex
C
      IND = 1
C
      DO 300 N = INDNEX, NDBL
         TINSTR(N) = PEVEN(IND)
         IND       = IND+1
300   CONTINUE
C
C---  transform curve
C
      CALL FHT (TINSTR, NDBL)
C
C---  normalize to max of 1.0 (I'm not sure why this works - it is
C---  done to make it agree with the gaussian produced by cgauss.
C---  ran out of time to investigate this question . . G.C.)
C
      CALL MNMAX (TINSTR, 2, NDBL, 1, DUM, TMAX)
C
      DO 310 N = 1, NDBL
         TINSTR(N) = TINSTR(N)/TMAX
310   CONTINUE
C
      END
      SUBROUTINE MINGAM (GAMMA, NSPEC, NT, TEMP, WLIL, WBIG, IDGAS,
     1   DOPPLR, GAMMIN)
C
C______________________________________________________________________
C
C  get minimum line width to use in determining grid size
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NM = 4)
      DIMENSION GAMMA(NW, NM, *), NT(NM, *), IDGAS(*)
      LOGICAL DOPPLR
      COMMON /PARMOL/ PARMOL(12, NM)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM, GASNM
C
C---  include estimate for doppler HALF width, assuming average
C---  transition freqeuency.  estimate total width crudely by larger
C---  of doppler width and collisional width.  use doppler half width
C---  instead of full width to get more points across gaussian
C---  profile.  should be sufficient for determining wavenumber grid
C---  for calculation, except for hydrogen due to narrowing.
C
      ALPH = 4.3014E-7*((WLIL+WBIG)/2.)*SQRT(TEMP)
      GAMMIN = 1.E32
C
      DO 100 M = 1, NSPEC
      DO 100 IPMP = 1, 2
C
         GAMTMP = 1.E32
         DO 200 K = 1, NT(M, IPMP)
         IF (GAMMA(K, M, IPMP) .LT. GAMTMP) GAMTMP = GAMMA(K, M, IPMP)
200      CONTINUE
C
         ALPHA = ALPH/SQRT(PARMOL(10, IDGAS(M)))
         GASNM = GASNAM(IDGAS(M))
         IF (ALPHA .LT. GAMMIN .AND. ALPHA .GT. GAMTMP .AND.
     1      GASNM .NE. 'H2' .AND. GASNM .NE. 'D2' .AND.
     2      GASNM .NE. 'HD') THEN
            GAMMIN = ALPHA
            DOPPLR = .TRUE.
            MMIN = M
         ELSEIF (GAMTMP .LT. GAMMIN) THEN
            GAMMIN = GAMTMP
            DOPPLR = .FALSE.
         ELSE
            DOPPLR = .FALSE.
         ENDIF
100   CONTINUE
C
      CALL DIVZER (GAMMIN, 2, 'MINGAM' , 'GAMMIN' )
C
      IF (DOPPLR) PRINT '(/3A)',
     1      ' . . . Doppler width of ', GASNAM(IDGAS(MMIN)),
     2      ' used to determine grid size'
C
      END
      SUBROUTINE MINMX (ARRAY, NPTS, BMIN, BMAX)
C
C______________________________________________________________________
C
C  finds minimum and maximum of arrays
C______________________________________________________________________
C
      DIMENSION ARRAY(NPTS)
C
      BMIN = ARRAY(1)
      BMAX = BMIN
C
      DO 100 I = 2, NPTS
        BMIN = AMIN1(BMIN, ARRAY(I))
        BMAX = AMAX1(BMAX, ARRAY(I))
100   CONTINUE
C
      END
      SUBROUTINE MOLIN (AC1, AG, ALPHAE, ANU4, ANU5, GAM, BE,
     1   BETAE, DGAMDR, DE, DELTE, GAME, GNE, GNO, H0, HE, RE,
     2   WE, WX, WY, WZ, CMASS)
C
C______________________________________________________________________
C
C  reads in molecular parameters
C______________________________________________________________________
C
      PARAMETER (NM = 4, NF = 30, NR = 17)
      DIMENSION AC1(*), AG(*), ALPHAE(*), GAM(*), BE(*),
     1   BETAE(*), DGAMDR(*), DE(*), DELTE(*), GAME(*), GNE(*),
     2   GNO(*), H0(*), HE(*), RE(*), WE(*), WX(*), WY(*),
     3   WZ(*), CMASS(*)
      COMMON /DFSPHI/ DFSPHI(NM)
      COMMON /CHINR/ CHINR(NM)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMRUN*40, NAMFIT*40
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      OPEN (15, FILE = 'cars.mol', STATUS = 'OLD')
      REWIND 15
      GO TO 110
110   CONTINUE
C
      READ (15, '(4(12X,A5))' ) (GASNAM(M), M = 1, NM)
C
      DO 120 M = 1, NM
         NAMFIT(10+M) = GASNAM(M)(1:5) // 'mole fraction'
120   CONTINUE
C
      READ (15, 1000) (WE(M), M = 1, NM)
      READ (15, 1000) (WX(M), M = 1, NM)
      READ (15, 1000) (WY(M), M = 1, NM)
      READ (15, 1000) (WZ(M), M = 1, NM)
      READ (15, 1000) (BE(M), M = 1, NM)
      READ (15, 1000) (ALPHAE(M), M = 1, NM)
      READ (15, 1000) (DE(M), M = 1, NM)
      READ (15, 1000) (BETAE(M), M = 1, NM)
      READ (15, 1000) (GAME(M), M = 1, NM)
      READ (15, 1000) (DELTE(M), M = 1, NM)
      READ (15, 1000) (H0(M), M = 1, NM)
      READ (15, 1000) (HE(M), M = 1, NM)
      READ (15, 1000) (RE(M), M = 1, NM)
      READ (15, 1000) (GAM(M), M = 1, NM)
      READ (15, 1000) (DGAMDR(M), M = 1, NM)
      READ (15, 1000) (GNE(M), M = 1, NM)
      READ (15, 1000) (GNO(M), M = 1, NM)
      READ (15, 1000) (DFSPHI(M), M = 1, NM)
      READ (15, 1000) (AG(M), M = 1, NM)
      READ (15, 1000) (CHINR(M), M = 1, NM)
      READ (15, 1000) (AC1(M), M = 1, NM)
      READ (15, 1000) (CMASS(M), M = 1, NM)
      READ (15, 1010, END = 130, ERR = 130) ANU4
      READ (15, 1010) ANU5
C
130   CLOSE (15)
C
1000  FORMAT(7X, 4E17.0)
1010  FORMAT(58X, E17.0)
      END
      SUBROUTINE MOLPAR
C
C______________________________________________________________________
C
C   reads molecular data and calculates parameters needed in
C   calculations
C        cani  :  centrifugal force correction to intensity
C        caniq :     "          "       "       "     "
C        fj    :  rotational term values for partition function
C        gj    :  degeneracy factors for nuclear spin
C        g0v   :  vibrational energy levels (term values)
C        termm :  term energies
C______________________________________________________________________
C
      PARAMETER (NM = 4, NJ = 130, NV = 6)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      COMMON /PARMOL/ PARMOL(12, NM)
      COMMON /CHINR/ CHINR(NM)
      COMMON /FJ/ FJ(0:NV, 0:NJ, NM)
      COMMON /GJ/ GJ(0:NJ, NM)
      COMMON /G0V/ G0V(0:NV, NM)
      COMMON /TERMM/ TERMM(0:NV, 0:NJ, NM)
C
C  local variables:
C
      DIMENSION AC1(NM), ALPHAE(NM), AG(NM), BE(NM), BETAE(NM),
     1   CANI(NM), CANIQ(NM), DE(NM), DELTE(NM), DGAMDR(NM),
     2   GAM(NM), GAME(NM), GNE(NM), GNO(NM), H0(NM), HE(NM),
     3   RE(NM), WE(NM), WX(NM), WY(NM), WZ(NM), CMASS(NM)
      INTEGER V
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL MOLIN (AC1, AG, ALPHAE, ANU4, ANU5, GAM, BE, BETAE,
     1   DGAMDR, DE, DELTE, GAME, GNE, GNO, H0, HE, RE, WE, WX,
     2   WY, WZ, CMASS)
C
C---  convert chinr to susceptibility per molecule
C
      DO 200 M = 1, NM
         CHINR(M)     = CHINR(M)/(AVAG/VSTP)
         IF (GASNAM(M) .EQ. 'CO2') THEN
C
C---  the resonant terms for co2 are done elsewhere.  the nonresonant
C---  susceptibility for co2 is 12.59.
C
         ELSEIF (GASNAM(M) .EQ. 'H2O') THEN
C
C---  the resonant terms for h2o are done elsewhere.  the nonresonant
C---  susceptibility for h2o is 18.5.
C
            CONTINUE
         ELSE
            W0 = WE(M)-WX(M)+0.75*WY(M)
            W0X0 = WX(M)-1.5*WY(M)
            W0Y0 = WY(M)
C
C---  even and odd terms for gj:
C
            DO 110 J = 0, NJ, 2
               GJ(J, M) = GNE(M)
110         CONTINUE
            DO 120 J = 1, NJ, 2
               GJ(J, M) = GNO(M)
120         CONTINUE
C
            CANI(M) = 4.*BE(M)*GAM(M)/(WE(M)*DGAMDR(M)*RE(M))
            CANIQ(M) = 6.*(BE(M)/WE(M))**2
C
            DO 130 V = 0, NV
               G0V(V, M) = W0*V-W0X0*V**2+W0Y0*V**3
130         CONTINUE
         ENDIF
200   CONTINUE
C
C---  rotational term values and term energies
C
      DO 300 M = 1, NM
         IF (GASNAM(M) .EQ. 'CO2') THEN
            CONTINUE
         ELSE
            DO 310 V = 0, NV
               VPH = FLOAT(V)+.5
               DO 310 J = 0, NJ
                  RJJP1 = FLOAT(J*(J+1))
                  C1 = BE(M)+VPH*(-ALPHAE(M)+VPH*GAME(M))
                  C2 = -(DE(M)+VPH*(BETAE(M)+VPH*DELTE(M)))
                  C3 = H0(M)+VPH*HE(M)
                  IF (GASNAM(M) .EQ. 'H2') THEN
C
C                 . . . use 5th-order polynomial for h2
C
                     DATA C4/-6.3183E-8/, C5/6.33551E-11/
                     FJ(V, J, M) = RJJP1*(C1+RJJP1*(C2+RJJP1*
     1                  (C3+RJJP1*(C4+RJJP1*C5))))
                  ELSE
                     FJ(V, J, M) = RJJP1*(C1+RJJP1*(C2+RJJP1*C3))
                  ENDIF
                  TERMM(V, J, M) = FJ(V, J, M)+WE(M)*VPH-
     1               WX(M)*VPH**2+WY(M)*VPH**3+WZ(M)*VPH**4
310         CONTINUE
         ENDIF
300   CONTINUE
C
C---  put molecular parameters to be used later in parmol:
C
      DO 400 M = 1, NM
         PARMOL(1, M) = AC1(M)
         PARMOL(2, M) = AG(M)
         PARMOL(3, M) = BE(M)
         PARMOL(4, M) = CANI(M)
         PARMOL(5, M) = CANIQ(M)
         PARMOL(6, M) = DGAMDR(M)
         PARMOL(7, M) = GAM(M)
         PARMOL(8, M) = RE(M)
         PARMOL(9, M) = WE(M)
         PARMOL(10, M) = CMASS(M)
         PARMOL(11, M) = 0.
         PARMOL(12, M) = 0.
400   CONTINUE
C
C---  acetylene constants
C
      PARMOL(11, 4) = ANU4
      PARMOL(12, 4) = ANU5
C
      END
      SUBROUTINE N2BAND (IDGAS1, LOFFQ, NQ, NT, TEMP, WBIG,
     1   WLIL, WTRAN, FOFF)
C
C______________________________________________________________________
C
C  if bandhead is outside of range, include it as last
C  transition and set loffq flag.
C______________________________________________________________________
C
      PARAMETER (NM = 4, NJ = 130, NV = 6, NW = 1000)
      DIMENSION NQ(NW, 4, NM, *), NT(NM, *), WTRAN(NW, NM, *)
      LOGICAL LOFFQ(2)
      COMMON /PARMOL/ PARMOL(12, NM)
      COMMON /TERMM/ TERMM(0:NV, 0:NJ, NM)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DO 100 IPMP = 1, 2
      IF (GASNAM(IDGAS1) .EQ. 'N2') THEN
         BEN2 = PARMOL(3, 1)
C
C---  jpeak is effective transition quantum number.
C
         JPEAK = 0.5896*SQRT(TEMP/BEN2)-0.5
         IF (IPMP .EQ. 1) THEN
            TD = TERMM(1, JPEAK, 1)-TERMM(0, JPEAK, 1)
         ELSE
            TD = TERMM(1, JPEAK, 1)-TERMM(0, JPEAK, 1) + FOFF
         ENDIF        
         IF ((TD .LT. WLIL) .OR. (TD .GT. WBIG)) THEN
            LOFFQ(IPMP) = .TRUE.
            NT(1, IPMP) = NT(1, IPMP) + 1
            NQ(NT(1, IPMP), 1, 1, IPMP) = 0
            NQ(NT(1, IPMP), 2, 1, IPMP) = 1
            NQ(NT(1, IPMP), 3, 1, IPMP) = JPEAK
            NQ(NT(1, IPMP), 4, 1, IPMP) = JPEAK
            WTRAN(NT(1, IPMP), 1, IPMP) = TD
         ENDIF
      ENDIF
100   CONTINUE      
C
      END
      SUBROUTINE NCONPT (NLINE, NTHE)
C
C______________________________________________________________________
C
C  determines number of points in fast transform, based on
C  number points necessary to resolve smallest feature
C  uses next power of 2 above nline
C______________________________________________________________________
C
      PARAMETER (NTM = 32*16384)
C
C---  nmax = log2(ntm)
C
      NMAX = INT(ALOG10(FLOAT(NTM))/ALOG10(2.)+0.1)
      NTHE = 1
C
      DO 100 N = 1, NMAX
         NTHE = NTHE*2
         IF (NTHE .GE. NLINE) RETURN
100   CONTINUE
C
      END
      SUBROUTINE NONRES (APHI, ATHETA, CHINR, CHNRPM, IDGAS,
     1   NSPEC, PARPR, TOTNUM, APSI, TWOPMP, YRN)
C
C______________________________________________________________________
C
C  calculates nonresonant contribution to susceptibility. (yrn)
C______________________________________________________________________
C
      DIMENSION CHINR(*), IDGAS(*), PARPR(*)
      LOGICAL TWOPMP
C
      IF (TWOPMP) THEN
C
C---  three-color cars
C     Use new factor of 0.5 because three-color cars is now calculated
C     using two separate susceptibilities, one for each pump, where
C     YRN is included in each.  Previously, one susceptibility was used
C     with the pump offset added to Raman shifts of species 3.
 
         TNRES = 0.5 * (COS(APHI)*COS(APSI-ATHETA)+COS(APSI-APHI)*
     1           COS(ATHETA)+COS(APHI-ATHETA)*COS(APSI))/3.
      ELSE
C
C---  two-color cars
C
         TNRES = SIN(ATHETA)*SIN(APHI)/3.+COS(ATHETA)*COS(APHI)
      ENDIF
C
      YRN   = 0.
      PANR  = 1.
C
      DO 100 M = 1, NSPEC
         IDG = IDGAS(M)
         PANR = PANR-ABS(PARPR(IDG))
         YRN  = YRN+TNRES*CHINR(IDG)*TOTNUM*ABS(PARPR(IDG))
100   CONTINUE
C
C---  non-resonant background from unspecified gases
C
      IF (PANR .GT. 0.) YRN = YRN+TNRES*PANR*TOTNUM*CHNRPM
      END
      SUBROUTINE NORML (CHIEXP, CHICON, CHIREF, IBEG, NDAT, NPL,
     1    USEREF, REFTEM, TEMP, REFP, PRESS, WDAT, WPL, CHNORM, CHIMAX)
C
C______________________________________________________________________
C
C  normalizes theoretical convolved susceptibility so it can
C  be compared to data.
C   output variables:
C     chnorm - the normalized version of chicon
C     chimax  - maximum of calculated spectrum without
C               expansion factor.
C______________________________________________________________________
C
      DIMENSION CHICON(*), CHNORM(*), CHIREF(*), WDAT(*),
     1   WPL(*)
      DOUBLE PRECISION WDAT, WPL
      LOGICAL USEREF
C
      IF (USEREF) THEN
C
C            . . . include correction for reference temp and pressure,
C                  and modify peak value by 1/chiexp
C
         RAT = (TEMP/REFTEM)*(REFP/PRESS)/CHIEXP
C
         DO 210 I = 1, NPL
C
C---  interpolate to get reference intensity corresponding to each
C---  wavenumber wpl(i)
C
            CALL TERPLD (NDAT, WDAT, CHIREF, WPL(I), CREF, IER)
            CHNORM(I) = CHICON(I)*RAT/CREF
210      CONTINUE
         CALL MNMAX (CHNORM, 2, NPL, 1, DUM, CHIMAX)
         CHIMAX = CHIMAX*CHIEXP
C
      ELSE
C
C            . . . normalize peak of theory to 1./chiexp
C                  this will be reversed for plotting
C                  keep chimax intact for later.
C
         CALL MNMAX (CHICON, 2, NPL, 1, DUM, CHIMAX)
         CALL DIVZER (CHIMAX, 3, 'NORML' , 'CHIMAX' )
C
         RAT    = 1./(CHIEXP*CHIMAX)
         DO 240 I = 1, NPL
            CHNORM(I) = CHICON(I)*RAT
240      CONTINUE
C
      ENDIF
C
      END
      SUBROUTINE NRINIT (APHI, ATHETA, CHIOFF, GAINF, NBRNCH,
     1   NREJEC, WIDMUL, FOFF, APSI)
C
C______________________________________________________________________
C
C  initializes variables needed in susceptibility calculation when
C  doing nonresonant array.
C______________________________________________________________________
C
      PARAMETER (NM = 4)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
      IF (NBRNCH .EQ. 1) THEN
C
C           . . . 0-branch rejection
C
         DEPOL = 0.75
      ELSEIF (NBRNCH .EQ. 2) THEN
C
C           . . . q-branch rejection
C
         IF (GASNAM(NREJEC) .EQ. 'N2') THEN
            DEPOL = 0.021995
         ELSEIF (GASNAM(NREJEC) .EQ. 'CO') THEN
            DEPOL = 0.0380
         ELSEIF (GASNAM(NREJEC) .EQ. 'H2') THEN
            DEPOL = 0.0125
         ELSEIF (GASNAM(NREJEC) .EQ. 'O2') THEN
            DEPOL = 0.047
         ELSEIF (GASNAM(NREJEC) .EQ. 'CO2') THEN
            DEPOL = 0.027
         ELSEIF (GASNAM(NREJEC) .EQ. 'H2O') THEN
            DEPOL = 0.025
         ELSEIF (GASNAM(NREJEC) .EQ. 'C2H2') THEN
            DEPOL = 0.061
         ELSE
            DEPOL = 0.02
         ENDIF
      ELSE
         DEPOL = 0.02
      ENDIF
C
      TANPHI = -COS(ATHETA)/(SIN(ATHETA)*DEPOL)
      APHI   = ATAN(TANPHI)
      CHIOFF = 0.
      GAINF  = 1.
      WIDMUL = 1.
      FOFF   = 0.
      APSI   = 0.
      RETURN
1000  FORMAT('   (' , I1, ') ' , A)
      END
      SUBROUTINE NRMPRB (NINS, WPR, TINSTR)
C
C______________________________________________________________________
C  normalizes arbitrary probe laser instrument function to unit area
C    parameters:  nins = number points found
C                 wpr  = wavenumber array read
C                 tinstr = instrument function as read
C______________________________________________________________________
C
      DIMENSION WPR(*), TINSTR(*)
      DOUBLE PRECISION WPR
C
      ANORM = 0.
      DO 100 I = 2, NINS
         ANORM = ANORM+(TINSTR(I)+TINSTR(I-1))*(WPR(I)-WPR(I-1))
100   CONTINUE
      ANORM = ANORM/2.
C
      DO 200 I = 1, NINS
         TINSTR(I) = TINSTR(I)/ANORM
200   CONTINUE
C
      END
      SUBROUTINE OPDATF (DATFIL, REC2, SAMFIL, ITITLE)
C
C______________________________________________________________________
C
C  opens data file (unit 10)
C    (VAX version)
C______________________________________________________________________
C
      LOGICAL SAMFIL
      CHARACTER* (*)REC2, DATFIL, ITITLE
C
C  local variables
C
      SAVE OLDFIL
      CHARACTER DUMSTR, OLDFIL*40, PROMP(4)*80
      DATA OLDFIL/ ' ' /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
50    CONTINUE
C
      IF (DATFIL .EQ. OLDFIL) THEN
         SAMFIL = .TRUE.
C
C          . . . same file was previously opened.  rewind and
C            read first two records anyway, as there are conditions
C            under which file will be read anyway.  error condition
C            will exist if main program closed logical unit 10 to
C            cause prompt for new file name.
C
         REWIND (10, ERR = 140)
         READ (10, '(A)' , ERR = 140) ITITLE
         READ (10, '(A)' ) REC2
         RETURN
      ENDIF
C
      SAMFIL = .FALSE.
      OPEN (10, FILE = DATFIL, STATUS = 'OLD', ERR = 60,
     1   IOSTAT = IOS)
      GO TO 80
60    CONTINUE
      PRINT '(3A)', ' !! ERROR - CANNOT OPEN FILE ' , DATFIL,
     1   ' !!'
      GO TO 150
C
80    CONTINUE
      OLDFIL = DATFIL
      READ (10, '(A)' ) ITITLE
      LDF = LENTH(DATFIL)
      REC2 = ' FILE IDENTIFICATION HEADER FOR FILE ' //
     1   DATFIL(1:LDF) // ':'
	CALL CLEAR_TEXT ()
      PRINT '(/A)' , REC2
      PRINT '(A)' ,
     1   ' ---------------------------------------------'
      PRINT '(1X, A)' , ITITLE
      READ (10, '(A)' ) REC2
      PRINT '(/1X, A)' , REC2
C
      PROMP(1) = 'This is correct file - continue'
      PROMP(2) = 'Not correct file - get another'
      PROMP(3) = 'Stop'
      IOPT     = 1
      CALL MENU (1, 3, IOPT, PROMP)
      IF (IOPT .EQ. 2) GO TO 150
      IF (IOPT .EQ. 3) CALL QUITS (0, DUMSTR)
      RETURN
C
140   OLDFIL = ' '
150   CONTINUE
      PRINT *, 'New data file name or stop ? <stop>'
      READ (5, '(A)' , ERR = 150) DATFIL
      IF (DATFIL .EQ. ' ') CALL QUITS (0, DUMSTR)
      GO TO 50
C
      END
      SUBROUTINE OPRFIL (PRBFIL, NINS, TINSTR, WPR)
C
C______________________________________________________________________
C  opens file and reads arbitrary probe laser instrument function
C    parameters:  prbfil = name of file
C                 nins = number points found
C                 wpr  = wavenumber array read
C                 tinstr = instrument function as read
C______________________________________________________________________
C
      DIMENSION WPR(*), TINSTR(*)
      DOUBLE PRECISION WPR
      CHARACTER DUMSTR
      CHARACTER* (*)PRBFIL
      CHARACTER*80 FILID
C
50    OPEN (55, FILE = PRBFIL, STATUS = 'OLD', ERR = 60)
      REWIND 55
      GO TO 80
60    CONTINUE
      PRINT '(3A)' ,
     1   ' !! ERROR - CANNOT OPEN INSTRUMENT FUNCTION FILE ' ,
     2   PRBFIL, ' !!'
65    PRINT *,
     1   'New instrument function file name or stop ? <stop>'
      READ (5, '(A)', ERR = 65) PRBFIL
      IF (PRBFIL .EQ. ' ') CALL QUITS (0, DUMSTR)
      GO TO 50
80    CONTINUE
      READ (55, '(A)' ) FILID
C
      DO 100 N = 1, 1000
         READ (55, *, ERR = 110, END = 120) WPR(N), TINSTR(N)
100   CONTINUE
C
      CALL QUITS (-1,
     1'!! ERROR - MUST INCREASE DIMENSION OF WPR IN OPRFIL !!')
C
110   CONTINUE
      CALL QUITS (-1,
     1   '!! ERROR - BAD ENTRY IN INSTRUMENT FUNCTION FILE !!')
120   CONTINUE
      NINS     = N-1
      CALL NRMPRB (NINS, WPR, TINSTR)
      END
      SUBROUTINE PARSE (STRING, NFOUND, MSAVE, NSAVE)
C
C______________________________________________________________________
C
C   interprets the character variable string for a series of inputs
C   representing the first and second subscripts (msave and nsave)
C   for a series of changes to the varfit array.  msave represents
C   the variable index (from 1 to nf), while nsave is from 1 to 4,
C   representing either: (1) the nominal value of the variable;
C   (2) min.; (3) max.;, or (4) fix/free flag.
C   on input, the last digit of each substring will be a,b,c, or d,
C   representing cases (1) - (4) above.  the first one or two digits
C   will be msave.  nfound is the number of substrings found, each of
C   which must be separated by blanks and/or commas.
C        (used by VAX version of carsft only)
C______________________________________________________________________
C
      CHARACTER* (*)STRING
      PARAMETER (NF = 30)
      DIMENSION MSAVE(*), NSAVE(*)
      CHARACTER*2 NUM(NF), SUB
      DATA NUM/ '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' ,
     1    '9' , '10' , '11' , '12' , '13' , '14' , '15' , '16'
     2   , '17' , '18' , '19' , '20' , '21' , '22' , '23' ,
     3   '24' , '25' , '26', '27', ' ', ' ', ' ' /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      NFOUND   = 0
C
C   loop on characters in string
C
      ICHAR    = 0
      LSTR     = LEN(STRING)
100   CONTINUE
C
C   move forward in string until significant character is
C   found
C
      ICHAR    = ICHAR+1
      IF (ICHAR .GT. LSTR) RETURN
      SUB      = STRING(ICHAR:ICHAR)
      IF (SUB .EQ. ' ' .OR. SUB .EQ. ',') GO TO 100
C
C   have good character - now move forward until next
C   separator is found to delimit substring
C
      NFIRST   = ICHAR
      ICHAR    = ICHAR+1
110   CONTINUE
      SUB      = STRING(ICHAR:ICHAR)
C
      IF (SUB .EQ. ' ' .OR. SUB .EQ. ',') THEN
C
C   just passed last good character in substring
C
         NLAST = ICHAR-1
      ELSE
         ICHAR = ICHAR+1
         GO TO 110
      ENDIF
C
C   see if last character is a letter
C
      SUB = STRING(NLAST:NLAST)
C
      IF (SUB .EQ. 'A' .OR. SUB .EQ. 'a') THEN
         N = 1
      ELSEIF (SUB .EQ. 'B' .OR. SUB .EQ. 'b') THEN
         N = 2
      ELSEIF (SUB .EQ. 'C' .OR. SUB .EQ. 'c') THEN
         N = 3
      ELSEIF (SUB .EQ. 'D' .OR. SUB .EQ. 'd') THEN
         N = 4
      ELSE
C
C         . . . last character is digit, or is illegal
C               assume for now that nominal value is desired
C
         N = 1
         SUB = STRING(NFIRST:NLAST)
         GO TO 150
      ENDIF
C
C   get substring consisting of digits if last character was letter
C
      SUB = STRING(NFIRST:NLAST-1)
150   CONTINUE
C
      DO 200 K = 1, NF
         IF (SUB .NE. NUM(K)) GO TO 200
         NFOUND = NFOUND+1
         MSAVE(NFOUND) = K
         NSAVE(NFOUND) = N
         GO TO 100
200   CONTINUE
C
900   CONTINUE
      PRINT *, ' !! ERROR - INPUT NOT INTERPRETABLE !!'
      PRINT *, ' NFOUND = ' , NFOUND
C
      DO 910 I = 1, NFOUND
         PRINT *, MSAVE(I), NSAVE(I)
910   CONTINUE
C
      END
      SUBROUTINE POPLAT (IDGAS, NSPEC, TEMP, MAXQ, MAXV4, MAXV5,
     1    NT, NQ, POP, POPN, POPV45)
C
C______________________________________________________________________
C
C  output variables:
C    maxq(1) : maximum vibrational quantum number at which have
C              significant population
C    maxq(2) : maximum rotational quantum number for each
C              vibrational state with significant population
C    maxv4 : max. v4 state for c2h2
C    maxv5 : max. v5 states (one for each v4 state of c2h2)
C    nt    : number of transitions for each species
C    pop   : populations
C    popn  : populations normalized to given vibrational level
C    popv45 : populations of exited vibrational states 4 & 5 for
C             c2h2, normalized by dividing by partition function
C             for these states
C______________________________________________________________________
C
      PARAMETER (NJ = 130, NM = 4, NV = 6, NV45 = 10, NW = 1000)
      DIMENSION IDGAS(*), MAXQ(2, *), MAXV5(*), NT(NM, *),
     1   NQ(NW, 4, NM, *), POP(0:NV, 0:NJ, *), POPN(0:NV, 0:NJ, *),
     2   POPV45(0:NV45, 0:*)
      COMMON /PARMOL/ PARMOL(12, NM)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      COMMON /FJ/ FJ(0:NV, 0:NJ, NM)
      COMMON /G0V/ G0V(0:NV, NM)
      COMMON /GJ/ GJ(0:NJ, NM)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C   local variables
C
      DIMENSION TV(0:NV)
      INTEGER V, V4, V5
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C   max. vibrational state and vibrational partition function qv:
C
      CEXOT = CEX/TEMP
C
      DO 400 M = 1, NSPEC
         IDG = IDGAS(M)
         IF (GASNAM(IDG) .EQ. 'CO2') THEN
C
C---  comment out following line to remove co2 routine
C
            CALL CO2POP (TEMP, NT, MAXQ, NQ, POP)
         ELSEIF (GASNAM(IDG) .EQ. 'H2O') THEN
C
C---  comment out following line to remove h2o routine
C
C            CALL H2OPOP (TEMP, NT, NQ, POP)
         ELSE
            QV  = 0.
            DO 150 V = 0, NV
               TV(V) = EXP(-G0V(V, IDG)*CEXOT)
               IF (TV(V) .LT. 0.005) GO TO 160
               QV = QV+TV(V)
150         CONTINUE
            V = NV
160         CONTINUE
C
C---  set maximum as loop index-1, since don't want to keep
C---  this state
C
            MAXQ(1, M) = V-1
C
C---  max. rotational state and rotational partition function qr:
C---  loop only allowed to go to nj-2, since later
C---  loops will access index maxj+2.
C
            QR = 0.
C
C---  CGEEV in subroutine KOSCHI has problems
C---  diagonalizing matrices of order >~70,
C---  so limit loop to determine MAXQ(2, M) or MAXJ to
C---  70 instead of NJ-2 (NJ is presently 100)
C
C            DO 210 J = 0, NJ-2
            DO 210 J = 0, 70
               TR = GJ(J, IDG)*(2.*FLOAT(J)+1.)*
     1              EXP(-FJ(0, J, IDG)*CEXOT)
               IF ((GJ(J, IDG) .NE. 0.) .AND. (TR .LT. 0.005))
     1            GO TO 220
               QR = QR+TR
210         CONTINUE
220         CONTINUE
C
            MAXQ(2, M) = J-1
C
C---  calculate partition function and populations of v,j states,
C---  pop(v,j,m).  popn(v,j,m) will be used for rotational narrowing
C---  model in susceptibility.
C
            QRR = 1./QR
            Q = 0.
            DO 280 J = 0, MAXQ(2, M)+2
               DO 280 V = 0, MAXQ(1, M)+1
                  P = GJ(J, IDG)*EXP(-FJ(V, J, IDG)*CEXOT)
                  POPN(V, J, M) = QRR*P
                  POP(V, J, M) = TV(V)*P
                  Q = Q+(2.*FLOAT(J)+1.)*POP(V, J, M)
280         CONTINUE
C
            QINV = 1./Q
            DO 250 J = 0, MAXQ(2, M)+2
               DO 250 V = 0, MAXQ(1, M)+1
                  POP(V, J, M) = POP(V, J, M)*QINV
250         CONTINUE
         ENDIF
C
         IF (GASNAM(IDG) .EQ. 'C2H2') THEN
C
C              . . . populations and partition function
C              for excited vibrational states v4, v5 of c2h2
C              include degree of degeneracy factor for doubly
C              degenerate vibrations.  see herzberg, ir and
C              raman spectra, p.80
C
            QC2H2 = 0.
            ANU4  = PARMOL(11, 4)
            ANU5  = PARMOL(12, 4)
            DATA EPS / 0.005 /
            DO 290 V4 = 0, NV45
               DO 285 V5 = 0, NV45
                  POPV45(V4, V5) = EXP(-(V4*ANU4+
     1            V5*ANU5)*CEXOT)*(V4+1)*(V5+1)
                  IF (POPV45(V4, V5) .LT. EPS) GO TO 288
                  QC2H2 = QC2H2+POPV45(V4, V5)
285            CONTINUE
288            CONTINUE
C
               IF (V5 .EQ. 0) THEN
                  MAXV5(V4) = 0
               ELSE
                  MAXV5(V4) = V5-1
               ENDIF
C
               IF (V5 .EQ. 0) GO TO 295
290         CONTINUE
295         CONTINUE
            MAXV4 = V4-1
            DO 320 V4 = 0, MAXV4
               DO 320 V5 = 0, MAXV5(V4)
                  POPV45(V4, V5) = POPV45(V4, V5)/QC2H2
320         CONTINUE
         ENDIF
400   CONTINUE
C
      END
      SUBROUTINE PRBFNC (NPRB, GAMPR, WPR, TINSTR)
C
C______________________________________________________________________
C
C   analytic function for probe instrument function in makprb
C     input parameters:
C       nprb - number of points in instrument function (1000 or less)
C       gampr - function width (fwhm)
C     output parameters:
C       wpr - wavenumber of instrument function
C       tinstr - unconvolved instrument function intensity
C______________________________________________________________________
C
      DIMENSION WPR(*), TINSTR(*)
      DOUBLE PRECISION WPR, W, DW
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  use double gaussian for probe instrument function
C
      DATA C1, PI, PKRAT, GAMRAT, EXP2 / 2.772589, 3.141593, 0.06,
     1   3., 0.8 /
C
      GAMPR1 = GAMPR
      GAMPR2 = GAMPR*GAMRAT
      AMP1 = 1./(1.+PKRAT)
      AMP2 = PKRAT/(1.+PKRAT)
      DW = 2.*8.*GAMPR/FLOAT(NPRB-1)
      W = -8.*GAMPR-DW
C
      DO 100 I = 1, NPRB
         W = W+DW
         ARG1 = C1*(W/GAMPR1)**2
         ARG2 = C1*(W/GAMPR2)**2
         WPR(I) = W
         TINSTR(I) = AMP1*EXP(-ARG1)+AMP2*EXP(-ABS(ARG2)**EXP2)
100   CONTINUE
C
      END
      SUBROUTINE PRNRES (ITASK, PRBFIL, VARRUN, VARFIT, GAUSPR,
     1   METHPN, DATFIL, SHOTST)
C
C______________________________________________________________________
C
C   outputs hardcopy of run parameters and fitting variables to
C   terminal printer
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17, NP = 30, NPP = 31)
      DIMENSION VARRUN(*), VARFIT(NF, *)
      LOGICAL GAUSPR, SHOTST
      CHARACTER DATFIL*(*), PRBFIL*(*)
      COMMON /CSTEP/ XVAR(NP), XMAX(NP), XMIN(NP), DELTX(NP),
     1   DELMN(NP), ERR(NP, NPP), FOBJ, NFS, NTRAC, MATRX,
     2   MASK(NP), NFMAX, NFLAT, JVARY, NXTRA, KFLAG, NOREP,
     3   KERFL, KW
C
C  local variables:
C
      CHARACTER*1 ANS, pse, namdev*8
      LOGICAL DUMLOG, dectrm
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      ANS = 'N'
      CALL YESNO ( 'Output results to terminal printer' , ANS)
      IF (ANS .NE. 'Y') RETURN
C
      DATA IESC, IFF / 27, 12 /
C
C---  the '+' carriage control character is not recognized by some
C---  compilers.  if so, the '+' should be replaced by 1x.  for some
C---  compilers that do not recognize '+', carriage return/line
C---  feed can be suppressed by putting a $ at the end of the format
C---  statement.
C
	 dectrm =
     +  (namdev() .eq. 'DECVT100') .or.
     +  (namdev() .eq. 'DECVT125') .or.
     +  (namdev() .eq. 'DECVT240') .or.
     +  (namdev() .eq. 'DECVT241')

	 if (dectrm) then
        WRITE (*, '(4A)') '+', IESC, '[5i'
	 else
	   write (*,*) 'Please press CTRL-Print Screen, then RETURN.'
	   write (*,*) 'Repeat when output stops.' 
	   read (*,'(a)') pse
	 endif

      CALL DISRUN (PRBFIL, VARRUN, DUMLOG, .TRUE.)
      IF ((ITASK .EQ. 2) .OR. (ITASK .EQ. 3) .OR. (ITASK .EQ. 5))
     1   WRITE (*, '(6X, A, T30, A)') 'Data file', DATFIL
      DATA DUMLOG / .FALSE. /
      CALL DISFIT (GAUSPR, METHPN, VARFIT, ITASK, DUMLOG, DUMLOG,
     1   .TRUE.)
      IF (ITASK .EQ. 3) CALL FITPRN (VARFIT, KFLAG, NOREP,
     1   FOBJ, .TRUE., SHOTST)

	 if (dectrm) then
        WRITE (*, '(5A)') '+', IFF, IESC, '[4i'
	 else
	   write(*,'(/,a)') '           .....end of output.....'  
	   read (*,'(a)') pse
	 endif

      PRINT *, ' '
C
      END
      SUBROUTINE PROBE (PRBFIL, NTHE, WLO, WHI, TINSTR)
C
C______________________________________________________________________
C
C   reads users file for non-gaussian probe instrument function,
C   normalizes and transforms it
C     input parameters:
C       prbfil - name of probe instrument function data file
C       nthe - number points in range
C       wlo, whi - limits of wavenumber calculation range
C     output parameters:
C       tinstr - hartley transformed instrument function at uniformly
C       spaced wavenumbers over double interval (2*nthe points)
C______________________________________________________________________
C
      PARAMETER (NTM = 32*16384, NTM1 = NTM/2)
      DIMENSION TINSTR(*)
      CHARACTER* (*)PRBFIL
C
C  local variables
C
C---  common shared with subroutines calcon and fitcal
C
      COMMON /COMM3/ PEVEN(4*NTM+5*NTM1)
      DIMENSION WPR(1000)
      DOUBLE PRECISION WINT, DW, WPR
C
      CALL OPRFIL (PRBFIL, NINS, TINSTR, WPR)
C
C---  produce interpolated curve at evenly
C---  spaced points.  call interpolated probe intensity peven.
C
      DW = (WHI-WLO)/FLOAT(NTHE-1)
C
      NEVEN = (WPR(NINS)-WPR(1))/DW+1
      IF (NEVEN .GT. NTM) THEN
	   PRINT*,NEVEN,' NEEDED, ',NTM,' AVAILABLE.'
	   CALL QUITS (-1,
     1'!! ERROR - MUST INCREASE DIMENSIONS OF PEVEN, ROUTINE PROBE !!')
	 ENDIF
      PEVEN(1) = TINSTR(1)
      WINT = WPR(1)
C
      DO 200 N = 2, NEVEN
         WINT = WINT+DW
         CALL TERPLD (NINS, WPR, TINSTR, WINT, PEVEN(N), IER)
200   CONTINUE
C
C---  locate maximum of curve, so can be located at index 1
C
      PMAX = 0.
C
      DO 240 N = 1, NEVEN
         IF (PEVEN(N) .GT. PMAX) THEN
            PMAX = PEVEN(N)
            NMAX = N
         ENDIF
240   CONTINUE
C
C---  form instrument function to be transformed . . .
C---  first part of array is all points including and to right of
C---  peak
C
      NRIGHT = NEVEN-NMAX+1
      IND    = NMAX
C
      DO 260 N = 1, NRIGHT
         TINSTR(N) = PEVEN(IND)
         IND       = IND+1
260   CONTINUE
C
      NEXT = NRIGHT+1
C
C---  fill array with zeroes until indnex, where will begin left
C---  part of curve. nleft is number points in left part
C
      NLEFT = NEVEN-NRIGHT
      NDBL  = 2*NTHE
      INDNEX = NDBL-NLEFT+1
C
      DO 280 N = NEXT, INDNEX-1
         TINSTR(N) = 0.
280   CONTINUE
C
C---  fill rest of array with left part, beginning at index indnex
C
      IND = 1
C
      DO 300 N = INDNEX, NDBL
         TINSTR(N) = PEVEN(IND)
         IND       = IND+1
300   CONTINUE
C
C---  transform curve
C
      CALL FHT (TINSTR, NDBL)
C
C---  normalize to max of 1.0 (I'm not sure why this works - it is
C---  done to make it agree with the gaussian produced by cgauss.
C---  ran out of time to investigate this question . . G.C.)
C
      CALL MNMAX (TINSTR, 2, NDBL, 1, DUM, TMAX)
C
      DO 310 N = 1, NDBL
         TINSTR(N) = TINSTR(N)/TMAX
310   CONTINUE
C
      END
      SUBROUTINE QUITS (IER, MESSAG)
C
C______________________________________________________________________
C
C   terminates program.  provisions is made for message print on
C   exit.
C
C    ier        action
C  -------   ----------------------------
C    < 0     print messag, but not ier (normal termination w/ message)
C    = 0     print neither ier nor messag (normal termination w/o mess)
C    > 0     print both. (error exit with flag)
C
C______________________________________________________________________
C
      CHARACTER* (*)MESSAG
      COMMON /PLTINI/ PLTINI
      LOGICAL PLTINI
C
      IF (IER .LT. 0) THEN
         PRINT '(//1X, A)' , MESSAG
      ELSEIF (IER .GT. 0) THEN
         PRINT '(//A/1X, A,/A, I3)' , ' FATAL ERROR EXIT' ,
     1   MESSAG, '  ERROR FLAG =' , IER
      ENDIF
C
      IF (PLTINI) CALL DONEPL
C
C---  close postmortem file
C
C      CLOSE (33)
C
      STOP
      END
      SUBROUTINE RENORM (CHIEXP, CHIMAX, CHNORM, DATPL, NPL,
     1   USEREF)
C
C______________________________________________________________________
C
C  rescales theory and data to physical units, since
C  data has been normalized to 1.0 and theory to 1/chiexp in
C  order to eliminate arbitrary nature of units in data, and
C  to avoid numerical problems with taking square roots of
C  extremely small numbers.  chimax here is max. of the
C  theoretical convolved array. (w/o influence of chiexp)
C______________________________________________________________________
C
      DIMENSION CHNORM(*), DATPL(*)
      LOGICAL USEREF
C
      IF (USEREF) RNORM = CHIEXP
      IF ( .NOT. USEREF) RNORM = CHIEXP*CHIMAX
C
      DO 620 I = 1, NPL
         CHNORM(I) = CHNORM(I)*RNORM
         DATPL(I)  = DATPL(I)*RNORM
620   CONTINUE
C
      END
      SUBROUTINE RITRES (IRIT)
C
C______________________________________________________________________
C
C  prompts user for whether to write spectra to output file,
C  then opens file
C
C    irit = 1 means write
C    irit = 0 means don't write
C______________________________________________________________________
C
      CHARACTER WRF*1, OUTFIL*40
      SAVE WRF, OUTFIL
      DATA WRF/ 'N' /, OUTFIL/ 'spec.out' /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
      CALL YESNO ( 'Write spectra to file' , WRF)
      PRINT *, ' '
      IRIT = 0
C
      IF (WRF .EQ. 'Y') THEN
         CALL OPFILE (OUTFIL, 25, 40, 'Name of output file ?' ,
     1    'UNKNOWN' )
         IRIT = 1
      ENDIF
      END
      SUBROUTINE RITPAR (IRIT, DATFIL, PRBFIL, VARRUN, VARFIT)
C
C______________________________________________________________________
C
C  prompts user for whether to write parameters for calculation
C  to output file, then opens file
C
C    irit = 1 means write
C    irit = 0 means don't write
C______________________________________________________________________
C
      PARAMETER (NF = 30, NM = 4, NR = 17)
C
      DIMENSION VARRUN(*), VARFIT(NF, *)
      CHARACTER DATFIL*(*), PRBFIL*(*)
C
C---  local variables
C
      CHARACTER WRF*1, OUTFIL*40
      SAVE WRF, OUTFIL
      DATA WRF/ 'N' /, OUTFIL/ 'tran.out' /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
      CALL YESNO ( 'Write transition parameters to file' , WRF)
      PRINT *, ' '
      IRIT = 0
C
      IF (WRF .EQ. 'Y') THEN
         CALL OPFILE (OUTFIL, 26, 40, 'Name of output file ?' ,
     1      'UNKNOWN' )
         IRIT = 1
         ITASK1 = VARRUN(1)
100      CALL VAROUT (DATFIL, 26, PRBFIL, VARRUN, ITASK1,
     1      OUTFIL, VARFIT)
      ENDIF
      END
      SUBROUTINE RITT7X (NPL, WPL)
C
C______________________________________________________________________
C
C  prompts user for whether to write wavenumber array wpl to outfile
C  for use by quickfitter library
C______________________________________________________________________
C
      DIMENSION WPL(*)
      DOUBLE PRECISION WPL
      CHARACTER WRF*1, OUTFIL*40
      SAVE WRF, OUTFIL
      DATA WRF/ 'N' /, OUTFIL/ 'x.lib' /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WRF = 'NO'
      CALL YESNO ( 'Write wavenumber file for quickfitter' , WRF)
C
      IF (WRF .EQ. 'Y') THEN
         OPEN (27, FILE = OUTFIL, STATUS = 'UNKNOWN', ERR = 50)
         REWIND 27
         GO TO 200
50       CALL QUITS (-1,
     1      '!! ERROR OPEN QUICKFITTER WAVENUMBER FILE !!')
200      CONTINUE
C
         DO 100 I = 1, NPL
            WRITE (27, '(F12.4)') WPL(I)
100      CONTINUE
C
         CLOSE (27)
      ENDIF
      END
      SUBROUTINE RITT7Y (TEMP, NPL, R, S)
C
C______________________________________________________________________
C
C  prompts user for whether to write real part of chi and square of
C  chi to outfile file for use by quickfitter library.  the file
C  name is encoded with the temperature, rounded down to the nearest
C  multiple of ten degrees.
C    output:
C      r : real part of resonant susceptibility
C      s : square of resonant susceptibility
C______________________________________________________________________
C
      DIMENSION R(*), S(*)
      CHARACTER WRF*1, OUTFIL*40, tchar*4
      SAVE WRF, OUTFIL
      DATA WRF/ 'N' /, OUTFIL/ 'txxx0.lib' /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WRF = 'Y'
      CALL YESNO ( 'Write intensity file for quickfitter' , WRF)
      IF (WRF .EQ. 'Y') THEN

c         L1 = INT(TEMP/1000)
c         L2 = INT((TEMP-1000*L1)/100)
c         L3 = INT((TEMP-1000*L1-100*L2)/10)
C
c         OUTFIL(2:2) = CHAR(L1+48)
c         OUTFIL(3:3) = CHAR(L2+48)
c         OUTFIL(4:4) = CHAR(L3+48)
C

      itemp = nint(temp/10.) * 10
      write (tchar,'(i4.4)') itemp
      outfil (2:4) = tchar (1:3)

         OPEN (28, FILE = OUTFIL, STATUS = 'UNKNOWN', FORM =
     1      'UNFORMATTED', ERR = 50)
         REWIND 28
         GO TO 200
50       CALL QUITS (-1,
     1      '!! ERROR OPENING QUICKFITTER INTENSITY FILE !!')
200      CONTINUE
C
         DO 100 I = 1, NPL
            WRITE (28) R(I), S(I)
100      CONTINUE
C
         CLOSE (28)
      ENDIF
      END
      SUBROUTINE ROTDIF (AMPL, DFSPHI, GAMMAQ, GAMJ, K, NQ,
     1   MAXJ, NV, POPN, WIDMUL, TERMM, WDF, WJ, WAVEN, XMODEL,
     2   XIMAG, XREAL)
C
C______________________________________________________________________
C
C  calculates imaginary and real parts of susceptibility for
C  q-branch at above atmospheric pressure, using rotational
C  diffusion model
C______________________________________________________________________
C
      PARAMETER (NW = 1000)
      DIMENSION NQ(NW, 4), TERMM(0:NV, 0:*), POPN(0:NV, 0:*),
     1   GAMMAQ(0:*), WAVEN(*)
      DOUBLE PRECISION WAVEN
      INTEGER XMODEL
      INTEGER VG, VU
      SAVE SUMU, SUMV, DNSAV
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DENOM = 4.*(WJ-WDF)**2+GAMJ**2
      XREAL = AMPL*2.*(WJ-WDF)/DENOM
      XIMAG = AMPL*GAMJ/DENOM
C
      IF (XMODEL .EQ. 2) THEN
C
C        . . . this is first transition of a vibrational manifold
C              calculate sumu and sumv, to be used for all
C              transitions in this manifold
C
         VG = NQ(K, 1)
         VU = NQ(K, 2)
         SUMU = 0.
         SUMV = 0.
         DO 410 J = 0, MAXJ
            CJ1 = WIDMUL*GAMMAQ(J)
            AJ1 = (1.-DFSPHI)*(2*J+1)*CJ1*POPN(VG, J)
            BJ1 = 2.*((TERMM(VU, J)-TERMM(VG, J))-(WDF+WAVEN(1)))
            DENOM1 = BJ1**2+CJ1**2
            SUMU   = SUMU+AJ1*CJ1/DENOM1
            SUMV   = SUMV+AJ1*BJ1/DENOM1
410      CONTINUE
         DNSAV = (1.-SUMU)**2+SUMV**2
      ENDIF
C
      XREAL = ((1.-SUMU)*XREAL+SUMV*XIMAG)/DNSAV
      XIMAG = ((1.-SUMU)*XIMAG-SUMV*XREAL)/DNSAV
C
      END
      SUBROUTINE RRIT23 (CHNORM, DATPL, ITITLE, NPL, WPL)
C
C______________________________________________________________________
C
C  writes results to file of comparison of convolved spectrum and data
C______________________________________________________________________
C
      DIMENSION CHNORM(*), DATPL(*), WPL(*)
      DOUBLE PRECISION WPL
      CHARACTER* (*)ITITLE
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL MINMX (DATPL, NPL, DUM, DATMAX)
      CALL MINMX (CHNORM, NPL, DUM, THEMAX)
      YOFF = -0.125*AMAX1 (DATMAX, THEMAX)
C
      WRITE (25, '(1X, 2A/1X, A)') 'C ', ITITLE(1:77), 'C '
      WRITE (25, 1010) 'C ' , 'Wavenumber' , 'Data' ,
     1   'Theory', 'Data-Theory'
      WRITE (25, '(1X, A, T49, E12.4)') 'C ', YOFF
      WRITE (25, 1010) 'C ' , '----------' , '----' ,
     1   '------', '-----------'
C
      DO 400 N = 1, NPL
         WRITE (25, 1000) WPL(N), DATPL(N), CHNORM(N),
     1      DATPL(N)-CHNORM(N)+YOFF
c         WRITE (25, 1000) WPL(N), CHNORM(N), DATPL(N)
400   CONTINUE
C
1000  FORMAT(T9, F10.4, 3(2X, E12.4))
1010  FORMAT(1X, A, T10, A, T26, A, T39, A, T50, A)
      END
      SUBROUTINE RRIT4 (NTHE, WAVEN, CHIT2)
C
C______________________________________________________________________
C
C  writes results of theoretical susceptibility calculation
C______________________________________________________________________
C
      DIMENSION WAVEN(*), CHIT2(*)
      DOUBLE PRECISION WAVEN
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WRITE (25, 1010) 'C ' , 'Wavenumber' , 'Susceptibility'
      WRITE (25, 1010) 'C ' , '----------' , '--------------'
C
      DO 400 N = 1, NTHE
         WRITE (25, '(T9, F10.4, T22, E14.4)') WAVEN(N), CHIT2(N)
400   CONTINUE
C
1010  FORMAT(1X, A, T10, A, T24, A)
      END
      SUBROUTINE RRIT5 (ITITLE, REFTEM, REFP, NDAT, WDAT,
     1   CHIDAT, CHIREF)
C
C______________________________________________________________________
C
C  writes new data file with new reference array
C______________________________________________________________________
C
      DIMENSION WDAT(*), CHIDAT(*), CHIREF(*)
      DOUBLE PRECISION WDAT
      CHARACTER* (*)ITITLE
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      REWIND 10
C
      WRITE (10, '(1X, A)' ) ITITLE
      WRITE (10, '(1X, A, F11.1, F11.4)' )
     1   'Reference Temperature, Pressure = ' , REFTEM, REFP
C
      DO 100 N = 1, NDAT
         WRITE (10, '(F12.4, 2E15.8)' ) WDAT(N), CHIDAT(N),
     1      CHIREF(N)
100   CONTINUE
C
      END
      SUBROUTINE RRIT6 (CHITHE, CHICON, NTHE, WAVEN)
C
C______________________________________________________________________
C
C  writes output file for task 6 - new reference spectrum, no data
C   file
C______________________________________________________________________
C
      DIMENSION CHITHE(*), CHICON(*), WAVEN(*)
      DOUBLE PRECISION WAVEN
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WRITE (25, 1000) 'C ' , 'Wavenumber' , 'Susceptibility' ,
     1   'Convolved Spectrum'
      WRITE (25, 1000) 'C ' , '----------' , '--------------' ,
     1   '------------------'
C
      DO 300 N = 1, NTHE
         WRITE (25, 1010) WAVEN(N), CHITHE(N), CHICON(N)
300   CONTINUE
C
1000  FORMAT(1X, A, T10, A, T24, A, T41, A)
1010  FORMAT(T9, F10.4, T22, E14.4, T41, E14.4)
      END
      SUBROUTINE RRPAR (AMPL, GAMMA, IDGAS, NSPEC, NQ, NT, POP,
     1   POPN, TTYPE, WTRAN, VARFIT)
C
C______________________________________________________________________
C
C  writes output parameters to file
C______________________________________________________________________
C
      PARAMETER (NJ = 130, NM = 4, NV = 6, NW = 1000, NV45 =
     1   10, NF = 30)
      DIMENSION AMPL(NW, NM, *), GAMMA(NW, NM, *), IDGAS(*), NT(NM, *),
     1    NQ(NW, 4, NM, *), POP(0:NV, 0:NJ, *), POPN(0:NV, 0:NJ, *),
     2    WTRAN(NW, NM, *), VARFIT(NF, *)
      CHARACTER*1 TTYPE(NW, NM, *)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DO 100 M = 1, NSPEC
      DO 100 IPMP = 1, 2
         WRITE (26, '(1X/ '' transition parameters used for '', A5,
     1      '' for pump freq'', I3/)')
     2      GASNAM(IDGAS(M)), IPMP
         IF (GASNAM(IDGAS(M)) .EQ. 'H2O') THEN
            WRITE (26, '(3X, ''TYPE   I  V1   J TAU'', 5X, ''FREQ'',
     1         9X, ''GAMMA'', 6X, ''POP(V1,J1)'', 5X, ''POP(V2,J2)'',
     2         7X, ''CHIAMP'')')
            WRITE (26, '(41X, ''(FWHM)'')')
            WRITE (26, *) ' '
            DO 300 K = 1, NT(M, IPMP)
               XJ1 = 1.
               XJ2 = 1.
               WRITE (26, '(4X, A1, 2X, 4I4, F12.4, F12.5, 3E15.4)')
     1            TTYPE(K, M, IPMP), NQ(K, 1, M, IPMP),
     2            NQ(K, 2, M, IPMP), NQ(K, 3, M, IPMP),
     2            NQ(K, 4, M, IPMP), WTRAN(K, M, IPMP),
     2            VARFIT(8, 1)*GAMMA(K, M, IPMP),
     3            XJ1*POP(NQ(K, 1, M, IPMP), NQ(K, 3, M, IPMP), M),
     3            XJ2*POP(NQ(K, 2, M, IPMP), NQ(K, 4, M, IPMP), M),
     4            AMPL(K, M, IPMP)*1.E-18/GAMMA(K, M, IPMP)
300         CONTINUE
         ELSEIF (GASNAM(IDGAS(M)) .EQ. 'CO2') THEN
            WRITE (26, '(3X, ''TYPE  NA  NA  V1  V2'', 5X, ''FREQ'',
     1         9X, ''GAMMA'', 6X, ''POP(V1   )'', 5X, ''POP(V2   )'',
     2         7X, ''CHIAMP'')')
            WRITE (26, '(41X, ''(FWHM)'')')
            WRITE (26, *) ' '
            DO 400 K = 1, NT(M, IPMP)
               XJ1 = 1.
               XJ2 = 1.
               WRITE (26, '(4X, A1, 2X, 4I4, F12.4, F12.5, 3E15.4)')
     1            TTYPE(K, M, IPMP), NQ(K, 1, M, IPMP),
     1            NQ(K, 2, M, IPMP), NQ(K, 3, M, IPMP),
     2            NQ(K, 4, M, IPMP), WTRAN(K, M, IPMP),
     2            VARFIT(8, 1)*GAMMA(K, M, IPMP),
     3            XJ1*POP(NQ(K, 1, M, IPMP), NQ(K, 3, M, IPMP), M),
     3            XJ2*POP(NQ(K, 2, M, IPMP), NQ(K, 4, M, IPMP), M),
     4            AMPL(K, M, IPMP)*1.E-18/GAMMA(K, M, IPMP)
400         CONTINUE
         ELSE
            WRITE (26, '(3X, ''TYPE  V1  V2  J1  J2'', 5X, ''FREQ'',
     1         9X, ''GAMMA'', 6X, ''POP(V1,J1)'', 5X, ''POP(V2,J2)'',
     2         7X, ''CHIAMP'')')
            WRITE (26, '(41X, ''(FWHM)'')')
            WRITE (26, *) ' '
            DO 200 K = 1, NT(M, IPMP)
               XJ1 = 2.*NQ(K, 3, M, IPMP)+1.
               XJ2 = 2.*NQ(K, 4, M, IPMP)+1.
               WRITE (26, '(4X, A1, 2X, 4I4, F12.4, F12.5, 3E15.4)')
     1            TTYPE(K, M, IPMP), NQ(K, 1, M, IPMP),
     1            NQ(K, 2, M, IPMP), NQ(K, 3, M, IPMP),
     2            NQ(K, 4, M, IPMP), WTRAN(K, M, IPMP),
     2            VARFIT(8, 1)*GAMMA(K, M, IPMP),
     3            XJ1*POP(NQ(K, 1, M, IPMP), NQ(K, 3, M, IPMP), M),
     3            XJ2*POP(NQ(K, 2, M, IPMP), NQ(K, 4, M, IPMP), M),
     4            AMPL(K, M, IPMP)*1.E-18/GAMMA(K, M, IPMP)
200         CONTINUE
         ENDIF
100   CONTINUE
      END
      SUBROUTINE SETDEF (DATFIL, PRBFIL, VARFIT, NF, VARRUN)
C
C______________________________________________________________________
C
C  sets default values for run and fit variables
C______________________________________________________________________
C
      DIMENSION VARRUN(*), VARFIT(NF, *)
      CHARACTER* (*)DATFIL, PRBFIL
C
      DATFIL = 'cars.dat'
      PRBFIL = 'none'
C
C---  run variables:
C
      VARRUN(1) = 1.
      VARRUN(2) = 0.
      VARRUN(3) = 0.
      VARRUN(4) = 0.
      VARRUN(5) = 8.5
      VARRUN(6) = 1.
      VARRUN(7) = 0.
      VARRUN(8) = -1.
      VARRUN(9) = 1.
      VARRUN(10) = 1.
      VARRUN(11) = 1.
      VARRUN(12) = 1.
      VARRUN(14) = 0.
      VARRUN(15) = 0.
      VARRUN(16) = 1.
      VARRUN(17) = 1.
C
C---  fit variables:
C
      DO 110 N = 1, NF
         VARFIT(N, 1) = 0.
110   CONTINUE
C
      VARFIT(3, 1) = 1.0
      VARFIT(4, 1) = 1.0
      VARFIT(8, 1) = 1.0
      VARFIT(25, 1) = 1.0
      VARFIT(26, 1) = 0.
C
C---  limits:
C
      DO 112 N = 1, NF
         VARFIT(N, 2) = 0.5*VARFIT(N, 1)
         VARFIT(N, 3) = 2.0*VARFIT(N, 1)
112   CONTINUE
C
C---  whether varfit is fixed (=1.) or free(=-1.)
C
      DO 115 N = 1, NF
         VARFIT(N, 4) = 1.
115   CONTINUE
C
      VARFIT(7, 4) = -1.
C
      END
      SUBROUTINE SETWVE (CHTMPI, CHTMPR, CHTMPSQ, NTHE, NTHE1, WAVTMP,
     1   WAVEN, CHIIM, CHIRL, CHIT2)
C
C______________________________________________________________________
C
C  interpolates theoretical susceptibilities on adaptive grid to
C  wavenumber array waven
C______________________________________________________________________
C
      PARAMETER (NTM = 32*16384, NTM1 = NTM/2)
      DIMENSION CHTMPR(NTM1, *), CHTMPI(NTM1, *), CHTMPSQ(NTM1),
     1    WAVTMP(*), WAVEN(*), CHIIM(NTM, *), CHIRL(NTM, *), CHIT2(*)
      DOUBLE PRECISION WAVEN, WAVTMP
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DO 500 I = 1, NTHE
         CALL TERPLD (NTHE1, WAVTMP, CHTMPSQ, WAVEN(I),
     1      CHIT2(I), IER)
      DO 500 IPMP = 1, 2
         CALL TERPLD (NTHE1, WAVTMP, CHTMPI(1, IPMP), WAVEN(I),
     1      CHIIM(I, IPMP), IER)
         CALL TERPLD (NTHE1, WAVTMP, CHTMPR(1, IPMP), WAVEN(I),
     1      CHIRL(I, IPMP), IER)
500   CONTINUE
C
      END
      SUBROUTINE SHIFTW (NTHE, WAVEN, NPL, WPL, SHIFT)
C
C______________________________________________________________________
C
C   adds line shift to wavenumber for susceptibility and convolved
C   spectrum if using correlated hard collision profile, since x'
C   instead of x was used in profile to improve fitting efficiency.
C______________________________________________________________________
C
      DIMENSION WAVEN(*), WPL(*)
      DOUBLE PRECISION WAVEN, WPL
C
      DO 100 I = 1, NTHE
         WAVEN(I) = WAVEN(I)+SHIFT
100   CONTINUE
C
      DO 200 I = 1, NPL
         WPL(I) = WPL(I)+SHIFT
200   CONTINUE
C
      END
      SUBROUTINE SQTRPL (CCONV2, NPL, NTHE, WAVEN, WPL, CHICON)
C
C______________________________________________________________________
C
C  interpolates square of convolved theoretical susceptibilities
C  at data values wpl, then takes sqrt to produce chicon.
C______________________________________________________________________
C
      DIMENSION CCONV2(*), CHICON(*), WAVEN(*), WPL(*)
      DOUBLE PRECISION WAVEN, WPL
C
      DO 500 I = 1, NPL
         CALL TERPLD (NTHE, WAVEN, CCONV2, WPL(I), CHIINT, IER)
C
CSPON remove the first line following and replace the second line
CSPON following with:
CSPON chicon(i) = chiint
C
         IF (CHIINT .LT. 0.) CHIINT = 0.
         CHICON(I) = SQRT(CHIINT)
500   CONTINUE
C
      END
      SUBROUTINE SWITCH (METHPN)
C
C______________________________________________________________________
C
C  if methpn=8, fitting variable 21 is line shift coefficient,
C  otherwise it is exponential gap parameter 1
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
C
      IF (METHPN .EQ. 8) THEN
         NAMFIT(21) = 'Line shift coefficient'
         LNFIT(21) = 22
      ELSE
         NAMFIT(21) = 'Exponential gap param. 1'
         LNFIT(21) = 24
      ENDIF
C
      END
      SUBROUTINE TASK1 (PRBFIL, VARFIT, VARRUN, BACKUP, EXTPRG)
C
C______________________________________________________________________
C
C   task1 is calculating convolved spectrum, with no data
C   output parameters: ( none )
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      DIMENSION VARFIT(NF, *), VARRUN(*)
      LOGICAL BACKUP, EXTPRG
      CHARACTER* (*)PRBFIL
C
C  local variables:
C
      PARAMETER (ND = 5000, NM = 4, NV45 = 10, NTM = 32*16384)
      DIMENSION WPL(ND), CHICON(ND), CHIREF(ND), CHNORM(ND),
     1   DUMDAT(ND)
      DIMENSION DELV4(0:NV45), DELV5(0:NV45), IDGAS(NM),
     1   PARKOS(4), PARPR(NM)
      LOGICAL DBLCON, GAUSPR, USEREF, HAVPRB, TOOMNY, SPCPRB,
     1   ENADPT, SHOTST
      CHARACTER*80 DUMSTR
C
C---  shared storage for spectra arrays:
C
      COMMON /DARR1/ CCONV2(2*NTM), DUMARR(NTM)
      COMMON /DARR2/ REFCON(4*NTM)
      COMMON /DARR3/ CHIT2(NTM)
      COMMON /SARR1/ REFTHE(NTM)
      COMMON /SARR2/ WAVEN(NTM)
      COMMON /COMM1/ WPLOT(NTM)
      DOUBLE PRECISION WPL, WAVEN, WBEGD, WENDD
      DATA DUMDAT / ND*0. /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
100   CONTINUE
      CALL DISFIT (GAUSPR, METHPN, VARFIT, ITASK, BACKUP, EXTPRG,
     1   .FALSE.)
      IF (BACKUP .OR. EXTPRG) RETURN
C
      CALL GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL, TEMP,
     2   TOTNUM, WEXPND, WOFFS, FOFF, BETA)
      CALL GASID (NM, PARPR, IDGAS, NSPEC)
C
C---  cpu timing marker
C
C      CALL CPUTIM (TIME, 'CPU time since login before wprep1 is: ')
C
C---  set up wavenumber array for convolution.
C
      CALL WPREP1 (WBEG, WEND, WEXPND, GAMPU, NPL, WPL, WMIN, WMAX)
C
C---  resonant calculation:
C
      HAVPRB = .FALSE.
C
      CALL CALCON (APHI, ATHETA, CHIOFF, CHNRPM, DBLCON, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, GAUSPR, HAVPRB, IDGAS, METHPN,
     2   NSPEC, PARKOS, PARPR, PRBFIL, PRESS, WIDMUL, TEMP, TOTNUM,
     3   WBEG, WEND, FOFF, CCONV2, CHIT2, NTHE, WAVEN, 'none',
     4   VARFIT, VARRUN, DEGSAT, WADD, BETA, APSI, TOOMNY, SPCPRB,
     5   1, ENADPT)
      IF (TOOMNY) GO TO 100
C
CSPON comment out the following call to sqrarr.
C
      CALL SQRARR (CHIT2, NTHE)
      CALL SQTRPL (CCONV2, NPL, NTHE, WAVEN, WPL, CHICON)
C
      IF (USEREF) THEN
C
C         . . . do nonresonant calculation, ratio taken
C               in subroutine norml
C
         PRINT *, 'Calculating reference susceptibility',
     1      ', one moment please . . .'
         PRINT *, ' '
         CALL NRINIT (APHI, ATHETA, CHIOFF, GAINF, NBRNCH,
     1      NREJEC, WIDMUL, FOFF, APSI)
         HAVPRB = .TRUE.
C
C              . . . HAVPRB indicates that there is no need to
C                    read probe function again, and also no need
C                    to make certain calls in calcon
C
         CALL CALCON (APHI, ATHETA, CHIOFF, CHNRPM, DBLCON, DELV4,
     1      DELV5, GAMPR, GAMPU, GAMPU2, GAUSPR, HAVPRB, IDGAS, METHPN,
     2      NSPEC, PARKOS, PARPR, PRBFIL, PRESS, WIDMUL, TEMP,
     3      TOTNUM, WBEG, WEND, FOFF, REFCON, REFTHE, NTHE, WAVEN,
     4      'none', VARFIT, VARRUN, DEGSAT, WADD, BETA, APSI,
     5      TOOMNY, SPCPRB, 1, ENADPT)
C
CSPON comment out the following call to sqrarr.
C
         CALL SQRARR (REFTHE, NTHE)
         CALL SQTRPL (REFCON, NPL, NTHE, WAVEN, WPL, CHIREF)
      ENDIF
C
      CALL NORML (CHIEXP, CHICON, CHIREF, 1, NPL, NPL, USEREF,
     1   TEMP, TEMP, PRESS, PRESS, WPL, WPL, CHNORM, CHIMAX)
C
C---  renormalize theory to physical units, reversing
C---  normalization done in norml.
C
      CALL RENORM (CHIEXP, CHIMAX, CHNORM, DUMDAT, NPL,
     1   USEREF)
C
      IF (METHPN .EQ. 8) CALL SHIFTW (NTHE, WAVEN, NPL, WPL,
     1   PRESS*PARKOS(1))
C
C---  output results:
C
      CALL RITRES (IRIT)
C
      DATA DUMSTR / ' ' /
      IF (IRIT .EQ. 1) CALL RRIT23 (CHNORM, DUMDAT,
     1   DUMSTR, NPL, WPL)
C
C       determine the indices of waven between wbeg
C       and wend so printing and plotting do not
C       include convolution end effects
C
      WBEGD = WBEG
      WENDD = WEND
      CALL SERCHD (NTHE, WAVEN, WBEGD, IB, IER)
      CALL SERCHD (NTHE, WAVEN, WENDD, IE, IER)
      NP = IE-IB+1
C
      CALL DBLSNG (NPL, WPL, WPLOT)
      IF (USEREF) THEN
         CALL PLRES (CHNORM, DUMDAT, ' ', 1, ITASK, ' ',
     1      NPL, PRBFIL, VARFIT, VARRUN, WPLOT)
         CALL DBLSNG (NP, WAVEN(IB), WPLOT)
         CALL PLRES (CHIT2(IB), DUMDAT, ' ', 2, ITASK, ' ',
     1      NP, PRBFIL, VARFIT, VARRUN, WPLOT)
         CALL DBLSNG (NPL, WPL, WPLOT)
         CALL PLRES (CHIREF, DUMDAT, ' ', 3, ITASK, ' ',
     1      NPL, PRBFIL, VARFIT, VARRUN, WPLOT)
      ELSE
         CALL PLRES (CHICON, DUMDAT, ' ', 1, ITASK, ' ',
     1      NPL, PRBFIL, VARFIT, VARRUN, WPLOT)
         CALL DBLSNG (NP, WAVEN(IB), WPLOT)
         CALL PLRES (CHIT2(IB), DUMDAT, ' ', 2, ITASK, ' ',
     1      NP, PRBFIL, VARFIT, VARRUN, WPLOT)
      ENDIF
C
C---  hardcopy output calculation parameters to printer port
C
C      CALL PRNRES (ITASK, PRBFIL, VARRUN, VARFIT, GAUSPR, METHPN,
C     1   ' ', SHOTST)
C
      END
      SUBROUTINE TASK23 (DATFIL, NEWREF, PRBFIL, VARFIT, VARRUN,
     1    BACKUP, EXTPRG)
C
C______________________________________________________________________
C
C   task 2 is calculating convolved spectrum, and comparing it
C   with experimental data
C   task 3 is same, but with least-squares fit done
C   output parameters: (none)
C______________________________________________________________________
C
      PARAMETER (NF = 30)
      DIMENSION VARFIT(NF, *), VARRUN(*)
      LOGICAL NEWREF, BACKUP, EXTPRG
      CHARACTER DATFIL*(*), PRBFIL*(*)
C
C  local variables: (save variables read from file in case
C    same file is chosen again)
C
      PARAMETER (NV45 = 10, ND = 5000, NM = 4, NTM = 32*16384)

      SAVE

      DIMENSION CHIDAT(ND), CHIREF(ND), WDAT(ND), WPL(ND),
     1   CHNORM(ND)
      DIMENSION IDGAS(NM), DELV4(0:NV45), DELV5(0:NV45),
     1   PARKOS(4), PARPR(NM)
      LOGICAL DBLCON, GAUSPR, USEREF, TOOMNY, SPCPRB, ENADPT,
     1   SHOTST
      CHARACTER ITITLE*80
C
C---  shared storage for spectra arrays:
C---  note darr2 is used as local in fitcal, thus cannot be used
C---  here, since fitcal is called in execution of task 3
C
      COMMON /FITDAT/ DATPL(ND)
      COMMON /DARR1/ CCONV2(2*NTM), DUMARR(NTM)
      COMMON /DARR3/ CHIT2(NTM)
      COMMON /SARR1/ CHICON(NTM)
      COMMON /SARR2/ WAVEN(NTM)
      COMMON /COMM1/ WPLOT(NTM)
      DOUBLE PRECISION WDAT, WPL, WAVEN
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
C
C---  get experimental data and process it:
C
      CALL DATG23 (DATFIL, NEWREF, USEREF, CHIDAT, CHIREF,
     1   ITITLE, NDAT, WDAT, REFTEM, REFP, VARRUN, BACKUP)
      IF (BACKUP) RETURN
C
      CALL DATPR (CHIDAT, CHIREF, GAINF, NDAT, USEREF, WDAT,
     1   DATPL)
C
      IF (USEREF) THEN
C
C          . . . limit range of comparison to avoid
C             extrapolation outside range stored on file
C
         IF (WBEG .LT. WDAT(1)) WBEG = WDAT(1)
         IF (WEND .GT. WDAT(NDAT)) WEND = WDAT(NDAT)
      ENDIF
C
50    CONTINUE
      CALL DISFIT (GAUSPR, METHPN, VARFIT, ITASK, BACKUP,
     1   EXTPRG, .FALSE.)
      IF (BACKUP .OR. EXTPRG) RETURN
      CALL GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL, TEMP,
     2   TOTNUM, WEXPND, WOFFS, FOFF, BETA)
      CALL GASID (NM, PARPR, IDGAS, NSPEC)
C
C   process data to get wpl and datpl, with woffs and wexpnd
C   known.  datpl will be considered constant during fit,
C   although technically data points could move in or out of
C   fit interval as function of woffs and wexpnd.  this
C   is not allowed in framework of least-squares fit, and
C   therefore must be ignored.
C
      CALL WPREP (DATPL, NDAT, WDAT, WBEG, WEND, WEXPND, WOFFS,
     1    IBEG, USEREF, NPL, WPL, WMIN, WMAX)
C
      IF (ITASK .EQ. 2) THEN
         CALL CALCON (APHI, ATHETA, CHIOFF, CHNRPM, DBLCON, DELV4,
     1      DELV5, GAMPR, GAMPU, GAMPU2, GAUSPR, .FALSE., IDGAS, METHPN,
     2      NSPEC, PARKOS, PARPR, PRBFIL, PRESS, WIDMUL, TEMP,
     3      TOTNUM, WMIN, WMAX, FOFF, CCONV2, CHIT2, NTHE, WAVEN,
     4      DATFIL, VARFIT, VARRUN, DEGSAT, WADD, BETA, APSI,
     5      TOOMNY, SPCPRB, 1, ENADPT)
         IF (TOOMNY) GO TO 50
C
         CALL SQTRPL (CCONV2, NPL, NTHE, WAVEN, WPL, CHICON)
         CALL NORML (CHIEXP, CHICON, CHIREF, IBEG, NDAT, NPL,
     1      USEREF, REFTEM, TEMP, REFP, PRESS, WDAT, WPL, CHNORM,
     2      CHIMAX)
      ELSE
         CALL CALC3 (CHIREF, IBEG, IDGAS, GAUSPR, NDAT, NPL,
     1      NSPEC, PRBFIL, REFTEM, REFP, DATPL, PRESS, TEMP,
     2      VARFIT, VARRUN, WDAT, WPL, WMIN, WMAX, CHNORM,
     3      CHIMAX, CHIT2, NTHE, WAVEN, DATFIL, GAMPR, GAMPU,
     4      FOFF, TOOMNY, SPCPRB, ENADPT, SHOTST, PARPR)
         IF (TOOMNY) GO TO 50
      ENDIF
C
CSPON comment out the following call to sqrarr.
C
      CALL SQRARR (CHIT2, NTHE)
C
C---  renormalize theory and data to physical units, reversing
C---  normalizations done in datpr and norml:
C
      CHIEXP = VARFIT(4, 1)
      CALL RENORM (CHIEXP, CHIMAX, CHNORM, DATPL, NPL, USEREF)
C
      IF (METHPN .EQ. 8) CALL SHIFTW (NTHE, WAVEN, NPL, WPL,
     1   PRESS*PARKOS(1))
C
C---  output results:
C
      CALL RITRES (IRIT)
      IF (IRIT .EQ. 1) CALL RRIT23 (CHNORM, DATPL, ITITLE, NPL,
     1   WPL)
C
      CALL DBLSNG (NPL, WPL, WPLOT)
      CALL PLRES (CHNORM, DATPL, DATFIL, 1, ITASK, ITITLE, NPL,
     1   PRBFIL, VARFIT, VARRUN, WPLOT)
      CALL DBLSNG (NTHE, WAVEN, WPLOT)
      CALL PLRES (CHIT2, DUMARR, DATFIL, 2, ITASK, ITITLE, NTHE,
     1   PRBFIL, VARFIT, VARRUN, WPLOT)
      IF (USEREF) THEN
         CALL DBLSNG (NDAT, WDAT, WPLOT)
         CALL PLRES (CHIREF, DUMARR, DATFIL, 3, ITASK,
     1      ITITLE, NDAT, PRBFIL, VARFIT, VARRUN, WPLOT)
      ENDIF
C
C---  hardcopy output calculation parameters to printer port
C
C      CALL PRNRES (ITASK, PRBFIL, VARRUN, VARFIT, GAUSPR, METHPN,
C     1   DATFIL, SHOTST)
C
      END
      SUBROUTINE TASK4 (VARFIT, VARRUN, BACKUP, EXTPRG)
C
C______________________________________________________________________
C
C   task4 calculates theoretical susceptibility
C______________________________________________________________________
C
      PARAMETER (NF = 30)
      DIMENSION VARFIT(NF, *), VARRUN(*)
      LOGICAL BACKUP, EXTPRG
C
C  local variables:
C
      PARAMETER (NJ = 130, NV = 6, NM = 4, NW = 1000, NV45 =
     1   10, NTM = 32*16384, ND = 5000)
      DIMENSION IDGAS(NM), DELV4(0:NV45), DELV5(0:NV45),
     1   PARPR(NM), AMPL(NW, NM, 2), WTRAN(NW, NM, 2), GAMMA(NW,
     2   NM, 2), NQ(NW, 4, NM, 2), MAXQ(2, NM), MAXV5(0:NV45),
     3   NT(NM, 2), POP(0:NV, 0:NJ, NM), POPN(0:NV, 0:NJ, NM),
     4   POPV45(0:NV45, 0:NV45), T11(NW, 2, NM, 2), PARKOS(4),
     5   DUMW(ND)
      COMPLEX AMPKOS(NW)
      LOGICAL LOFFQ(2), DBLCON, GAUSPR, USEREF, TWOPMP, TOOMNY,
     1   SPCPRB, ENADPT, SHOTST, DOPPLR
      INTEGER XMODEL(NW, NM, 2)
      CHARACTER*1 TTYPE(NW, NM, 2)
      CHARACTER*20 PRBFIL
      CHARACTER*40 DATFIL
      CHARACTER*80 ITITLE
C
C---  shared storage for spectra arrays:
C
      COMMON /DARR1/ CHIT2(2*NTM), DUMARR(NTM)
      COMMON /DARR2/ CHIIM(NTM, 2), CHIRL(NTM, 2)
      COMMON /SARR2/ WAVEN(NTM)
      COMMON /COMM1/ WPLOT(NTM)
      DOUBLE PRECISION WAVEN, DUMW
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      PRBFIL = 'none'
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
100   CONTINUE
      CALL DISFIT (GAUSPR, METHPN, VARFIT, ITASK, BACKUP, EXTPRG,
     1   .FALSE.)
      IF (BACKUP .OR. EXTPRG) RETURN
      CALL GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL, TEMP,
     2   TOTNUM, WEXPND, WOFFS, FOFF, BETA)
      CALL GASID (NM, PARPR, IDGAS, NSPEC)
C
      CALL INICAL (IDGAS, METHPN, NSPEC, PRESS, TEMP, WBEG,
     1   WEND, WTRAN, GAMMA, LOFFQ, NQ, MAXQ, MAXV4, MAXV5, NT,
     2   POP, POPN, POPV45, TTYPE, XMODEL, T11, VARFIT, WADD,
     3   WLIL, WBIG, FOFF, APSI, TWOPMP, PARPR)
C
      CALL CHIAMP (APHI, ATHETA, NT, IDGAS, LOFFQ, NQ, NSPEC,
     1   POP, T11, TEMP, TOTNUM, XMODEL, APSI, TWOPMP, AMPKOS,
     2   MAXQ, AMPL)
C
C  calculate waven array, then corresponding chit2
C
      CALL MINGAM (GAMMA, NSPEC, NT, TEMP, WLIL, WBIG, IDGAS,
     1   DOPPLR, GAMMIN)
      CALL THEWAV (GAMMIN, NSPEC, NT, NTM, WBEG, WEND, WTRAN,
     1   NTHE, WAVEN, TOOMNY)
C
      IF (TOOMNY) GO TO 100
      CHIOFF = 0.
      CALL CHICAL (AMPKOS, AMPL, APHI, ATHETA, CHNRPM, DELV4,
     1   DELV5, GAMMA, IDGAS, .TRUE., NT, NQ, MAXQ, MAXV4, MAXV5,
     2   NSPEC, NTHE, PARKOS, PARPR, POP, POPN, POPV45, PRESS,
     3   TEMP, TOTNUM, WAVEN, WIDMUL, WBEG, WTRAN, XMODEL,
     4   FOFF, DEGSAT, BETA, APSI, TWOPMP, CHIIM, CHIRL, CHIT2,
     5   NTM, 1)
C
CSPON comment out the following call to sqrarr.
C
      CALL SQRARR (CHIT2, NTHE)
C
      IF (METHPN .EQ. 8) CALL SHIFTW (NTHE, WAVEN, 1, DUMW,
     1   PRESS*PARKOS(1))
C
C---  output results:
C
      CALL RITPAR (IRIT1, 'none', PRBFIL, VARRUN, VARFIT)
      IF (IRIT1 .EQ. 1) CALL RRPAR (AMPL, GAMMA, IDGAS,
     1   NSPEC, NQ, NT, POP, POPN, TTYPE, WTRAN, VARFIT)
C
      CALL RITRES (IRIT)
      IF (IRIT .EQ. 1) CALL RRIT4 (NTHE, WAVEN, CHIT2)
C
      CALL DBLSNG (NTHE, WAVEN, WPLOT)
      CALL PLRES (CHIT2, DUMARR, DATFIL, 2, ITASK, ITITLE, NTHE,
     1   PRBFIL, VARFIT, VARRUN, WPLOT)
C
C---  hardcopy output calculation parameters to printer port
C
C      CALL PRNRES (ITASK, PRBFIL, VARRUN, VARFIT, GAUSPR, METHPN,
C     1   ' ', SHOTST)
C
      END
      SUBROUTINE TASK5 (DATFIL, PRBFIL, VARFIT, VARRUN, BACKUP,
     1   EXTPRG)
C
C______________________________________________________________________
C
C   task5 is calculating new reference array for a data file
C______________________________________________________________________
C
      PARAMETER (NF = 30)
      DIMENSION VARFIT(NF, *), VARRUN(*)
      LOGICAL BACKUP, EXTPRG
      CHARACTER DATFIL*(*), PRBFIL*(*)
C
C  local variables:
C
      PARAMETER (ND = 5000, NM = 4, NV45 = 10, NTM = 32*16384)
      DIMENSION IDGAS(NM), PARPR(NM), CHIDAT(ND), CHIREF(ND),
     1   DELV4(0:NV45), DELV5(0:NV45), PARKOS(4), WDAT(ND)
      CHARACTER ITITLE*80
      LOGICAL DBLCON, GAUSPR, USEREF, TOOMNY, SPCPRB, ENADPT, SHOTST
C
C---  shared storage for spectra arrays:
C
      COMMON /FITDAT/ DATPL(ND)
      COMMON /DARR1/ CCONV2(2*NTM), DUMARR(NTM)
      COMMON /DARR2/ CHIT2(4*NTM)
      COMMON /SARR2/ WAVEN(NTM)
      COMMON /COMM1/ WPLOT(NTM)
      DOUBLE PRECISION WDAT, WAVEN, WBEGD, WENDD
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
100   CONTINUE
      CALL DISFIT (GAUSPR, METHPN, VARFIT, ITASK, BACKUP, EXTPRG,
     1   .FALSE.)
      IF (BACKUP .OR. EXTPRG) RETURN
      CALL GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL, TEMP,
     2   TOTNUM, WEXPND, WOFFS, FOFF, BETA)
      CALL GASID (NM, PARPR, IDGAS, NSPEC)
      CALL NRINIT (APHI, ATHETA, CHIOFF, GAINF, NBRNCH, NREJEC,
     1   WIDMUL, FOFF, APSI)
C
C---  get experimental data and process it
C
      CALL DATG5 (DATFIL, CHIDAT, ITITLE, NDAT, WDAT)
C
      CALL DATPR (CHIDAT, CHIREF, GAINF, NDAT, USEREF, WDAT,
     1   DATPL)
C
      IF (USEREF) THEN
C
C          . . . limit range of comparison to avoid
C             extrapolation outside range stored on file
C
         IF (WBEG .LT. WDAT(1)) WBEG = WDAT(1)
         IF (WEND .GT. WDAT(NDAT)) WEND = WDAT(NDAT)
      ENDIF
C
C      . . .   do entire wdat array, rather than just between
C      wbeg and wend, for new data file.  the plot, if requested,
C      will only be between wbeg and wend.
C
      WMIN = WDAT(1)
      WMAX = WDAT(NDAT)
      CALL CALCON (APHI, ATHETA, CHIOFF, CHNRPM, DBLCON, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, GAUSPR, .FALSE., IDGAS, METHPN,
     2   NSPEC, PARKOS, PARPR, PRBFIL, PRESS, WIDMUL, TEMP, TOTNUM,
     3   WMIN, WMAX, FOFF, CCONV2, CHIT2, NTHE, WAVEN,
     4   DATFIL, VARFIT, VARRUN, DEGSAT, WADD, BETA, APSI, TOOMNY,
     5   SPCPRB, 1, ENADPT)
      IF (TOOMNY) GO TO 100
C
C---  interpolate at data values and sqrt to produce convolved
C---  spectrum chiref at wavelengths wdat
C
CSPON comment out the following call to sqrarr.
C
      CALL SQRARR (CHIT2, NTHE)
      CALL SQTRPL (CCONV2, NDAT, NTHE, WAVEN, WDAT, CHIREF)
C
      IF (METHPN .EQ. 8) CALL SHIFTW (NTHE, WAVEN, NDAT, WDAT,
     1   PRESS*PARKOS(1))
C
C---  output results:
C
C---  write new data file with freshly computed ref. array
C
      CALL RRIT5 (ITITLE, TEMP, PRESS, NDAT, WDAT, CHIDAT,
     1   CHIREF)
C
C   give opportunity to plot reference susceptibility and
C   reference spectrum (after convolution)
C   first, get indices so plots will reflect requested
C   wavenumber range only
C
      WBEGD = WBEG
      WENDD = WEND
      CALL SERCHD (NTHE, WAVEN, WBEGD, IB, IER)
      CALL SERCHD (NTHE, WAVEN, WENDD, IE, IER)
      NP = IE-IB+1
      CALL DBLSNG (NP, WAVEN(IB), WPLOT)
      CALL PLRES (CHIT2(IB), DUMARR, DATFIL, 2, ITASK, ITITLE, NP,
     1   PRBFIL, VARFIT, VARRUN, WPLOT)
      CALL SERCHD (NDAT, WDAT, WBEGD, IB, IER)
      CALL SERCHD (NDAT, WDAT, WENDD, IE, IER)
      NP = IE-IB+1
      CALL DBLSNG (NP, WDAT(IB), WPLOT)
      CALL PLRES (CHIREF(IB), DUMARR, DATFIL, 3, ITASK, ITITLE,
     1   NP, PRBFIL, VARFIT, VARRUN, WPLOT)
C
C---  hardcopy output calculation parameters to printer port
C
C      CALL PRNRES (ITASK, PRBFIL, VARRUN, VARFIT, GAUSPR, METHPN,
C     1   DATFIL, SHOTST)
C
      END
      SUBROUTINE TASK6 (PRBFIL, VARFIT, VARRUN, BACKUP, EXTPRG)
C
C______________________________________________________________________
C
C   task6 is calculating reference array, no data file
C______________________________________________________________________
C
      PARAMETER (NF = 30)
      DIMENSION VARFIT(NF, *), VARRUN(*)
      LOGICAL BACKUP, EXTPRG
      CHARACTER* (*)PRBFIL
C
C---  local variables:
C
      PARAMETER (NM = 4, NV45 = 10, NTM = 32*16384, ND = 5000)
      DIMENSION WPL(ND), CHICON(ND), CHIREF(ND), CHNORM(ND),
     1   DUMDAT(ND)
      DIMENSION IDGAS(NM), PARPR(NM), DELV4(0:NV45),
     1   DELV5(0:NV45), PARKOS(4)
      LOGICAL DBLCON, GAUSPR, USEREF, TOOMNY, SPCPRB, ENADPT, SHOTST
C
C---  shared storage for spectra arrays:
C
      COMMON /DARR1/ CCONV2(2*NTM), DUMARR(NTM)
      COMMON /DARR2/ CHIT2(4*NTM)
      COMMON /SARR2/ WAVEN(NTM)
      COMMON /COMM1/ WPLOT(NTM)
      DOUBLE PRECISION WPL, WAVEN, WBEGD, WENDD
      DATA DUMDAT / ND*0. /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
100   CONTINUE
      CALL DISFIT (GAUSPR, METHPN, VARFIT, ITASK, BACKUP, EXTPRG,
     1   .FALSE.)
      IF (BACKUP .OR. EXTPRG) RETURN
      CALL GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL, TEMP,
     2   TOTNUM, WEXPND, WOFFS, FOFF, BETA)
      CALL GASID (NM, PARPR, IDGAS, NSPEC)
      CALL NRINIT (APHI, ATHETA, CHIOFF, GAINF, NBRNCH, NREJEC,
     1   WIDMUL, FOFF, APSI)
C
C---  set up wavenumber array for convolution.
C
      CALL WPREP1 (WBEG, WEND, WEXPND, GAMPU, NPL, WPL, WMIN, WMAX)
C
      CALL CALCON (APHI, ATHETA, CHIOFF, CHNRPM, DBLCON, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, GAUSPR, .FALSE., IDGAS, METHPN,
     2   NSPEC, PARKOS, PARPR, PRBFIL, PRESS, WIDMUL, TEMP, TOTNUM,
     3   WBEG, WEND, FOFF, CCONV2, CHIT2, NTHE, WAVEN, 'none',
     4   VARFIT, VARRUN, DEGSAT, WADD, BETA, APSI, TOOMNY, SPCPRB,
     5   1, ENADPT)
      IF (TOOMNY) GO TO 100
C
CSPON comment out the following call to sqrarr.
C
      CALL SQRARR (CHIT2, NTHE)
      CALL SQTRPL (CCONV2, NPL, NTHE, WAVEN, WPL, CHICON)
      CALL NORML (CHIEXP, CHICON, CHIREF, 1, NPL, NPL, .FALSE.,
     1   TEMP, TEMP, PRESS, PRESS, WPL, WPL, CHNORM, CHIMAX)
C
C---  renormalize theory to physical units, reversing
C---  normalization done in norml.
C
      CALL RENORM (CHIEXP, CHIMAX, CHNORM, DUMDAT, NPL,
     1   USEREF)
C
      IF (METHPN .EQ. 8) CALL SHIFTW (NTHE, WAVEN, NPL, WPL,
     1   PRESS*PARKOS(1))
C
C---  output results:
C
      CALL RITRES (IRIT)
      IF (IRIT .EQ. 1) CALL RRIT6 (CHIT2, CCONV2, NTHE, WAVEN)
C
C       determine the indices of waven between wbeg
C       and wend so printing and plotting do not
C       include convolution end effects
C
      WBEGD = WBEG
      WENDD = WEND
      CALL SERCHD (NTHE, WAVEN, WBEGD, IB, IER)
      CALL SERCHD (NTHE, WAVEN, WENDD, IE, IER)
      NP = IE-IB+1
C
      CALL DBLSNG (NPL, WPL, WPLOT)
      CALL PLRES (CHICON, DUMDAT, ' ', 3, ITASK, ' ', NPL,
     1   PRBFIL, VARFIT, VARRUN, WPLOT)
      CALL DBLSNG (NP, WAVEN(IB), WPLOT)
      CALL PLRES (CHIT2(IB), DUMDAT, ' ', 2, ITASK, ' ', NP,
     1   PRBFIL, VARFIT, VARRUN, WPLOT)
C
C---  hardcopy output calculation parameters to printer port
C
C      CALL PRNRES (ITASK, PRBFIL, VARRUN, VARFIT, GAUSPR, METHPN,
C     1   ' ', SHOTST)
C
      END
      SUBROUTINE TASK7 (PRBFIL, VARFIT, VARRUN, BACKUP, EXTPRG)
C
C______________________________________________________________________
C
C   task7 is generating libraries for quickfitter program ftcars
C   output parameters: ( none )
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      DIMENSION VARFIT(NF, *), VARRUN(*)
      LOGICAL BACKUP, EXTPRG
      CHARACTER* (*)PRBFIL
C
C  local variables:
C
      PARAMETER (ND = 5000, NM = 4, NV45 = 10, NTM = 32*16384)
      DIMENSION WPL(ND), CHICON(ND), DUMDAT(ND), RLSPC(ND), SQRSPC(ND)
      DIMENSION DELV4(0:NV45), DELV5(0:NV45), IDGAS(NM),
     1   PARKOS(4), PARPR(NM)
      LOGICAL DBLCON, GAUSPR, USEREF, HAVPRB, TOOMNY, SPCPRB,
     1   ENADPT, SHOTST
C
C---  shared storage for spectra arrays:
C
      COMMON /DARR1/ CCONV2(2*NTM), DUMARR(NTM)
      COMMON /DARR2/ REFCON(4*NTM)
      COMMON /DARR3/ CHIT2(NTM)
      COMMON /SARR1/ REFTHE(NTM)
      COMMON /SARR2/ WAVEN(NTM)
      COMMON /COMM1/ WPLOT(NTM)
      DOUBLE PRECISION WPL, WAVEN, WBEGD, WENDD
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
100   CONTINUE
      CALL DISFIT (GAUSPR, METHPN, VARFIT, ITASK, BACKUP, EXTPRG,
     1   .FALSE.)
      IF (BACKUP .OR. EXTPRG) RETURN
C
      CALL GETFIT (PRESS, VARFIT, APHI, CHIEXP, CHIOFF, DELV4,
     1   DELV5, GAMPR, GAMPU, GAMPU2, NM, PARKOS, PARPR, WIDMUL, TEMP,
     2   TOTNUM, WEXPND, WOFFS, FOFF, BETA)
      CALL GASID (NM, PARPR, IDGAS, NSPEC)
C
C---  set up wavenumber array for convolution, with no wavenumber
C---  offset or expansion.
C
      WEXPND = 1.
      CALL WPREP1 (WBEG, WEND, WEXPND, GAMPU, NPL, WPL, WMIN, WMAX)
C
      HAVPRB = .FALSE.
      CHIOFF = 0.
C
      DO 200 ILIB = 1, 3
C
         CALL CALCON (APHI, ATHETA, CHIOFF, CHNRPM, DBLCON, DELV4,
     1      DELV5, GAMPR, GAMPU, GAMPU2, GAUSPR, HAVPRB, IDGAS, METHPN,
     2      NSPEC, PARKOS, PARPR, PRBFIL, PRESS, WIDMUL, TEMP,
     3      TOTNUM, WBEG, WEND, FOFF, CCONV2, CHIT2, NTHE, WAVEN,
     4      'none', VARFIT, VARRUN, DEGSAT, WADD, BETA, APSI,
     5      TOOMNY, SPCPRB, ILIB, ENADPT)
         IF (TOOMNY) GO TO 100
         HAVPRB = .TRUE.
C
         IF (ILIB .EQ. 1) THEN
            CALL SQRARR (CHIT2, NTHE)
            CALL SQTRPL (CCONV2, NPL, NTHE, WAVEN, WPL, CHICON)
            PRINT *, 'Calculating real part of susceptibility',
     1         ', one moment please . . .'
            PRINT *, ' '
         ELSEIF (ILIB .EQ. 2) THEN
C

            DO 300 N = 1, NPL
               CALL TERPLD (NTHE, WAVEN, CCONV2, WPL(N),
     1            RLSPC(N), IER)
300         CONTINUE
C
            PRINT *, 'Calculating square of susceptibility',
     1         ', one moment please . . .'
            PRINT *, ' '
         ELSEIF (ILIB .EQ. 3) THEN
C
            DO 400 N = 1, NPL
               CALL TERPLD (NTHE, WAVEN, CCONV2, WPL(N),
     1            SQRSPC(N), IER)
400         CONTINUE
C
         ENDIF
200   CONTINUE
C
      IF (METHPN .EQ. 8) CALL SHIFTW (NTHE, WAVEN, NPL, WPL,
     1   PRESS*PARKOS(1))
C
C---  output results:
C
      CALL RITT7X (NPL, WPL)
C
      CALL RITT7Y (TEMP, NPL, RLSPC, SQRSPC)
C
C       determine the indices of waven between wbeg
C       and wend so printing and plotting do not
C       include convolution end effects
C
      WBEGD = WBEG
      WENDD = WEND
      CALL SERCHD (NTHE, WAVEN, WBEGD, IB, IER)
      CALL SERCHD (NTHE, WAVEN, WENDD, IE, IER)
      NP = IE-IB+1
C
      CALL DBLSNG (NPL, WPL, WPLOT)
      CALL PLRES (CHICON, DUMDAT, ' ', 1, ITASK, ' ',
     1   NPL, PRBFIL, VARFIT, VARRUN, WPLOT)
      CALL DBLSNG (NP, WAVEN(IB), WPLOT)
      CALL PLRES (CHIT2(IB), DUMDAT, ' ', 2, ITASK, ' ',
     1   NP, PRBFIL, VARFIT, VARRUN, WPLOT)
C
C---  hardcopy output calculation parameters to printer port
C
C      CALL PRNRES (ITASK, PRBFIL, VARRUN, VARFIT, GAUSPR, METHPN,
C     1   ' ', SHOTST)
C
      END
      SUBROUTINE THEWAV (GAMMIN, NSPEC, NT, NTM, WBEG, WEND,
     1   WTRAN, NTHE, WAVEN, TOOMNY)
C
C______________________________________________________________________
C
C  creates waven array, including transition line centers, for task4
C    output parameters:
C      waven : wavenumber array
C      nthe  : number points in waven
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NM = 4)
      DIMENSION NT(NM, *), WAVEN(*), WTRAN(NW, NM, *)
      DIMENSION WCEN(NW*NM)
      DOUBLE PRECISION WAVEN, WREG, DW
      LOGICAL TOOMNY
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  get subset of wavecenters in region of interest
C---  then sort into ascending order
C
      NCEN   = 0
C
      DO 100 M = 1, NSPEC
      DO 100 IPMP = 1, 2
         DO 100 K = 1, NT(M, IPMP)
            IF (WTRAN(K, M, IPMP) .GT. WBEG .AND. WTRAN(K, M, IPMP) .LT.
     1      WEND) THEN
               NCEN = NCEN+1
               WCEN(NCEN) = WTRAN(K, M, IPMP)
            ENDIF
100   CONTINUE
C
C---  sorting routine from ref. 2
C
      CALL SSORT (WCEN, DUM, NCEN, 1)
C
C---  get interval dw as fraction of minimum line width
C
      DATA FGAM / 0.2 /
      DW = GAMMIN*FGAM
      DW1 = DW
      NEVEN = IFIX((WEND-WBEG)/DW1)+1
      IF (NEVEN .LT. 256) THEN
         NEVEN = 256
         DW = (WEND-WBEG)/255.
         TOOMNY = .FALSE.
      ELSEIF (NEVEN .GT. NTM) THEN
         PRINT '(/2A, I10, A)',
     1      ' !! SPECTRUM REQUIRES TOO MANY WAVENUMBER ',
     2      'GRID POINTS: ', NEVEN, ' !!'
         TOOMNY = .TRUE.
	   CALL PRESS_ENTER()
         RETURN
      ELSE
         TOOMNY = .FALSE.
      ENDIF
      PRINT '(A, T54, I6)' ,
     1   ' . . . Number evenly spaced points to be used =' ,
     2   NEVEN
      PRINT '(A, T54, I6/)' ,
     1   ' . . . Added line centers within interval =' , NCEN
C
C---  create waven array as evenly spaced points between wbeg and
C---  wend, adding any transition wavelengths found in interval
C
      IC    = 1
      NTHE  = 1
      WAVEN(1) = WBEG
C
      DO 200 I = 2, NEVEN
         WREG = WBEG+(I-1)*DW
         IF (IC .LE. NCEN) THEN
            IF (WCEN(IC) .LT. WREG) THEN
150            CONTINUE
               NTHE = NTHE+1
               WAVEN(NTHE) = WCEN(IC)
               IC = IC+1
               IF (IC .GT. NCEN) GO TO 180
               IF (WCEN(IC) .GT. WREG) GO TO 180
               GO TO 150
            ENDIF
         ENDIF
180      CONTINUE
         NTHE = NTHE+1
         WAVEN(NTHE) = WREG
200   CONTINUE
C
      IF (NTHE .GT. NTM) THEN
         PRINT '(/2A, I10, A)',
     1      ' !! SPECTRUM REQUIRES TOO MANY WAVENUMBER ',
     2      'GRID POINTS: ', NTHE, ' !!'
         TOOMNY = .TRUE.
		CALL PRESS_ENTER ()
      ENDIF
C
      END
      SUBROUTINE TRNCOD (NQ, IDGAS, LOFFQ, NSPEC, NT, METHPN,
     1   TTYPE, XMODEL)
C
C______________________________________________________________________
C
C   determines character codes to use in logic for each transition
C
C     ttype : indicates type of transition for each transition
C             retained:
C        'Q' = q-branch;  'S' = s-branch;  'O' = o-branch;
C        'R' = rotational;   'W' = water vapor
C
C     xmodel : indicates susceptibility model to be used for each
C              transition:
C        1:  'NOMI' = nominal model
C        2:  'RFUL' = rotational diffusion, first part
C        3:  'RPAR' = rotational diffusion, second part
C        4:  'KFUL' = exponential gap model, full analysis
C        5:  'KPAR' = exponential model, use stored results
C        6:  'VOGT' = Voigt profile model
C        7:  'GTRY' = Galatry profile model
C        8:  'HDCL' = hard collision model
C        9:  'SATR' = cw saturated line model
C       10:  'C2H2' = acetylene model
C       11:  'CRHC' = Correlated hard collision model
C       12:  'CO2 ' = CO2 q-branch model
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NM = 4)
      DIMENSION IDGAS(*), NQ(NW, 4, NM, *), NT(NM, *)
      INTEGER XMODEL(NW, NM, *)
      CHARACTER*(*) TTYPE(NW, NM, *)
      LOGICAL LOFFQ(2)
      INTEGER VG, VGPREV, VU
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DO 500 IPMP = 1, 2
      DO 500 M = 1, NSPEC
C
C---  transition type:
C
         DO 400 K = 1, NT(M, IPMP)
            IF (GASNAM(IDGAS(M)) .EQ. 'CO2') THEN
               XMODEL(K, M, IPMP) = 12
               TTYPE(K, M, IPMP) = 'Q'
               GO TO 400
            ELSEIF (GASNAM(IDGAS(M)) .EQ. 'H2O') THEN
               TTYPE(K, M, IPMP) = 'W'
               GO TO 90
            ENDIF
            VG = NQ(K, 1, M, IPMP)
            VU = NQ(K, 2, M, IPMP)
            JG = NQ(K, 3, M, IPMP)
            JU = NQ(K, 4, M, IPMP)
C
            IF (VG .EQ. VU) THEN
               IF (JU .EQ. JG+2) TTYPE(K, M, IPMP) = 'R'
            ELSE
               IF (JU .EQ. JG) TTYPE(K, M, IPMP)   = 'Q'
               IF (JU .EQ. JG+2) TTYPE(K, M, IPMP) = 'S'
               IF (JU .EQ. JG-2) TTYPE(K, M, IPMP) = 'O'
            ENDIF
C
C   chi model to use: assume nominal model as default
C
90          XMODEL(K, M, IPMP) = 1
            IF (GASNAM(IDGAS(M)) .EQ. 'N2') THEN
C
C         . . . use nominal model if off-resonant n2 q-branch
C
               IF (LOFFQ(IPMP) .AND. K .EQ. NT(M, IPMP)) GO TO 400
C
            ELSEIF (GASNAM(IDGAS(M)) .EQ. 'C2H2') THEN
               XMODEL(K, M, IPMP) = 10
               GO TO 400
            ENDIF
C
C   q-branch allows options for rotational diffusion or
C   exponential gap model
C
            IF (TTYPE(K, M, IPMP) .EQ. 'Q') THEN
               IF (METHPN .EQ. 2) THEN
C
C              . . . rotational diffusion desired.  set xmodel = 2
C              ('RFUL') for first transition of vibrational manifold
C              only.  other transitions will use a different model
C              in rotdif, for which xmodel = 3 ('RPAR')
C
                  XMODEL(K, M, IPMP) = 3
                  IF (K .EQ. 1) THEN
                     XMODEL(K, M, IPMP) = 2
                  ELSE
                     VGPREV = NQ(K-1, 1, M, IPMP)
                     IF ((VG .NE. VGPREV) .OR. (TTYPE(K-1, M, IPMP)
     1               .NE. 'Q')) XMODEL(K, M, IPMP) = 2
                  ENDIF
               ELSEIF (METHPN .EQ. 3) THEN
C
C            . . . exponential gap model desired, but only want to do
C            matrix manipulations for first transition in vibrational
C            manifold (XMODEL='KFUL').  for other transitions, xmodel=
C            'KPAR', and koschi will be called, but stored results will
C            be used. at present, limit use of model to first gas only
C
                  IF (M .NE. 1) GO TO 400
                  XMODEL(K, M, IPMP) = 5
                  IF (K .EQ. 1) THEN
                     XMODEL(K, M, IPMP) = 4
                  ELSE
                     VGPREV = NQ(K-1, 1, M, IPMP)
                     IF ((VG .NE. VGPREV) .OR. (TTYPE(K-1, M, IPMP)
     1                  .NE. 'Q')) XMODEL(K, M, IPMP) = 4
                  ENDIF
               ENDIF
            ENDIF
            IF (METHPN .EQ. 4) THEN
               XMODEL(K, M, IPMP) = 6
            ELSEIF (METHPN .EQ. 5) THEN
               XMODEL(K, M, IPMP) = 9
            ELSEIF (METHPN .EQ. 6) THEN
               XMODEL(K, M, IPMP) = 7
            ELSEIF (METHPN .EQ. 7) THEN
               XMODEL(K, M, IPMP) = 8
            ELSEIF (METHPN .EQ. 8) THEN
               XMODEL(K, M, IPMP) = 11
            ENDIF
400      CONTINUE
500   CONTINUE
C
      END
      SUBROUTINE TTERMS (IDGAS, NT, NQ, NW, NSPEC, TTYPE, T11)
C
C______________________________________________________________________
C
C   calculates t11, used by chiamp
C______________________________________________________________________
C
      PARAMETER (NM = 4)
      DIMENSION IDGAS(*), NT(NM, *), NQ(NW, 4, NM, *), T11(NW, 2, NM, *)
      CHARACTER*1 TTYPE(NW, NM, *)
C
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
      COMMON /PARMOL/ PARMOL(12, NM)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
C   c = speed of light, cm/sec;  fo45=(4./45.)
C
      DATA C/2.998E10/, FO45/0.8888889E-01/
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DO 100 M = 1, NSPEC
      DO 100 IPMP = 1, 2
         IDG = IDGAS(M)
         IF (GASNAM(IDG) .EQ. 'CO2') THEN
C
C---  comment out following line to remove co2 routine
C
            CALL CO2T11 (NT, T11, IPMP)
         ELSEIF (GASNAM(IDG) .EQ. 'H2O') THEN
C
C---  comment out following line to remove h2o routine
C
C            CALL H2OT11 (NT, T11)
         ELSE
            AC1 = PARMOL(1, IDG)
            AG  = PARMOL(2, IDG)
            AG2 = AG**2
            BE  = PARMOL(3, IDG)
            CANI = PARMOL(4, IDG)
            CANIQ = PARMOL(5, IDG)
            DGAMDR = PARMOL(6, IDG)
            GAM    = PARMOL(7, IDG)
            RE     = PARMOL(8, IDG)
            WE     = PARMOL(9, IDG)
C
C---  ac1 = (dalpha/dq)/sqrt(2.*pi*c*wraman*reduced mass).
C---  the factor 1./2.*pi*c in dchi gives the energy denominator
C---  in chi3 the correct units.
C
C---  ag = (dgamma/dq)/(dalpha/dq), i.e., the ratio of the anisotropy
C---  of the polarizability derivative to the mean polarizability
C---  derivative.
C
            DCHI   = AC1**2/(6.*PI*C)*1.E18
C
            DO 200 K = 1, NT(M, IPMP)
               JG = NQ(K, 3, M, IPMP)
               RJ = JG
               RJP1 = JG+1
               RJM1 = JG-1
               RJP2 = JG+2
               R2JP1 = 2*JG+1
               R2JM1 = 2*JG-1
               R2JP3 = 2*JG+3
C
               IF (TTYPE(K, M, IPMP) .EQ. 'Q') THEN
C
C                   .... q branch
C
                  BJJ = RJ*RJP1/(R2JM1*R2JP3)
                  CENTQ = 1.-CANIQ*RJ*RJP1
                  T11(K, 1, M, IPMP) = DCHI*(1.+FO45*BJJ*AG2)*CENTQ
                  T11(K, 2, M, IPMP) = DCHI*BJJ*AG2/15.*CENTQ
               ELSEIF (TTYPE(K, M, IPMP) .EQ. 'S') THEN
C
C               .... s branch  (ref. 17)
C
                  BJJ = 1.5*RJP1*RJP2/(R2JP3*R2JP1)
                  CENTS = (1.-CANI*R2JP3)**2
                  T11(K, 1, M, IPMP) = DCHI*FO45*BJJ*AG2*CENTS
                  T11(K, 2, M, IPMP) = 0.75*T11(K, 1, M, IPMP)
               ELSEIF (TTYPE(K, M, IPMP) .EQ. 'O') THEN
C
C                 .... o branch (ref. 17)
C
                  BJJ = 1.5*RJ*RJM1/(R2JM1*R2JP1)
                  CENTO = (1.+CANI*(2.*RJ-1.))**2
                  T11(K, 1, M, IPMP) = DCHI*FO45*BJJ*AG2*CENTO
                  T11(K, 2, M, IPMP) = 0.75*T11(K, 1, M, IPMP)
               ELSEIF (TTYPE(K, M, IPMP) .EQ. 'R') THEN
C
C                 .... pure rotational branch.
C         .... constant in t1111 is 2*(.52918e-8)**6/(3.*hbar)*1.e18.
C
                  BJJ = 1.5*RJP1*RJP2/(R2JP3*R2JP1)
                  CENTS = (1.+(2.*BE/WE)**2*DGAMDR*RE/
     1                    GAM*(RJ*RJP1+R2JP3))**2
                  T11(K, 1, M, IPMP) = 1.3876E-5/
     1                           (2.*PI*C)*FO45*BJJ*GAM**2*CENTS
                  T11(K, 2, M, IPMP) = 0.75*T11(K, 1, M, IPMP)
               ENDIF
200         CONTINUE
         ENDIF
100   CONTINUE
C
      END
      SUBROUTINE UMODF (GAUSPR, ITASK, METHPN, BACKUP, EXTPRG,
     1   MODS, VARFIT)
C
C______________________________________________________________________
C
C  allows user to modify fit variables
C   (called in VAX version only)
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      DIMENSION VARFIT(*)
      LOGICAL BACKUP, MODS, LPRIN, GAUSPR, EXTPRG
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
C
C   local variables:
C
      DIMENSION CHANG(NF), DAT(10)
      LOGICAL ERROR
      CHARACTER STRING*80
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL SWITCH (METHPN)
C
530   CONTINUE
      PRINT *, 'Numbers of items to be changed ? <none> '
      READ (5, '(A)' ) STRING
C
      IF (STRING .EQ. ' ') THEN
C
C                 . . . exit on blank string (carriage return only)
C
         CALL FITCHK (ITASK, METHPN, ERROR)
         MODS = .FALSE.
         IF (ERROR) THEN
            MODS = .TRUE.
         ENDIF
         PRINT *, ' '
         RETURN
      ENDIF
C
      CALL INTERP (0, STRING, -8, NCHANG, CHANG, IER)
      IF (IER .NE. 0) GO TO 530
C
      MODS = .TRUE.
C
      DO 600 N = 1, NCHANG
         NVAR = CHANG(N)
         IF (NVAR .EQ. 0) THEN
            BACKUP = .TRUE.
         ELSEIF (NVAR .EQ. -1) THEN
            EXTPRG = .TRUE.
         ELSEIF (NVAR .GE. 1 .AND. NVAR .LE. NF .AND.
     1   LPRIN(GAUSPR, METHPN, NVAR, ITASK, VARFIT)) THEN
580         CONTINUE
            STRING = 'New value for ' //
     1      NAMFIT(NVAR)(1:LNFIT(NVAR)) // ' ? <unchanged> '
            LEN    = LNFIT(NVAR)+29
            PRINT *, STRING(1:LEN)
            READ (5, '(A)' ) STRING
            CALL INTERP (0, STRING, -1, NFOUND, DAT, IER)
            IF (NFOUND .EQ. 1) VARFIT(NVAR) = DAT(1)
            IF (IER .NE. 0) GO TO 580
         ELSE
            PRINT '(A, I2, A)' , ' !! ERROR . . ITEM ' , NVAR,
     1      ' NOT ON LIST - IGNORED !!'
         ENDIF
600   CONTINUE
C
C
      END
      SUBROUTINE UMODF3 (BACKUP, EXTPRG, GAUSPR, METHPN, MODS,
     1   VARFIT)
C
C______________________________________________________________________
C
C  allows user to modify fit variables
C   (called in VAX version only)
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17)
      DIMENSION VARFIT(NF, *)
      LOGICAL BACKUP, MODS, LPRIN, EXTPRG, GAUSPR
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
C
C   local variables:
C
      DIMENSION DAT(4), MSAVE(30), NSAVE(30)
      CHARACTER STRING*80, PROMP(3)*7
      LOGICAL ERROR
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CALL SWITCH (METHPN)
C
      PRINT '(1x,2a)',
     1   'Input items to be changed - e.g., 2B means min. value ',
     2   'of variable #2 <none>'
      READ (5, '(A)' ) STRING
C
      IF (STRING .EQ. ' ') THEN
         CALL FITCHK (3, METHPN, ERROR)
         MODS = .FALSE.
         IF (ERROR) THEN
            MODS = .TRUE.
         ENDIF
         PRINT *, ' '
         RETURN
      ENDIF
C
C   backup or exit selection must be only one chosen in this case -
C
      IF (STRING .EQ. '0') THEN
         BACKUP = .TRUE.
         RETURN
      ELSEIF (STRING .EQ. '-1') THEN
         EXTPRG = .TRUE.
         RETURN
      ENDIF
C
      CALL PARSE (STRING, NCH, MSAVE, NSAVE)
      MODS = .TRUE.
C
      DO 420 N = 1, NCH
         NVAR = MSAVE(N)
C
         IF (LPRIN(GAUSPR, METHPN, NVAR, 3, VARFIT)) THEN
            NS = NSAVE(N)
            IF (NS .EQ. 4) THEN
               VARFIT(NVAR, 4) = -VARFIT(NVAR, 4)
            ELSE
               DATA PROMP/ 'nominal' , 'minimum' , 'maximum' /
410            CONTINUE
               STRING = 'New ' // PROMP(NS) // ' value for ' //
     1          NAMFIT(NVAR)(1:LNFIT(NVAR)) //
     2         ' ? <unchanged> '
               LEN    = LNFIT(NVAR)+37
               PRINT *, STRING(1:LEN)
               READ (5, '(A)' ) STRING
               CALL INTERP (0, STRING, -1, NFOUND, DAT, IER)
               IF (NFOUND .EQ. 1) VARFIT(NVAR, NS) = DAT(1)
               IF (IER .NE. 0) GO TO 410
            ENDIF
C
         ELSE
            PRINT '(A, I2, A)' , ' !! ERROR . . ITEM ' , NVAR,
     1      ' NOT ON LIST - IGNORED !!'
         ENDIF
C
420   CONTINUE
C
      END
      SUBROUTINE UMODR (ITASK, BACKUP, MODS, PRBFIL, VARRUN)
C
C______________________________________________________________________
C
C   allows user to change values of run variables
C   (called by VAX version only)
C______________________________________________________________________
C
      PARAMETER (NM = 4, NF = 30, NR = 17)
      DIMENSION VARRUN(*)
      CHARACTER* (*)PRBFIL
      LOGICAL BACKUP, MODS
C
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
      COMMON /NAMTSK/ NAMTSK(7)
      CHARACTER NAMTSK*50
C
C  local variables:
C
      DIMENSION CHANG(8), DAT(4)
      CHARACTER STRING*80, PROMP(8)*80
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
90    CONTINUE
      PRINT *, 'Numbers of items to be changed ? <none> '
      READ (5, '(A)' ) STRING
C
      IF (STRING .EQ. ' ') THEN
         MODS = .FALSE.
         RETURN
      ENDIF
C
      MODS = .TRUE.
C
      CALL INTERP (0, STRING, -8, NCHANG, CHANG, IER)
      IF (IER .GT. 0) GO TO 90
C
      DO 200 N = 1, NCHANG
         NCH = CHANG(N)
         IF (NCH .EQ. 4) THEN
            DATA IWADD / 1 /
            PROMP(1) = 'Default'
            PROMP(2) = 'Input from keyboard'
            CALL MENU (1, 2, IWADD, PROMP)
            IF (IWADD .EQ. 1) THEN
	          VARRUN(4) = 0.
	          GO TO 200
	       ENDIF
	    ENDIF

         IF (NCH .EQ. 0) THEN
            BACKUP = .TRUE.
         ELSEIF (NCH .EQ. 1) THEN
		   CALL CLEAR_TEXT ()
            CALL MENU (1, 7, ITASK, NAMTSK)
            VARRUN(NCH) = ITASK
C
C---  change to a variable with numerical value:
C
	    ELSEIF ((NCH .GE. 2 .AND. NCH .LE. 5) .OR. (NCH .EQ. 7)
     1      .OR. (NCH .EQ. 11) .OR. (NCH .EQ. 14) .OR.
     2      (NCH .EQ. 15)) THEN
95          CONTINUE
            STRING      = 'New value for ' //
     1      NAMRUN(NCH)(1:LNRUN(NCH)) // ' ? <unchanged> '
            LEN = LNRUN(NCH)+29
            PRINT *, STRING(1:LEN)
            READ (5, '(A)' ) STRING
            IF (STRING .NE. ' ') THEN
               CALL INTERP (0, STRING, 1, NFOUND, DAT, IER)
               IF (NFOUND .EQ. 1) VARRUN(NCH) = DAT(1)
               IF (IER .NE. 0) GO TO 95
            ENDIF
C
C---  mods to variables which are multiple choice:
C
         ELSEIF (NCH .EQ. 6) THEN
            PROMP(1) = 'Isolated line model'
            PROMP(2) = 'Rotational diffusion model'
            PROMP(3) = 'Exponential gap model'
            PROMP(4) = 'Voigt profile model'
            PROMP(5) = 'Saturated line model'
            PROMP(6) = 'Galatry profile model'
            PROMP(7) = 'Hard collision model'
            PROMP(8) = 'Correlated hard collision model'
            methpn = varrun(nch)
      		CALL CLEAR_TEXT ()
            CALL MENU (1, 8, METHPN, PROMP)
            VARRUN(NCH) = METHPN
         ELSEIF (NCH .EQ. 9) THEN
	       nbrnch = varrun(nch)
            IF (NBRNCH .EQ. 0) NBRNCH = 1
            PROMP(1) =
     1      'O-branch subtraction in ref. spectrum calc.'
            PROMP(2) =
     1      'Q-branch subtraction in ref. spectrum calc.'
            PROMP(3) = 'Neither O- nor Q-branch subtraction'
            CALL MENU (1, 3, NBRNCH, PROMP)
            IF (NBRNCH .EQ. 3) NBRNCH = 0
            VARRUN(NCH) = NBRNCH
         ELSEIF (NCH .EQ. 10) THEN
            DO 190 IG = 1, 4
               PROMP(IG) = 'Gas to be rejected = ' //
     1         GASNAM(IG)
190         CONTINUE
	       nrejec = varrun(nch)
            CALL MENU (1, 4, NREJEC, PROMP)
            VARRUN(NCH) = NREJEC
C
C---  variables which serve as logical flags:
C
         ELSEIF (NCH .EQ. 8 .OR. NCH .EQ. 12 .OR. NCH .EQ. 16.
     1      .OR. NCH .EQ. 17) THEN
            VARRUN(NCH) = -VARRUN(NCH)
         ELSEIF (NCH .EQ. 13) THEN
            PRINT *, 'Probe instrument function file name ? '
            PRINT *, '   (for gaussian, type none)'
            READ (5, '(A)' ) PRBFIL
            IF (PRBFIL .EQ. ' ') PRBFIL = 'none'
         ELSE
            PRINT *, ' !! ERROR - ILLEGAL OPTION CHOSEN !!'
         ENDIF
200   CONTINUE
C
      END
      SUBROUTINE VARERR (ERRMES, NLINE)
C
C______________________________________________________________________
C
C   prints non-fatal error message for bad input line in run
C   parameter file.
C
C______________________________________________________________________
C
      CHARACTER* (*)ERRMES
C
      PRINT '(A, I3, A/4X, A, A)' , ' !! NONFATAL ERROR ON LINE' ,
     1   NLINE, ' OF RUN PARAMETER FILE:' , ERRMES,
     2   ' . . LINE WILL BE IGNORED !!'
C
      END
      SUBROUTINE VARIN (DATFIL, PRBFIL, VARRUN, VARFIT)
C
C______________________________________________________________________
C
C   reads data file containing run control and fitting variables
C     output parameters:
C       datfil : name of data file (if appropriate)
C       prbfil : name of probe data file
C       varrun : run control variables (see block data for meanings)
C       varfit : fitting variables (see block data for meanings)
C______________________________________________________________________
C
      PARAMETER (NF = 30, NR = 17, NM = 4)
      CHARACTER* (*)DATFIL, PRBFIL
      DIMENSION VARRUN(NR), VARFIT(NF, *)
C
C  local variables
C
      CHARACTER STRING*80, INFO*80, KEY*80
      LOGICAL LFOUND
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  reset default values:
C
      CALL SETDEF (DATFIL, PRBFIL, VARFIT, NF, VARRUN)
C
      DO 100 NLINE = 1, 1000
         READ (22, '(1X, A)' , END = 120) STRING
C
C---  nstart is position immediately after equal sign
C
         NSTART = INDEX(STRING, '=' )+1
         IF (NSTART .EQ. 1) THEN
            CALL VARERR ( 'NO EQUAL SIGN FOUND' , NLINE)
            GO TO 100
         ENDIF
C
C---  form keyword as substring up to equal sign
C---  ifc = 0 implies blank line - ignore without diagnostic
C
         IFC = IFIRSC(STRING)
         IF (IFC .EQ. 0) GO TO 100
         KEY = STRING(IFC:NSTART-2)
C
C---  form info as significant substring after = sign
C
         LENSTR = LENTH(STRING)
         INDF = IFIRSC(STRING(NSTART:LENSTR))+NSTART-1
         INFO = STRING(INDF:LENSTR)
C
C---  check for data or probe file:
C
         IF (KEY(1:4) .EQ. 'data' .OR. KEY(1:4) .EQ. 'DATA'
     1   .OR. KEY(1:4) .EQ. 'Data') THEN
            DATFIL = INFO
            GO TO 100
         ENDIF
         IF (KEY(1:10) .EQ. 'Probe inst') THEN
            PRBFIL = INFO
            GO TO 100
         ENDIF
C
C---  check against run control variable names:
C
         CALL INPRUN (KEY, NLINE, INFO, VARRUN, LFOUND)
         IF (LFOUND) GO TO 100
C
C---  no match found among run control variables;
C---  see if it is fitting variable
C
         CALL INPFIT (KEY, NLINE, STRING(NSTART:LENSTR),
     1   VARFIT)
100   CONTINUE
C
120   CONTINUE
C
C       . . .   end of file reached
      CLOSE (22)
      END
      SUBROUTINE VAROUT (DATNAM, IDEV, PRBFIL, VARRUN, ITASK,
     1   RUNFIL, VARFIT)
C
C______________________________________________________________________
C
C   writes data file containing run control and fitting variables
C      output parameters: (none)
C______________________________________________________________________
C
      PARAMETER (NF = 30, NM = 4, NR = 17)
      DIMENSION VARRUN(NR), VARFIT(NF, *)
      CHARACTER DATNAM*(*), PRBFIL*(*), RUNFIL*(*)
C
      COMMON /NAMVAR/ NAMRUN(NR), NAMFIT(NF)
      CHARACTER NAMFIT*40, NAMRUN*40
      COMMON /LENNAM/ LNRUN(NR), LNFIT(NF)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
C
      LOGICAL DBLCON, USEREF, SPCPRB, ENADPT, SHOTST, GAUSPR
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  use default name for fitting variable 21 in all cases
C
      CALL SWITCH(1)
C
      IF (IDEV. EQ. 22)
     1   CALL OPFILE (RUNFIL, 22, 40, 'Name of run parameter file'
     2   , 'unknown' )
      WRITE (IDEV, '(1X, A, A)' ) 'Data file =' , DATNAM
      WRITE (IDEV, '(1X, A, A)' )
     1   'Probe instrument function file =' , PRBFIL
C
C---  run control variables first
C
      CALL GETRUN (VARRUN, PRBFIL, NBRNCH, ATHETA, CHNRPM,
     1   DBLCON, GAINF, GAUSPR, ITASK, METHPN, NREJEC, USEREF,
     2   WBEG, WEND, DEGSAT, WADD, APSI, SPCPRB, ENADPT, SHOTST)
C
      DO 100 N = 1, NR
         IF (N .EQ. 6) THEN
            IF (METHPN .EQ. 1) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Isolated line model'
            IF (METHPN .EQ. 2) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Rotational diffusion model'
            IF (METHPN .EQ. 3) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Exponential gap model'
            IF (METHPN .EQ. 4) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Voigt profile model'
            IF (METHPN .EQ. 5) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Saturated line model'
            IF (METHPN .EQ. 6) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Galatry profile model'
            IF (METHPN .EQ. 7) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Hard collision model'
            IF (METHPN .EQ. 8) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Correlated hard collision model'
         ELSEIF (N .EQ. 8) THEN
            IF (USEREF) WRITE (IDEV, 1020) NAMRUN(N)(1:LNRUN(N)),
     1       'yes'
            IF ( .NOT. USEREF) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'no'
         ELSEIF (N .EQ. 9) THEN
            IF (NBRNCH .EQ. 1) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'O'
            IF (NBRNCH .EQ. 2) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'Q'
            IF (NBRNCH .EQ. 0) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'none'
         ELSEIF (N .EQ. 10) THEN
            WRITE (IDEV, 1020) NAMRUN(N)(1:LNRUN(N)),
     1      GASNAM(NREJEC)
         ELSEIF (N .EQ. 12) THEN
            IF (DBLCON) WRITE (IDEV, 1020) NAMRUN(N)(1:LNRUN(N)),
     1       'double'
            IF (.NOT. DBLCON) WRITE (IDEV, 1020)
     1      NAMRUN(N)(1:LNRUN(N)), 'single'
         ELSEIF (N .EQ. 13) THEN
C
C---  probe instrument function file, varrun(13), already written
C---  above
C
            CONTINUE
         ELSEIF (N .EQ. 16) THEN
            IF (ENADPT) WRITE (IDEV, 1020) NAMRUN(N)(1:LNRUN(N)),
     1         'enable'
            IF (.NOT. ENADPT) WRITE (IDEV, 1020)
     1         NAMRUN(N)(1:LNRUN(N)), 'disable'
         ELSEIF (N .EQ. 17) THEN
            IF (SHOTST) WRITE (IDEV, 1020) NAMRUN(N)(1:LNRUN(N)),
     1         'shot noise'
            IF (.NOT. SHOTST) WRITE (IDEV, 1020)
     1         NAMRUN(N)(1:LNRUN(N)), 'none'
         ELSE
C
C            . . . numerical value
C
            WRITE (IDEV, 1000) NAMRUN(N)(1:LNRUN(N)), VARRUN(N)
         ENDIF
100   CONTINUE
C
C---  fitting variables: don't write constraints unless fitting
C---  was done, so when file is read, they will be generated as
C---  the nominal values are read
C
C      NRIT = 1
C      IF (ITASK .EQ. 3) NRIT = 4
C
C---  for the time being, write all four fitting variables
C
      NRIT = 4
C
      DO 200 N = 1, NF
         WRITE (IDEV, 1010) NAMFIT(N)(1:LNFIT(N)), (VARFIT(N, K),
     1      K = 1, NRIT)
200   CONTINUE
C
      IF (IDEV. EQ. 22) CLOSE (22)
1000  FORMAT(1X, A, ' =' , G15.8)
1010  FORMAT(1X, A, ' =' , 4(G12.6, 1X))
1020  FORMAT(1X, A, ' =' , A)
      END
      SUBROUTINE VGTPFL (TEMP, GAMJ, WJ, WDF, WAVEN, CMASS, AMPL,
     1   XREAL, XIMAG)
C
C______________________________________________________________________
C
C  calculates voigt profile for susceptibility
C   output parameters:
C      xreal : real part of susceptibility
C      ximag : imaginary part of susceptibility
C______________________________________________________________________
C
      DOUBLE PRECISION WAVEN(*)
      COMMON /CONST/ AVAG, CEX, PI, VSTP, SQTPI
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      WJ0 = WJ+WAVEN(1)
C
C---  constant = sqrt(2k/m0)/c.
C
      ALPHAD = 4.3014E-7*WJ0*SQRT(TEMP/CMASS)
      XV = -(WDF-WJ)/ALPHAD
      YV = GAMJ/(2.*ALPHAD)
      CALL CPF (XV, YV, WRV, WIV)
C
C---  lineshape function is not normalized to unit area.  area of
C---  lorentzian lineshape is pi/2, so voigt profile is
C---  corrected accordingly.
C
      AC = SQTPI/(2.*ALPHAD)
      XREAL = AMPL*WIV*AC
      XIMAG = AMPL*WRV*AC
C
      END
      SUBROUTINE VGTRY (X, Y, Z, GR, GI)
C
C______________________________________________________________________
C  this routine computes the standardized galatry function
C  g(x', y, z) where:
C  x'(x) is the distance from the shifted line center, x' = x-s
C  y(y) is the collisional broadening parameter, and
C  z(z) is the collisional narrowing parameter.
C  x', y, z are normalized by the 1/e doppler halfwidth.
C  y corresponds to voigt a coefficient, and g(x, y, 0) = v(x, a).
C
C  the voigt function is the real part of the complex probability
C  function computed by the external routine cpf.
C
C  philip l. varghese               <821116.1042>
C______________________________________________________________________
C
      COMPLEX I, Q, W, DW(8), A(75), ALPHA, DELT
      DIMENSION C(8), FACT(8), DELR(8), DELI(8)
      DATA I, ZOLD, RTPI/(0.,1.), 0., 1.772454/
      DATA FACT/1., 2., 6., 24., 120., 720., 5040., 40320./
      DATA N2MAX, N3MAX/75, 75/
C
      IF (Z .GT. 0.) GO TO 5
C
C---  if z = 0, compute voigt function (real(cpf))
C
      CALL CPF(X, Y, GR, GI)
      RETURN
C
C---  branch to regions i-iii of y,z plane, depending on inputs
C
5     CONTINUE
      IF (Z .GT. 5.) GO TO 200
      IF (Z .LE. 0.1) GO TO 90
      IF (Y .GE. (4.*Z**0.868)) GO TO 300
      GO TO 200
90    IF ((Y .GE. 0.5) .OR. (Z .GT. 0.04) .OR. (ABS(X) .GT. 2.))
     1   GO TO 300
C
C---  region i - (0.<y<0.5, 0.<z<0.04, x<2.) expansion about the voigt
C---  function
C
C---  compute the coefficients of the asymptotic expansion of
C                     n=N                       n=N
C---  exp(-1/(2*z*z)*Sigma{(-z*t)**n/n!} = 1 + Sigma{c(n)*t**n}
C                     n=3                       n=3
C
C---  if the value of z is unchanged since the last call, the
C---  coefficients are not recomputed
C
100   CONTINUE
      IF (Z .EQ. ZOLD) GO TO 15
      ZOLD = Z
      DO 10 J = 3, 8
10       C(J) = -(-Z)**(J-2)/(2.*FACT(J))
      C(6) = C(6)+C(3)*C(3)/2.
      C(7) = C(7)+C(3)*C(4)
      C(8) = C(8)+C(3)*C(5)+C(4)*C(4)/2.
15    CALL CPF(X, Y, WR, WI)
      Q = CMPLX(X, Y)
      W = CMPLX(WR, WI)
C
c--- compute derivatives (dw) of the complex probability function (w)
C
      DW(1) = 2.*(I/RTPI-Q*W)
      DW(2) = -2.*(Q*DW(1)+W)
      DO 20 J = 3, 8
20       DW(J) = -2.*(Q*DW(J-1)+(J-1)*DW(J-2))
C
C---  compute correction to the voigt profile.  if the asymptotic
C---  expansion diverges, then terminate
C
      DELT = C(3)*DW(3)*I
      DELR(3) = REAL(DELT)
      DO 30 J = 4, 8
         DELR(J) = REAL(C(J)*DW(J)/I**J)
         DELI(J) = AIMAG(C(J)*DW(J)/I**J)
         IF (ABS(DELR(J)) .GE. ABS(DELR(J-1))) GO TO 35
         DELT = DELT+CMPLX(DELR(J), DELI(J))
30       CONTINUE
35    GR = WR+REAL(DELT)
      GI = WI+AIMAG(DELT)
      RETURN
C
C---  region ii - a & b (0.<y<4z**.868, 0.1<z<5.) & (all y, z>5.)
C
C---  the number of terms in the sum was determined empirically
C
200   CONTINUE
      N2 = 4.+(1.+3.*EXP(-1.1*Y))/Z**1.05
      IF (N2 .GT. N2MAX) N2 = N2MAX
      Q = CMPLX(Y, -X)
      DELL = 0.5/(Z*Z)
      ALPHA = DELL*(1.+2.*Z*Q)
      W = 1./ALPHA
      A(1) = W*DELL/(ALPHA+1.)
      W = W+A(1)
      DO 210 J = 2, N2
         A(J) = A(J-1)*DELL/(ALPHA+J)
         W = W+A(J)
210      CONTINUE
      GR = REAL(W/(RTPI*Z))
      GI = AIMAG(W/(RTPI*Z))
      RETURN
C
C---  region iii - (y>1., 0.04<z<0.1) & (y>4z**.868, 0.6<z<5.)
C
C---  the number of terms in the continued fraction was determined
C---  empirically
C
300   CONTINUE
      N3 = 2.+37.*EXP(-0.6*Y)
      IF (N3 .GT. N3MAX) N3 = N3MAX
      Q = CMPLX(Y, -X)
      A(N3) = 0.5*N3/(N3*Z+Q)
C
C---  initial j in do loop below is n3 in varghese's thesis, have
C---  changed to n3-1 for consistency
C
      DO 310 J = N3-1, 1, -1
         A(J) = 0.5*J/(J*Z+Q+A(J+1))
310      CONTINUE
      A(1) = 1./(Q+A(1))
      GR = REAL(A(1))/RTPI
      GI = AIMAG(A(1))/RTPI
C
      RETURN
      END
      SUBROUTINE WAVCEN (WTRAN, NSPEC, NT, NTHE, NTM, NW, WAVEN,
     1    WBEG, WEND)
C
C______________________________________________________________________
C
C  gets line centers to be added to arrays in task4
C   output parameters:
C      nthe : number elements in waven after adding centers
C      waven : wavenumbers array with centers added
C______________________________________________________________________
C
      PARAMETER (NM = 4)
      DIMENSION WTRAN(NW, NM, *), NT(NM, *), WAVEN(*)
      DOUBLE PRECISION WAVEN
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DO 100 M = 1, NSPEC
      DO 100 IPMP = 1, 2
         DO 100 K = 1, NT(M, IPMP)
            IF ((WTRAN(K, M, IPMP) .GT. WBEG) .AND. (WTRAN(K, M, IPMP)
     1      .LT. WEND)) THEN
               NTHE = NTHE+1
               IF (NTHE .GT. NTM) CALL QUITS (0,
     1'!! FROM WAVCEN - CODE DIMENSIONED INSUFFICIENTLY !!')
               WAVEN(NTHE) = WTRAN(K, M, IPMP)
            ENDIF
100   CONTINUE
C
      END
      SUBROUTINE WAVMOD (IBEG, NPL, WDAT, WEXPND, WOFFS, WPL,
     1   WMIN, WMAX)
C
C______________________________________________________________________
C
C  produces shifted and expanded version of experimental wavenumber
C  array wdat, starting at position ibeg, for npl points
C     output parameters:
C       wpl   : shifted and expanded version of wdat
C       wmin  : minimum value of wpl (wpl(1))
C       wmax  : maximum value of wpl (wpl(npl))
C______________________________________________________________________
C
      DIMENSION WDAT(*), WPL(*)
      DOUBLE PRECISION WDAT, WPL, WSTRT
C
      WSTRT = WDAT(1)+WOFFS
C
      DO 80 I = 1, NPL
         DIFOLD = WDAT(IBEG+I-1)-WDAT(1)
         WPL(I) = WSTRT+DIFOLD*WEXPND
80    CONTINUE
C
      WMIN = WPL(1)
      WMAX = WPL(NPL)
C
      END
      SUBROUTINE WAVOFF (CCONV2, NPL, NTHE, WAVEN, WPL, CHICON,
     1   CHIEXP, CHIREF, IBEG, NDAT, USEREF, REFTEM, TEMP, REFP, PRESS,
     2   WDAT, CHNORM, CHIMAX, WOFDEL, WEXDEL, CHISV, CHRFSV, JVARY)
C
C______________________________________________________________________
C
C  when fitting variable being adjusted is woffs or wavexp,
C  interpolates square of convolved theoretical susceptibilities
C  at data values wpl, then takes sqrt to produce chicon.
C  also normalizes theoretical convolved susceptibility so it can
C  be compared to data.
C   output variables:
C     chnorm - the normalized version of chicon
C     chimax  - maximum of calculated spectrum without
C               expansion factor.
C______________________________________________________________________
C
      DIMENSION CHICON(*), CHNORM(*), CHIREF(*), WDAT(*),
     1   WPL(*), CCONV2(*), WAVEN(*), CHISV(*), CHRFSV(*)
      DOUBLE PRECISION WDAT, WPL, WAVEN
      LOGICAL USEREF
C
C---  add change slope*wdel in chicon to unperturbed values.
C---  wdel due to perturbation in woffs or wexpnd
C
      IL = 1
C
      DO 100 I = 1, NPL
        CALL SERCHD (NTHE, WAVEN, WPL(I), IL, IER)
        DW1 = WAVEN(IL+1)-WAVEN(IL)
        DW2 = WDAT(IBEG+I-1)-WDAT(1)
        SLOPE = (CCONV2(IL+1)-CCONV2(IL))/DW1
        WDEL = WOFDEL+DW2*WEXDEL
        CHICON(I) = CHISV(I)+CHISV(I)*SLOPE*WDEL/(2.*CCONV2(IL))
        IF (CHICON(I) .LT. 0.) CHICON(I) = 0.
100   CONTINUE
C
      IF (USEREF) THEN
C
C            . . . include correction for reference temp and
C                  pressure, and modify peak value by 1/chiexp
C
         RAT = (TEMP/REFTEM)*(REFP/PRESS)/CHIEXP
C
C---  interpolate to get reference intensity corresponding to each
C---  wavenumber wpl(i)
C
         IL = 1
C
         DO 210 I = 1, NPL
            CALL SERCHD (NDAT, WDAT, WPL(I), IL, IER)
            DW1 = WDAT(IL+1)-WDAT(IL)
            DW2 = WDAT(IBEG+I-1)-WDAT(1)
            SLOPE = (CHIREF(IL+1)+CHIREF(IL))/DW1
            WDEL = WOFDEL+DW2*WEXDEL
            CREF = CHRFSV(I)+SLOPE*WDEL
            CHNORM(I) = CHICON(I)*RAT/CREF
210      CONTINUE
C
         CALL MNMAX (CHNORM, 2, NPL, 1, DUM, CHIMAX)
         CHIMAX = CHIMAX*CHIEXP
C
      ELSE
C
C            . . . normalize peak of theory to 1./chiexp
C                  this will be reversed for plotting
C                  keep chimax intact for later.
C
         CALL MNMAX (CHICON, 2, NPL, 1, DUM, CHIMAX)
         CALL DIVZER (CHIMAX, 3, 'NORML' , 'CHIMAX' )
C
         RAT = 1./(CHIEXP*CHIMAX)
         DO 240 I = 1, NPL
            CHNORM(I) = CHICON(I)*RAT
240      CONTINUE
C
      ENDIF
C
      END
      SUBROUTINE WLIMS (GAM0, PRESS, WMIN, WMAX, WADD, WBIG, WLIL)
C
C______________________________________________________________________
C
C   sets limits within which to include transitions.  default is
C   500 line widths either side of [wmin,wmax].  can also input
C   directly from run parameter menu
C______________________________________________________________________
C
      IF (WADD .EQ. 0.) THEN
         TOST = 500.*GAM0
      ELSE
         TOST = ABS(WADD)
      ENDIF
C
      WLIL = WMIN-TOST
      IF (WLIL .LT. 1.) WLIL = 1.0
      WBIG = WMAX+TOST
      END
      SUBROUTINE WPREP (DATPL, NDAT, WDAT, WBEG, WEND, WEXPND,
     1   WOFFS, IBEG, USEREF, NPL, WPL, WMIN, WMAX)
C
C______________________________________________________________________
C
C  processes data arrays which are dependent on woffs and wexpnd
C  called if doing task2 to obtain all the following output parameters,
C  and once before entering fit to obtain ibeg, npl, and datpl, which
C  are considered constant during fit.
C    output parameters:
C      ibeg   : index in data arrays of point corresponding to wbeg
C      npl    : number of data points in [wbeg,wend]
C      wpl    : shifted and expanded version of wavenumber data wdat
C      wmin, wmax : minimum, maximum values in wpl
C      datpl  : intensity data corresponding to wpl
C______________________________________________________________________
C
      DIMENSION DATPL(*), WDAT(*), WPL(*)
      DOUBLE PRECISION WDAT, WPL, WBEGD, WENDD, WPL1, DIFOLD
      LOGICAL USEREF
C
C---  shift and expand entire data array to form temporary wpl
C
      WPL(1) = WDAT(1)+WOFFS
      WPL1   = WPL(1)
C
      DO 80 I = 2, NDAT
         DIFOLD = WDAT(I)-WDAT(1)
         WPL(I) = WPL1+DIFOLD*WEXPND
80    CONTINUE
C
C---  check to see if there are any data points in plot range - if
C---  not, quit
C
      IF ((WPL(1) .GE. WEND) .OR. (WPL(NDAT) .LE. WBEG)) CALL QUITS
     1   (-1, '!! FROM WPREP - NO DATA POINTS WITHIN PLOT RANGE !!')
C
C---  produce subset of wpl between user-requested limits wbeg, wend
C
      WBEGD = WBEG
      WENDD = WEND
      CALL SERCHD (NDAT, WPL, WBEGD, IBEG, IER)
      CALL SERCHD (NDAT, WPL, WENDD, IEND, IER)
      IBEG = IBEG+1
      NPL  = 0
C
      DO 100 I = IBEG, IEND
         NPL = NPL+1
         WPL(NPL) = WPL(I)
100   CONTINUE
C
      WMIN = WPL(1)
      WMAX = WPL(NPL)
C
      DO 120 I = 1, NPL
         DATPL(I) = DATPL(IBEG+I-1)
120   CONTINUE
C
C---  normalize data to 1 if using unreferenced data.  After fitting,
C---  data will be renormalized to max. value of theoretical calculation
C
      IF (.NOT. USEREF) THEN
         CALL MNMAX (DATPL, 2, NPL, 1, DUM, DPLMAX)
         CALL DIVZER (DPLMAX, 1, 'WPREP' , 'DPLMAX' )
C
         DO 140 I = 1, NPL
            DATPL(I) = DATPL(I)/DPLMAX
140      CONTINUE
C
      ENDIF
C
      END
      SUBROUTINE WPREP1 (WBEG, WEND, WEXPND, GAMPU, NPL, WPL,
     1   WMIN, WMAX)
C
C______________________________________________________________________
C
C  creates wavelength array modified by wexpnd for doing convolution
C  for task1, task6, and task7.  the number of points in the array is
C  the lesser of (10.*((wend-wbeg)/gampu)+1) or 5000.
C    output parameters:
C      npl    : number of convolution points in [wbeg,wend]
C      wpl    : expanded version of wavenumber array for convolution
C      wmin, wmax : minimum, maximum values in wpl
C______________________________________________________________________
C
      PARAMETER (ND = 5000)
      DOUBLE PRECISION WPL(ND), DW, DIFOLD
C
C---  expand wpl by wexpnd.  woffs wavenumber shift is assumed = 0.
C---  use gampu/10. as dw.
C
      WPL(1) = WBEG
      DW = AMAX1(GAMPU/10.0, 1.0E-4)
      NPL = INT((WEND-WBEG)/DW)+1
      IF (NPL .GT. ND) THEN
         NPL = ND
         DW = (WEND-WBEG)/(ND - 1)
      ENDIF
C
      DO 80 I = 2, NPL
         DIFOLD = FLOAT(I-1)*DW
         WPL(I) = WBEG+DIFOLD*WEXPND
80    CONTINUE
C
      WMIN = WPL(1)
      WMAX = WPL(NPL)
C
      END
