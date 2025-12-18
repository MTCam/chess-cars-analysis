C      PROGRAM CO2CRS
C
C    Program for calculation of CARS (coherent anti-Stokes Raman
C    spectroscopy) Q-branch spectra for the co2 molecule.  Robert J. 
C    Hall of United Technologies Research Center was especially helpful 
C    in providing insight for much of this program, and his assistance
C    is gratefully acknowledged.  The code is based to a large extent
C    on the approach described in R. J. Hall and J. H. Stufflebeam,
C    Appl. Opt. 23, 4319 (1984).
C
C    CO2CRS can act as a stand-alone program if the various write
C    statements are used.  However, its primary use is as a library
C    for calculating co2 cars spectra via calls from CARSFT, a large
C    code for a wide variety of cars applications.  To include 
C    CO2CRS in CARSFT, the calls to the CO2CRS subroutines in 
C    CARSFT should be uncommented, and this source code should be
C    compiled, turned into a library, and linked to CARSFT.
C
C    In addition to this source code for CO2CRS an additional file
C    called CO2.MOL is needed.  This file contains molecular constants 
C    for the co2 transitions.  No formal instruction manual has been 
C    written for CO2CRS; for this reason the source code contains 
C    a large number of comment statements intended to assist the user.
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
C
C      PARAMETER (NM = 4, NCO2 = 12, NS = 200, NTR = 60, NJC = 150)
C      PARAMETER (NW = 1000, NV = 6, NJ = 130)
C      DIMENSION T11(NW, 2, NM)
C      DIMENSION NT(NM), IDGAS(NM), MAXQ(2, NM), AMPL(NW, NM),
C     1   NQ(NW, 4, NM), WTRAN(NW, NM), WAVEN(1), POP(0:NV, 0:NJ, NM)
C      COMMON /CO2/ MCO2, B000, LERG, IVIBL2(NS), 
C     1   EJCO2(0:NJC, 2, NTR), CO2PRM(NTR, NCO2), POPCO2(0:NJC, 2, 
C     2   NTR), PPNCO2(0:NJC, NTR), AMPCO2(0:NJC, NTR),
C     3   WJK(0:NJC, NTR), RATIO, ERG(NS)
C      COMMON /GASNAM/ GASNAM(NM)
C      CHARACTER*5 GASNAM
Cvax
C      DOUBLE PRECISION WAVEN, ERG
Cend
C
C---  the following data is useful for running a test problem as a
C---  stand-alone program, and is not needed for a library for carsft.
C
C      DATA TEMP, WLIL, WBIG, TOTNUM / 1600., 1360., 1460., 1.E19 /
C      DATA IDGAS / 1, 2, 3, 4 / 
C      DATA DFSPHI, GAMJ, WIDMUL, WDF, WAVEN / 0., 0.06839, 1., 
C     1   1400., 0. /
C      GASNAM(1) = 'CO2'
C
C      CALL CO2TRN (IDGAS, NT, TEMP, WLIL, WBIG, MAXQ, NQ, WTRAN)
C
C      CALL CO2POP (TEMP, NT, MAXQ, NQ, POP)
C
C      CALL CO2AMP (TOTNUM, 1, MAXQ, NT, AMPL)
C
C      CALL CO2T11 (NT, T11)
C
C      DO 100 K = 1, NT(MCO2)
C         CALL CO2SPC (DFSPHI, GAMJ, NT, K, MAXQ, WIDMUL, WDF, WAVEN, 
C     1      XIMAG, XREAL)
C100   CONTINUE
C
C      END

      SUBROUTINE CO2AMP (TOTNUM, K, MAXQ, NT, IPMP, AMPL)
C
C______________________________________________________________________
C
C  calculates amplitudes for co2 transitions.
C  called by subroutine chiamp in carsft.
C    output variables:
C      ampco2  :  population contribution to amplitude for vibrational
C                 transition k = 1 to nt(co2)
C______________________________________________________________________
C
      PARAMETER (NS = 200, NTR = 60, NCO2 = 12, NJC = 150, NW =1000,
     1   NM = 4)
      DIMENSION MAXQ(2, *), NT(NM, *), AMPL(NW, NM, *)
      COMMON /CO2/ MCO2, B000, LERG, IVIBL2(NS), 
     1   EJCO2(0:NJC, 2, NTR, 2), CO2PRM(NTR, 2, NCO2), POPCO2(0:NJC, 2, 
     2   NTR, 2), PPNCO2(0:NJC, NTR, 2), AMPCO2(0:NJC, NTR, 2),
     3   WJK(0:NJC, NTR, 2), RATIO, ERG(NS)
Cvax
      DOUBLE PRECISION ERG
Cend
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
CSPON when converting to spontaneous raman spectroscopy, replace
CSPON all population differences with population of initial state.
C
      JMAX = MAXQ(2, MCO2)
      DEGEN = CO2PRM(K, IPMP, 10)
C
      IF (DEGEN .EQ. 2.) THEN
         DO 200 J = 0, JMAX, 2
            AMPCO2(J, K, IPMP) =
     1         (POPCO2(J, 1, K, IPMP) - POPCO2(J, 2, K, IPMP)) *
     2         FLOAT(2*J+1)*TOTNUM*AMPL(K, MCO2, IPMP)
            AMPCO2(J+1, K, IPMP) =
     1         (POPCO2(J+1, 1, K, IPMP) - POPCO2(J+1, 2, K, IPMP)) *
     2         FLOAT(2*J+3)*TOTNUM*AMPL(K, MCO2, IPMP)
200      CONTINUE
      ELSE
         DO 300 J = 0, JMAX, 2
            AMPCO2(J, K, IPMP) =
     1         (POPCO2(J, 1, K, IPMP) - POPCO2(J, 2, K, IPMP)) *
     2         FLOAT(2*J+1)*TOTNUM*AMPL(K, MCO2, IPMP)
            AMPCO2(J+1, K, IPMP) = 0.
300      CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CO2GAM (JMAX, WIDMUL, GAMJ, GAM)
C
C______________________________________________________________________
C
C  calculates co2 rotational linewidths.  assumes same widths for all
C  vibrational states.
C    output variables:
C      gam  :  linewidth (fwhm)
C______________________________________________________________________
C
      DIMENSION GAM(0:*)
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DATA G10, G20, G30 / 0.10865, 1.0828E-5, 115.6 /
C
C---  correct for gamj(300)
C
      G1 = G10/0.24
      G2 = G20/0.24      
C
C---  fit data of l. rosenmann, j. m. hartmann, and j. taine, appl.
C---  opt. 27, 3902 (1988), to quadratic; truncate at minimum.
C
      DO 100 J = 0, JMAX
         IF (J .GT. G30) THEN
            GAM(J) = G1
         ELSE
            GAM(J) = G1+G2*(J-G30)**2
         ENDIF
         GAM(J) = WIDMUL*GAMJ*GAM(J)
100   CONTINUE
C
      END
      SUBROUTINE CO2POP (TEMP, NT, MAXQ, NQ, POP)
C
C______________________________________________________________________
C
C  calculates co2 rovibrational populations.
C  called by subroutine poplat in carsft.
C  output variables:
C    popco2(j, 1, v) : initial state rotational populations
C    popco2(j, 2, v) : final state rotational populations
C    ppnco2(j, v) : normalized initial state rotational populations 
C______________________________________________________________________
C
      PARAMETER (NS = 200, NTR = 60, NCO2 = 12, NJC = 150)
      PARAMETER (NV = 6, NJ = 130, NW = 1000, NM=4)
      DIMENSION NT(NM, *), MAXQ(2, *), NQ(NW, 4, NM, *),
     1   POP(0:NV, 0:NJ, *)
      COMMON /CO2/ MCO2, B000, LERG, IVIBL2(NS), 
     1   EJCO2(0:NJC, 2, NTR, 2), CO2PRM(NTR, 2, NCO2), POPCO2(0:NJC, 2, 
     2   NTR, 2), PPNCO2(0:NJC, NTR, 2), AMPCO2(0:NJC, NTR, 2),
     3   WJK(0:NJC, NTR, 2), RATIO, ERG(NS)
Cvax
      DOUBLE PRECISION ERG
Cend
C
C  local variables:
C
      DIMENSION POPV(NS), PJ(0:NJC, 2)
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      CONS = 1.439/TEMP
      JMAX = MAXQ(2, MCO2)
      MAXQ(1, MCO2) = 0
C
C---  vibrational partition function:
C
      QVCO2 = 0.
C
      DO 100 I = 1, LERG
         IF (IVIBL2(I) .NE. 0) THEN
            DEGEN = 2.
         ELSE
            DEGEN = 1.
         ENDIF      
         QVCO2 = QVCO2+DEGEN*EXP(-CONS*ERG(I))
100   CONTINUE
C
C---  approximate rotational partition function from herzberg,
C---  infrared and raman spectra, p. 505.  factor of 2 is due to
C---  alternating lines.
C
      QRCO2 = 1./(2.*CONS*B000)
      QCO2 = QVCO2*QRCO2
C
C      WRITE (6, '(1X/1X, A, 3(2X, F9.3))') 'QVCO2, QRCO2, QCO2 = ',
C     1   QVCO2, QRCO2, QCO2
C
C---  calculate partition function and populations of v,j states,
C
	DO 600 IPMP = 1, 2
      DO 600 K = 1, NT(MCO2, IPMP)
         DEGEN = CO2PRM(K, IPMP, 10)
         F1 = DEGEN*EXP(-CONS*CO2PRM(K, IPMP, 1))/QVCO2
         F2 = F1*EXP(-CONS*CO2PRM(K, IPMP, 11))
C
C---  in subroutine rrpar in carsft, v1 and v2 for co2 are set to
C---  zero, and j1 and j2 are the initial and final vibrational level
C---  index (i.e., i = 1 to lerg).  pop in rrpar is the total 
C---  population of the vibrational levels, not of individual 
C---  rovibrational states.
C
         POP(0, NQ(K, 3, MCO2, IPMP), MCO2) = F1
         POP(0, NQ(K, 4, MCO2, IPMP), MCO2) = F2
         QR1 = 0.
         QR2 = 0.
         QR3 = 0.
         QR4 = 0.
C
         DO 200 J = 0, JMAX, 2
            A2J1 = 2.*J+1.
            PJ(J, 1) = EXP(-CONS*EJCO2(J, 1, K, IPMP))
            PJ(J, 2) = EXP(-CONS*EJCO2(J, 2, K, IPMP))
            QR1 = QR1+A2J1*PJ(J, 1)
            QR2 = QR2+A2J1*PJ(J, 2)
            IF (DEGEN .EQ. 2.) THEN
               A2J1 = 2.*J+3.
               PJ(J+1, 1) = EXP(-CONS*EJCO2(J+1, 1, K, IPMP))
               PJ(J+1, 2) = EXP(-CONS*EJCO2(J+1, 2, K, IPMP))
               QR3 = QR3+A2J1*PJ(J+1, 1)
               QR4 = QR4+A2J1*PJ(J+1, 2)
            ELSE
               PJ(J+1, 1) = 0.
               PJ(J+1, 2) = 0.
            ENDIF         
200      CONTINUE
C
         QRR1 = 1./QR1
         QRR2 = 1./QR2
         IF (DEGEN .EQ. 2.) THEN
            QRR3 = 1./QR3
            QRR4 = 1./QR4
         ENDIF
C
C         WRITE (6, '(1X/1X, A, I3, 4(2X, F9.3))') 
C     1      'K, QR1, QR2, QR3, QR4 = ', K, QR1, QR2, QR3, QR4
C         WRITE (6, '(1X/1X, 2A/)')
C     1      '  K   J  POP(K,J)I   POP(K,J)F   PPN(K,J)I',
C     2      '   POP(K,J+1)I POP(K,J+1)F PPN(K,J+1)I'
C
         DO 300 J = 0, JMAX, 2
C
C---  ppnco2 is the initial state rotational population normalized to a 
C---  particular vibrational level.  the factor 2j+1 is included later.
C---  the l-doubling degeneracy factor degen must be removed because f1
C---  and f2 reflect the total vibrational population including both
C---  symmetric and antisymmetric states if l is not zero.
C
            PPNCO2(J, K, IPMP) = PJ(J, 1)*QRR1
            PNCO2F = PJ(J, 2)*QRR2
            POPCO2(J, 1, K, IPMP) = PPNCO2(J, K, IPMP)*F1/DEGEN
            POPCO2(J, 2, K, IPMP) = PNCO2F*F2/DEGEN
C
            PPNCO2(J+1, K, IPMP) = PJ(J+1, 1)*QRR3
            PNCO2F = PJ(J+1, 2)*QRR4
            POPCO2(J+1, 1, K, IPMP) = PPNCO2(J+1, K, IPMP)*F1/DEGEN
            POPCO2(J+1, 2, K, IPMP) = PNCO2F*F2/DEGEN
C
C            WRITE (6, '(2(1X, I3), 6(2X, E10.4))') K, J,
C     1         POPCO2(J, 1, K), POPCO2(J, 2, K), PPNCO2(J, K),
C     2         POPCO2(J+1, 1, K), POPCO2(J+1, 2, K), PPNCO2(J+1, K)
300      CONTINUE
600   CONTINUE
C
      RETURN
      END
      SUBROUTINE CO2SPC (DFSPHI, GAMJ, K, MAXQ, WIDMUL, WDF, WAVEN, 
     1   IPMP, XREAL, XIMAG)
C
C______________________________________________________________________
C
C  calculates imaginary and real parts of susceptibility for
C  co2 Q-branch using rotational diffusion model.
C  called by subroutine chical in carsft.
C    input parameters:
C      dfsphi : degree of vibrational dephasing.  value is believed to
C               be zero, but set to 0.025 to avoid numerical problems
C               for transitions (including 1388 cm-1 line) that have
C               nearly zero vibrational-rotational interaction (delb).
C      gamj   : temperature and pressure dependence only of linewidth
C      k      : index of transition
C      maxq   : maxq(2, mco2) is maximum rotational state
C      widmul : line width multiplier
C      wdf    : delta omega minus waven(1)
C      waven  : wavenumber array for susceptibility calculation
C    output variables:
C      xreal, ximag : real and imaginary part of resonant 
C                     susceptibility
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NS = 200, NJC = 150, NTR = 60, NCO2 = 12)
      DIMENSION MAXQ(2, *), WAVEN(*)
      COMMON /CO2/ MCO2, B000, LERG, IVIBL2(NS), 
     1   EJCO2(0:NJC, 2, NTR, 2), CO2PRM(NTR, 2, NCO2), POPCO2(0:NJC, 2, 
     2   NTR, 2), PPNCO2(0:NJC, NTR, 2), AMPCO2(0:NJC, NTR, 2),
     3   WJK(0:NJC, NTR, 2), RATIO, ERG(NS)
Cvax
      DOUBLE PRECISION WAVEN, ERG
Cend
C
C  local variables:
C
      DIMENSION WJ(0:NJC), GAM(0:NJC)
      SAVE GAM
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DEGEN = CO2PRM(K, IPMP, 10)
      CJ0 = (1.-DFSPHI)/DEGEN
      WJ0 = CO2PRM(K, IPMP, 11)-WAVEN(1)
      JMAX = MAXQ(2, MCO2)
      SUMU = 0.
      SUMV = 0.
C
C---  calculate linewidths.
C
      IF (K .EQ. 1) CALL CO2GAM (JMAX, WIDMUL, GAMJ, GAM)
C
      DO 100 J = 0, JMAX, 2
         AJ1 = CJ0*GAM(J)*FLOAT(2*J+1)*PPNCO2(J, K, IPMP)
         WJ(J) = WJ0+WJK(J, K, IPMP)
         BJ1 = 2.*(WJ(J)-WDF)
         DENOM1 = BJ1**2+GAM(J)**2
         SUMU   = SUMU+AJ1*GAM(J)/DENOM1
         SUMV   = SUMV+AJ1*BJ1/DENOM1
100   CONTINUE
C
      IF (DEGEN .EQ. 2.) THEN
         DO 300 J = 1, JMAX, 2
            AJ1 = CJ0*GAM(J)*FLOAT(2*J+1)*PPNCO2(J, K, IPMP)
            WJ(J) = WJ0+WJK(J, K, IPMP)
            BJ1 = 2.*(WJ(J)-WDF)
            DENOM1 = BJ1**2+GAM(J)**2
            SUMU   = SUMU+AJ1*GAM(J)/DENOM1
            SUMV   = SUMV+AJ1*BJ1/DENOM1
300      CONTINUE
      ENDIF
C
      DNSAV = (1.-SUMU)**2+SUMV**2
C
      XREAL = 0.
      XIMAG = 0.
C
      DO 200 J = 0, JMAX, 2
         DENOM = 4.*(WJ(J)-WDF)**2+GAM(J)**2
         AOVERD = AMPCO2(J, K, IPMP)/DENOM
         XRL = 2.*(WJ(J)-WDF)*AOVERD
         XIM = GAM(J)*AOVERD
         XREAL = XREAL+((1.-SUMU)*XRL+SUMV*XIM)/DNSAV
         XIMAG = XIMAG+((1.-SUMU)*XIM-SUMV*XRL)/DNSAV
200   CONTINUE
C
      IF (DEGEN .EQ. 2.) THEN
         DO 400 J = 1, JMAX, 2
            DENOM = 4.*(WJ(J)-WDF)**2+GAM(J)**2
            AOVERD = AMPCO2(J, K, IPMP)/DENOM
            XRL = 2.*(WJ(J)-WDF)*AOVERD
            XIM = GAM(J)*AOVERD
            XREAL = XREAL+((1.-SUMU)*XRL+SUMV*XIM)/DNSAV
            XIMAG = XIMAG+((1.-SUMU)*XIM-SUMV*XRL)/DNSAV
400      CONTINUE
      ENDIF
C
      END
      SUBROUTINE CO2TRN (NSPEC, MMCO2, NT, TEMP, WLIL, WBIG, MAXQ, NQ,
     1   WTRAN, FOFF)
C
C______________________________________________________________________
C
C   calculates vibrational energy levels and rotational
C   constants for co2 susceptibility calculation.
C   called by subroutine inical in carsft.
C        lerg  :  total number of vibrational state energies
C         erg  :  vibrational state energies
C      ivibl2  :  quantum number l2 of state erg specifiying bending
C                 angular momentum about internuclear axis
C        wave  :  wavenumber of vibrational transitions between
C                 wlil and wbig
C       ejco2  :  rotational splitting of vibrational transition wave
C______________________________________________________________________
C
      PARAMETER (ND = 10, ND1 = 11, NS = 200, NTR = 60, NCO2 = 12,
     1   NM = 4, NJC = 150, NW = 1000)
      DIMENSION NT(NM, *), MAXQ(2, *), NQ(NW, 4, NM, *),
     1   WTRAN(NW, NM, 2)
      COMMON /CO2/ MCO2, B000, LERG, IVIBL2(NS), 
     1   EJCO2(0:NJC, 2, NTR, 2), CO2PRM(NTR, 2, NCO2), POPCO2(0:NJC, 2, 
     2   NTR, 2), PPNCO2(0:NJC, NTR, 2), AMPCO2(0:NJC, NTR, 2),
     3   WJK(0:NJC, NTR, 2), RATIO, ERG(NS)
      COMMON /GASNAM/ GASNAM(NM)
      CHARACTER*5 GASNAM
Cvax
      DOUBLE PRECISION ERG
Cend
C
C  local variables:
C
      DIMENSION IDYAD(NS), IVIBI3(NS), NZ(NS), 
     1   BROT(NS), BROT2(NS), DROT(NS), DROT2(NS), VECT(ND1, NS)
      DIMENSION EINIT(NTR, 2), BINIT(NTR, 2), DINIT(NTR, 2),
     1   DELB(NTR, 2), DELD(NTR, 2), BINIT2(NTR, 2), DINIT2(NTR, 2),
     2   DELB2(NTR, 2), DELD2(NTR, 2), FLAG(NTR, 2), WAVE(NTR, 2),
     3   VMAT(NTR, 2)
      EQUIVALENCE (CO2PRM(1, 1, 1), EINIT), (CO2PRM(1, 1, 2), BINIT),
     1   (CO2PRM(1, 1, 3), DINIT), (CO2PRM(1, 1, 4), DELB),
     2   (CO2PRM(1, 1, 5), DELD), (CO2PRM(1, 1, 6), BINIT2),
     3   (CO2PRM(1, 1, 7), DINIT2), (CO2PRM(1, 1, 8), DELB2),
     4   (CO2PRM(1, 1, 9), DELD2), (CO2PRM(1, 1, 10), FLAG),
     5   (CO2PRM(1, 1, 11), WAVE), (CO2PRM(1, 1, 12), VMAT)
Cvax
      DOUBLE PRECISION BROT, BROT2, DROT, DROT2
Cend
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      MCO2 = MMCO2
      DO 100 IPMP = 1, 2
C
         CALL MAKEB (TEMP, WLIL, WBIG, B000, LERG, VECT, IDYAD, 
     1      IVIBL2, IVIBI3, NZ, ERG, BROT, BROT2, DROT, DROT2, 
     2      ITRMAX, EINIT, BINIT, DINIT, DELB, DELD, BINIT2, 
     3      DINIT2, DELB2, DELD2, FLAG, WAVE, VMAT, EJCO2, MCO2,
     4      MAXQ, NQ, WTRAN, WJK, RATIO, IPMP, FOFF)
C
         NT(MCO2, IPMP) = ITRMAX
C            WRITE (6, '(1X/1X, A, I1, A, I3)') 'NT(', MCO2, ') = ', 
C     1         ITRMAX
100   CONTINUE
C
      RETURN
      END
      SUBROUTINE CO2T11 (NT, T11, IPMP)
C
C______________________________________________________________________
C
C  calculates strengths for co2 transitions.  assumes isotropic
C  scattering only; anisotropic scattering ignored.
C  called by subroutine tterms in carsft.
C    output variables:
C      t11  :  matrix element contribution to transition amplitude
C______________________________________________________________________
C
      PARAMETER (NW = 1000, NTR = 60, NCO2 = 12, NS = 200, NJC = 150,
     1   NM = 4)
      DIMENSION NT(NM, *), T11(NW, 2, NM, *)
      COMMON /CO2/ MCO2, B000, LERG, IVIBL2(NS), 
     1   EJCO2(0:NJC, 2, NTR, 2), CO2PRM(NTR, 2, NCO2), POPCO2(0:NJC, 2, 
     2   NTR, 2), PPNCO2(0:NJC, NTR, 2), AMPCO2(0:NJC, NTR, 2),
     3   WJK(0:NJC, NTR, 2), RATIO, ERG(NS)
      COMMON /VECTOR/ C24, C14
Cvax
      DOUBLE PRECISION ERG
Cend
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
      DATA PI, C, RDMASS, OMEGA1, RATSIG / 3.141592653, 2.998E10, 8.,
     1   1285., 0.743 /
C
C---  ratsig is the ratio of the raman cross section to that for n2.
C---  the depolarization ratio has been ignored.  for these values,
C---  see raman spectroscopy of gases and liquids, ed. a weber 
C---  (springer-verlag, berlin, 1979), pp. 130ff.  as did hall and
C---  stufflebeam, we use the penney et al. value for the cross
C---  section for the 1285 cm-1 transition.  units for dadq are cm2.
C---  value for dadq is 1.2148e-16.
C
      DADQ = 1.39E-18*SQRT(RATSIG*RDMASS*OMEGA1)
C
C---  ac1 = (dalpha/dq)/sqrt(2.*pi*c*wraman*reduced mass).
C---  the factor 1./2.*pi*c in dchi gives the energy denominator
C---  in chi3 the correct units.  value for ac1co2 is 2.1346e-12.

      AC1CO2 = DADQ/SQRT(2.*PI*C*OMEGA1*RDMASS*1.6726E-24)
C
C---  must correct for term (c24+ratio*c14) in subroutine co2wav.
C---  new value for ac1co2 is 3.3556e-12.
C
      AC1CO2 = AC1CO2/ABS(C24+RATIO*C14)
C
C---  value for dchi is 1.993e-17.  have neglected centrifugal
C---  corrections here; if included they are accounted for in
C---  subroutine co2spc.
C
      DCHI = AC1CO2**2/(6.*PI*C)*1.E18
C
C---  approximate bjj by 0.25, since many rotational states are
C---  occupied, even at 300 k.
C
      BJJ = 0.25
C
C---  rho is the depolarization ratio.
C
      DATA RHO / 0.027 /
C
C---  ag2 = ((dg/dq)/(da/dq))**2.
C
      AG2 = 45.*RHO/(BJJ*(3.-4.*RHO))
      R2 = BJJ*AG2/45.
C
      DO 100 K = 1, NT(MCO2, IPMP)
         R1 = CO2PRM(K, IPMP, 12)*DCHI
         T11(K, 1, MCO2, IPMP) = R1*(1.+4.*R2)
         T11(K, 2, MCO2, IPMP) = 3.*R1*R2
100   CONTINUE
C
      RETURN
      END
      SUBROUTINE CO2WAV (TEMP, WLIL, WBIG, B000, LERG, VECT, IDYAD, 
     1   IVIBL2, IVIBI3, NZ, ERG, BROT, BROT2, DROT, DROT2, ITRMAX, 
     2   EINIT, BINIT, DINIT, DELB, DELD, BINIT2, DINIT2, DELB2, 
     3   DELD2, FLAG, WAVE, VMAT, EJCO2, MCO2, MAXQ, NQ, WTRAN, WJK,
     4   RATIO, IPMP, FOFF)
C
C______________________________________________________________________
C
C   calculates parameters for Q-branch transitions of co2 within 
C   wavenumber range wlil to wbig.
C        lerg        :  total number of vibrational state energies
C         erg        :  vibrational state energies
C      ivibl2        :  quantum number l2 of state erg specifiying 
C                       bending angular momentum about internuclear axis
C        wlil        :  minimum wavenumber of calculation range
C        wbig        :  maximum wavenumber of calculation range
C        wave        :  wavenumber of vibrational transitions between
C                       wlil and wbig
C        einit       :  initial state energy
C        binit, binit2   :  rotational constant b of initial state
C        dinit, dinit2   :  rotational constant d of initial state
C        delb, delb2 :  difference in b between final and initial state
C        deld, deld2 :  difference in d between final and initial state
C        flag        :  flag = 2 means l-doubling, flag = 1 otherwise
C        vmat        :  transition matrix element
C        itrmax      :  number of transitions withing range
C        ejco2       :  rotational energy of initial and final states
C        wjk         :  wavenumber of ro-vibrational transition
C______________________________________________________________________
C
      PARAMETER (ND = 10, ND1 = 11, NS = 200, NTR = 60, NCO2 = 12,
     1   NJC = 150, NJ = 130, NW = 1000, NM = 4)
      DIMENSION VECT(ND1, *), IDYAD(*), IVIBL2(*), IVIBI3(*), NZ(*),
     1   ERG(*), BROT(*), BROT2(*), DROT(*), DROT2(*)
      DIMENSION EINIT(NTR, *), BINIT(NTR, *), DINIT(NTR, *),
     1   DELB(NTR, *), DELD(NTR, *), BINIT2(NTR, *), DINIT2(NTR, *),
     2   DELB2(NTR, *), DELD2(NTR, *), FLAG(NTR, *), WAVE(NTR, *), 
     3   VMAT(NTR, *), EJCO2(0:NJC, 2, NTR, *), MAXQ(2, *),
     4   NQ(NW, 4, NM, *), WTRAN(NW, NM, *), WJK(0:NJC, NTR, *)
Cvax
      DOUBLE PRECISION ERG, BROT, BROT2, DROT, DROT2
Cend
C
C  local variables:
C
      DIMENSION POP(NS)
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  calculate vibrational partition function to estimate transition
C---  strength.
C
      QVCO2 = 0.
C
      DO 100 I = 1, LERG
         IF (IVIBL2(I) .NE. 0) THEN
            DEGEN = 2.
         ELSE
            DEGEN = 1.
         ENDIF      
         POP(I) = DEGEN*EXP(-1.439*ERG(I)/TEMP)
         QVCO2 = QVCO2+POP(I)
100   CONTINUE
C
      DO 500 I = 1, LERG
         IF (BROT2(I) .EQ. 0.) BROT2(I) = BROT(I)
         IF (DROT2(I) .EQ. 0.) DROT2(I) = DROT(I)
         POP(I) = POP(I)/QVCO2
500   CONTINUE
C
C---  max vibrational state vmax is not used, but set to zero:
C
      MAXQ(1, MCO2) = 0
C
C---  max. rotational state jmax:
C
      DATA POPMIN / 0.01 /
      JMAX = IFIX(SQRT(TEMP*ABS(ALOG(POPMIN))/(1.439*B000)))
      JMAX = MIN(JMAX, NJC-2)
      MAXQ(2, MCO2) = JMAX
C      WRITE (6, '(1X/1X, A, I3)') 'JMAX = ', JMAX
C      WRITE (6, '(1X/1X, 2A/)')
C     1   '   K    I    J    FREQ    DELB*10(3)  DELD*10(7)',
C     2   '     VMAT    POP(I)-POP(J)'
C
      ITRANS = 0
C
C---  i corresponds to initial state, j to final state of transition.
C
      DO 200 I = 1, LERG
C
         DO 300 J = 1, LERG
            I2 = IDYAD(J)
            IF (I2 .NE. (IDYAD(I)+2)) GO TO 300
            I3 = IVIBI3(J)
            L2 = IVIBL2(J)
            IF (I3 .NE. IVIBI3(I) .OR. L2 .NE. IVIBL2(I)) GO TO 300
            IF (IPMP .EQ. 1) THEN
               TD = ERG(J)-ERG(I)
	      ELSE
	         TD = ERG(J)-ERG(I)+FOFF
	      ENDIF
            IF (TD .LT. WLIL .OR. TD .GT. WBIG) GO TO 300
C
            SUM1 = 0.
            SUM2 = 0.
C
            DO 400 K = 1, NZ(J)-1
               M1 = NZ(J)-K
               M2 = M1+IVIBL2(J)
               SUM1 = SUM1+SQRT(FLOAT(K))*VECT(K, I)*VECT(K+1, J)
               SUM2 = SUM2+SQRT(FLOAT(M1*M2))*VECT(K, I)*VECT(K, J)           
400         CONTINUE
C
C---  the constant ratio was determined by setting the ratio of the
C---  cross section for the 1285 line to the 1388 line to 0.668, the
C---  overall average of relative cross sections in raman spectroscopy 
C---  of gases and liquids, ed. a weber (springer-verlag, berlin, 1979), 
C---  pp. 138.  => (c24+ratio*c14)**2/(c23+ratio*c13)**2 = 0.668.
C---  here, j=3,4 correspond to 1388 and 1285 cm-1 lines, respectively.
C---  hall and stufflebeam, and b. zilles and r. carter, "computer
C---  program to simulate raman scattering," nasa report cr-145151,
C---  arrive at the same answer.
C
            RATIO = -0.125
            VM = (SUM1+RATIO*SUM2)**2
C
C---  eliminate weak transitions to reduce calculation time.
C
            DATA TRMIN / 1.5E-5 /
            TR = (VM*(POP(I)-POP(J)))**2
            IF (TR .LT. TRMIN) THEN
               GO TO 300
            ELSE
               ITRANS = ITRANS+1
            ENDIF
            IF (ITRANS .GT. NTR) THEN
               WRITE (6, '(1X, 2A)')
     1            '!! TOO MANY TRANSITIONS IN CO2 CALCULATION -- ',
     2            'PLEASE INCREASE DIMENSION NTR !!'
               STOP
            ENDIF
C
            VMAT(ITRANS, IPMP) = VM
            NQ(ITRANS, 1, MCO2, IPMP) = 0
            NQ(ITRANS, 2, MCO2, IPMP) = 0
            NQ(ITRANS, 3, MCO2, IPMP) = I
            NQ(ITRANS, 4, MCO2, IPMP) = J
            WTRAN(ITRANS, MCO2, IPMP) = TD
            WAVE(ITRANS, IPMP) = TD
            EINIT(ITRANS, IPMP) = ERG(I)
            BINIT(ITRANS, IPMP) = BROT(I)
            DINIT(ITRANS, IPMP) = DROT(I)
            DELB(ITRANS, IPMP) = BROT(J)-BROT(I)
            DELD(ITRANS, IPMP) = (DROT(J)-DROT(I))
C
            IF (L2 .GT. 0) THEN
               FLAG(ITRANS, IPMP) = 2.
               BINIT2(ITRANS, IPMP) = BROT2(I)
               DINIT2(ITRANS, IPMP) = DROT2(I)
               DELB2(ITRANS, IPMP) = BROT2(J)-BROT2(I)
               DELD2(ITRANS, IPMP) = DROT2(J)-DROT2(I)
            ELSE 
               FLAG(ITRANS, IPMP) = 1.
            ENDIF
C
C---  each l-doublet has only alternate j levels.
C
            DO 600 JROT = 0, JMAX, 2
               AJ2 = JROT*(JROT+1.)
               AJ22 = AJ2**2
               EJCO2(JROT, 1, ITRANS, IPMP) = BROT(I)*AJ2-DROT(I)*AJ22
               EJCO2(JROT, 2, ITRANS, IPMP) = BROT(J)*AJ2-DROT(J)*AJ22
               IF (L2 .GT. 0) THEN
                  AJ2 = (JROT+1.)*(JROT+2.)
                  AJ22 = AJ2**2
                  EJCO2(JROT+1, 1, ITRANS, IPMP) = BROT2(I)*AJ2-
     1                                       DROT2(I)*AJ22
                  EJCO2(JROT+1, 2, ITRANS, IPMP) = BROT2(J)*AJ2-
     1                                       DROT2(J)*AJ22
               ELSE
                  EJCO2(JROT+1, 1, ITRANS, IPMP) = 0.
                  EJCO2(JROT+1, 2, ITRANS, IPMP) = 0.
               ENDIF
600         CONTINUE
C            
C            WRITE (6, '(3(2X, I3), 2X, F8.3, 3(2X, F10.7), 
C     1         2X, E10.4)') ITRANS, I, J, WAVE(ITRANS), 
C     2         DELB(ITRANS)*1.E3, DELD(ITRANS)*1.E7, VMAT(ITRANS), 
C     3         POP(I)-POP(J)
300      CONTINUE
200   CONTINUE
C
      ITRMAX = ITRANS
C
C     Check for overunning POP(K,NQ,MCO2) in CO2POP.
C
      MX = MAX( NQ(ITRMAX, 3, MCO2, IPMP), NQ(ITRMAX, 4, MCO2, IPMP) )

	IF (MX .GT. NJ) THEN
	   WRITE(*,*)
     1   '!! TOO MANY TRANSITIONS IN CO2 CALCULATION -- ',
     2   'PLEASE INCREASE DIMENSION NJ TO', MX,' !!'
	   STOP
	ENDIF
C
      DO 700 K = 1, ITRMAX
         DO 700 J = 0, JMAX, 2
C
C---  use delb and deld for even-j states, delb2 and deld2 for
C---  odd-j states.
C
            WJK(J, K, IPMP) = DELB(K, IPMP)*J*(J+1.)-
     1                  DELD(K, IPMP)*J**2*(J+1.)**2
            IF (FLAG(K, IPMP) .EQ. 2.) THEN
               WJK(J+1, K, IPMP) = DELB2(K, IPMP)*J*(J+1.)-
     1                       DELD2(K, IPMP)*J**2*(J+1.)**2
            ELSE
               WJK(J+1, K, IPMP) = 0.
            ENDIF
700   CONTINUE
C
      RETURN
      END
      SUBROUTINE EBCO2 (ECO2, BCO2, I1MAX, I2MAX, I3MAX, B000)
C
C______________________________________________________________________
C
C   calculates unperturbed vibrational energy levels and rotational
C   constants for co2 without fermi resonances
C        eco2  :  unperturbed vibrational energy
C        bco2  :  unperturbed rotational constants
C        Ii    :  vibrational quantum numbers i = 1,2,3
C        l     :  component of bending angular momentum along
C                 internuclear axis
C______________________________________________________________________
C
      PARAMETER (NV = 3, ND = 10)
      DIMENSION ECO2(0:5, 0:ND, 0:ND, 0:3), BCO2(0:5, 0:ND, 0:ND, 0:3)
Cvax
      DOUBLE PRECISION BCO2
Cend
C  local variables:
C
      DIMENSION WCO2(NV), XCO2(NV, NV), YCO2(NV, NV, NV), AB(NV),
     1   GB(NV, NV)
      DATA WCO2 / 1337.55, 667.365, 2361.62 /
      DATA XCO2(1, 1), XCO2(2, 2), XCO2(3, 3), XCO2(1, 2),
     1   XCO2(1, 3), XCO2(2, 3) / -2.94, 1.10, -12.47, -3.64,
     2   -19.66, -12.37 /
      DATA YCO2 / 27*0. /
      DATA G22 / -0.88 /
      DATA AB / 0.001232, -0.000737, 0.003058 /
      DATA GB / 9*0. /
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  equations and constants come from i. suzuki, j. mol. spectrosc. 25, 
C---  478 (1968), or a. chedin, j. mol. spectrosc. 76, 430 (1979).
C
      DO 100 I1 = 0, I1MAX
C
         DO 100 I3 = 0, I3MAX
C
            DO 100 I2 = 0, I2MAX
C
               DO 100 L2 = MOD(I2, 2), I2, 2
                  ECO2(I1, I2, L2, I3) = WCO2(1)*I1+WCO2(2)*I2+
     1            WCO2(3)*I3+XCO2(1, 1)*I1*I1+XCO2(2, 2)*I2*I2+
     2            XCO2(3, 3)*I3*I3+XCO2(1, 2)*I1*I2+XCO2(1, 3)*
     3            I1*I3+XCO2(2, 3)*I2*I3+YCO2(1, 1, 1)*I1*I1*I1+
     4            YCO2(2, 2, 2)*I2*I2*I2+YCO2(3, 3, 3)*I3*I3*I3+
     5            YCO2(1, 1, 2)*I1*I1*I2+YCO2(1, 2, 2)*I1*I2*I2+
     6            YCO2(1, 3, 3)*I1*I3*I3+YCO2(1, 1, 3)*I1*I1*I3+
     7            YCO2(2, 2, 3)*I2*I2*I3+YCO2(2, 3, 3)*I2*I3*I3+
     8            YCO2(1, 2, 3)*I1*I2*I3+G22*L2*L2
C
                  BCO2(I1, I2, L2, I3) = B000-AB(1)*I1-AB(2)*I2-
     1            AB(3)*I3+GB(1, 1)*I1*I1+GB(2, 2)*I2*I2+
     2            GB(3, 3)*I3*I3+GB(1, 2)*I1*I2+GB(1, 3)*I1*I3+
     3            GB(2, 3)*I2*I3
100   CONTINUE
C
      RETURN
      END
      SUBROUTINE GETEB (ERG, BROT, BROT2, DROT, DROT2)
C
C______________________________________________________________________
C
C   gets tabulated co2 vibrational energies and rotational constants
C   from rothman and young (33).
C        erg   :  vibrational energy gv
C        brot  :  rotational constant b
C        brot2 :  rotational constant b for second l-doublet
C        drot  :  rotational constant d
C        drot2 :  rotational constant d for second l-doublet
C______________________________________________________________________
C
      DIMENSION ERG(*), BROT(*), BROT2(*), DROT(*), DROT2(*)
      CHARACTER*80 DUMMY
Cvax
      DOUBLE PRECISION ERG, BROT, BROT2, DROT, DROT2
Cend
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  notes on tabulated parameters:
C---    in general, more accurate values are from l. s. rothman,
C---    appl. opt. 25, 1795 (1986).
C
      OPEN (17, FILE = 'co2.mol', STATUS = 'OLD', ERR = 100)
      REWIND 17
      GO TO 110
100   WRITE (6, '(1X, A)')
     1   '!! CANNOT FIND FILE CO2.MOL WITH CO2 MOLECULAR DATA !!'
      STOP
110   CONTINUE
C
C---  ignore comment lines starting with "c" and initial header line
C
      READ (17, '(A))') DUMMY
      IF (DUMMY(1:1) .EQ. 'C' .OR. DUMMY(2:2) .EQ. 'C') GO TO 110
C
200   READ (17, *, END = 300) I, ERG(I), BROT(I), BROT2(I),
     1   DROT(I), DROT2(I)
      DROT(I) = DROT(I)*1.E-7
      DROT2(I) = DROT2(I)*1.E-7
      GO TO 200   
C
300   CLOSE (17)
C
      RETURN
      END
      SUBROUTINE MAKEB (TEMP, WLIL, WBIG, B000, LERG, VECT, IDYAD, 
     1   IVIBL2, IVIBI3, NZ, ERG, BROT, BROT2, DROT, DROT2, ITRMAX, 
     2   EINIT, BINIT, DINIT, DELB, DELD, BINIT2, DINIT2, DELB2, 
     3   DELD2, FLAG, WAVE, VMAT, EJCO2, MCO2, MAXQ, NQ, WTRAN, WJK,
     4   RATIO, IPMP, FOFF)
C
C______________________________________________________________________
C
C   calculates vibrational energy levels and rotational
C   constants for co2 including fermi resonances
C        eco2  :  unperturbed vibrational energy
C        bco2  :  unperturbed rotational constants
C        vi    :  vibrational quantum numbers i = 1,2,3
C        l     :  component of bending angular momentum along
C                 internuclear axis
C______________________________________________________________________
C
      PARAMETER (ND = 10, ND1 = 11, NS = 200, NCO2 = 12, NTR = 60,
     1   NJC = 150, NW = 1000, NM = 4)
      DIMENSION IDYAD(*), IVIBL2(*), IVIBI3(*), NZ(*), ERG(*), BROT(*),
     1   BROT2(*), DROT(*), DROT2(*), VECT(ND1, *)
      DIMENSION EINIT(NTR, *), BINIT(NTR, *), DINIT(NTR, *),
     1   DELB(NTR, *), DELD(NTR, *), BINIT2(NTR, *), DINIT2(NTR, *),
     2   DELB2(NTR, *), DELD2(NTR, *), FLAG(NTR, *), WAVE(NTR, *), 
     3   VMAT(NTR, *), EJCO2(0:NJC, 2, NTR, *), MAXQ(2, *),
     4   NQ(NW, 4, NM, *), WTRAN(NW, NM, *), WJK(0:NJC, NTR, *)
      COMMON /VECTOR/ C24, C14
Cvax
      DOUBLE PRECISION ERG, BROT, BROT2, DROT, DROT2
Cend
C
C  local variables:
C
      DIMENSION ECO2(0:5, 0:ND, 0:ND, 0:3), BCO2(0:5, 0:ND, 0:ND, 0:3)
      DIMENSION E1(5), BMAT(0:ND), EMAT(ND1), WORK(2*ND1)
      COMPLEX EIGVAL(ND1), EIGVCT(ND1*ND1), ZMAT(ND1*ND1)
Cvax
      DOUBLE PRECISION BCO2
Cend
      DATA I1MAX, I2MAX, I3MAX / 5, 10, 1 /
      DATA E1 / 74.47, 0.3583, 0.4975, 0.2808, 1.9657E-4 /
C
      NPOS(IR, IC, NORD) = IC*NORD+IR+1
C
C::::::::::::::::::::::::::::::::::::::::::::::::: end declarations
C
C---  generate unperturbed vibrational energies and rotational
C---  constants.
C
      B000 = 0.39021 
      CALL EBCO2 (ECO2, BCO2, I1MAX, I2MAX, I3MAX, B000)
C
C      DO 101 I1 = 0, I1MAX
C         DO 101 I2 = 0, I2MAX
C            DO 101 L2 = MOD(I2, 2), I2, 2
C               DO 101 I3 = 0, I3MAX
C                  WRITE (6, '(1X, 4(I2, 2X), 2(G15.7, 2X))') 
C     1               I1, I2, L2, I3, ECO2(I1, I2, L2, I3),
C     2               BCO2(I1, I2, L2, I3)
101   CONTINUE
C
C---  lerg corresponds roughly to order of transitions in table 2 in
C---  rothman and young (33), except for transitions with vibrational 
C---  quantum numbers > Iimax.
C
      LERG = 0
      LERG1 = 1
C
C---  the j-dependent term involving delta is calculated only
C---  for the most probable j, jm, and is assumed constant.
C
      JM = IFIX(SQRT(TEMP/(2.*1.439*B000)))
C
C---  ignore jm for now.
C
      JM = 0
C
C---  calculate polyad for each v3, v2, and l2 such that v1-2v2 is
C---  constant.
C
      DO 556 I3 = 0, I3MAX
C
         DO 556 I2 = 0, I2MAX
C
            DO 556 L2 = MOD(I2, 2), I2, 2
C
C---  nsiz is number of terms in polyad.
C
               NSIZ = ((I2-L2)/2)+1
C               WRITE (6, '(1X/1X, 2A, I2, A/)') 'Elements of Polyad ',
C     1            'of order ', NSIZ, ' are:'
C
               DO 300 M = 1, ND1*ND1
300               ZMAT(M) = CMPLX(0., 0.)
C
               DO 557 I1 = 0, NSIZ-1
                  J2 = I2-2*I1
C                  WRITE (6, '(1X, A, 4(I2, 3X))') 
C     1               'I1, I2, L2, I3 = ', I1, J2, L2, I3
                  NP = NPOS(I1, I1, NSIZ)
                  ZMAT(NP) = CMPLX(ECO2(I1, J2, L2, I3), 0.)
                  BMAT(I1) = BCO2(I1, J2, L2, I3)
C
C---  constants come from suzuki (32), pp. 487-488.
C
                  EFAC = -E1(1)/SQRT(2.)+E1(2)*(I1+1)+E1(3)*J2+
     1                   E1(4)*(I3+0.5)+E1(5)*JM*(JM+1)
                  EFAC = 0.5*EFAC*SQRT(FLOAT(J2**2-L2**2))*
     1                   SQRT(FLOAT(I1+1))
                  ZMAT(NPOS(I1, I1+1, NSIZ)) = CMPLX(EFAC, 0.)
                  ZMAT(NPOS(I1+1, I1, NSIZ)) = CMPLX(EFAC, 0.)
557            CONTINUE
C
C            WRITE (6, '(1X/1X, A/)') '       I   J   MATRIX ELEMENT'
C
            DO 102 I = 1, NSIZ
               DO 102 J = 1, NSIZ
C                  WRITE (6, '(5X, 2(2X, I2), 4X, F10.4)') 
C     1               I, J, REAL(ZMAT(NPOS(I-1, J-1, NSIZ)))
102         CONTINUE   
C
C---  diagonalize secular determinant of polyad matrix.
C
            IF (NSIZ .EQ. 1) THEN
               EIGVCT(1) = CMPLX(1., 0.)
               EIGVAL(1) = ZMAT(1)
            ELSE
               CALL CGEEV (ZMAT, NSIZ, NSIZ, EIGVAL, EIGVCT, NSIZ,
     1            WORK, 1, IER)
               IF (IER. NE. 0) THEN
                  WRITE (6, '(1X, A, I1, 2A)')
     1               '!! ERROR FLAG = ', IER, ' RETURNED FROM CGEEV ',
     2               'IN SUBROUTINE MAKEB'
                  STOP
               ENDIF
            ENDIF
            LERG1 = LERG+1
C
551         DO 554 I = 0, NSIZ-1
               LERG = LERG+1
               BROT(LERG) = 0.
C
C---  the jth column of eigvct(i,j) corresponds to the eigenvector
C---  associated with eigval(j).  the row i corresponds to the ith
C---  unperturbed basis set wave function psizero(i).
C
               DO 559 K = 0, NSIZ-1
                  VECT(K+1, LERG) = REAL(EIGVCT(NPOS(K, I, NSIZ)))
                  BROT(LERG) = BROT(LERG)+VECT(K+1, LERG)**2*BMAT(K)
559            CONTINUE
C
               IDYAD(LERG) = I2
               IVIBL2(LERG) = L2
               IVIBI3(LERG) = I3
               NZ(LERG) = NSIZ
               BROT2(LERG) = 0.
               DROT2(LERG) = 0.
               ERG(LERG) = REAL(EIGVAL(I+1))
554         CONTINUE
C
C            WRITE (6, '(1X/1X, A/)') '       I   J   EIGENVECTOR'
C
            DO 103 I = 1, NSIZ
               DO 103 J = 1, NSIZ
                  IJ = NPOS(I-1, J-1, NSIZ)
C                  WRITE (6, '(5X, 2(2X, I2), 2X, F11.7)') 
C     1               I, J, REAL(EIGVCT(IJ))
103         CONTINUE
C
C            WRITE (6, '(1X/1X, A/)') 
C     1      '   V  IDY   L2   I3      GV          BV'
C
            DO 104 I = LERG1, LERG
C               WRITE (6, '(4(2X, I3), 2X, F10.4, 2X, F10.7)')
C     1            I, IDYAD(I), IVIBL2(I), IVIBI3(I), ERG(I), BROT(I)
104   CONTINUE
C
556   CONTINUE      
C
C---  get c24 and c14 for calculating transition amplitude in
C---  subroutine co2t11.
C
      C24 = VECT(2, 4)
      C14 = VECT(1, 4)
C
C---  get more accurate values of erg, brot, brot2, drot, and drot2
C---  for selected transitions.
C
      CALL GETEB (ERG, BROT, BROT2, DROT, DROT2)
C
C      WRITE (6, '(1X/1X, 2A/)') '   V   L2   I3      GV          BV',
C     1   '          BV2      DV*10(7)    DV2*10(7)'
C
C      DO 105 I = 1, LERG
C         WRITE (6, '(3(2X, I3), 2X, F10.4, 4(2X, F10.7))')
C     1      I, IVIBL2(I), IVIBI3(I), ERG(I), BROT(I), BROT2(I),
C     2      DROT(I)*1.E7, DROT2(I)*1.E7
105   CONTINUE
C
      CALL CO2WAV (TEMP, WLIL, WBIG, B000, LERG, VECT, IDYAD, IVIBL2, 
     1   IVIBI3, NZ, ERG, BROT, BROT2, DROT, DROT2, ITRMAX, EINIT, 
     2   BINIT, DINIT, DELB, DELD, BINIT2, DINIT2, DELB2, DELD2, FLAG, 
     3   WAVE, VMAT, EJCO2, MCO2, MAXQ, NQ, WTRAN, WJK, RATIO, IPMP,
     4   FOFF)
C
      END
