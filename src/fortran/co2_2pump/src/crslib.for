      SUBROUTINE DBLSNG (N, X, Y)
C
C------------------------------------------------------------------
C  converts double precision array x to single precision array y
C     n = number of points in array
C------------------------------------------------------------------
      DIMENSION X(*), Y(*)
      DOUBLE PRECISION X
C
      DO 100 I = 1, N
         Y(I) = X(I)
100   CONTINUE
C
      END
      SUBROUTINE FHT (F, N)
C
C------------------------------------------------------------------
C  fast hartley transform routine.  obtained from o.buneman,
C  ref. 4 & 5.  version 02-01-84, by g.l. clark
C     n = number of points (must be power of 2)
C     f = function values on input, transform on output
C------------------------------------------------------------------
C
      DIMENSION F(N)
      SAVE
C
C--- dimensioned for maximum value of n = 2**18 = 262144.
C    Note: n must be greater than or equal to 2*NTM in carsfit.for.
C
      PARAMETER (NP2 = 18, NMAX = 2**NP2, ND = NMAX/4)
      DIMENSION T(ND), S(ND), M(NP2)
C
      IF (N .GT. NMAX) THEN
         PRINT *,
     1   '!!ERROR - NEED TO INCREASE DIMENSIONS OF ARRAYS'
         PRINT *, '    IN FHT ROUTINE'
	    PRINT *,
     1   N,' NEEDED, ',NMAX,' AVAILABLE.'
      ENDIF
C
C--- pretabulation section, done first time only
C
      IF (N .EQ. N0) GO TO 100
      N0     = N
      K      = 1
      L      = 0
C
C--- create table of powers of 2
C
36    CONTINUE
      M(L+1) = K
      K      = K+K
      L      = L+1
      IF (K .LT. N) GO TO 36
C
      L0     = L
      L      = L-3
      N1     = M(L)
      N2     = N1+N1
      N3     = N2+N1
      N4     = N3+N1
      S(N1)  = .38268343
      S(N2)  = .70710678
      S(N3)  = .92387953
      S(N4)  = 1.
      N4     = N4-1
      H      = .50979558
      R      = .25
      G      = 1.
C
62    CONTINUE
      R      = R*G
      G      = 1.5-G
      L      = L-1
      I      = M(L)
      A      = 0.
C
      DO 72 K = I, N4, N1
         K1 = K+I
         S(K) = H*(S(K1)+A)
         A    = S(K1)
72    CONTINUE
C
      H = .7001-.16016004/(H+.3004)
      N1 = I
      IF (N1 .GT. 1) GO TO 62
C
      IF (G .NE. 1.) R = R*.70710678
      K = N4
C
      DO 85 I = 1, N4
         T(I) = S(I)/(1.+S(K))
         K    = K-1
85    CONTINUE
C
C::: end of pretabulation section ::::::::::::::::::::::::::::::::::::::
C
C--- begin fast hartley transform
C--- permutation:
C
100   CONTINUE
      K = 0
      I = 0
103   CONTINUE
      I = I+1
      J = L0+1
106   CONTINUE
      J = J-1
      K = K-M(J)
      IF (K .GE. 0) GO TO 106
C
      K = K+M(J)+M(J)
      IF (I .LE. K) GO TO 103
      G = F(I+1)
      F(I+1) = F(K+1)
      F(K+1) = G
      IF (I .LT. N-2) GO TO 103
C
C--- four-point transform loop, followed by recursion steps:
C
      DO 128 I = 1, N, 4
         A = F(I)-F(I+1)
         B = F(I+2)-F(I+3)
         G = F(I)+F(I+1)
         H = F(I+2)+F(I+3)
         F(I) = R*(G+H)
         F(I+2) = R*(G-H)
         F(I+1) = R*(A+B)
         F(I+3) = R*(A-B)
128   CONTINUE
C
      L1 = L0-1
      L  = 4
C
133   CONTINUE
      L2 = L+L
      L1 = L1-1
      J0 = M(L1)
C
      DO 163 I0 = 1, N, L2
         I = I0
         I1 = I+L
         A  = F(I1)
         F(I1) = F(I)-A
         F(I)  = F(I)+A
         K     = I1-1
C
         DO 157 J = J0, N4, J0
            I = I+1
            I1 = I+L
            K1 = K+L
            A  = F(I1)+F(K1)*T(J)
            B  = F(K1)-A*S(J)
            A  = A+B*T(J)
            F(I1) = F(I)-A
            F(I)  = F(I)+A
            F(K1) = F(K)+B
            F(K)  = F(K)-B
            K     = K-1
157      CONTINUE
C
         K1 = K+L
         A  = F(K1)
         F(K1) = F(K)-A
         F(K)  = F(K)+A
163   CONTINUE
C
      L = L2
      IF (L .LT. N) GO TO 133
      END
      FUNCTION IFIRSC(STRING)
C
C-----------------------------------------------------------------------
C  position of first non-blank character in string of arbitrary length
C  if no characters are found, ifirsc is set = 0
C-----------------------------------------------------------------------
C
      CHARACTER* (*)STRING
C
      NLOOP = LEN(STRING)
C
      IF (NLOOP .EQ. 0) THEN
         IFIRSC = 0
         RETURN
      ENDIF
C
      DO 100 I = 1, NLOOP
         IF (STRING(I:I) .NE. ' ') GO TO 120
100   CONTINUE
C
      IFIRSC = 0
      RETURN
120   CONTINUE
      IFIRSC = I
      END
      SUBROUTINE INTERP (ICARD, STRING, NEXPEC, NFOUND, VALUE,
     1   IERR)
C
C----------------------------------------------------------------------
C   distills real variables from the string string
C      formal parameters:
C      ------------------
C         icard - data statement number (ignored if 0)
C         string - the character string
C         NEXPEC - number of real variables expected to be contained
C                  in string.  diagnostic will be issued if number
C                  found does not match number input.  if nvarin
C                  is input negative, an unknown number of variables
C                  is expected, between 0 and abs(nvarin)
C         nfound - the actual number found, only in the case that
C                  there were as many or less than NEXPEC
C         value - array of real values returned
C         ierr  - error flag: 0 if no errors found, >0 if errors
C
C   version 10-09-84:  capability added to handle lower case e for
C           exponent
C----------------------------------------------------------------------
C
      CHARACTER STRING*(*), IHOL*17
      DIMENSION L1(7, 7), INCODE(76), VALUE(*)
      LOGICAL UNNOWN
      DATA IPLUS, IMINUS, IPOINT, IE, IZERO, I9, IBLANK,
     1   ICOMMA/1, 2, 3, 4, 5, 14, 15, 16/
      DATA L1/1, 2, 3, 5, 7, 8, 9, 1, -2, 4, 6, -5, 8, 9, -1, -
     1   2, -3, -4, 7, 8, 9, -1, -2, -3, -4, 7, 8, 9, 1, 2, -3,
     2    -4, -5, 8, -7, 1, 2, -3, -4, -5, 8, 9, 1, 2, -3, -4,
     3   -5, 8, -7/
C
      IERR   = 0
      UNNOWN = .FALSE.
      IF (NEXPEC .LT. 0) UNNOWN = .TRUE.
      NEXP = IABS(NEXPEC)
      ILENTH = LENTH(STRING)
C
C--- get internal integer code array for string
C
      DATA IHOL/ '+-.E0123456789 ,e' /
C
      KIN   = 0
C
      DO 70 I = 1, ILENTH
         DO 65 J = 1, 17
            IF (STRING(I:I) .NE. IHOL(J:J)) GO TO 65
C
C            . . . match found - check for lower case e,
C                  treated same as upper case
C
            KIN = KIN+1
            IF (J .EQ. 17) THEN
               INCODE(KIN) = 4
            ELSE
               INCODE(KIN) = J
            ENDIF
            GO TO 70
65       CONTINUE
C
C            . . . no match found - bad character in string
C
         IERR = 1
         GO TO 999
70    CONTINUE
C
      NUMBER = 1
      ISYM   = 1
      IDONE  = 0
C
      ISIGMA = INCODE(1)
C
C             first character can be a number, plus, minus,
C             decimal point or a blank
C
      IF (ISIGMA .GE. IZERO .AND. ISIGMA .LE. I9) THEN
         L1I = 1
      ELSEIF (ISIGMA .EQ. IPOINT) THEN
         L1I = 2
      ELSEIF (ISIGMA .EQ. IPLUS) THEN
         L1I = 3
      ELSEIF (ISIGMA .EQ. IMINUS) THEN
         L1I = 4
      ELSEIF (ISIGMA .EQ. IBLANK) THEN
         L1I = 6
      ENDIF
C
80    CONTINUE
C
C             initialize
C     sum             current value of number
C     idp             passed a decimal point flag
C     ieng            engineering format being used
C     ixval           expnt value
C     ixsign          expnt sign (1= creating pos expnt -1=neg expnt
C                       0= not currently creating exponent)
C     div             divisor factor
C     isign           sign on number (1=pos -1=neg)
C
      SUM = 0.0
      IDP = 0
      IENG = 0
      IXVAL = 0
      IXSIGN = 0
      DIV    = 1.0
      EXPNT  = 1.0
      ISIGN  = 1
      NCOMMA = 0
C
C             get next character
C
100   CONTINUE
C
      IF (ISYM .GT. ILENTH) THEN
         IDONE = 1
         GO TO 600
      ENDIF
C
      ISYM = ISYM+1
      IALPHA = IBLANK
      IF (ISYM .LE. ILENTH) IALPHA = INCODE(ISYM)
C
      IF (IALPHA .GE. IZERO .AND. IALPHA .LE. I9) THEN
         L1J = 1
      ELSEIF (IALPHA .EQ. IPOINT) THEN
         L1J = 2
      ELSEIF (IALPHA .EQ. IPLUS) THEN
         L1J = 3
      ELSEIF (IALPHA .EQ. IMINUS) THEN
         L1J = 4
      ELSEIF (IALPHA .EQ. IE) THEN
         L1J = 5
      ELSEIF (IALPHA .EQ. IBLANK) THEN
         L1J = 6
      ELSEIF (IALPHA .EQ. ICOMMA) THEN
         L1J = 7
      ENDIF
C
C                                                  b     c
C                                                  l     o
C                    n                             a     m
C                    u                             n     m
C                    m     .     +     -     e     k     a
C                *********************************************
C                *                                           *
C         number -   1     1    -1    -1     1     1     1   *
C                *                                           *
C             .  *   2    -2    -2    -2     2     2     2   *
C                *                                           *
C             +  *   3     4    -3    -3    -3    -3    -3   *
C                *                                           *
C             -  *   5     6    -4    -4    -4    -4    -4   *
C                *                                           *
C             e  *   7    -5     7     7    -5    -5    -5   *
C                *                                           *
C         blank  *   8     8     8     8     8     8     8   *
C                *                                           *
C             ,  *   9     9     9     9    -7     9    -7   *
C                *                                           *
C                *********************************************
C
      L = L1(L1I, L1J)
C
      IF (L .LT. 0) THEN
         IERR = 1
C
C--- reduce
C
      ELSEIF (L .EQ. 1) THEN
C
C              . . . character is a number
C
         IF (IDP .EQ. 0) THEN
            IF (IXSIGN .EQ. 0) THEN
               SUM = SUM*10.0+(FLOAT(INCODE(ISYM-1)-IZERO))
            ELSE
               IXVAL = IXVAL*10+(INCODE(ISYM-1)-IZERO)
            ENDIF
         ELSE
            IF (IXSIGN .NE. 0) IERR = 1
            DIV = DIV/10.0
            SUM = SUM+(FLOAT(INCODE(ISYM-1)-IZERO))*DIV
         ENDIF
         IF (IALPHA .EQ. IBLANK .OR. IALPHA .EQ. ICOMMA) GO TO
     1   500
      ELSEIF (L .EQ. 2) THEN
C
C                  . . . character is a decimal point
C
         IF (IDP .NE. 0) IERR = 1
         IF (IXSIGN .NE. 0) IERR = 1
         IDP = 1
         IF (IALPHA .EQ. IBLANK .OR. IALPHA .EQ. ICOMMA) GO TO
     1   500
      ELSEIF (L .EQ. 3) THEN
C
C                  . . . plus followed by a number
C
         IF (IENG .NE. 0) IXSIGN = 1
      ELSEIF (L .EQ. 4) THEN
C
C                  . . . plus followed by a decimal point
C
         IF (IENG .EQ. 0) IERR   = 1
      ELSEIF (L .EQ. 5) THEN
C
C                  . . . minus followed by a number
C
         IF (IENG .EQ. 0) THEN
            IF (ISIGN .NE. 1) IERR = 1
            ISIGN = -1
         ELSE
            IXSIGN = -1
         ENDIF
C
      ELSEIF (L .EQ. 6) THEN
C
C                 . . . minus sign followed by dot
C
         IF (IXSIGN .NE. 0) IERR = 1
         IF (ISIGN .NE. 1) IERR  = 1
         ISIGN = -1
      ELSEIF (L .EQ. 7) THEN
C
C                 . . . e followed by a  plus, minus or number
C
         IF (IENG .NE. 0) IERR = 1
         IENG = 1
         IXSIGN = 1
         IDP    = 0
      ELSEIF (L .EQ. 9) THEN
C
C             9 - comma
C
         IF (NCOMMA .NE. 0) IERR = 1
         NCOMMA = 1
      ENDIF
C
      IF (IERR .NE. 0) GO TO 999
C
      L1I = L1J
      GO TO 100
500   CONTINUE
C
C             finished interpretting a number, if this is the
C             the last number to be interpreted, make sure that
C             only blanks follow.
C
      IF (NUMBER .EQ. NEXP .AND. ISYM .LT. ILENTH) THEN
         DO 520 I = ISYM, ILENTH
            IF (INCODE(I) .NE. IBLANK) THEN
               IERR = 3
               GO TO 999
            ENDIF
520      CONTINUE
      ENDIF
C
600   CONTINUE
C
      IF (IENG .EQ. 1) THEN
         EXPNT = 10.0**IXVAL
         IF (IXSIGN .LT. 0) EXPNT = 1.0/EXPNT
      ENDIF
C
      VALUE(NUMBER) = SUM*EXPNT*FLOAT(ISIGN)
C
      IF (IDONE .NE. 1) THEN
C
C---  ready for next number
C
         IF (NUMBER .EQ. NEXP) THEN
            NFOUND = NEXP
            RETURN
         ENDIF
         NUMBER = NUMBER+1
         L1I    = L1J
         GO TO 80
      ENDIF
C
C--- end of card was reached, check to see if
C--- the expected amount of numbers have been found
C
      IF (NEXP+1 .NE. NUMBER) THEN
         NFOUND = NUMBER-1
         IF (UNNOWN) RETURN
         IERR   = 2
         GO TO 999
      ENDIF
C
      RETURN
999   CONTINUE
C
      PRINT *, '!! INPUT ERROR - '
      IF (IERR .EQ. 1) PRINT *, 'ILLEGAL CHARACTER FOUND !!'
      IF (IERR .EQ. 2) PRINT '(A,I4)' ,
     1   ' TOO FEW DATA ITEMS.  NUMBER FOUND=' , NFOUND
      IF (IERR .EQ. 3) PRINT *, 'TOO MANY DATA ITEMS !!'
      IF (ICARD .NE. 0) PRINT '(A,I3,A)' ,
     1   '   DATA STATEMENT NUMBER' , ICARD,
     2   ' WILL BE IGNORED.'
      END
      FUNCTION LENTH(STRING)
C
C-----------------------------------------------------------------------
C  position of last non-blank character in string of arbitrary length
C-----------------------------------------------------------------------
C
      CHARACTER* (*)STRING
C
      NLOOP = LEN(STRING)
C
      DO 100 I = NLOOP, 1, -1
         IF (STRING(I:I) .NE. ' ') GO TO 120
100   CONTINUE
C
120   CONTINUE
      LENTH = I
      END
      SUBROUTINE MENU (NBEG, NEND, IOPT, PR)
C
C-----------------------------------------------------------------
C  prompts user to select an option from a menu.  checks that
C  input is within bounds, diagnoses until legal option chosen.
C  default is input value, if user simply responds with <cr>
C  maximum = 9 menu items
C     parameters:
C       nbeg  - beginning number of item in menu (could be 0)
C       nend  - final number of item (must be < 10)
C       iopt  - user-chosen item
C       pr    - array of character strings describing menu items
C  version 10-04-84
C    revisions: nbeg, nend added in place of nopt, to allow a
C    menu item numbered 0.
C-----------------------------------------------------------------
C
      CHARACTER* (*)PR(NBEG:NEND)
      CHARACTER*1 INP, IOK(0:9)
      DIMENSION LEN(0:9)
      DATA IOK/ '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' ,
     1    '8' , '9' /
C
C:::::::::::::::::::::::::::::::::::::: end declarations ::::::::::
C
      IF (NBEG .LT. 0 .OR. NEND .GT. 9) THEN
         PRINT *,
     1'ERROR in routine MENU - option numbers must be 0 to 9'
         RETURN
      ENDIF
C
C--- get maximum length of prompt
C--- len = length of prompt string, limited to 66 characters
C--- so box will fit on screen (prompt simply truncated)
C
      LMAX = 0
C
      DO 50 N = NBEG, NEND
         LEN(N) = LENTH(PR(N))
         IF (LEN(N) .GT. 66) LEN(N) = 66
         IF (LEN(N) .GT. LMAX) LMAX = LEN(N)
50    CONTINUE
C
C--- lbox  = total length of box, which should be divisible by 2
C
      LBOX = 12+LMAX
      IF (MOD(LBOX, 2) .NE. 0) LBOX = LBOX+1
C
C--- lcol  = # colons on each side of header
C--- lblnk = # blanks in second line
C
      LCOL = (LBOX-6)/2
      LBLNK = LBOX-4
      PRINT '(/80A1)' , ' ' , ( ':' , I = 1, LCOL), ' ' , 'M' ,
     1    'E' , 'N' , 'U' , ' ' , ( ':' , I = 1, LCOL)
      PRINT '(80A1)' , ' ' , ':' , ':' , ( ' ' , I = 1, LBLNK),
     1    ':' , ':'
C
      DO 100 N = NBEG, NEND
C
C--- lterm = # terminating blanks after each prompt line
C
         LTERM = LBOX-12-LEN(N)
         PRINT '(A6,I1,A2,A,A2,50A1)' , ' ::  {' , N, '} ' ,
     1   PR(N)(1:LEN(N)), '  ' , ( ' ' , I = 1, LTERM), ':' ,
     2   ':'
100   CONTINUE
C
      PRINT '(80A1)' , ' ' , ':' , ':' , ( ' ' , I = 1, LBLNK),
     1    ':' , ':'
      PRINT '(80A1)' , ' ' , ( ':' , I = 1, LBOX)
C
150   CONTINUE
      PRINT '(A,I2,A)' , ' Your choice ? <' , IOPT, '> '
      READ '(A1)' , INP
      IF (INP .EQ. ' ') GO TO 260
C
      DO 200 N = NBEG, NEND
         IF (INP .EQ. IOK(N)) GO TO 250
200   CONTINUE
C
      PRINT '(A,I2,A,I2)' ,
     1   ' !! ERROR - INPUT VALUE MUST BE BETWEEN' , NBEG,
     2   ' AND' , NEND
      GO TO 150
250   IOPT = N
260   CONTINUE
      PRINT *, ' '
      END
      SUBROUTINE MNMAX (ARR, IOPT, M, N, RMIN, RMAX)
C
C-----------------------------------------------------------------------
C   determines minimum and/or maximum of real 2-d array
C    input parameters:
C        arr   :  the array
C        iopt  :  = 1, find minimum only
C                 = 2, find maximum only
C                 = 3, find both min and max
C        m     :  row dimension of array
C        n     :  column dimension of array
C        rmin  :  minimum value found (undefined if iopt = 2)
C        rmax  :  maximum value found (undefined if iopt = 1)
C-----------------------------------------------------------------------
C
      REAL ARR(M, N)
      RMIN = ARR(1, 1)
      RMAX = ARR(1, 1)
C
      IF (IOPT .EQ. 1 .OR. IOPT .EQ. 3) THEN
         DO 100 J = 1, N
            DO 100 I = 1, M
               IF (ARR(I, J) .LT. RMIN) RMIN = ARR(I, J)
100      CONTINUE
      ENDIF
C
      IF (IOPT .EQ. 2 .OR. IOPT .EQ. 3) THEN
         DO 120 J = 1, N
            DO 120 I = 1, M
               IF (ARR(I, J) .GT. RMAX) RMAX = ARR(I, J)
120      CONTINUE
      ENDIF
C
      END
      SUBROUTINE OPFILE (FILNAM, LUNIT, MAXLEN, PROMPT, STAT)
C
C-----------------------------------------------------------------------
C   prompts for file name, attempts to open file,
C   diagnoses if name too long, or file cant be opened
C     input parameters:
C       filnam = name of file  ;   lunit = logical unit # desired
C       maxlen = max. # characters in file name
C       prompt = character string for prompting user
C       stat   = file status (character)
C-----------------------------------------------------------------------
C
      CHARACTER* (*)FILNAM, PROMPT, STAT
      CHARACTER*40 LOCNAM
C
      LIN = LENTH(FILNAM)
      LPR = LENTH(PROMPT)
      IF (PROMPT .EQ. ' ') GO TO 70
80    CONTINUE
      PRINT '(/1X,A,A2,A,A2)' , PROMPT(1:LPR), ' <' ,
     1   FILNAM(1:LIN), '> '
      READ '(A)' , LOCNAM
      LLOC = LENTH(LOCNAM)
C
      IF (LLOC .GT. MAXLEN) THEN
         PRINT '(A,I2)' ,
     1   '!!! ERROR - MAXIMUM # CHARACTERS IN NAME IS ' ,
     2   MAXLEN
         GO TO 80
      ENDIF
C
      IF (LOCNAM .NE. ' ') THEN
         FILNAM = LOCNAM(1:LLOC)
         LIN    = LLOC
      ENDIF
C
70    OPEN (UNIT = LUNIT, FILE = FILNAM(1:LIN), STATUS = STAT,
     1   ERR = 90, IOSTAT = IOS)
      RETURN
C
90    CONTINUE
      PRINT '(A,A,A,I3,A)' , ' !! ERROR IN OPENING FILE ' ,
     1   FILNAM(1:LIN), ' IOS =' , IOS, ' - TRY AGAIN'
      GO TO 80
C
      END
      SUBROUTINE REVARD (ARR, N)
C
C-----------------------------------------------------------------------
C   reverses the elements of array arr of length n.  i.e., puts first
C   element in last, 2nd in next-to-last, etc.  used to change
C   monotonically decreasing array to increasing order.  double
C   precision version
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION ARR(N), HOLD
C
      NLIM = N/2
C
      DO 100 I = 1, NLIM
         HOLD = ARR(I)
         IUP  = N+1-I
         ARR(I) = ARR(IUP)
         ARR(IUP) = HOLD
100   CONTINUE
C
      END
      SUBROUTINE REVARR (ARR, N)
C
C-----------------------------------------------------------------------
C   reverses the elements of array arr of length n.  i.e., puts first
C   element in last, 2nd in next-to-last, etc.  used to change
C   monotonically decreasing array to increasing order.
C-----------------------------------------------------------------------
C
      DIMENSION ARR(N)
C
      NLIM = N/2
C
      DO 100 I = 1, NLIM
         HOLD = ARR(I)
         IUP  = N+1-I
         ARR(I) = ARR(IUP)
         ARR(IUP) = HOLD
100   CONTINUE
C
      END
      SUBROUTINE SEARCH (N, X, XVAL, IL, IER)
      INTEGER N, IL, IER
      REAL X(N), XVAL
C
C-----------------------------------------------------------------------
C        search for xval in array x.
C
C     performs a linear search, from left to right.   for improved
C     efficiency when locating an increasing sequence of xvals,
C     the starting index for the search may be specified by the user.
C-----------------------------------------------------------------------
C
C     on input..
C        n       is the number of points in array x.
C                restriction.. n.ge.2  (not checked)
C        x       is the array of values to be searched.
C                x is assumed to be strictly increasing. (not checked)
C        xval    is the value being searched for.
C        il      is the index at which the search is to begin.
C                to initialize the search, set il.le.1 .
C
C     on return..
C        il      is the index such that
C
C                   x(il) .lt. xval .le. x(il+1) .
C
C        ier     is an error flag, with the following meaning.
C                 ier =-1  if xval .lt. x(1).  in this case il=1 on
C                          return.
C                 ier = 0  if x(1) .le. xval .le. x(n).  (normal return)
C                 ier = 1  if xval .gt. x(n) .  in this case il=n-1 on
C                          return.
C
C     notes..
C        1.  if  xval.eq.x(1), the returned values are  il=1, ier=0.
C        2.  ier and the three lines that set its value may
C            be deleted if the user does not wish to be notified
C            about extrapolation.
C
C     fortran intrinsics used.. max0, min0.
C
C-----------------------------------------------------------------------
C
C     programmed by.. f. n. fritsch, mss, lawrence livermore laboratory.
C     date last changed.. 21 december 1978 (fnf).
C
C     change record..
C        78-12-21   modified to reinitialize search if xval.le.x(il).
C
C-----------------------------------------------------------------------
C
C        local declarations.
C
      INTEGER I, IMIN
C
C        adjust il so that  1 .le. il .lt. n .
C
      IL = MAX0(IL, 1)
      IL = MIN0(IL, N-1)
C
C        check whether search should be reinitialized.
C
      IF (XVAL .GT. X(IL)) GO TO 2
      IL = 1
C
C        check for extrapolation to the left.
C
      IF (XVAL .GE. X(1)) GO TO 2
      IER = -1
      RETURN
C
C        normal case.
C
2     CONTINUE
      IMIN = IL+1
      NN   = N
C
      DO 3 I = IMIN, NN
         IF (XVAL .LE. X(I)) THEN
            IL = I-1
            IER = 0
            RETURN
         ENDIF
3     CONTINUE
C
C        extrapolation to the right.
C
      IL = N-1
      IER = 1
      END
      SUBROUTINE SERCHD (N, X, XVAL, IL, IER)
      INTEGER N, IL, IER
      DOUBLE PRECISION X(N), XVAL
C
C-----------------------------------------------------------------------
C        search for xval in array x.  double precision version.
C
C     performs a linear search, from left to right.   for improved
C     efficiency when locating an increasing sequence of xvals,
C     the starting index for the search may be specified by the user.
C-----------------------------------------------------------------------
C
C     on input..
C        n       is the number of points in array x.
C                restriction.. n.ge.2  (not checked)
C        x       is the array of values to be searched.
C                x is assumed to be strictly increasing. (not checked)
C        xval    is the value being searched for.
C        il      is the index at which the search is to begin.
C                to initialize the search, set il.le.1 .
C
C     on return..
C        il      is the index such that
C
C                   x(il) .lt. xval .le. x(il+1) .
C
C        ier     is an error flag, with the following meaning.
C                 ier =-1  if xval .lt. x(1).  in this case il=1 on
C                          return.
C                 ier = 0  if x(1) .le. xval .le. x(n).  (normal return)
C                 ier = 1  if xval .gt. x(n) .  in this case il=n-1 on
C                          return.
C
C     notes..
C        1.  if  xval.eq.x(1), the returned values are  il=1, ier=0.
C        2.  ier and the three lines that set its value may
C            be deleted if the user does not wish to be notified
C            about extrapolation.
C
C     fortran intrinsics used.. max0, min0.
C
C-----------------------------------------------------------------------
C
C     programmed by.. f. n. fritsch, mss, lawrence livermore laboratory.
C     date last changed.. 21 december 1978 (fnf).
C
C     change record..
C        78-12-21   modified to reinitialize search if xval.le.x(il).
C
C-----------------------------------------------------------------------
C
C        local declarations.
C
      INTEGER I, IMIN
C
C        adjust il so that  1 .le. il .lt. n .
C
      IL = MAX0(IL, 1)
      IL = MIN0(IL, N-1)
C
C        check whether search should be reinitialized.
C
      IF (XVAL .GT. X(IL)) GO TO 2
      IL = 1
C
C        check for extrapolation to the left.
C
      IF (XVAL .GE. X(1)) GO TO 2
      IER = -1
      RETURN
C
C        normal case.
C
2     CONTINUE
      IMIN = IL+1
      NN   = N
C
      DO 3 I = IMIN, NN
         IF (XVAL .LE. X(I)) THEN
            IL = I-1
            IER = 0
            RETURN
         ENDIF
3     CONTINUE
C
C        extrapolation to the right.
C
      IL = N-1
      IER = 1
      END
      SUBROUTINE SQRARR (CHI, N)
C
C-----------------------------------------------------------------------
C  takes square root of a chi array, in case user wants to
C  print or plot it.
C-----------------------------------------------------------------------
C
      DIMENSION CHI(N)
C
      DO 100 I = 1, N
         IF (CHI(I) .GE. 0.) THEN
            CHI(I) = SQRT(CHI(I))
         ELSE
            CHI(I) = 0.
         ENDIF
100   CONTINUE
C
      END
      SUBROUTINE TERPLD (N, X, Y, XVAL, YVAL, IER)
C
C-----------------------------------------------------------------------
C   linear interpolation of double precision array
C     input arguments:
C         n  = number points in x, y
C         x   = input x array
C         y   = input y array
C         xval  = x value where interpolation desired
C     output argument:
C         yval  = desired y value resulting from interpolation
C         ier = error flag 0=ok, -1=extrapolation to left,
C               +1=extrapolation to right
C-----------------------------------------------------------------------
C
      DIMENSION X(*), Y(*)
      DOUBLE PRECISION X, XVAL
C
C--- save value of il so search begins at same index next time
C--- terpol is called
C
      SAVE IL
      DATA IL / 1 /
      CALL SERCHD (N, X, XVAL, IL, IER)
C
C--- compute interpolation:
C
      DX1 = X(IL+1)-X(IL)
      DX2 = XVAL-X(IL)
      YVAL  = Y(IL)+((Y(IL+1)-Y(IL))/DX1)*DX2
      END
      SUBROUTINE TERPOL (N, X, Y, XVAL, YVAL, IER)
C
C-----------------------------------------------------------------------
C   linear interpolation
C     input arguments:
C         n  = number points in x, y
C         x   = input x array
C         y   = input y array
C         xval  = x value where interpolation desired
C     output argument:
C         yval  = desired y value resulting from interpolation
C         ier = error flag 0=ok, -1=extrapolation to left,
C               +1=extrapolation to right
C-----------------------------------------------------------------------
C
      DIMENSION X(*), Y(*)
C
C--- save value of il so search begins at same index next time
C--- terpol is called
C
      SAVE IL
      DATA IL / 1 /
      CALL SEARCH (N, X, XVAL, IL, IER)
C
C--- compute interpolation:
C
      SLOPE = (Y(IL+1)-Y(IL))/(X(IL+1)-X(IL))
      YVAL  = Y(IL)+SLOPE*(XVAL-X(IL))
      END
      SUBROUTINE YESNO (PROMPT, YORN)
C
C-----------------------------------------------------------------
C    Reads a character until it has obtained either a 'y', an
C    'n', or a blank.  Handles either upper or lower case,
C    returns upper case.
C     if user response to prompt is <cr> only, yorn is returned
C     as the upper case of the input value.
C-----------------------------------------------------------------
C
      CHARACTER* (*)PROMPT, YORN
      CHARACTER*1 ANS
C
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

      SUBROUTINE PRESS_ENTER()

	PRINT*,'Press ENTER to continue...'
	READ(*,*)

	RETURN
	END
