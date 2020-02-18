C
C     ..................................................................
C
C        SUBROUTINE CHISQ
C
C        PURPOSE
C           COMPUTE CHI-SQUARE FROM A CONTINGENCY TABLE
C
C        USAGE
C           CALL CHISQ(A,N,M,CS,NDF,IERR,TR,TC)
C
C        DESCRIPTION OF PARAMETERS
C           A    - INPUT MATRIX, N BY M, CONTAINING CONTINGENCY TABLE
C           N    - NUMBER OF ROWS IN A
C           M    - NUMBER OF COLUMNS IN A
C           CS   - CHI-SQUARE (OUTPUT)
C           NDF  - NUMBER OF DEGREES OF FREEDOM (OUTPUT)
C           IERR - ERROR CODE (OUTPUT)
C                    0 - NORMAL CASE
C                    1 - EXPECTED VALUE IS LESS THAN 1.0 IN ONE OR
C                        MORE CELLS
C                    3 - NUMBER OF DEGREES OF FREEDOM IS ZERO
C           TR   - WORK VECTOR OF LENGTH N
C           TC   - WORK VECTOR OF LENGTH M
C
C        REMARKS
C           IF ONE OR MORE CELLS CONTAIN AN EXPECTED VALUE (I.E.,
C           THEORETICAL VALUE) LESS THAN 1.0, CHI-SQUARE WILL BE
C           COMPUTED, BUT ERROR CODE WILL BE SET TO 1.
C           SEE REFERENCE GIVEN BELOW.
C           CHI-SQUARE IS SET TO ZERO IF EITHER N OR M IS ONE (ERROR
C           CODE 3).
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DESCRIBED IN S. SIEGEL, 'NONPARAMETRIC STATISTICS FOR THE
C           BEHAVIORAL SCIENCES', MCGRAW-HILL, NEW YORK, 1956,
C           CHAPTER 6 AND CHAPTER 8.
C
C     ..................................................................
C
      SUBROUTINE CHISQ(A,N,M,CS,NDF,IERR,TR,TC)
      DIMENSION A(1),TR(1),TC(1)
C
      NM=N*M
      IERR=0
      CS=0.0
C
C        FIND DEGREES OF FREEDOM
C
      NDF=(N-1)*(M-1)
      IF(NDF) 5,5,10
    5 IERR=3
      RETURN
C
C        COMPUTE TOTALS OF ROWS
C
   10 DO 90 I=1,N
      TR(I)=0.0
      IJ=I-N
      DO 90 J=1,M
      IJ=IJ+N
   90 TR(I)=TR(I)+A(IJ)
C
C        COMPUTE TOTALS OF COLUMNS
C
      IJ=0
      DO 100 J=1,M
      TC(J)=0.0
      DO 100 I=1,N
      IJ=IJ+1
  100 TC(J)=TC(J)+A(IJ)
C
C        COMPUTE GRAND TOTAL
C
      GT=0.0
      DO 110 I=1,N
  110 GT=GT+TR(I)
C
C        COMPUTE CHI SQUARE FOR 2 BY 2 TABLE (SPECIAL CASE)
C
      IF(NM-4) 130,120,130
  120 CS=GT*(ABS(A(1)*A(4)-A(2)*A(3))-GT/2.0)**2  /(TC(1)*TC(2)*TR(1)
     1*TR(2))
      RETURN
C
C        COMPUTE CHI SQUARE FOR OTHER CONTINGENCY TABLES
C
  130 IJ=0
      DO 140 J=1,M
      DO 140 I=1,N
      IJ=IJ+1
      E=TR(I)*TC(J)/GT
      IF(E-1.0) 135, 140, 140
  135 IERR=1
  140 CS=CS+(A(IJ)-E)*(A(IJ)-E)/E
      RETURN
      END
