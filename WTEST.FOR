C
C     ..................................................................
C
C        SUBROUTINE WTEST
C
C        PURPOSE
C           TEST DEGREE OF ASSOCIATION AMONG A NUMBER OF VARIABLES BY
C           THE KENDALL COEFFICIENT OF CONCORDANCE
C
C        USAGE
C           CALL WTEST(A,R,N,M,WA,W,CS,NDF,NR)
C
C        DESCRIPTION OF PARAMETERS
C           A   - INPUT MATRIX, N BY M, OF ORIGINAL DATA
C           R   - OUTPUT MATRIX, N BY M, OF RANKED DATA.SMALLEST VALUE
C                 IS RANKED 1, LARGEST IS RANKED N. TIES ARE ASSIGNED
C                 AVERAGE OF TIED RANKS
C           N   - NUMBER OF VARIABLES
C           M   - NUMBER OF CASES
C           WA  - WORK AREA VECTOR OF LENGTH 2*M
C           W   - KENDALL COEFFICIENT OF CONCORDANCE(OUTPUT)
C           CS  - CHI-SQUARE (OUTPUT)
C           NDF - NUMBER OF DEGREES OF FREEDOM (OUTPUT)
C           NR  - CODE, 0 FOR UNRANKED DATA IN A, 1 FOR RANKED DATA
C                 IN A (INPUT)
C
C        REMARKS
C           CHI-SQUARE IS SET TO ZERO IF M IS 7 OR SMALLER
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           RANK
C           TIE
C
C        METHOD
C           DESCRIBED IN S. SIEGEL, 'NONPARAMETRIC STATISTICS FOR THE
C           BEHAVIORAL SCIENCES', MCGRAW-HILL, NEW YORK, 1956,
C           CHAPTER 9
C     ..................................................................
C
C
      SUBROUTINE WTEST (A,R,N,M,WA,W,CS,NDF,NR)
      DIMENSION A(1),R(1),WA(1)
C
      FM=M
      FN=N
C
C        DETERMINE WHETHER DATA IS RANKED
C        RANK DATA FOR ALL VARIABLES ASSIGNING TIED OBSERVATIONS AVERAGE
C        OF TIED RANKS AND COMPUTE CORRECTION FOR TIED SCORES
C
      T=0.0
      KT=1
      DO 20 I=1,N
      IJ=I-N
      IK=IJ
      IF(NR-1) 5,2,5
    2 DO 3 J=1,M
      IJ=IJ+N
      K=M+J
    3 WA(K)=A(IJ)
      GO TO 15
    5 DO 10 J=1,M
      IJ=IJ+N
   10 WA(J)=A(IJ)
      CALL RANK(WA,WA(M+1),M)
   15 CALL TIE(WA(M+1),M,KT,TI)
      T=T+TI
      DO 20 J=1,M
      IK=IK+N
      IW=M+J
   20 R(IK)=WA(IW)
C
C        CALCULATE VECTOR OF SUMS OF RANKS
C
      IR=0
      DO 40 J=1,M
      WA(J)=0.0
      DO 40 I=1,N
      IR=IR+1
   40 WA(J)=WA(J)+R(IR)
C
C        COMPUTE MEAN OF SUMS OF RANKS
C
      SM=0.0
      DO 50 J=1,M
   50 SM=SM+WA(J)
      SM=SM/FM
C
C        COMPUTE SUM OF SQUARES OF DEVIATIONS
C
      S=0.0
      DO 60 J=1,M
   60 S=S+(WA(J)-SM)*(WA(J)-SM)
C
C        COMPUTE W
C
      W=S/(((FN*FN)*(FM*FM*FM-FM)/12.0)-FN*T)
C
C        COMPUTE DEGREES OF FREEDOM AND CHI-SQUARE IF M IS OVER 7
C
      CS=0.0
      NDF=0
      IF(M-7) 70,70,65
   65 CS=FN*(FM-1.0)*W
      NDF=M-1
   70 RETURN
      END
