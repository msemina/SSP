C
C     ..................................................................
C
C        SUBROUTINE TWOAV
C
C        PURPOSE
C           TEST WHETHER A NUMBER OF SAMPLES ARE FROM THE SAME
C           POPULATION BY THE FRIEDMAN TWO-WAY ANALYSIS OF VARIANCE TEST
C
C        USAGE
C           CALL TWOAV(A,R,N,M,W,XR,NDF,NR)
C
C        DESCRIPTION OF PARAMETERS
C           A   - INPUT MATRIX, N BY M, OF ORIGINAL DATA
C           R   - OUTPUT MATRIX, N BY M, OF RANKED DATA
C           N   - NUMBER OF GROUPS
C           M   - NUMBER OF CASES IN EACH GROUP
C           W   - WORK AREA OF LENGTH 2*M
C           XR  - FRIEDMAN STATISTIC (OUTPUT)
C           NDF - NUMBER OF DEGREES OF FREEDOM (OUTPUT)
C           NR  - CODE, 0 FOR UNRANKED DATA IN A, 1 FOR RANKED DATA
C                 IN A (INPUT)
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           RANK
C
C        METHOD
C           DESCRIBED IN S. SIEGEL, 'NONPARAMETRIC STATISTICS FOR THE
C           BEHAVIORAL SCIENCES', MCGRAW-HILL, NEW YORK, 1956,
C           CHAPTER 7
C
C     ..................................................................
C
      SUBROUTINE TWOAV (A,R,N,M,W,XR,NDF,NR)
      DIMENSION A(1),R(1),W(1)
C
C        DETERMINE WHETHER DATA IS RANKED
C
      IF(NR-1) 10, 30, 10
C
C        RANK DATA IN EACH GROUP AND ASSIGN TIED OBSERVATIONS AVERAGE
C        OF TIED RANK
C
   10 DO 20 I=1,N
      IJ=I-N
      IK=IJ
      DO 15 J=1,M
      IJ=IJ+N
   15 W(J)=A(IJ)
      CALL RANK (W,W(M+1),M)
      DO 20 J=1,M
      IK=IK+N
      IW=M+J
   20 R(IK)=W(IW)
      GO TO 35
   30 NM=N*M
      DO 32 I=1,NM
   32 R(I)=A(I)
C
C        CALCULATE SUM OF SQUARES OF SUMS OF RANKS
C
   35 RTSQ=0.0
      IR=0
      DO 50 J=1,M
      RT=0.0
      DO 40 I=1,N
      IR=IR+1
   40 RT=RT+R(IR)
   50 RTSQ=RTSQ+RT*RT
C
C        CALCULATE FRIEDMAN TEST VALUE, XR
C
      FNM=N*(M+1)
      FM=M
      XR=(12.0/(FM*FNM))*RTSQ-3.0*FNM
C
C        FIND DEGREES OF FREEDOM
C
      NDF=M-1
      RETURN
      END
