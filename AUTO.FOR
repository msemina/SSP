C
C     ..................................................................
C
C        SUBROUTINE AUTO
C
C        PURPOSE
C           TO FIND AUTOCOVARIANCES OF SERIES A FOR LAGS 0 TO L-1.
C
C        USAGE
C           CALL AUTO (A,N,L,R)
C
C        DESCRIPTION OF PARAMETERS
C           A    - INPUT VECTOR OF LENGTH N CONTAINING THE TIME SERIES
C                  WHOSE AUTOCOVARIANCE IS DESIRED.
C           N    - LENGTH OF THE VECTOR A.
C           L    - AUTOCOVARIANCE IS CALCULATED FOR LAGS OF 0, 1, 2,...,
C                  L-1.
C           R    - OUTPUT VECTOR OF LENGTH L CONTAINING AUTOCOVARIANCES
C                  OF SERIES A.
C
C        REMARKS
C           THE LENGTH OF R IS DIFFERENT FROM THE LENGTH OF A.  N MUST
C           BE GREATER THAN L.  IF NOT, R(1) IS SET TO ZERO AND RETURN
C           IS MADE TO THE CALLING PROGRAM.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DESCRIBED IN R.B. BLACKMAN AND J.W. TUKEY, 'THE MEASURMENT
C        OF POWER SPECTRA', DOVER PUBLICATIONS INC., NEW YORK, 1959.
C
C     ..................................................................
C
      SUBROUTINE AUTO (A,N,L,R)
      DIMENSION A(1),R(1)
C
C     CALCULATE AVERAGE OF TIME SERIES A
C
      AVER=0.0
      IF(N-L) 50,50,100
   50 R(1)=0.0
      RETURN
  100 DO 110 I=1,N
  110 AVER=AVER+A(I)
      FN=N
      AVER=AVER/FN
C
C     CALCULATE AUTOCOVARIANCES
C
      DO 130 J=1,L
      NJ=N-J+1
      SUM=0.0
      DO 120 I=1,NJ
      IJ=I+J-1
  120 SUM=SUM+(A(I)-AVER)*(A(IJ)-AVER)
      FNJ=NJ
  130 R(J)=SUM/FNJ
      RETURN
      END
