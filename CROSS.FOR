C
C     ..................................................................
C
C        SUBROUTINE CROSS
C
C        PURPOSE
C           TO FIND THE CROSSCOVARIANCES OF SERIES A WITH SERIES B
C           (WHICH LEADS AND LAGS A).
C
C        USAGE
C           CALL CROSS (A,B,N,L,R,S)
C
C        DESCRIPTION OF PARAMETERS
C           A    - INPUT VECTOR OF LENGTH N CONTAINING FIRST TIME
C                  SERIES.
C           B    - INPUT VECTOR OF LENGTH N CONTAINING SECOND TIME
C                  SERIES.
C           N    - LENGTH OF SERIES A AND B.
C           L    - CROSSCOVARIANCE IS CALCULATED FOR LAGS AND LEADS OF
C                  0, 1, 2,..., L-1.
C           R    - OUTPUT VECTOR OF LENGTH L CONTAINING CROSSCOVARI-
C                  ANCES OF A WITH B, WHERE B LAGS A.
C           S    - OUTPUT VECTOR OF LENGTH L CONTAINING CROSSCOVARI-
C                  ANCES OF A WITH B, WHERE B LEADS A.
C
C        REMARKS
C           N MUST BE GREATER THAN L.  IF NOT, R(1) AND S(1) ARE SET TO
C           ZERO AND RETURN IS MADE TO THE CALLING PROGRAM.
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
      SUBROUTINE CROSS (A,B,N,L,R,S)
      DIMENSION A(1),B(1),R(1),S(1)
C
C     CALCULATE AVERAGES OF SERIES A AND B
C
      FN=N
      AVERA=0.0
      AVERB=0.0
      IF(N-L)50,50,100
   50 R(1)=0.0
      S(1)=0.0
      RETURN
  100 DO 110 I=1,N
      AVERA=AVERA+A(I)
  110 AVERB=AVERB+B(I)
      AVERA=AVERA/FN
      AVERB=AVERB/FN
C
C     CALCULATE CROSSCOVARIANCES OF SERIES A AND B
C
      DO 130 J=1,L
      NJ=N-J+1
      SUMR=0.0
      SUMS=0.0
      DO 120 I=1,NJ
      IJ=I+J-1
      SUMR=SUMR+(A(I)-AVERA)*(B(IJ)-AVERB)
  120 SUMS=SUMS+(A(IJ)-AVERA)*(B(I)-AVERB)
      FNJ=NJ
      R(J)=SUMR/FNJ
  130 S(J)=SUMS/FNJ
      RETURN
      END
