C
C     ..................................................................
C
C        SUBROUTINE EXSMO
C
C        PURPOSE
C           TO FIND THE TRIPLE EXPONENTIAL SMOOTHED SERIES S OF THE
C           GIVEN SERIES X.
C
C        USAGE
C           CALL EXSMO (X,NX,AL,A,B,C,S)
C
C        DESCRIPTION OF PARAMETERS
C           X     - INPUT VECTOR OF LENGTH NX CONTAINING TIME SERIES
C                   DATA WHICH IS TO BE EXPONENTIALLY SMOOTHED.
C           NX    - THE NUMBER OF ELEMENTS IN X.
C           AL    - SMOOTHING CONSTANT, ALPHA.  AL MUST BE GREATER THAN
C                   ZERO AND LESS THAN ONE.
C           A,B,C - COEFFICIENTS OF THE PREDICTION EQUATION WHERE S IS
C                   PREDICTED T PERIODS HENCE BY
C                                 A + B*T + C*T*T/2.
C                   AS INPUT-- IF A=B=C=0, PROGRAM WILL PROVIDE INITIAL
C                   VALUES.  IF AT LEAST ONE OF A,B,C IS NOT ZERO,
C                   PROGRAM WILL TAKE GIVEN VALUES AS INITIAL VALUES.
C                   AS OUTPUT-- A,B,C CONTAIN LATEST, UPDATED COEFFI-
C                   CIENTS OF PREDICTION.
C           S     - OUTPUT VECTOR OF LENGTH NX CONTAINING TRIPLE
C                   EXPONENTIALLY SMOOTHED TIME SERIES.
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           REFER TO R. G. BROWN, 'SMOOTHING, FORECASTING AND PREDICTION
C           OF DISCRETE TIME SERIES', PRENTICE-HALL, N.J., 1963,
C           PP. 140 TO 144.
C
C     ..................................................................
C
      SUBROUTINE EXSMO (X,NX,AL,A,B,C,S)
      DIMENSION X(1),S(1)
C
C     IF A=B=C=0.0, GENERATE INITIAL VALUES OF A, B, AND C
C
      IF(A) 140, 110, 140
  110 IF(B) 140, 120, 140
  120 IF(C) 140, 130, 140
  130 C=X(1)-2.0*X(2)+X(3)
      B=X(2)-X(1)-1.5*C
      A=X(1)-B-0.5*C
C
  140 BE=1.0-AL
      BECUB=BE*BE*BE
      ALCUB=AL*AL*AL
C
C     DO THE FOLLOWING FOR I=1 TO NX
C
      DO 150 I=1,NX
C
C        FIND S(I) FOR ONE PERIOD AHEAD
C
      S(I)=A+B+0.5*C
C
C        UPDATE COEFFICIENTS A, B, AND C
C
      DIF=S(I)-X(I)
      A=X(I)+BECUB*DIF
      B=B+C-1.5*AL*AL*(2.0-AL)*DIF
  150 C=C-ALCUB*DIF
      RETURN
      END
