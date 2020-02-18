C
C     ..................................................................
C
C        SUBROUTINE SMO
C
C        PURPOSE
C           TO SMOOTH OR FILTER SERIES A BY WEIGHTS W.
C
C        USAGE
C           CALL SMO (A,N,W,M,L,R)
C
C        DESCRIPTION OF PARAMETERS
C           A - INPUT VECTOR OF LENGTH N CONTAINING TIME SERIES DATA.
C           N - LENGTH OF SERIES A.
C           W - INPUT VECTOR OF LENGTH M CONTAINING WEIGHTS.
C           M - NUMBER OF ITEMS IN WEIGHT VECTOR.  M MUST BE AN ODD
C               INTEGER.  (IF M IS AN EVEN INTEGER, ANY FRACTION
C               RESULTING FROM THE CALCULATION OF (L*(M-1))/2 IN (1)
C               AND (2) BELOW WILL BE TRUNCATED.)
C           L - SELECTION INTEGER.  FOR EXAMPLE, L=12 MEANS THAT WEIGHTS
C               ARE APPLIED TO EVERY 12-TH ITEM OF A.  L=1 APPLIES
C               WEIGHTS TO SUCCESSIVE ITEMS OF A.  FOR MONTHLY DATA,
C               L=12 GIVES YEAR-TO-YEAR AVERAGES AND L=1 GIVES MONTH-TO-
C               MONTH AVERAGES.
C           R - OUTPUT VECTOR OF LENGTH N.  FROM IL TO IH ELEMENTS OF
C               THE VECTOR R ARE FILLED WITH THE SMOOTHED SERIES AND
C               OTHER ELEMENTS WITH ZERO, WHERE
C                    IL=(L*(M-1))/2+1  ................ (1)
C                    IH=N-(L*(M-1))/2  ................ (2)
C
C        REMARKS
C           N MUST BE GREATER THAN OR EQUAL TO THE PRODUCT OF L*M.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           REFER TO THE ARTICLE 'FORTRAN SUBROUTINES FOR TIME SERIES
C           ANALYSIS', BY J. R. HEALY AND B. P. BOGERT, COMMUNICATIONS
C           OF ACM, V.6, NO.1, JANUARY, 1963.
C
C     ..................................................................
C
      SUBROUTINE SMO (A,N,W,M,L,R)
      DIMENSION A(1),W(1),R(1)
C
C     INITIALIZATION
C
      DO 110 I=1,N
  110 R(I)=0.0
      IL=(L*(M-1))/2+1
      IH=N-(L*(M-1))/2
C
C     SMOOTH SERIES A BY WEIGHTS W
C
      DO 120 I=IL,IH
      K=I-IL+1
      DO 120 J=1,M
      IP=(J*L)-L+K
  120 R(I)=R(I)+A(IP)*W(J)
      RETURN
      END
