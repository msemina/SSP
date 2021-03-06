C
C     ..................................................................
C
C        SUBROUTINE DMATX
C
C        PURPOSE
C           COMPUTE MEANS OF VARIABLES IN EACH GROUP AND A POOLED
C           DISPERSION MATRIX FOR ALL THE GROUPS. NORMALLY THIS SUB-
C           ROUTINE IS USED IN THE PERFORMANCE OF DISCRIMINANT ANALYSIS.
C
C        USAGE
C           CALL DMATX (K,M,N,X,XBAR,D,CMEAN)
C
C        DESCRIPTION OF PARAMETERS
C           K     - NUMBER OF GROUPS
C           M     - NUMBER OF VARIABLES (MUST BE THE SAME FOR ALL
C                   GROUPS).
C           N     - INPUT VECTOR OF LENGTH K CONTAINING SAMPLE SIZES OF
C                   GROUPS.
C           X     - INPUT VECTOR CONTAINING DATA IN THE MANNER EQUIVA-
C                   LENT TO A 3-DIMENSIONAL FORTRAN ARRAY, X(1,1,1),
C                   X(2,1,1), X(3,1,1), ETC.  THE FIRST SUBSCRIPT IS
C                   CASE NUMBER, THE SECOND SUBSCRIPT IS VARIABLE NUMBER
C                   AND THE THIRD SUBSCRIPT IS GROUP NUMBER.  THE
C                   LENGTH OF VECTOR X IS EQUAL TO THE TOTAL NUMBER OF
C                   DATA POINTS, T*M, WHERE T = N(1)+N(2)+...+N(K).
C           XBAR  - OUTPUT MATRIX (M X K) CONTAINING MEANS OF VARIABLES
C                   IN K GROUPS.
C           D     - OUTPUT MATRIX (M X M) CONTAINING POOLED DISPERSION.
C           CMEAN - WORKING VECTOR OF LENGTH M.
C
C        REMARKS
C           THE NUMBER OF VARIABLES MUST BE GREATER THAN OR EQUAL TO
C           THE NUMBER OF GROUPS.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           REFER TO 'BMD COMPUTER PROGRAMS MANUAL', EDITED BY W. J.
C           DIXON, UCLA, 1964, AND T. W. ANDERSON, 'INTRODUCTION TO
C           MULTIVARIATE STATISTICAL ANALYSIS', JOHN WILEY AND SONS,
C           1958, SECTION 6.6-6.8.
C
C     ..................................................................
C
      SUBROUTINE DMATX (K,M,N,X,XBAR,D,CMEAN)
      DIMENSION N(1),X(1),XBAR(1),D(1),CMEAN(1)
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
C     DOUBLE PRECISION XBAR,D,CMEAN
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        ...............................................................
C
C     INITIALIZATION
C
      MM=M*M
      DO 100 I=1,MM
  100 D(I)=0.0
C
C     CALCULATE MEANS
C
      N4=0
      L=0
      LM=0
      DO 160 NG=1,K
      N1=N(NG)
      FN=N1
      DO 130 J=1,M
      LM=LM+1
      XBAR(LM)=0.0
      DO 120 I=1,N1
      L=L+1
  120 XBAR(LM)=XBAR(LM)+X(L)
  130 XBAR(LM)=XBAR(LM)/FN
C
C     CALCULATE SUMS OF CROSS-PRODUCTS OF DEVIATIONS
C
      LMEAN=LM-M
      DO 150 I=1,N1
      LL=N4+I-N1
      DO 140 J=1,M
      LL=LL+N1
      N2=LMEAN+J
  140 CMEAN(J)=X(LL)-XBAR(N2)
      LL=0
      DO 150 J=1,M
      DO 150 JJ=1,M
      LL=LL+1
  150 D(LL)=D(LL)+CMEAN(J)*CMEAN(JJ)
  160 N4=N4+N1*M
C
C     CALCULATE THE POOLED DISPERSION MATRIX
C
      LL=-K
      DO 170 I=1,K
  170 LL=LL+N(I)
      FN=LL
      DO 180 I=1,MM
  180 D(I)=D(I)/FN
C
      RETURN
      END
