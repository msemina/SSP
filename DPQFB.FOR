C
C     ..................................................................
C
C        SUBROUTINE DPQFB
C
C        PURPOSE
C           TO FIND AN APPROXIMATION Q(X)=Q1+Q2*X+X*X TO A QUADRATIC
C           FACTOR OF A GIVEN POLYNOMIAL P(X) WITH REAL COEFFICIENTS.
C
C        USAGE
C           CALL DPQFB(C,IC,Q,LIM,IER)
C
C        DESCRIPTION OF PARAMETERS
C           C   - DOUBLE PRECISION INPUT VECTOR CONTAINING THE
C                 COEFFICIENTS OF P(X) - C(1) IS THE CONSTANT TERM
C                 (DIMENSION IC)
C           IC  - DIMENSION OF C
C           Q   - DOUBLE PRECISION VECTOR OF DIMENSION 4 - ON INPUT Q(1)
C                 AND Q(2) CONTAIN INITIAL GUESSES FOR Q1 AND Q2 - ON
C                 RETURN Q(1) AND Q(2) CONTAIN THE REFINED COEFFICIENTS
C                 Q1 AND Q2 OF Q(X), WHILE Q(3) AND Q(4) CONTAIN THE
C                 COEFFICIENTS A AND B OF A+B*X, WHICH IS THE REMAINDER
C                 OF THE QUOTIENT OF P(X) BY Q(X)
C           LIM - INPUT VALUE SPECIFYING THE MAXIMUM NUMBER OF
C                 ITERATIONS TO BE PERFORMED
C           IER - RESULTING ERROR PARAMETER (SEE REMARKS)
C                 IER= 0 - NO ERROR
C                 IER= 1 - NO CONVERGENCE WITHIN LIM ITERATIONS
C                 IER=-1 - THE POLYNOMIAL P(X) IS CONSTANT OR UNDEFINED
C                          - OR OVERFLOW OCCURRED IN NORMALIZING P(X)
C                 IER=-2 - THE POLYNOMIAL P(X) IS OF DEGREE 1
C                 IER=-3 - NO FURTHER REFINEMENT OF THE APPROXIMATION TO
C                          A QUADRATIC FACTOR IS FEASIBLE, DUE TO EITHER
C                          DIVISION BY 0, OVERFLOW OR AN INITIAL GUESS
C                          THAT IS NOT SUFFICIENTLY CLOSE TO A FACTOR OF
C                          P(X)
C
C        REMARKS
C           (1)  IF IER=-1 THERE IS NO COMPUTATION OTHER THAN THE
C                POSSIBLE NORMALIZATION OF C.
C           (2)  IF IER=-2 THERE IS NO COMPUTATION OTHER THAN THE
C                NORMALIZATION OF C.
C           (3)  IF IER =-3  IT IS SUGGESTED THAT A NEW INITIAL GUESS BE
C                MADE FOR A QUADRATIC FACTOR.  Q, HOWEVER, WILL CONTAIN
C                THE VALUES ASSOCIATED WITH THE ITERATION THAT YIELDED
C                THE SMALLEST NORM OF THE MODIFIED LINEAR REMAINDER.
C           (4)  IF IER=1, THEN, ALTHOUGH THE NUMBER OF ITERATIONS LIM
C                WAS TOO SMALL TO INDICATE CONVERGENCE, NO OTHER PROB-
C                LEMS HAVE BEEN DETECTED, AND Q WILL CONTAIN THE VALUES
C                ASSOCIATED WITH THE ITERATION THAT YIELDED THE SMALLEST
C                NORM OF THE MODIFIED LINEAR REMAINDER.
C           (5)  FOR COMPLETE DETAIL SEE THE DOCUMENTATION FOR
C                SUBROUTINES PQFB AND DPQFB.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           COMPUTATION IS BASED ON BAIRSTOW'S ITERATIVE METHOD.  (SEE
C           WILKINSON, J.H., THE EVALUATION OF THE ZEROS OF ILL-CON-
C           DITIONED POLYNOMIALS (PART ONE AND TWO), NUMERISCHE MATHE-
C           MATIK, VOL.1 (1959), PP. 150-180, OR HILDEBRAND, F.B.,
C           INTRODUCTION TO NUMERICAL ANALYSIS, MC GRAW-HILL, NEW YORK/
C           TORONTO/LONDON, 1956, PP. 472-476.)
C
C     ..................................................................
C
      SUBROUTINE DPQFB(C,IC,Q,LIM,IER)
C
C
      DIMENSION C(1),Q(1)
      DOUBLE PRECISION A,B,AA,BB,CA,CB,CC,CD,A1,B1,C1,H,HH,Q1,Q2,QQ1,
     1                 QQ2,QQQ1,QQQ2,DQ1,DQ2,EPS,EPS1,C,Q
C
C        TEST ON LEADING ZERO COEFFICIENTS
      IER=0
      J=IC+1
    1 J=J-1
      IF(J-1)40,40,2
    2 IF(C(J))3,1,3
C
C        NORMALIZATION OF REMAINING COEFFICIENTS
    3 A=C(J)
      IF(A-1.D0)4,6,4
    4 DO 5 I=1,J
      C(I)=C(I)/A
      CALL OVERFL(N)
      IF(N-2)40,5,5
    5 CONTINUE
C
C        TEST ON NECESSITY OF BAIRSTOW ITERATION
    6 IF(J-3)41,38,7
C
C        PREPARE BAIRSTOW ITERATION
    7 EPS=1.D-14
      EPS1=1.D-6
      L=0
      LL=0
      Q1=Q(1)
      Q2=Q(2)
      QQ1=0.D0
      QQ2=0.D0
      AA=C(1)
      BB=C(2)
      CB=DABS(AA)
      CA=DABS(BB)
      IF(CB-CA)8,9,10
    8 CC=CB+CB
      CB=CB/CA
      CA=1.D0
      GO TO 11
    9 CC=CA+CA
      CA=1.D0
      CB=1.D0
      GO TO 11
   10 CC=CA+CA
      CA=CA/CB
      CB=1.D0
   11 CD=CC*.1D0
C
C        START BAIRSTOW ITERATION
C        PREPARE NESTED MULTIPLICATION
   12 A=0.D0
      B=A
      A1=A
      B1=A
      I=J
      QQQ1=Q1
      QQQ2=Q2
      DQ1=HH
      DQ2=H
C
C        START NESTED MULTIPLICATION
   13 H=-Q1*B-Q2*A+C(I)
      CALL OVERFL(N)
      IF(N-2)42,14,14
   14 B=A
      A=H
      I=I-1
      IF(I-1)18,15,16
   15 H=0.D0
   16 H=-Q1*B1-Q2*A1+H
      CALL OVERFL(N)
      IF(N-2)42,17,17
   17 C1=B1
      B1=A1
      A1=H
      GO TO 13
C        END OF NESTED MULTIPLICATION
C
C        TEST ON SATISFACTORY ACCURACY
   18 H=CA*DABS(A)+CB*DABS(B)
      IF(LL)19,19,39
   19 L=L+1
      IF(DABS(A)-EPS*DABS(C(1)))20,20,21
   20 IF(DABS(B)-EPS*DABS(C(2)))39,39,21
C
C        TEST ON LINEAR REMAINDER OF MINIMUM NORM
   21 IF(H-CC)22,22,23
   22 AA=A
      BB=B
      CC=H
      QQ1=Q1
      QQ2=Q2
C
C        TEST ON LAST ITERATION STEP
   23 IF(L-LIM)28,28,24
C
C        TEST ON RESTART OF BAIRSTOW ITERATION WITH ZERO INITIAL GUESS
   24 IF(H-CD)43,43,25
   25 IF(Q(1))27,26,27
   26 IF(Q(2))27,42,27
   27 Q(1)=0.D0
      Q(2)=0.D0
      GO TO 7
C
C        PERFORM ITERATION STEP
   28 HH=DMAX1(DABS(A1),DABS(B1),DABS(C1))
      IF(HH)42,42,29
   29 A1=A1/HH
      B1=B1/HH
      C1=C1/HH
      H=A1*C1-B1*B1
      IF(H)30,42,30
   30 A=A/HH
      B=B/HH
      HH=(B*A1-A*B1)/H
      H=(A*C1-B*B1)/H
      Q1=Q1+HH
      Q2=Q2+H
C        END OF ITERATION STEP
C
C        TEST ON SATISFACTORY RELATIVE ERROR OF ITERATED VALUES
      IF(DABS(HH)-EPS*DABS(Q1))31,31,33
   31 IF(DABS(H)-EPS*DABS(Q2))32,32,33
   32 LL=1
      GO TO 12
C
C        TEST ON DECREASING RELATIVE ERRORS
   33 IF(L-1)12,12,34
   34 IF(DABS(HH)-EPS1*DABS(Q1))35,35,12
   35 IF(DABS(H)-EPS1*DABS(Q2))36,36,12
   36 IF(DABS(QQQ1*HH)-DABS(Q1*DQ1))37,44,44
   37 IF(DABS(QQQ2*H)-DABS(Q2*DQ2))12,44,44
C        END OF BAIRSTOW ITERATION
C
C        EXIT IN CASE OF QUADRATIC POLYNOMIAL
   38 Q(1)=C(1)
      Q(2)=C(2)
      Q(3)=0.D0
      Q(4)=0.D0
      RETURN
C
C        EXIT IN CASE OF SUFFICIENT ACCURACY
   39 Q(1)=Q1
      Q(2)=Q2
      Q(3)=A
      Q(4)=B
      RETURN
C
C        ERROR EXIT IN CASE OF ZERO OR CONSTANT POLYNOMIAL
   40 IER=-1
      RETURN
C
C        ERROR EXIT IN CASE OF LINEAR POLYNOMIAL
   41 IER=-2
      RETURN
C
C        ERROR EXIT IN CASE OF NONREFINED QUADRATIC FACTOR
   42 IER=-3
      GO TO 44
C
C        ERROR EXIT IN CASE OF UNSATISFACTORY ACCURACY
   43 IER=1
   44 Q(1)=QQ1
      Q(2)=QQ2
      Q(3)=AA
      Q(4)=BB
      RETURN
      END
