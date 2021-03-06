C
C     ..................................................................
C
C        SUBROUTINE PROBT
C
C        PURPOSE
C           TO OBTAIN MAXIMUM LIKELIHOOD ESTIMATES FOR THE PARAMETERS A
C           AND B IN THE PROBIT EQUATION  Y = A + BX.  AN ITERATIVE
C           SCHEME IS USED.  THE INPUT TO THE SUBROUTINE CONSISTS OF K
C           DIFFERENT DOSAGE LEVELS APPLIED TO K GROUPS OF SUBJECTS, AND
C           THE NUMBER OF SUBJECTS IN EACH GROUP RESPONDING TO THE
C           RESPECTIVE DOSAGE OF THE DRUG.
C
C        USAGE
C           CALL PROBT (K,X,S,R,LOG,ANS,W1,W2,IER)
C
C        DESCRIPTION OF PARAMETERS
C           K   - NUMBER OF DIFFERENT DOSE LEVELS OF THE DRUG.  K SHOULD
C                 BE GREATER THAN 2.
C           X   - INPUT VECTOR OF LENGTH K CONTAINING THE DOSE LEVEL OF
C                 THE DRUG TESTED.  X MUST BE NON-NEGATIVE.
C           S   - INPUT VECTOR OF LENGTH K CONTAINING THE NUMBER OF
C                 SUBJECTS TESTED AT EACH DOSE LEVEL
C           R   - INPUT VECTOR OF LENGTH K CONTAINING THE NUMBER OF
C                 SUBJECTS AT EACH LEVEL RESPONDING TO THE DRUG
C           LOG - INPUT OPTION CODE
C                 1- IF IT IS DESIRED TO CONVERT THE DOSE LEVELS TO
C                    COMMON LOGARITHMS.  THE DOSAGE LEVELS SHOULD BE
C                    NON-NULL IN THIS CASE.
C                 0- IF NO CONVERSION IS DESIRED
C           ANS - OUTPUT VECTOR OF LENGTH 4 CONTAINING THE FOLLOWING
C                 RESULTS
C                 ANS(1)- ESTIMATE OF THE INTERCEPT CONSTANT A
C                 ANS(2)- ESTIMATE OF THE PROBIT REGRESSION COEFFICIENT
C                         B
C                 ANS(3)- CHI-SQUARED VALUE FOR A TEST OF SIGNIFICANCE
C                         OF THE FINAL PROBIT EQUATION
C                 ANS(4)- DEGREES OF FREEDOM FOR THE CHI-SQUARE
C                         STATISTIC
C           W1  - OUTPUT VECTOR OF LENGTH K CONTAINING THE PROPORTIONS
C                 OF SUBJECTS RESPONDING TO THE VARIOUS DOSE LEVELS OF
C                 THE DRUG
C           W2  - OUTPUT VECTOR OF LENGTH K CONTAINING THE VALUES OF THE
C                 EXPECTED PROBIT FOR THE VARIOUS LEVELS OF A DRUG
C           IER - 1 IF K IS NOT GREATER THAN 2.
C                 2 IF SOME DOSAGE LEVEL IS NEGATIVE, OR IF THE INPUT
C                   OPTION CODE LOG IS 1 AND SOME DOSAGE LEVEL IS ZERO.
C                 3 IF SOME ELEMENT OF S IS NOT POSITIVE.
C                 4 IF NUMBER OF SUBJECTS RESPONDING IS GREATER THAN
C                 NUMBER OF SUBJECTS TESTED.
C                 ONLY IF IER IS ZERO IS A PROBIT ANALYSIS PERFORMED.
C                 OTHERWISE, ANS, W1, AND W2 ARE SET TO ZERO.
C
C        REMARKS
C           THE PROGRAM WILL ITERATE ON THE PROBIT EQUATION UNTIL TWO
C           SUCCESSIVE SOLUTIONS PRODUCE CHANGES OF LESS THAN 10**(-7).
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NDTR
C           NDTRI
C
C        METHOD
C           REFER TO D. J. FINNEY, PROBIT ANALYSIS. 2ND ED. (CAMBRIDGE,
C           1952)
C
C     ..................................................................
C
      SUBROUTINE PROBT (K,X,S,R,LOG,ANS,W1,W2,IER)
C
      DIMENSION X(1),S(1),R(1),ANS(1),W1(1),W2(1)
C
C        TEST WHETHER LOG CONVERSION IS NEEDED
C
      IER=0
      IF(K-2)5,5,7
    5 IER = 1
      GO TO 90
    7 DO 8 I=1,K
      IF(X(I))12,8,8
    8 CONTINUE
      IF(LOG-1) 16,10,16
   10 DO 15 I=1,K
      IF(X(I))12,12,14
   12 IER=2
      GO TO 90
   14 X(I)= ALOG10(X(I))
   15 CONTINUE
C
C        COMPUTE PROPORTIONS OF OBJECTS RESPONDING
C
   16 DO 18 I=1,K
      IF(S(I)-R(I)) 17,18,18
   17 IER=4
      GO TO 90
   18 CONTINUE
   20 DO 23 I=1,K
      IF(S(I))21,21,22
   21 IER=3
      GO TO 90
   22 W1(I)=R(I)/S(I)
   23 CONTINUE
C
C        COMPUTE INITIAL ESTIMATES OF INTERCEPT AND PROBIT REGRESSION
C        COEFFICIENT
C
      WN=0.0
      XBAR=0.0
      SNWY=0.0
      SXX=0.0
      SXY=0.0
C
      DO 30 I=1,K
      P=W1(I)
      IF(P) 30, 30, 24
   24 IF(P-1.0) 25, 30, 30
   25 WN=WN+1.0
C
      CALL NDTRI (P,Z,D,IER)
C
      Z=Z+5.0
      XBAR=XBAR+X(I)
      SNWY=SNWY+Z
      SXX=SXX+X(I)**2
      SXY=SXY+X(I)*Z
   30 CONTINUE
C
      B=(SXY-(XBAR*SNWY)/WN)/(SXX-(XBAR*XBAR)/WN)
      XBAR=XBAR/WN
      SNWY=SNWY/WN
      A=SNWY-B*XBAR
      DD=0.0
C
C        COMPUTE EXPECTED PROBIT
C
      DO 31 I=1,K
   31 W2(I)=A+B*X(I)
C
   33 SNW=0.0
      SNWX=0.0
      SNWY=0.0
      SNWXX=0.0
      SNWXY=0.0
      DO 50 I=1,K
      Y=W2(I)
C
C        FIND A WEIGHTING COEFFICIENT FOR PROBIT ANALYSIS
C
      D=Y-5.0
C
      CALL NDTR (D,P,Z)
C
      Q=1.0-P
      W=(Z*Z)/(P*Q)
C
C        COMPUTE WORKING PROBIT
C
      IF(Y-5.0) 35, 35, 40
   35 WP=(Y-P/Z)+W1(I)/Z
      GO TO 45
   40 WP=(Y+Q/Z)-(1.0-W1(I))/Z
C
C        SUM INTERMEDIATE RESULTS
C
   45 WN=W*S(I)
      SNW=SNW+WN
      SNWX=SNWX+WN*X(I)
      SNWY=SNWY+WN*WP
      SNWXX=SNWXX+WN*X(I)**2
   50 SNWXY=SNWXY+WN*X(I)*WP
C
C        COMPUTE NEW ESTIMATES OF INTERCEPT AND COEFFICIENT
C
      XBAR=SNWX/SNW
C
      SXX=SNWXX-(SNWX)*(SNWX)/SNW
      SXY=SNWXY-(SNWX)*(SNWY)/SNW
      B=SXY/SXX
C
      A=SNWY/SNW-B*XBAR
C
C        EXAMINE THE CHANGES IN Y
C
      SXX=0.0
      DO 60 I=1,K
      Y=A+B*X(I)
      D=W2(I)-Y
      SXX=SXX+D*D
   60 W2(I)=Y
      IF(( ABS(DD-SXX))-(1.0E-7)) 65, 65, 63
   63 DD=SXX
      GO TO 33
C
C        STORE INTERCEPT AND COEFFICIENT
C
   65 ANS(1)=A
      ANS(2)=B
C
C        COMPUTE CHI-SQUARE
C
      ANS(3)=0.0
      DO 70 I=1,K
      Y=W2(I)-5.0
C
      CALL NDTR (Y,P,D)
C
      AA=R(I)-S(I)*P
      DD=S(I)*P*(1.0-P)
   70 ANS(3)=ANS(3)+AA*AA/DD
C
C        DEGREES OF FREEDOM FOR CHI-SQUARE
C
      ANS(4)=K-2
C
   80 RETURN
   90 DO 100 I=1,K
      W1(I)=0.0
  100 W2(I)=0.0
      DO 110 I=1,4
  110 ANS(I)=0.0
      GO TO 80
      END
