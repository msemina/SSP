C
C     ..................................................................
C
C        SUBROUTINE PECS
C
C        PURPOSE
C           ECONOMIZATION OF A POLYNOMIAL FOR UNSYMMETRIC RANGE
C
C        USAGE
C           CALL PECS (P,N,BOUND,EPS,TOL,WORK)
C
C        DESCRIPTION OF PARAMETERS
C           P     - COEFFICIENT VECTOR OF GIVEN POLYNOMIAL
C           N     - DIMENSION OF COEFFICIENT VECTOR
C           BOUND - RIGHT HAND BOUNDARY OF INTERVAL
C           EPS   - INITIAL ERROR BOUND
C           TOL   - TOLERANCE FOR ERROR
C           WORK  - WORKING STORAGE OF DIMENSION N
C
C        REMARKS
C           THE INITIAL COEFFICIENT VECTOR P IS REPLACED BY THE
C           ECONOMIZED VECTOR.
C           THE INITIAL ERROR BOUND EPS IS REPLACED BY A FINAL
C           ERROR BOUND.
C           N IS REPLACED BY THE DIMENSION OF THE REDUCED POLYNOMIAL.
C           IN CASE OF AN ARBITRARY INTERVAL (XL,XR) IT IS NECESSARY
C           FIRST TO CALCULATE THE EXPANSION OF THE GIVEN POLYNOMIAL
C           WITH ARGUMENT X IN POWERS OF T = (X-XL).
C           THIS IS ACCOMPLISHED THROUGH SUBROUTINE PCLD.
C           OPERATION IS BYPASSED IN CASE OF N LESS THAN 1.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           SUBROUTINE PECS TAKES AN (N-1)ST DEGREE POLYNOMIAL
C           APPROXIMATION TO A FUNCTION F(X) VALID WITHIN A TOLERANCE
C           EPS OVER THE INTERVAL (0,BOUND) AND REDUCES IT IF POSSIBLE
C           TO A POLYNOMIAL OF LOWER DEGREE VALID WITHIN TOLERANCE
C           TOL.
C           THE COEFFICIENT VECTOR OF THE N-TH SHIFTED CHEBYSHEV
C           POLYNOMIAL IS CALCULATED FROM THE RECURSION FORMULA
C           A(K) = -A(K+1)*K*L*(2*K-1)/(2*(N+K-1)*(N-K+1)).
C           REFERENCE
C           K. A. BRONS, ALGORITHM 37, TELESCOPE 1, CACM VOL. 4, 1961,
C           NO. 3, PP. 151.
C
C     ..................................................................
C
      SUBROUTINE PECS(P,N,BOUND,EPS,TOL,WORK)
C
      DIMENSION P(1),WORK(1)
      FL=BOUND*0.5
C
C     TEST OF DIMENSION
C
    1 IF(N-1)2,3,6
    2 RETURN
    3 IF(EPS+ABS(P(1))-TOL)4,4,5
    4 N=0
      EPS=EPS+ABS(P(1))
    5 RETURN
C
C     CALCULATE EXPANSION OF CHEBYSHEV POLYNOMIAL
C
    6 NEND=N-1
      WORK(N)=-P(N)
      DO 7 J=1,NEND
      K=N-J
      FN=(NEND-1+K)*(N-K)
      FK=K*(K+K-1)
    7 WORK(K)=-WORK(K+1)*FK*FL/FN
C
C        TEST FOR FEASIBILITY OF REDUCTION
C
      FN=ABS(WORK(1))
      IF(EPS+FN-TOL)8,8,5
C
C     REDUCE POLYNOMIAL
C
    8 EPS=EPS+FN
      N=NEND
      DO 9 J=1,NEND
    9 P(J)=P(J)+WORK(J)
      GOTO 1
      END
