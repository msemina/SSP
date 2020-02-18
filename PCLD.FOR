C
C     ..................................................................
C
C        SUBROUTINE PCLD
C
C        PURPOSE
C           SHIFT OF ORIGIN (COMPLETE LINEAR SYNTHETIC DIVISION)
C
C        USAGE
C           CALL PCLD(X,IDIMX,U)
C
C        DESCRIPTION OF PARAMETERS
C           X     - VECTOR OF COEFFICIENTS, ORDERED FROM SMALLEST TO
C                   LARGEST POWER. IT IS REPLACED BY VECTOR OF
C                   TRANSFORMED COEFFICIENTS.
C           IDIMX - DIMENSION OF X
C           U     - SHIFT PARAMETER
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           COEFFICIENT VECTOR X(I) OF POLYNOMIAL P(Z) IS TRANSFORMED
C           SUCH THAT Q(Z)=P(Z-U) WHERE Q(Z) DENOTES THE POLYNOMIAL
C           WITH TRANSFORMED COEFFICIENT VECTOR.
C
C     ..................................................................
C
      SUBROUTINE PCLD (X,IDIMX,U)
      DIMENSION X(1)
C
      K=1
    1 J=IDIMX
    2 IF (J-K) 4,4,3
    3 X(J-1)=X(J-1)+U*X(J)
      J=J-1
      GO TO 2
    4 K=K+1
      IF (IDIMX-K) 5,5,1
    5 RETURN
      END
