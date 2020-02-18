C
C     ..................................................................
C
C        SUBROUTINE PDER
C
C        PURPOSE
C           FIND DERIVATIVE OF A POLYNOMIAL
C
C        USAGE
C           CALL PDER(Y,IDIMY,X,IDIMX)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - VECTOR OF COEFFICIENTS FOR DERIVATIVE, ORDERED FROM
C                   SMALLEST TO LARGEST POWER
C           IDIMY - DIMENSION OF Y (EQUAL TO IDIMX-1)
C           X     - VECTOR OF COEFFICIENTS FOR ORIGINAL POLYNOMIAL,
C                   ORDERED FROM SMALLEST TO LARGEST POWER
C           IDIMX - DIMENSION OF X
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DIMENSION OF Y IS SET AT DIMENSION OF X LESS ONE. DERIVATIVE
C           IS THEN CALCULATED BY MULTIPLYING COEFFICIENTS BY THEIR
C           RESPECTIVE EXPONENTS.
C
C     ..................................................................
C
      SUBROUTINE PDER(Y,IDIMY,X,IDIMX)
      DIMENSION X(1),Y(1)
C
C     TEST OF DIMENSION
      IF (IDIMX-1) 3,3,1
    1 IDIMY=IDIMX-1
      EXPT=0.
      DO 2 I=1,IDIMY
      EXPT=EXPT+1.
    2 Y(I)=X(I+1)*EXPT
      GO TO 4
    3 IDIMY=0
    4 RETURN
      END
