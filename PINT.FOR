C
C     ..................................................................
C
C        SUBROUTINE PINT
C
C        PURPOSE
C           FIND INTEGRAL OF A POLYNOMIAL WITH CONSTANT OF INTEGRATION
C           EQUAL TO ZERO
C
C        USAGE
C           CALL PINT(Y,IDIMY,X,IDIMX)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - VECTOR OF COEFFICIENTS FOR INTEGRAL, ORDERED FROM
C                   SMALLEST TO LARGEST POWER
C           IDIMY - DIMENSION OF Y (EQUAL TO IDIMX+1)
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
C           DIMENSION OF Y IS SET AT DIMENSION OF X PLUS ONE, AND THE
C           CONSTANT TERM IS SET TO ZERO. INTEGRAL IS THEN CALCULATED
C           BY DIVIDING COEFFICIENTS BY THEIR RESPECTIVE EXPONENTS.
C
C     ..................................................................
C
      SUBROUTINE PINT(Y,IDIMY,X,IDIMX)
      DIMENSION X(1),Y(1)
C
      IDIMY=IDIMX+1
      Y(1)=0.
      IF(IDIMX)1,1,2
    1 RETURN
    2 EXPT=1.
      DO 3 I=2,IDIMY
      Y(I)=X(I-1)/EXPT
    3 EXPT=EXPT+1.
      GO TO 1
      END
