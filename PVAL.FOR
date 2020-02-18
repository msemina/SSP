C
C     ..................................................................
C
C        SUBROUTINE PVAL
C
C        PURPOSE
C           EVALUATE A POLYNOMIAL FOR A GIVEN VALUE OF THE VARIABLE
C
C        USAGE
C           CALL PVAL(RES,ARG,X,IDIMX)
C
C        DESCRIPTION OF PARAMETERS
C           RES    - RESULTANT VALUE OF POLYNOMIAL
C           ARG    - GIVEN VALUE OF THE VARIABLE
C           X      - VECTOR OF COEFFICIENTS, ORDERED FROM SMALLEST TO
C                    LARGEST POWER
C           IDIMX  - DIMENSION OF X
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EVALUATION IS DONE BY MEANS OF NESTED MULTIPLICATION
C
C     ..................................................................
C
      SUBROUTINE PVAL(RES,ARG,X,IDIMX)
      DIMENSION X(1)
C
      RES=0.
      J=IDIMX
    1 IF(J)3,3,2
    2 RES=RES*ARG+X(J)
      J=J-1
      GO TO 1
    3 RETURN
      END
