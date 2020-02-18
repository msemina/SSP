C
C     ..................................................................
C
C        SUBROUTINE PCLA
C
C        PURPOSE
C           MOVE POLYNOMIAL X TO Y
C
C        USAGE
C           CALL PCLA(Y,IDIMY,X,IDIMX)
C
C        DESCRIPTION OF PARAMETERS
C           Y     - VECTOR OF RESULTANT COEFFICIENTS, ORDERED FROM
C                   SMALLEST TO LARGEST POWER
C           IDIMY - DIMENSION OF Y
C           X     - VECTOR OF COEFFICIENTS FOR POLYNOMIAL, ORDERED
C                   FROM SMALLEST TO LARGEST POWER
C           IDIMX - DIMENSION OF X
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           IDIMY IS REPLACED BY IDIMX AND VECTOR X IS MOVED TO Y
C
C     ..................................................................
C
      SUBROUTINE PCLA (Y,IDIMY,X,IDIMX)
      DIMENSION X(1),Y(1)
C
      IDIMY=IDIMX
      IF(IDIMX) 30,30,10
   10 DO 20 I=1,IDIMX
   20 Y(I)=X(I)
   30 RETURN
      END
