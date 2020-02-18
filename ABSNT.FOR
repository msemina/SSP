C
C     ..................................................................
C
C        SUBROUTINE ABSNT
C
C        PURPOSE
C           TEST MISSING OR ZERO VALUES FOR EACH OBSERVATION IN
C           MATRIX A.
C
C        USAGE
C           CALL ABSNT (A,S,NO,NV)
C
C        DESCRIPTION OF PARAMETERS
C           A  - OBSERVATION MATRIX, NO BY NV
C           S  - OUTPUT VECTOR OF LENGTH NO INDICATING THE FOLLOWING
C                CODES FOR EACH OBSERVATION.
C                1  THERE IS NOT A MISSING OR ZERO VALUE.
C                0  AT LEAST ONE VALUE IS MISSING OR ZERO.
C           NO - NUMBER OF OBSERVATIONS. NO MUST BE > OR = TO 1.
C           NV - NUMBER OF VARIABLES FOR EACH OBSERVATION. NV MUST BE
C                GREATER THAN OR EQUAL TO 1.
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           A TEST IS MADE FOR EACH ROW (OBSERVATION) OF THE MATRIX A.
C           IF THERE IS NOT A MISSING OR ZERO VALUE, 1 IS PLACED IN
C           S(J). IF AT LEAST ONE VALUE IS MISSING OR ZERO, 0 IS PLACED
C           IN S(J).
C
C     ..................................................................
C
      SUBROUTINE ABSNT(A,S,NO,NV)
      DIMENSION A(1),S(1)
C
      DO 20 J=1,NO
      IJ=J-NO
      S(J)=1.0
      DO 10 I=1,NV
      IJ=IJ+NO
      IF(A(IJ)) 10,5,10
    5 S(J)=0
      GO TO 20
   10 CONTINUE
   20 CONTINUE
      RETURN
      END
