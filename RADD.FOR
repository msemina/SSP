C
C     ..................................................................
C
C        SUBROUTINE RADD
C
C        PURPOSE
C           ADD ROW OF ONE MATRIX TO ROW OF ANOTHER MATRIX
C
C        USAGE
C           CALL RADD(A,IRA,R,IRR,N,M,MS,L)
C
C        DESCRIPTION OF PARAMETERS
C           A   - NAME OF INPUT MATRIX
C           IRA - ROW IN MATRIX A TO BE ADDED TO ROW IRR OF MATRIX R
C           R   - NAME OF OUTPUT MATRIX
C           IRR - ROW IN MATRIX R WHERE SUMMATION IS DEVELOPED
C           N   - NUMBER OF ROWS IN A
C           M   - NUMBER OF COLUMNS IN A AND R
C           MS  - ONE DIGIT NUMBER FOR STORAGE MODE OF MATRIX A
C                  0 - GENERAL
C                  1 - SYMMETRIC
C                  2 - DIAGONAL
C           L   - NUMBER OF ROWS IN R
C
C        REMARKS
C           MATRIX R MUST BE A GENERAL MATRIX
C           MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A UNLESS
C           A IS GENERAL
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           LOC
C
C        METHOD
C           EACH ELEMENT OF ROW IRA OF MATRIX A IS ADDED TO
C           CORRESPONDING ELEMENT OF ROW IRR OF MATRIX R
C
C     ..................................................................
C
      SUBROUTINE RADD(A,IRA,R,IRR,N,M,MS,L)
      DIMENSION A(1),R(1)
C
      IR=IRR-L
      DO 2 J=1,M
      IR=IR+L
C
C        LOCATE INPUT ELEMENT FOR ANY MATRIX STORAGE MODE
C
      CALL LOC(IRA,J,IA,N,M,MS)
C
C        TEST FOR ZERO ELEMENT IN DIAGONAL MATRIX
C
      IF(IA) 1,2,1
C
C        ADD ELEMENTS
C
    1 R(IR)=R(IR)+A(IA)
    2 CONTINUE
      RETURN
      END
