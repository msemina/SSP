C
C     ..................................................................
C
C        SUBROUTINE SRMA
C
C        PURPOSE
C           MULTIPLY ROW OF MATRIX BY A SCALAR AND ADD TO ANOTHER ROW
C           OF THE SAME MATRIX
C
C        USAGE
C           CALL SRMA(A,C,N,M,LA,LB)
C
C        DESCRIPTION OF PARAMETERS
C           A  - NAME OF MATRIX
C           C  - SCALAR
C           N  - NUMBER OF ROWS IN A
C           M  - NUMBER OF COLUMNS IN A
C           LA - ROW IN A TO BE MULTIPLIED BY SCALAR
C           LB - ROW IN A TO WHICH PRODUCT IS ADDED
C                IF 0 IS SPECIFIED, PRODUCT REPLACES ELEMENTS IN ROW LA
C
C        REMARKS
C           MATRIX A MUST BE A GENERAL MATRIX
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EACH ELEMENT OF ROW LA IS MULTIPLIED BY SCALAR C AND THE
C           PRODUCT IS ADDED TO THE CORRESPONDING ELEMENT OF ROW LB.
C           ROW LA REMAINS UNAFFECTED BY THE OPERATION.
C           IF PARAMETER LB CONTAINS ZERO, MULTIPLICATION BY THE SCALAR
C           IS PERFORMED AND THE PRODUCT REPLACES ELEMENTS IN ROW LA.
C
C     ..................................................................
C
      SUBROUTINE SRMA(A,C,N,M,LA,LB)
      DIMENSION A(1)
C
      LAJ=LA-N
      LBJ=LB-N
      DO 3 J=1,M
C
C        LOCATE ELEMENT IN BOTH ROWS
C
      LAJ=LAJ+N
      LBJ=LBJ+N
C
C        CHECK LB FOR ZERO
C
      IF(LB) 1,2,1
C
C        IF NOT, MULTIPLY BY CONSTANT AND ADD TO OTHER ROW
C
    1 A(LBJ)=A(LAJ)*C+A(LBJ)
      GO TO 3
C
C        OTHERWISE, MULTIPLY ROW BY CONSTANT
C
    2 A(LAJ)=A(LAJ)*C
    3 CONTINUE
      RETURN
      END
