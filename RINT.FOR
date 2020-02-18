C
C     ..................................................................
C
C        SUBROUTINE RINT
C
C        PURPOSE
C           INTERCHANGE TWO ROWS OF A MATRIX
C
C        USAGE
C           CALL RINT(A,N,M,LA,LB)
C
C        DESCRIPTION OF PARAMETERS
C           A  - NAME OF MATRIX
C           N  - NUMBER OF ROWS IN A
C           M  - NUMBER OF COLUMNS IN A
C           LA - ROW TO BE INTERCHANGED WITH ROW LB
C           LB - ROW TO BE INTERCHANGED WITH ROW LA
C
C        REMARKS
C           MATRIX A MUST BE A GENERAL MATRIX
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EACH ELEMENT OF ROW LA IS INTERCHANGED WITH CORRESPONDING
C           ELEMENT OF ROW LB
C
C     ..................................................................
C
      SUBROUTINE RINT(A,N,M,LA,LB)
      DIMENSION A(1)
C
      LAJ=LA-N
      LBJ=LB-N
      DO 3 J=1,M
C
C        LOCATE ELEMENTS IN BOTH ROWS
C
      LAJ=LAJ+N
      LBJ=LBJ+N
C
C        INTERCHANGE ELEMENTS
C
      SAVE=A(LAJ)
      A(LAJ)=A(LBJ)
    3 A(LBJ)=SAVE
      RETURN
      END
