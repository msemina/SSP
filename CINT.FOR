C
C     ..................................................................
C
C        SUBROUTINE CINT
C
C        PURPOSE
C           INTERCHANGE TWO COLUMNS OF A MATRIX
C
C        USAGE
C           CALL CINT(A,N,LA,LB)
C
C        DESCRIPTION OF PARAMETERS
C           A  - NAME OF MATRIX
C           N  - NUMBER OF ROWS IN A
C           LA - COLUMN TO BE INTERCHANGED WITH COLUMN LB
C           LB - COLUMN TO BE INTERCHANGED WITH COLUMN LA
C
C        REMARKS
C           MATRIX A MUST BE A GENERAL MATRIX
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           EACH ELEMENT OF COLUMN LA IS INTERCHANGED WITH CORRESPONDING
C           ELEMENT OF COLUMN LB
C
C     ..................................................................
C
      SUBROUTINE CINT(A,N,LA,LB)
      DIMENSION A(1)
C
C        LOCATE STARTING POINT OF BOTH COLUMNS
C
      ILA=N*(LA-1)
      ILB=N*(LB-1)
C
      DO 3 I=1,N
      ILA=ILA+1
      ILB=ILB+1
C
C        INTERCHANGE ELEMENTS
C
      SAVE=A(ILA)
      A(ILA)=A(ILB)
    3 A(ILB)=SAVE
      RETURN
      END
