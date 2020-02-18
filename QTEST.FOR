C
C     ..................................................................
C
C        SUBROUTINE QTEST
C
C        PURPOSE
C           TEST WHETHER THREE OR MORE MATCHED GROUPS OF DICHOTOMOUS
C           DATA DIFFER SIGNIFICANTLY BY THE COCHRAN Q-TEST
C
C        USAGE
C           CALL QTEST(A,N,M,Q,NDF)
C
C        DESCRIPTION OF PARAMETERS
C           A   - INPUT MATRIX, N BY M, OF DICHOTOMOUS DATA (0 AND 1)
C           N   - NUMBER OF SETS IN EACH GROUP
C           M   - NUMBER OF GROUPS
C           Q   - COCHRAN Q STATISTIC (OUTPUT)
C           NDF - NUMBER OF DEGREES OF FREEDOM (OUTPUT)
C
C        REMARKS
C           M MUST BE THREE OR GREATER
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DESCRIBED IN S. SIEGEL, 'NONPARAMETRIC STATISTICS FOR THE
C           BEHAVIORAL SCIENCES', MCGRAW-HILL, NEW YORK, 1956,
C           CHAPTER 7
C
C     ..................................................................
C
      SUBROUTINE QTEST(A,N,M,Q,NDF)
      DIMENSION A(1)
C
C        COMPUTE SUM OF SQUARES OF ROW TOTALS, RSQ, AND GRAND TOTAL OF
C        ALL ELEMENTS, GD
C
      RSQ=0.0
      GD=0.0
      DO 20 I=1,N
      TR=0.0
      IJ=I-N
      DO 10 J=1,M
      IJ=IJ+N
   10 TR=TR+A(IJ)
      GD=GD+TR
   20 RSQ=RSQ+TR*TR
C
C        COMPUTE SUM OF SQUARES OF COLUMN TOTALS, CSQ
C
      CSQ=0.0
      IJ=0
      DO 40 J=1,M
      TC=0.0
      DO 30 I=1,N
      IJ=IJ+1
   30 TC=TC+A(IJ)
   40 CSQ=CSQ+TC*TC
C
C        COMPUTE COCHRAN Q TEST VALUE
C
      FM=M
      Q=(FM-1.0)*(FM*CSQ-GD*GD)/(FM*GD-RSQ)
C
C        FIND DEGREES OF FREEDOM
C
      NDF=M-1
      RETURN
      END
