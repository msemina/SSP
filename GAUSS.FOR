C
C     ..................................................................
C
C        SUBROUTINE GAUSS
C
C        PURPOSE
C           COMPUTES A NORMALLY DISTRIBUTED RANDOM NUMBER WITH A GIVEN
C           MEAN AND STANDARD DEVIATION
C
C        USAGE
C           CALL GAUSS(IX,S,AM,V)
C
C        DESCRIPTION OF PARAMETERS
C           IX -IX MUST CONTAIN AN ODD INTEGER NUMBER WITH NINE OR
C               LESS DIGITS ON THE FIRST ENTRY TO GAUSS. THEREAFTER
C               IT WILL CONTAIN A UNIFORMLY DISTRIBUTED INTEGER RANDOM
C               NUMBER GENERATED BY THE SUBROUTINE FOR USE ON THE NEXT
C               ENTRY TO THE SUBROUTINE.
C           S  -THE DESIRED STANDARD DEVIATION OF THE NORMAL
C               DISTRIBUTION.
C           AM -THE DESIRED MEAN OF THE NORMAL DISTRIBUTION
C           V  -THE VALUE OF THE COMPUTED NORMAL RANDOM VARIABLE
C
C        REMARKS
C           THIS SUBROUTINE USES RANDU WHICH IS MACHINE SPECIFIC
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           RANDU
C
C        METHOD
C           USES 12 UNIFORM RANDOM NUMBERS TO COMPUTE NORMAL RANDOM
C           NUMBERS BY CENTRAL LIMIT THEOREM. THE RESULT IS THEN
C           ADJUSTED TO MATCH THE GIVEN MEAN AND STANDARD DEVIATION.
C           THE UNIFORM RANDOM NUMBERS COMPUTED WITHIN THE SUBROUTINE
C           ARE FOUND BY THE POWER RESIDUE METHOD.
C
C     ..................................................................
C
      SUBROUTINE GAUSS(IX,S,AM,V)
      A=0.0
      DO 50 I=1,12
      CALL RANDU(IX,IY,Y)
      IX=IY
   50 A=A+Y
      V=(A-6.0)*S+AM
      RETURN
      END
