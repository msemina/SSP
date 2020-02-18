# (SSP) - IBM Scientific Subroutine Package
The Scientific Subroutine Package (SSP) is a collection of FORTRAN subroutines divided, for the sake of presentation, into three groups: statistics, matrix manipulation, and other mathematics. It is a collection of input/output-free computational building blocks that can be combined with a user's input, output, or computational routines to meet his needs. The package can be applied to the solution of many problems in industry, science, and engineering. 

Below are listed the subroutines of SSP/1130, grouped into related functional areas. In the case of six statistical entries (Multiple Linear Regression to Factor Analysis) the abstract gives the sequence of several SSP subroutines needed to perform the statistical function. 

# STATISTICS 

## Data Screening

  TALLY — totals, means, standard deviations, minimums, and maximums 
  BOUND — selection of observations within bounds 
  SUBST — subset selection from observation matrix 
  ABSNT — detection of missing data 
  SUBMX — build subset matrix 
  
## Elementary Statistics 

  MOMEN — first four moments 
  TTSTT — tests on population means 

## Correlation 

  CORRE — means, standard deviations, and correlations 
  
## Multiple Linear Regression 

  Abstract (CORRE, ORDER, MINV, MULTR in sequence) 
  ORDER — rearrangement of intercorrelations MULTR- -multiple regression and correlation 

## Polynomial Regression 

  Abstract (GDATA, ORDER, MINV, MULTR in sequence) 
  GDATA- -data generation 

## Canonical Correlation 

  Abstract (CORRE, CANOR, MINV, NROOT, EIGEN in sequence) 
  CANOR — canonical correlation 
  NROOT — eigenvalues and eigenvectors of a special nonsymmetric matrix 

## Analysis of Variance 

  Abstract (AVDAT, AVCAL, MEANQ in sequence) 
  AVDAT — data storage allocation 
  AVCAL — 2 and A operation 
  MEANQ — mean square operation 

## Discriminant Analysis 

  Abstract (DMATX, MINV, DISCR in sequence) 
  DMATX — means and dispersion matrix 
  DISCR — discriminant functions 

## Factor Analysis 

  Abstract (CORRE, EIGEN, TRACE, LOAD, VARMX in sequence) 
  TRACE — cumulative percentage of eigenvalues 
  LOAD — factor loading 
  VARMX — varimax rotation 

## Time Series 

  AUTO — autocovariances 
  CROSS — crosscovariances 
  SMO— application of filter coefficients (weights) 
  EXSMO — triple exponential smoothing 
  
## Nonparametric Statistics 

CHISQ — x test for a contingency table 
UTEST— Mann- Whitney U-test 
TWOAV — Friedman two-way analysis of variance 
QTEST— Cochran Q-test 
SRANK — Spearman rank correlation 
KRANK — Kendall rank correlation 
WTEST — Kendall coefficient of concordance 
RANK — rank observations 
TIE — calculation of ties in ranked observations 

## Random Number Generators 

RANDU — uniform random numbers 
GAUSS — normal random numbers 


# MATREK MANIPULATION 

MINV — Matrix inversion 
EIGEN — eigenvalues and eigenvectors of a real, symmetric matrix 
SIMQ — solution of simultaneous linear, algebraic equations 
GMADD — add two general matrices 
GMSUB — subtract two general matrices 
GMPRD— product of two general matrices 
GMTRA--transpose of a general matrix 
GTPRD — transpose product of two general matrices 
MADD — add two matrices 
MSUB — subtract two matrices 
MPRD — matrix product (row into column) 
MTRA — transpose a matrix 
TPRD — transpose product 
MATA — transpose product of matrix by itself 
SADD — add scalar to matrix 
SSUB — subtract scalar from a matrix 
SMPY — matrix multiplied by a scalar 
SDIV — matrix divided by a scalar 
RADD — add row of one matrix to row of another matrix 
CADD — add column of one matrix to column of another matrix 
SRMA — scalar multiply row and add to another row 
SCMA — scalar multiply column and add to another column 
RINT — interchange two rows 
CINT — interchange two columns 
RSUM — sum the rows of a matrix 
CSUM — sum the columns of a matrix 
RTAB — tabulate the rows of a matrix 
CTAB — tabulate the columns of a matrix 
RSRT — sort matrix rows 
CSRT — sort matrix columns 
RCUT-partition row-wise 
CCUT — partition column-wise 
RTIE — adjoin two matrices row-wise 
CTIE — adjoin two matrices column-wise 
MCPY — matrix copy 
XCPY — copy submatrix from given matrix 
RCPY — copy row of matrix into vector 
CCPY — copy column of matrix into vector 
DC PY— copy diagonal of matrix into vector 
SCLA — matrix clear and add scalar 
DC LA — raplace diagonal with scalar 
MSTR — storage conversion 
MFUN — matrix transformation by a function 
RECP — reciprocal function for MFUN 
LOC— location in compressed-stored matrix 
ARRAY— vector storage— double dimensioned storage conversion 

# OTHER MATHEMATICAL AREAS 

## Integration 

QUADR — integral of tabulated function 
SMPSN — integral of given function by Simpson's rule 
RK1 — integral of first-order differential equation by Runge-Kutta method 
RK2 —tabulated integral of first-order differential equation by Runge-Kutta method 
RK3— tabulated integral of a system of six first-order differential equations by Runge-Kutta method 

## Fourier Analysis 

FORIF— Fourier analysis of a given function 
FORIT— Fourier analysis of a tabulated function 

## Special Operations and Mathematical Functions 

GAMMA- -gamma function 
LEGEN— Legendre polynomial 
BESJ— J Bessel function 
BESY— Y Bessel function 
BESI— I Bessel function 
BESK— K Bessel function 
CELI1— elliptic integral of the first kind 
CELI2— elliptic integral of the second kind 

## Exponential integral 

SICI— sine cosine integral 
CS— Fresnel integrals 

## Roots of Nonlinear Equations 

RTWIT — refine estimate of root by Wegstein's iteration 
RTMIT— determine root within a range by Mueller's iteration 
RTNIT— refine estimate of root by Newton's iteration 

## Roots of Polynomial 

POLRT— real and complex roots of a real polynomial 

## Polynomial Operations 

  PADD — add two polynomials 
  PADDM— multiply polynomial by constant and add to another polynomial 
  PCLA— replace one polynomial by another 
  PSUB— subtract one polynomial from another 
  PMPY— multiply two polynomials 
  PDIV — divide one polynomial by another 
  PQSD- -quadratic synthetic division of a polynomial 
  PVAL — value of a polynomial 
  PVSUB— substitute variable of polynomial by another polynomial 
  PCLD — complete linear synthetic division 
  PILD — evaluate polynomial and its first derivative 
  PDER — derivative of a polynomial 
  PINT— integral of a polynomial 
  PGCD — greatest common divisor of two polynomials 

## Thanks Robert H. (Bob) Todd
