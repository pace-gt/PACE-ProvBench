      MODULE GENERAL_DATA

      IMPLICIT NONE

      INCLUDE 'mpif.h'

      INTEGER, PARAMETER :: NSPECI = 1, ND = 5 ! + 2 + NSPECI

      INTEGER, PARAMETER :: NTRACE = 10
      INTEGER, PARAMETER :: ICHEM = 0, ISGSK = 0, NSCHEME = 4

      LOGICAL, PARAMETER :: IPERIODIC = .TRUE.,
     >                      JPERIODIC = .FALSE.

cmm TML parameters (PERT3D and COEF_AMP set in input.data)
      INTEGER :: PERT3D ! 0 for 2D perturb, 1 for 3D pertub
      LOGICAL :: KPERIODIC ! will be set to true if PERT3D = 1
      REAL (KIND = 8) :: COEF_AMP
      REAL (KIND = 8) :: TAU_LIMIT

      LOGICAL, PARAMETER :: VISCOUS = .TRUE.

      INTEGER,PARAMETER :: INFLOW = 0, IOUTFLOW = 0

      INTEGER, PARAMETER :: NRHO  = 1,
     >                      NRHOU = 2,
     >                      NRHOV = 3,
     >                      NRHOW = 4,
     >                      NRHOE = 5
      INTEGER :: NRHOK, NRHOC, NEQU
 
      INTEGER :: IBC0, IBC1, JBC0, JBC1, KBC0, KBC1

      INTEGER :: IAM, ICMAX, JCMAX, KCMAX, NPROCS,
     >           ISTART, IEND, JSTART, JEND, KSTART, KEND,
     >           NPX, NPY, NPZ, NCX, NCY, NCZ,
     >           ABOVE, BELOW, RIGHT, LEFT, INWARD, OUTWARD,
     >           NEIGHBOR(26)
      INTEGER :: EAST, WEST, NORTH, SOUTH, IN, OUT

      REAL (KIND = 8) ::
     >          RM00,P00,T00,AM0,U00,CREF,PREF,RHOREF,TREF,UREF,
     >          PEXIT, G00, R00, RMUREF, RNUREF, CMU, RSTAG,ASTR2,
     >          EKREF, CNUK, CEPSK, CPROD, CFL,RELNO, TSTND, ALREF,
     >          URMSFAC, SLREF, EFORM, TPROD, XLEN, YLEN, ZLEN,
     >          TIME, DSEED, DT

      INTEGER :: NADV, NEND, IRSTRT, NSTART, IRSTFLAG, NPRNT, IFLPRNT,
     >           IPRINT, ITPTS, JTPTS, KTPTS, ITRACE, NSTAT, ITIME,
     >           IFLCNT, NFILE, NUNIT, ISTATCNT,
     >           IADD, JADD, KADD,
     >           IBDD, ICDD, JBDD, JCDD, KBDD, KCDD,
     >           IMAX, JMAX, KMAX, IDYN, N, M, NM1

      INTEGER, DIMENSION (50) :: IGLOC,JGLOC,KGLOC

      REAL (KIND = 8),  DIMENSION (NSPECI) :: CPK(NSPECI),CVK(NSPECI),
     >          CONC0(NSPECI), RLEWS(NSPECI), RGK(NSPECI), RMWT(NSPECI),
     >          SCI(NSPECI), SCTI(NSPECI), HFORM(NSPECI), HFK(NSPECI)

      CHARACTER (LEN = 10) SPNAME(NSPECI)

      CHARACTER (LEN = 60) PREFIX
      INTEGER :: LENGTH

      REAL (KIND = 8), PARAMETER ::
     >           R0         =      0.0D0,
     1           R1         =      1.0D0,
     1           R2         =      2.0D0,
     1           R3         =      3.0D0,
     1           R4         =      4.0D0,
     1           R5         =      5.0D0,
     1           R6         =      6.0D0,
     1           R7         =      7.0D0,
     1           R8         =      8.0D0,
     1           R9         =      9.0D0,
     1           R10        =      1.0D1,
     1           R11        =      1.1D1,
     1           R12        =      1.2D1,
     1           R14        =      1.4D1,
     1           R15        =      1.5D1,
     1           R16        =      1.6D1,
     1           R17        =      1.7D1,
     1           R18        =      1.8D1,
     1           R19        =      1.9D1,
     1           R20        =      2.0D1

      REAL (KIND = 8), PARAMETER ::
     1           R2I        =      R1/R2,
     1           R3I        =      R1/R3,
     1           R4I        =      R1/R4,
     1           R5I        =      R1/R5,
     1           R6I        =      R1/R6,
     1           R7I        =      R1/R7,
     1           R8I        =      R1/R8,
     1           R9I        =      R1/R9,
     1           R10I       =      R1/R10,
     1           R11I       =      R1/R11,
     1           R12I       =      R1/R12,
     1           R14I       =      R1/R14,
     1           R15I       =      R1/R15,
     1           R16I       =      R1/R16,
     1           R17I       =      R1/R17,
     1           R18I       =      R1/R18,
     1           R19I       =      R1/R19,
     1           R20I       =      R1/R20

      REAL (KIND = 8), PARAMETER ::
     1           R2D3       =      R2/R3,
     1           R4D3       =      R4/R3,
     1           R3D4       =      R3/R4,
     1           R3D2       =      R3/R2,
     1           R5D3       =      R5/R3,
     1           R5D4       =      R5/R4,
     1           R2D5       =      R2/R5,
     1           R3D5       =      R3/R5,
     1           R4D5       =      R4/R5,
     1           R6D5       =      R6/R5,
     1           R5D6       =      R5/R6,
     1           R7D6       =      R7/R6,
     1           R2D7       =      R2/R7,
     1           R3D7       =      R3/R7,
     1           R4D7       =      R4/R7,
     1           R5D7       =      R5/R7,
     1           R6D7       =      R6/R7,
     1           R8D7       =      R8/R7,
     1           R3D8       =      R3/R8,
     1           R5D8       =      R5/R8,
     1           R7D8       =      R7/R8,
     1           R9D8       =      R9/R8,
     1           R2D9       =      R2/R9,
     1           R4D9       =      R4/R9,
     1           R5D9       =      R5/R9,
     1           R7D9       =      R7/R9,
     1           R8D9       =      R8/R9,
     1           R10D9      =      R10/R9,
     1           R3D10      =      R3/R10,
     1           R7D10      =      R7/R10,
     1           R9D10      =      R9/R10,
     1           R11D10     =      R11/R10,
     1           R5D12      =      R5/R12,
     1           R7D12      =      R7/R12,
     1           R11D12     =      R11/R12,
     1           R3D16      =      R3/R16

      REAL (KIND = 8), PARAMETER ::
     >           RUNIV  =  8.31451D3,
     >           PR     =  0.7200D0,
     >           PRT    =  0.9000D0,
     >           PSIG   =  0.1500D0,
     >           SMALL  =  1.0000D-6,
     >           ZERO   =  0.0000D0,
     >           HALF   =  0.5000D0,
     >           QUART  =  0.2500D0,
     >           ONE    =  1.0000D0,
     >           TWO    =  2.0000D0

      !# CONSTANT AIR PROPERTIES
      REAL (KIND = 8), PARAMETER ::
     >                     WTAIR = 28.85D0,
     >                     GMAIR = 1.4D0,
     >                     RGAIR = RUNIV / 28.85D0,
     >                     CPAIR = RGAIR * GMAIR / (GMAIR - 1.0D0),
     >                     CVAIR = CPAIR - RGAIR

      REAL (KIND = 8) :: RMDOT_REF, FORCEX

      END MODULE GENERAL_DATA
!-----------------------------------------------------------------------
      MODULE VARIABLE_DATA

      IMPLICIT NONE

      REAL (KIND = 8), DIMENSION(:,:), ALLOCATABLE ::
     >         UIN, VIN, WIN, EKIN, TIN, TOLD, URMS, VRMS, WRMS

      REAL (KIND = 8), DIMENSION(:,:,:), ALLOCATABLE ::
     >         X, Y, Z, VOL, 
     >         DTV,
     >         SIX, SIY, SIZ, SJX, SJY,SJZ, SKX, SKY, SKZ, SIJKSQ,
     >         U, V, W, T, P, H,
     >         UAV, VAV, WAV, TAV, PAV, HAV,
     >         GRADG, YKIN, BOUND

      REAL (KIND = 8), DIMENSION(:,:,:,:), ALLOCATABLE ::
     >         T11, T12, T13, T21, T22, T23, T31, T32, T33, DS1, DS,
     >         HF, QAV, DQ

      REAL (KIND = 8), DIMENSION(:,:,:,:,:), ALLOCATABLE ::
     >         Q

      END MODULE VARIABLE_DATA
