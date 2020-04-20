      PROGRAM LES3D 

      USE GENERAL_DATA, ONLY: PREFIX, NUNIT, IMAX, JMAX, KMAX,
     >                        PERT3D, COEF_AMP, XLEN, YLEN, ZLEN,
     >                        UREF, TREF, PREF, PEXIT, TSTND,
     >                        SLREF, EFORM, TPROD, IAM, TAU_LIMIT,
     >                        IRSTRT, NEND, NPRNT, IFLPRNT, NSTAT,
     >                        RELNO, ALREF, RNUREF, MPI_COMM_WORLD,
     >                        NPROCS, MPI_MAX, MPI_DOUBLE_PRECISION,
     >                        IBDD, ICDD, JBDD, JCDD, KBDD, KCDD, IBDD,
     >                        ICDD, JBDD, JCDD, KBDD, KCDD,
     >                        ISTART, INFLOW, NM1, KPERIODIC,
     >                        IADD, JADD, KADD, ITIME, IDYN, ISTATCNT,
     >                        ICMAX, JCMAX, KCMAX, LENGTH, MPI_WTIME,
     >                        RHOREF, CFL, DT, NEQU, ITRACE, TIME,
     >                        IFLCNT, IRSTFLAG, NADV, NSTART,
     >                        NSCHEME, M, N, NRHOE
      USE VARIABLE_DATA, ONLY: Q, BOUND, T

      IMPLICIT NONE

      INTEGER :: I, J, K, IERR, NP, IWORK, NSTATS

      REAL (KIND = 8) :: BUFS(1), BUFR(1) 
      REAL (KIND = 8), ALLOCATABLE, DIMENSION (:) :: WORK

      REAL (KIND = 8) :: DELM, TAU
      REAL (KIND = 8) :: TMAX, RMACH, DX
      REAL (KIND = 8) :: TSTART, TEND, TTOTAL, TOTAL_TIME
      INTEGER :: NSECS, NMINS, NHOUR

      CALL MPI_INIT(IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NPROCS, IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, IAM, IERR)

      DO NP = 0, NPROCS-1
         IF ( IAM .EQ. NP ) THEN
            OPEN(10, FILE = 'input.data', FORM = 'FORMATTED')
            READ(10,*)
            READ(10,*) PREFIX, NUNIT
            READ(10,*)
            READ(10,*) IMAX, JMAX, KMAX
            READ(10,*)
            READ(10,*) IRSTRT, NEND, NPRNT, IFLPRNT, NSTAT, ITIME
cmm ITIME now represents the number of times an output 
cmm (screen, momentum thickness computation) will be produced.
cmm The frequency is recomputed in TMSTEP.
cmm The exception is if ITIME is 0, then it is assumed the run
cmm is a timing run, and thus there will be no output
cmm and NEND is not recomputed.
            READ(10,*)
            READ(10,*) PERT3D, COEF_AMP
cmm Compute the parameters for the given TML
cmm so that it automatically runs for the correct duration
            TAU_LIMIT = 80.0D0 - LOG10(COEF_AMP) * 40.0D0
            IF (PERT3D == 1) THEN
               KPERIODIC = .TRUE.
            ELSE
               KPERIODIC = .FALSE.
            ENDIF
cmm Number of timesteps and write frequency are set in TMSTEP
            READ(10,*)
            READ(10,*) XLEN, YLEN, ZLEN
            READ(10,*)
            READ(10,*) SLREF, EFORM, TPROD
            READ(10,*)
            READ(10,*) UREF, TREF, PREF, PEXIT, TSTND,
     >                                    RELNO, ALREF, RNUREF
            CLOSE(10)
         ENDIF
         CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      ENDDO

      DO LENGTH = 60, 1, -1
         IF ( PREFIX(LENGTH:LENGTH) .NE. ' ' ) GOTO 10
      ENDDO
      CALL EJECT ( 'PFS FILE PREFIX IS TOO LONG')
10    CONTINUE

      DX = XLEN / DBLE(IMAX - 1)
      ZLEN = DX * DBLE(KMAX - 1)

      CALL GRIDMAP

      !# SET UP THE POINTERS TO THE CONSERVATIVE VARIABLES

      NEQU = NRHOE

      CALL ALLOC ()

      CALL GRID

      CALL SETIV

      CALL FLOWIO()

      CALL MPI_BARRIER ( MPI_COMM_WORLD, IERR )

      M = 1

      IF ( NSCHEME .EQ. 2 ) THEN
         CALL PARALLEL ( 2000 + M )
         CFL = 0.50
      ELSE
         CALL PARALLEL ( 2000 + M )
         CFL = 0.25
      ENDIF

      IF ( IAM .EQ. 0 ) THEN
         WRITE(6,'(//," 1. GEOMETRY"/)')
         WRITE(6,'(/9X,"IMAX = ",I3,
     >                 3X,"JMAX = ",I3,3X,"KMAX = ",I3)')
     >                        IMAX, JMAX, KMAX

         WRITE(6,'(/9X,"XLEN = ",F6.3,
     >              3X,"YLEN = ",F6.3,
     >              3X,"ZLEN = ",F6.3)') XLEN, YLEN, ZLEN

         WRITE(6,'(//," 2. FLOW PROPERTIES"/)')
         WRITE(6,'(9X,A,F13.5)')"UREF         = ", UREF
         WRITE(6,'(9X,A,F13.5)')"TREF         = ", TREF
         WRITE(6,'(9X,A,F13.5)')"PREF         = ", PREF
         WRITE(6,'(9X,A,F13.5)')"RHOREF       = ", RHOREF
         WRITE(6,'(9X,A,F13.5)')"REYNOLDS NUM = ", RELNO

         WRITE(6,'(//," 3. NUMERICAL SCHEME"/)')
         WRITE(6,'(9X,"SPATIAL ORDER   = ",$)')
         IF(NSCHEME .EQ. 4) THEN
            WRITE(6,'(A)') "FOURTH ORDER"
         ELSE
            WRITE(6,'(A)') "SECOND ORDER"
         ENDIF
         WRITE(6,'(9X,"CFL             = ",F5.3)') CFL
         WRITE(6,'(9X,"LES MODELING    = ",$)')
         WRITE(6,'(A)') "NONE: DNS"
         WRITE(6,'(//," 4. SPECIES PROPERTIES"/)')
         WRITE(6,'(9X,"CHEMISTRY MODEL = ",$)')
         WRITE(6,'(A)')"NO CHEMISTRY: AIR FLOW"
         WRITE(6,*)
      ENDIF

      NSTART   = 1
      NADV     = 0
      M        = 1
      IRSTFLAG = 0
      IFLCNT   = 0
      ISTATCNT = 0
      ITRACE   = 0
      TIME     = 0.0D0
      DT       = 0.0D0

      CALL MPI_BARRIER ( MPI_COMM_WORLD, IERR )

      N = 0
      IF ( NSCHEME .EQ. 2 ) THEN
         CALL PARALLEL ( 2000 + N )
      ELSE
         CALL PARALLEL ( 2000 + N )
      ENDIF

      CALL SETBC()

      IF ( NSTAT .GT. 0 ) THEN
         NSTATS = 10
         IWORK = (ICMAX+6) * (JCMAX+6) * (KCMAX+6) * NSTATS + 1
         ALLOCATE ( WORK(IWORK) )
         WORK = 0.0D0
      ENDIF 

      TAU  = 0.0D+00
      NADV = NSTART

      IF ( IAM .EQ. 0 ) THEN
         OPEN ( 50, FILE = 'tml.dat', FORM = 'FORMATTED' )
      ENDIF

      TSTART = MPI_WTIME()

      CALL TMSTEP ( TMAX, RMACH )

      DO WHILE ( NADV .LE. NEND .AND. TAU .LT. TAU_LIMIT )
   
        IF ( MOD ( NADV, ITIME ) .EQ. 0 .AND. NADV .GT. NSTART)
     >                             CALL TMSTEP ( TMAX, RMACH )

        IF (MOD (NADV,ITIME) == 0 .OR. NADV == 1) THEN
           CALL ANALYSIS ( TAU, DELM )
        ENDIF
        IF ( IAM .EQ. 0 .AND. MOD ( NADV, ITIME ) .EQ. 0 ) THEN
           WRITE(6,1000) NADV, DT, TIME, TAU, DELM
           WRITE(50,'(2(F10.5,1X))') TAU, DELM
        ENDIF
1000    FORMAT(' NADV = ', I6,
     >         ' DT   = ', E10.4,
     >         ' TIME = ', F8.5,
     >         ' TAU  = ', F8.4,
     >         ' DELM = ', F8.4)


        IF ( IDYN .GT. 0 ) THEN
           CALL EJECT ( 'LDKM, PLEASE RECOMPILE WITH -DLDKM!' )
        ENDIF

        IADD = MOD ( NADV, 2 )
        JADD = MOD ( (NADV + IADD) / 2, 2 )
        KADD = MOD ( (2 * JADD + IADD + NADV) / 4, 2 )

        IF ( IADD .EQ. 1 ) THEN
           IBDD = 1
           ICDD = 0
        ELSE
           IBDD = -1
           ICDD = -2
        END IF

        IF ( JADD .EQ. 1 ) THEN
           JBDD = 1
           JCDD = 0
        ELSE
           JBDD = -1
           JCDD = -2
        END IF

        IF ( KADD .EQ. 1 ) THEN
           KBDD = 1
           KCDD = 0
        ELSE
           KBDD = -1
           KCDD = -2
        END IF

! Basic Equation we are solving here: dQ/dt = - (1/V) Sum(F \dot A) + S
! where Q = conserved variable, V = Cell Volume, F = flux vector,
!              A = cell face area and S = volume source term.

        DO N = 1, 2
           M = 3 - N
           NM1 = N - 1

! Compute all Fluxes (F) for Sum(F \dot A)
           CALL FLUXI()

           CALL FLUXJ()

           IF ( KMAX .GT. 2 ) CALL FLUXK()

! Done with the fluxes

! Solve Finite Difference (in time) using above equation:
! Q(N+1) = Q(N) - dt [ (1/V) Sum(F \dot A) - S ]

           CALL UPDATE()

! GO AHEAD AND PUT THE BOUND() DATA INTO THE VARIABLES RHO, RHOU, RHOV, ETC...
! AND LET THE BASIC EXCHANGE METHOD DO ITS THANG.

           IF ( ISTART .EQ. 1 .AND. INFLOW .EQ. 1 ) THEN
              I = 0
              DO K = 1, KCMAX
                 DO J = 1, JCMAX
                    Q(I,J,K,1,M) = BOUND(J,K,1)
                    Q(I,J,K,2,M) = BOUND(J,K,1) * BOUND(J,K,2)
                    Q(I,J,K,3,M) = BOUND(J,K,1) * BOUND(J,K,3)
                    Q(I,J,K,4,M) = BOUND(J,K,1) * BOUND(J,K,4)
                    T(I,J,K)     = BOUND(J,K,5)
                 ENDDO
              ENDDO
           ENDIF

! Exchange boundary information
           IF ( NSCHEME .EQ. 2 ) THEN
              CALL PARALLEL(2000 + N)
           ELSE
              CALL PARALLEL(2000 + N)
           ENDIF

! Set boundary conditions for next time-step
           CALL SETBC()

           IADD = MOD ( IADD + 1, 2 )
           JADD = MOD ( JADD + 1, 2 )
           KADD = MOD ( KADD + 1, 2 )

           IF ( IADD .EQ. 1 ) THEN
              IBDD = 1
              ICDD = 0
           ELSE
              IBDD = -1
              ICDD = -2
           ENDIF

           IF ( JADD .EQ. 1 ) THEN
              JBDD = 1
              JCDD = 0
           ELSE
              JBDD = -1
              JCDD = -2
           ENDIF

           IF ( KADD .EQ. 1 ) THEN
              KBDD = 1
              KCDD = 0
           ELSE
              KBDD = -1
              KCDD = -2
           ENDIF

        ENDDO

CFGCFG

!        IF ( MOD ( NADV, IFLPRNT ) == 0 ) THEN
!           IFLCNT = IFLCNT + 1
!           CALL FLOWIO()
!        ENDIF

CFGCFG

         NADV = NADV + 1
         TIME = TIME + DT

      ENDDO
 
      IF ( IAM .EQ. 0 ) THEN
         CLOSE ( 50 )
      ENDIF

      TEND   = MPI_WTIME()
      TTOTAL = TEND - TSTART

      CALL MPI_REDUCE ( TTOTAL, TOTAL_TIME, 1, MPI_DOUBLE_PRECISION,
     >                MPI_MAX, 0, MPI_COMM_WORLD, IERR )

      IF ( IAM .EQ. 0 ) THEN
         NSECS = INT(TOTAL_TIME)
         NHOUR = NSECS / 3600
         NMINS = MOD(NSECS,3600) / 60
         NSECS = NSECS - 3600 * NHOUR - 60 * NMINS
         WRITE(6,*) 'FOR NPROCS = ', NPROCS
         WRITE(6,'(/,5X,"TOTAL COMPUTER TIME  = ",I2,2(":",I2.2),
     >             /,5X,"TIME PER STEP (SECS) = ",F10.5,/)')
     >          NHOUR, NMINS, NSECS, TOTAL_TIME / (NEND - NSTART + 1)
         WRITE(6,'(/,5X,"CPU TIME PER STEP PER CELL (SECS) = "
     >                 ,1PE12.4,/)')
     >   TOTAL_TIME / DBLE(NEND - NSTART + 1) * DBLE(NPROCS)
     >             / DBLE((IMAX-1)*(JMAX-1)*(KMAX-1))
      ENDIF

      IF(ALLOCATED(WORK)) DEALLOCATE(WORK)
      CALL MPI_FINALIZE ( IERR )

      END
!-----------------------------------------------------------------------
      SUBROUTINE EJECT ( MESSAGE )

      USE GENERAL_DATA, ONLY: MPI_COMM_WORLD

      IMPLICIT NONE

      INTEGER :: IERROR

      CHARACTER (LEN = *) MESSAGE

      WRITE(6,'("EXITING: ",A)') MESSAGE
      CALL MPI_ABORT (MPI_COMM_WORLD, IERROR)

      STOP
      END
!-----------------------------------------------------------------------
      SUBROUTINE ANALYSIS ( TAU, DELM )

      USE GENERAL_DATA, ONLY: IMAX, JMAX, KMAX, ICMAX, JCMAX, KCMAX,
     >                        XLEN, YLEN, ZLEN, JSTART, MPI_COMM_WORLD,
     >                        MPI_DOUBLE_PRECISION, MPI_SUM, UREF, TIME
      USE VARIABLE_DATA, ONLY: U, Q

      IMPLICIT NONE

      REAL (KIND = 8), INTENT (OUT) :: TAU, DELM

      REAL (KIND = 8), ALLOCATABLE, DIMENSION (:) :: UB, RB, BUF

      REAL (KIND = 8) :: DX, DY, DZ

      INTEGER I, J, K, JJ, ERROR

      ALLOCATE ( UB(JMAX-1), RB(JMAX-1), BUF(JMAX-1) )

      UB = 0.0D+00
      RB = 0.0D+00

      DX = XLEN / DBLE(IMAX - 1)
      DY = YLEN / DBLE(JMAX - 1)
      DZ = ZLEN / DBLE(KMAX - 1)

      DO K = 1, KCMAX
         DO J = 1, JCMAX
            JJ = J + JSTART - 1
            DO I = 1, ICMAX
               UB(JJ) = UB(JJ) + U(I,J,K)     * DX * DZ
               RB(JJ) = RB(JJ) + Q(I,J,K,1,1) * DX * DZ
            ENDDO
         ENDDO
      ENDDO

      CALL MPI_ALLREDUCE ( UB, BUF, JMAX-1, MPI_DOUBLE_PRECISION,
     >                     MPI_SUM, MPI_COMM_WORLD, ERROR )
      UB = BUF

      CALL MPI_ALLREDUCE ( RB, BUF, JMAX-1, MPI_DOUBLE_PRECISION,
     >                     MPI_SUM, MPI_COMM_WORLD, ERROR )
      RB = BUF

      DELM = 0.0D+00
      DO J = 1, JMAX-1
         UB(J) = UB(J) / UREF / RB(J)
         DELM = DELM + (0.25D+00 - UB(J) * UB(J)) * DY
      ENDDO

      TAU = TIME / (0.446D0 / UREF)

      DEALLOCATE ( UB, RB, BUF )

      RETURN
      END
