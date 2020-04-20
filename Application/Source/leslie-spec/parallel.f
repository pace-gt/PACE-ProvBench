      SUBROUTINE PARALLEL (MSGFLAG)

      USE GENERAL_DATA, ONLY: NSCHEME, NCX, NPX, NCY, NPY, NCZ, NPZ,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        ICMAX, JCMAX, KCMAX, M
      USE VARIABLE_DATA, ONLY: Q

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: MSGFLAG
      INTEGER :: NLEVELS, MSG, L
      INTEGER :: I1, I2, J1, J2, K1, K2

      IF (NSCHEME .EQ. 2) THEN
         NLEVELS = 1
      ELSE
         NLEVELS = 2
      ENDIF

      MSG = MSGFLAG + 10000

      DO L = 1, 5
         CALL MPICX (MSG + L, Q(-2,-2,-2,L,M), NLEVELS)
      ENDDO

      IF(NCX .GT.   1 .OR. (NCX .EQ.   1 .AND. IPERIODIC)) THEN

         I1 = 1 - NLEVELS
         I2 = 0

         J1 = 1 - NLEVELS
         J2 = JCMAX + NLEVELS

         K1 = 1 - NLEVELS
         K2 = KCMAX + NLEVELS

         CALL GHOST(I1, I2, J1, J2, K1, K2)
      ENDIF

      IF(NCX .LT. NPX .OR. (NCX .EQ. NPX .AND. IPERIODIC)) THEN

         I1 = ICMAX + 1
         I2 = ICMAX + NLEVELS

         J1 = 1 - NLEVELS
         J2 = JCMAX + NLEVELS

         K1 = 1 - NLEVELS
         K2 = KCMAX + NLEVELS
         
         CALL GHOST(I1, I2, J1, J2, K1, K2)
      ENDIF

      IF(NCY .GT.   1 .OR. (NCY .EQ.   1 .AND. JPERIODIC)) THEN

         I1 = 1 - NLEVELS
         I2 = ICMAX + NLEVELS

         J1 = 1 - NLEVELS
         J2 = 0

         K1 = 1 - NLEVELS
         K2 = KCMAX + NLEVELS

         CALL GHOST(I1, I2, J1, J2, K1, K2)
      ENDIF

      IF(NCY .LT. NPY .OR. (NCY .EQ. NPY .AND. JPERIODIC)) THEN

         I1 = 1 - NLEVELS
         I2 = ICMAX + NLEVELS

         J1 = JCMAX + 1
         J2 = JCMAX + NLEVELS

         K1 = 1 - NLEVELS
         K2 = KCMAX + NLEVELS

         CALL GHOST(I1, I2, J1, J2, K1, K2)
      ENDIF

      IF(NCZ .GT.   1 .OR. (NCZ .EQ.   1 .AND. KPERIODIC)) THEN

         I1 = 1 - NLEVELS
         I2 = ICMAX + NLEVELS

         J1 = 1 - NLEVELS
         J2 = JCMAX + NLEVELS

         K1 = 1 - NLEVELS
         K2 = 0

         CALL GHOST(I1, I2, J1, J2, K1, K2)
      ENDIF

      IF(NCZ .LT. NPZ .OR. (NCZ .EQ. NPZ .AND. KPERIODIC)) THEN

         I1 = 1 - NLEVELS
         I2 = ICMAX + NLEVELS
        
         J1 = 1 - NLEVELS
         J2 = JCMAX + NLEVELS
            
         K1 = KCMAX + 1
         K2 = KCMAX + NLEVELS

         CALL GHOST(I1, I2, J1, J2, K1, K2)
      ENDIF

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE GHOST(I1, I2, J1, J2, K1, K2)

      USE GENERAL_DATA, ONLY: M, HALF
      USE VARIABLE_DATA, ONLY: U, V, W, T, Q, P, HF

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: I1, I2, J1, J2, K1, K2
      INTEGER :: I, J, K
      REAL (KIND = 8) :: RKE, EI

      DO K = K1, K2
         DO J = J1, J2

            DO I = I1, I2
               U(I,J,K) = Q(I,J,K,2,M) / Q(I,J,K,1,M)
               V(I,J,K) = Q(I,J,K,3,M) / Q(I,J,K,1,M)
               W(I,J,K) = Q(I,J,K,4,M) / Q(I,J,K,1,M)
            END DO

               DO I = I1, I2
                  RKE = HALF * (U(I,J,K) * U(I,J,K) +
     >                          V(I,J,K) * V(I,J,K) +
     >                          W(I,J,K) * W(I,J,K) )
                  EI = Q(I,J,K,5,M) / Q(I,J,K,1,M) - RKE
                  T(I,J,K) = (EI - HF(I,J,K,1)) / HF(I,J,K,3)
                  P(I,J,K) = Q(I,J,K,1,M) * HF(I,J,K,4) * T(I,J,K)
               END DO

         END DO
      END DO

      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE GRIDMAP()

      USE GENERAL_DATA, ONLY: NPROCS, ISTART, JSTART, KSTART,
     >                        IEND, JEND, KEND, IMAX, JMAX, KMAX,
     >                        ICMAX, JCMAX, KCMAX, NPX, NPY, NPZ,
     >                        MPI_COMM_WORLD, IAM, NCX, NCY, NCZ,
     >                        WEST, EAST, NORTH, SOUTH, IN, OUT

      IMPLICIT NONE

      REAL (KIND = 8) :: RATIO, RLOGRATIO
      INTEGER :: NX, NY, NZ, SURF, VOLM, I, J, K, IERROR

      !# FIRST, FIND A GOOD DISTRIBUTION

      RATIO = 0.0D+00
CFG
      RLOGRATIO = DLOG(DBLE(NPROCS))/DLOG(2.D0)
      IF(ABS(RLOGRATIO-DBLE(INT(RLOGRATIO))).LT.1.D-5) GOTO 123
CFG
      DO NX = 1, NPROCS
         DO NY = 1, NPROCS
            DO NZ = 1, NPROCS
               IF ( NX * NY * NZ .EQ. NPROCS ) THEN
                  SURF = 0
                  VOLM = 0
                  DO K = 1, NZ
                     DO J = 1, NY
                        DO I = 1, NX
                           CALL MAP (ISTART, IEND, I-1, NX, 1, IMAX-1)
                           ICMAX = IEND - ISTART + 1
                           CALL MAP (JSTART, JEND, J-1, NY, 1, JMAX-1)
                           JCMAX = JEND - JSTART + 1
                           CALL MAP (KSTART, KEND, K-1, NZ, 1, KMAX-1)
                           KCMAX = KEND - KSTART + 1
                           SURF = SURF + 2 * ( JCMAX * KCMAX +
     >                                         ICMAX * KCMAX + 
     >                                         ICMAX * JCMAX )
                           VOLM = VOLM + ICMAX * JCMAX * KCMAX
                        ENDDO
                     ENDDO
                  ENDDO

                  IF ( DBLE(VOLM) / DBLE(SURF) .GT. RATIO ) THEN
                     RATIO = DBLE(VOLM) / DBLE(SURF)
                     NPX = NX
                     NPY = NY
                     NPZ = NZ
                  ENDIF

               ENDIF
            ENDDO
         ENDDO
      ENDDO
      GOTO 124
CFG
 123  CONTINUE
      NPZ = INT(DLOG(DBLE(NPROCS))/DLOG(2.D0)/3.D0)
      CALL MPI_BARRIER ( MPI_COMM_WORLD, IERROR )
      NPX = 2**NPZ
      NPZ = INT(DLOG(DBLE(NPROCS)/DBLE(NPX))/DLOG(2.D0)/2.D0)
      NPY = 2**NPZ
      NPZ = NPROCS / (NPY * NPX)
      CALL MPI_BARRIER ( MPI_COMM_WORLD, IERROR )
      IF (IAM == 0) WRITE(*,*) "PROC MAP",NPX,NPY,NPZ
 124  CONTINUE
CFG
      NCX = MOD( (IAM + 1), NPX)
      NCZ = (IAM + 1) / (NPX * NPY) + 1
      NCY = ((IAM + 1) - (NCZ - 1)*NPX*NPY)
     >                             / NPX + 1

      IF (NCX .EQ. 0) THEN
         NCX = NPX
         NCY = NCY - 1
      ENDIF

      IF (NCY .EQ. 0) THEN
         NCY = NPY
         NCZ = NCZ - 1
      ENDIF                                      

      CALL MAP (ISTART, IEND, NCX-1, NPX, 1, IMAX-1)
      CALL MAP (JSTART, JEND, NCY-1, NPY, 1, JMAX-1)
      CALL MAP (KSTART, KEND, NCZ-1, NPZ, 1, KMAX-1)

      ICMAX = IEND - ISTART + 1
      JCMAX = JEND - JSTART + 1
      KCMAX = KEND - KSTART + 1

      WEST  = NPX * NPY * (NCZ - 1) + NPX * (NCY - 1) + (NCX - 1) - 1
      EAST  = NPX * NPY * (NCZ - 1) + NPX * (NCY - 1) + (NCX + 1) - 1
      SOUTH = NPX * NPY * (NCZ - 1) + NPX * (NCY - 2) + NCX - 1
      NORTH = NPX * NPY * (NCZ - 1) + NPX * NCY       + NCX - 1
      IN    = NPX * NPY * (NCZ - 2) + NPX * (NCY - 1) + NCX - 1
      OUT   = NPX * NPY * NCZ       + NPX * (NCY - 1) + NCX - 1

      IF (NCX .EQ.   1) WEST  = IAM + NPX - 1
      IF (NCX .EQ. NPX) EAST  = IAM - NPX + 1
      IF (NCY .EQ.   1) SOUTH = SOUTH + NPX * NPY
      IF (NCY .EQ. NPY) NORTH = NORTH - NPX * NPY
      IF (NCZ .EQ.   1) IN    = IN + NPX * NPY * NPZ
      IF (NCZ .EQ. NPZ) OUT   = OUT - NPX * NPY * NPZ

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE MAP (NSTART, NEND, N, NP, N1, N2)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N1, N2, NP, N
      INTEGER, INTENT(OUT) :: NSTART, NEND
      INTEGER :: NMAX, NR, NCHUNK

      NMAX = N2 - N1 + 1

      NCHUNK = NMAX / NP
      NR = MOD(NMAX, NP)
 
      NSTART = N1 + N * NCHUNK
      NEND   = NSTART  + NCHUNK - 1
      IF(N .LE. NR-1)THEN
         NSTART = NSTART + N
         NEND   = NEND   + N + 1
      ELSE
         NSTART = NSTART + NR
         NEND   = NEND   + NR
      END IF
      IF(N .EQ. NP-1) NEND = NMAX
 
      RETURN
      END
!---------------------------------------------------------------------
      SUBROUTINE MPICX (MSGFLAG, VAR , NLEVELS)

      USE GENERAL_DATA, ONLY: WEST, EAST, NORTH, SOUTH, IN, OUT,
     >                        IPERIODIC, JPERIODIC, KPERIODIC,
     >                        ICMAX, JCMAX, KCMAX,
     >                        NPX, NPY, NPZ, NCX, NCY, NCZ,
     >                        MPI_DOUBLE_PRECISION, MPI_STATUS_SIZE,
     >                        MPI_COMM_WORLD,
     >                        ISTART, JSTART, KSTART

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: MSGFLAG, NLEVELS
      REAL (KIND = 8), INTENT(INOUT) ::
     >  VAR(-2:ICMAX+3,-2:JCMAX+3,-2:KCMAX+3)

      INTEGER :: I, J, K
      INTEGER :: NSIZE1, NSIZE2, NSIZE3
      INTEGER :: ICNT, JCNT, KCNT
      INTEGER :: IERR

      INTEGER, PARAMETER :: MSGTRS = 1000,
     >                      MSGTRW = 2000,
     >                      MSGTRN = 3000,
     >                      MSGTRE = 4000,
     >                      MSGTSN = 1000,
     >                      MSGTSE = 2000,
     >                      MSGTSS = 3000,
     >                      MSGTSW = 4000,
     >                      MSGTRI = 6000,
     >                      MSGTRO = 5000,
     >                      MSGTSI = 5000,
     >                      MSGTSO = 6000

      REAL (KIND = 8), ALLOCATABLE, DIMENSION (:) ::
     >          R_N, R_W, R_S, R_E, R_I, R_O,
     >          S_N, S_W, S_S, S_E, S_I, S_O

      INTEGER :: MSGIDRW, MSGIDRE, MSGIDRN, MSGIDRS, MSGIDRI, MSGIDRO

      INTEGER MPI_STATUS(MPI_STATUS_SIZE)

      NSIZE1 = (ICMAX + NLEVELS) - (1 - NLEVELS) + 1
      NSIZE2 = (JCMAX + NLEVELS) - (1 - NLEVELS) + 1
      NSIZE3 = (KCMAX + NLEVELS) - (1 - NLEVELS) + 1

      ALLOCATE ( R_N(NLEVELS * NSIZE1 * NSIZE3),
     >           R_W(NLEVELS * NSIZE2 * NSIZE3),
     >           R_S(NLEVELS * NSIZE1 * NSIZE3),
     >           R_E(NLEVELS * NSIZE2 * NSIZE3),
     >           R_I(NLEVELS * NSIZE1 * NSIZE2),
     >           R_O(NLEVELS * NSIZE1 * NSIZE2),
     >           S_N(NLEVELS * NSIZE1 * NSIZE3),
     >           S_W(NLEVELS * NSIZE2 * NSIZE3),
     >           S_S(NLEVELS * NSIZE1 * NSIZE3),
     >           S_E(NLEVELS * NSIZE2 * NSIZE3),
     >           S_I(NLEVELS * NSIZE1 * NSIZE2),
     >           S_O(NLEVELS * NSIZE1 * NSIZE2), STAT = IERR )
      IF ( IERR .NE. 0 ) CALL EJECT ('ALLOCATION ERROR: MPICX')

!-----------------------------------------------------------------------
!     EAST-WEST COMMUNICATION
!-----------------------------------------------------------------------
      IF(NPX .EQ. 1 .AND. IPERIODIC) THEN
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = 1, NLEVELS
                  VAR(ICMAX+I,J,K) = VAR(I,J,K)
                  VAR(1-I,J,K) = VAR(ICMAX-I+1,J,K)
               END DO
            END DO
         END DO
         GOTO 10
      ENDIF
         
      IF(NCX .GT.   1 .OR. (NCX .EQ.   1 .AND. IPERIODIC)) THEN
         CALL MPI_IRECV(R_W,
     >                  NLEVELS * NSIZE2 * NSIZE3,
     >                  MPI_DOUBLE_PRECISION,
     >                  WEST,
     >                  MSGFLAG + MSGTRW,
     >                  MPI_COMM_WORLD,
     >                  MSGIDRW,
     >                  IERR)
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_IRECV FAILED: R_W')
      END IF

      IF(NCX .LT. NPX .OR. (NCX .EQ. NPX .AND. IPERIODIC)) THEN
         CALL MPI_IRECV(R_E,
     >                  NLEVELS * NSIZE2 * NSIZE3,
     >                  MPI_DOUBLE_PRECISION,
     >                  EAST,
     >                  MSGFLAG + MSGTRE,
     >                  MPI_COMM_WORLD,
     >                  MSGIDRE,
     >                  IERR)
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_IRECV FAILED: R_E')
      END IF

      IF(NCX .GT.   1 .OR. (NCX .EQ.   1 .AND. IPERIODIC)) THEN
         ICNT = 0
         DO K = 1-NLEVELS,KCMAX+NLEVELS
            DO J = 1-NLEVELS,JCMAX+NLEVELS
               DO I = 1,NLEVELS
                  ICNT = ICNT + 1
                  S_W(ICNT) = VAR(I,J,K)
               END DO
            END DO
         END DO
         CALL MPI_SEND(S_W,
     >                 NLEVELS * NSIZE2 * NSIZE3,
     >                 MPI_DOUBLE_PRECISION,
     >                 WEST,
     >                 MSGFLAG + MSGTSW,
     >                 MPI_COMM_WORLD,
     >                 IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_SEND FAILED: S_W')
      END IF

      IF(NCX .LT. NPX .OR. (NCX .EQ. NPX .AND. IPERIODIC)) THEN
         ICNT = 0
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = ICMAX-NLEVELS+1, ICMAX
                  ICNT = ICNT + 1
                  S_E(ICNT) = VAR(I,J,K)
               ENDDO
            END DO
         END DO
         CALL MPI_SEND(S_E,
     >                 NLEVELS * NSIZE2 * NSIZE3,
     >                 MPI_DOUBLE_PRECISION,
     >                 EAST,
     >                 MSGFLAG + MSGTSE,
     >                 MPI_COMM_WORLD,
     >                 IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_SEND FAILED: S_E')
      END IF

      IF(NCX .LT. NPX .OR. (NCX .EQ. NPX .AND. IPERIODIC)) THEN
         CALL MPI_WAIT( MSGIDRE, MPI_STATUS, IERR)

         ICNT = 0
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = ICMAX+1, ICMAX+NLEVELS
                  ICNT = ICNT + 1
                  VAR(I,J,K) = R_E(ICNT) 
               ENDDO
            END DO
         END DO
      END IF

      IF(NCX .GT.   1 .OR. (NCX .EQ.   1 .AND. IPERIODIC)) THEN
         CALL MPI_WAIT( MSGIDRW, MPI_STATUS, IERR)
         ICNT = 0
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = 1-NLEVELS, 0
                  ICNT = ICNT + 1
                  VAR(I,J,K) = R_W(ICNT)
               END DO
            END DO
         END DO
      END IF

10    CONTINUE
!-----------------------------------------------------------------------
!     NORTH-SOUTH COMMUNICATION
!-----------------------------------------------------------------------
      IF(NPY .EQ. 1 .AND. JPERIODIC) THEN
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = 1, NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  VAR(I,JCMAX+J,K) = VAR(I,J,K)
                  VAR(I,1-J,K) = VAR(I,JCMAX-J+1,K)
               END DO
            END DO
         END DO
         GOTO 20
      ENDIF

      IF(NCY .LT. NPY .OR. (NCY .EQ. NPY .AND. JPERIODIC)) THEN
         CALL MPI_IRECV(R_N,
     >                  NLEVELS * NSIZE1 * NSIZE3,
     >                  MPI_DOUBLE_PRECISION,
     >                  NORTH,
     >                  MSGFLAG + MSGTRN,
     >                  MPI_COMM_WORLD,
     >                  MSGIDRN,
     >                  IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_IRECV FAILED: R_N')
      END IF

      IF(NCY .GT.   1 .OR. (NCY .EQ.   1 .AND. JPERIODIC)) THEN
         CALL MPI_IRECV(R_S,
     >                  NLEVELS * NSIZE1 * NSIZE3,
     >                  MPI_DOUBLE_PRECISION,
     >                  SOUTH,
     >                  MSGFLAG + MSGTRS,
     >                  MPI_COMM_WORLD,
     >                  MSGIDRS,
     >                  IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_IRECV FAILED: R_S')
      END IF

      IF(NCY .LT. NPY .OR. (NCY .EQ. NPY .AND. JPERIODIC)) THEN
         JCNT = 0
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = JCMAX-NLEVELS+1, JCMAX
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  JCNT = JCNT + 1
                  S_N(JCNT) = VAR(I,J,K)
               END DO
            ENDDO
         END DO
         CALL MPI_SEND(S_N,
     >                 NLEVELS * NSIZE1 * NSIZE3,
     >                 MPI_DOUBLE_PRECISION,
     >                 NORTH,
     >                 MSGFLAG + MSGTSN,
     >                 MPI_COMM_WORLD,
     >                 IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_SEND FAILED: S_N')
      END IF

      IF(NCY .GT.   1 .OR. (NCY .EQ.   1 .AND. JPERIODIC)) THEN
         JCNT = 0
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = 1, NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  JCNT = JCNT+1
                  S_S(JCNT) = VAR(I,J,K)
               ENDDO
            END DO
         END DO
         CALL MPI_SEND(S_S,
     >                 NLEVELS * NSIZE1 * NSIZE3,
     >                 MPI_DOUBLE_PRECISION,
     >                 SOUTH,
     >                 MSGFLAG + MSGTSS,
     >                 MPI_COMM_WORLD,
     >                 IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_SEND FAILED: S_S')
      END IF

      IF(NCY .LT. NPY .OR. (NCY .EQ. NPY .AND. JPERIODIC))  THEN
         CALL MPI_WAIT( MSGIDRN, MPI_STATUS, IERR)

         JCNT = 0
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = JCMAX+1, JCMAX+NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  JCNT = JCNT + 1
                  VAR(I,J,K) = R_N(JCNT) 
               END DO
            END DO
         END DO
      END IF

      IF(NCY .GT.   1 .OR. (NCY .EQ.   1 .AND. JPERIODIC)) THEN
         CALL MPI_WAIT( MSGIDRS, MPI_STATUS, IERR)

         JCNT = 0
         DO K = 1-NLEVELS, KCMAX+NLEVELS
            DO J = 1-NLEVELS, 0
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  JCNT = JCNT + 1
                  VAR(I,J,K) = R_S(JCNT)                
               END DO
            END DO
         END DO
      END IF

20    CONTINUE
!-----------------------------------------------------------------------
!     IN-OUT COMMUNICATION
!-----------------------------------------------------------------------
      IF(NPZ .EQ. 1 .AND. KPERIODIC) THEN
         DO K = 1, NLEVELS
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  VAR(I,J,KCMAX+K) = VAR(I,J,K)
                  VAR(I,J,1-K) = VAR(I,J,KCMAX-K+1)
               END DO
            END DO
         END DO
         GOTO 30
      ENDIF

      IF(NCZ .GT.   1 .OR. (NCZ .EQ.   1 .AND. KPERIODIC)) THEN
         CALL MPI_IRECV(R_I,
     >                  NLEVELS * NSIZE1 * NSIZE2,
     >                  MPI_DOUBLE_PRECISION,
     >                  IN,
     >                  MSGFLAG + MSGTRI,
     >                  MPI_COMM_WORLD,
     >                  MSGIDRI,
     >                  IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_IRECV FAILED: R_I')
      END IF

      IF(NCZ .LT. NPZ .OR. (NCZ .EQ. NPZ .AND. KPERIODIC)) THEN
         CALL MPI_IRECV(R_O,
     >                  NLEVELS * NSIZE1 * NSIZE2,
     >                  MPI_DOUBLE_PRECISION,
     >                  OUT,
     >                  MSGFLAG + MSGTRO,
     >                  MPI_COMM_WORLD,
     >                  MSGIDRO,
     >                  IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_IRECV FAILED: R_O')
      END IF

      IF(NCZ .GT.   1 .OR. (NCZ .EQ.   1 .AND. KPERIODIC)) THEN
         KCNT = 0
         DO K = 1, NLEVELS
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  KCNT = KCNT + 1
                  S_I(KCNT) = VAR(I,J,K)
               END DO
            ENDDO
         END DO
         CALL MPI_SEND(S_I,
     >                 NLEVELS * NSIZE1 * NSIZE2,
     >                 MPI_DOUBLE_PRECISION,
     >                 IN,
     >                 MSGFLAG + MSGTSI,
     >                 MPI_COMM_WORLD,
     >                 IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_SEND FAILED: S_I')
      END IF

      IF(NCZ .LT. NPZ .OR. (NCZ .EQ. NPZ .AND. KPERIODIC)) THEN
         KCNT = 0
         DO K = KCMAX-NLEVELS+1, KCMAX
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  KCNT = KCNT + 1
                  S_O(KCNT) = VAR(I,J,K)
               END DO
            END DO
         END DO
         CALL MPI_SEND(S_O,
     >                 NLEVELS * NSIZE1 * NSIZE2,
     >                 MPI_DOUBLE_PRECISION,
     >                 OUT,
     >                 MSGFLAG + MSGTSO,
     >                 MPI_COMM_WORLD,
     >                 IERR )
         IF ( IERR .NE. 0 ) CALL EJECT ('MPI_SEND FAILED: S_O')
      END IF

      IF(NCZ .GT.   1 .OR. (NCZ .EQ.   1 .AND. KPERIODIC)) THEN
         CALL MPI_WAIT( MSGIDRI, MPI_STATUS, IERR)

         KCNT = 0
         DO K = 1-NLEVELS, 0
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  KCNT = KCNT + 1
                  VAR(I,J,K) = R_I(KCNT)
               END DO
            END DO
         END DO
      END IF

      IF(NCZ .LT. NPZ .OR. (NCZ .EQ. NPZ .AND. KPERIODIC)) THEN
         CALL MPI_WAIT( MSGIDRO, MPI_STATUS, IERR)

         KCNT = 0
         DO K = KCMAX+1, KCMAX+NLEVELS
            DO J = 1-NLEVELS, JCMAX+NLEVELS
               DO I = 1-NLEVELS, ICMAX+NLEVELS
                  KCNT = KCNT + 1
                  VAR(I,J,K) = R_O(KCNT)
               END DO
            END DO
         END DO
      END IF

30    CONTINUE

      DEALLOCATE ( R_N, R_W, R_S, R_E, R_I, R_O,
     >            S_N, S_W, S_S, S_E, S_I, S_O )

      RETURN
      END
