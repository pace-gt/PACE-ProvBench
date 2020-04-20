      SUBROUTINE TMSTEP ( TMAX, RMACH )

      USE GENERAL_DATA, ONLY: ICMAX, JCMAX, KCMAX, CFL, DT, NADV, ITIME,
     >                        CMU, PR, M, IAM, NSTART, NEND, UREF,
     >                        MPI_COMM_WORLD, MPI_MIN, TAU_LIMIT,
     >                        MPI_DOUBLE_PRECISION
      USE VARIABLE_DATA, ONLY: U, V, W, SIX, SIY, SIZ, SJX, SJY, SJZ,
     >                         SKX, SKY, SKZ, SIJKSQ, HF, T, Q, VOL, DTV

      IMPLICIT NONE

      REAL (KIND = 8) :: DTMIN, TMAX, RMACH, UMAX
      REAL (KIND = 8) :: QSI, QSJ, QSK, GAMMA, CC, RMU, RNU, VIS
      INTEGER :: I, J, K, IERROR
      REAL (KIND = 8), DIMENSION(1:3) :: BUFS, BUFR
      REAL (KIND = 8) :: NONDIM_DT

      DTMIN  = 1.0D0
      TMAX   = 0.0D0
      RMACH  = 0.0D0
      UMAX   = 0.0D0

      DO K = 1, KCMAX
         DO J = 1, JCMAX
            DO I = 1, ICMAX

               QSI = U(I,J,K) * SIX(I,J,K) +
     >               V(I,J,K) * SIY(I,J,K) +
     >               W(I,J,K) * SIZ(I,J,K)
               QSJ = U(I,J,K) * SJX(I,J,K) +
     >               V(I,J,K) * SJY(I,J,K) +
     >               W(I,J,K) * SJZ(I,J,K)
               QSK = U(I,J,K) * SKX(I,J,K) +
     >               V(I,J,K) * SKY(I,J,K) +
     >               W(I,J,K) * SKZ(I,J,K)

               GAMMA = HF(I,J,K,2) / HF(I,J,K,3)

               CC  = SQRT(GAMMA * HF(I,J,K,4) * T(I,J,K))
               RMU = CMU * T(I,J,K) * SQRT(T(I,J,K)) /
     >                                  (T(I,J,K) + 110.0D0)
               RNU = GAMMA * RMU / (PR * Q(I,J,K,1,M))
               VIS = 2.0D0 * RNU * SIJKSQ(I,J,K) * SIJKSQ(I,J,K) /
     >                                       VOL(I,J,K)

               DTMIN = MIN( DTMIN, VOL(I,J,K) /
     >                            (ABS(QSI) + ABS(QSJ) + ABS(QSK) +
     >                            (SIJKSQ(I,J,K) * CC) + VIS))
               TMAX  = MAX( TMAX, T(I,J,K))

               RMACH = MAX( RMACH, SQRT( U(I,J,K) * U(I,J,K) +
     >                             V(I,J,K) * V(I,J,K) +
     >                             W(I,J,K) * W(I,J,K) ) / CC)
            ENDDO
         ENDDO
      ENDDO

      BUFS(1) = DTMIN
      BUFS(2) = 1.0D0 / TMAX
      BUFS(3) = 1.0D0 
      IF ( RMACH .GT. 0.0D0 ) BUFS(3) = 1.0D0 / RMACH

      CALL MPI_ALLREDUCE ( BUFS, BUFR, 3, MPI_DOUBLE_PRECISION,
     >                     MPI_MIN, MPI_COMM_WORLD, IERROR )

      DTMIN = BUFR(1)
      DT    = CFL * DTMIN
      TMAX  = 1.0D0 / BUFR(2)
      RMACH = 1.0D0 / BUFR(3)

cmm Set the number of steps for the run at the first iteration
cmm unless ITIME is 0 (timing run)
      IF (NADV == NSTART) THEN
         IF(ITIME.NE.0) THEN
            NONDIM_DT = DT / (0.446D0 / UREF)
            NEND = TAU_LIMIT / NONDIM_DT * 1.1D0 ! 10% overestimate
            IF (IAM == 0) WRITE(*,*) "NEND SET TO ", NEND
            ITIME = NEND / ITIME
            ITIME = MAX(ITIME,1)
            IF (IAM == 0) WRITE(*,*) "OUTPUT EVERY ", ITIME, "STEP"
         ELSE
cmm ITIME cannot be left at 0 for the operation MOD(ITIME,NADV)
            ITIME = 99999999
            IF (IAM == 0) WRITE(*,*) "NEND FROM input.data : ", NEND
            IF (IAM == 0) WRITE(*,*) "NO RUNTIME SCREEN OUTPUT"
         ENDIF
      END IF
      DO K = 1, KCMAX
         DO J = 1, JCMAX
            DO I = 1, ICMAX
               DTV(I,J,K) = DT / VOL(I,J,K)
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END
