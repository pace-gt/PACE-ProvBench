      SUBROUTINE FLOWIO

      USE GENERAL_DATA, ONLY: ISTART, JSTART, KSTART,
     >                        IMAX, JMAX, KMAX, IAM,
     >                        ICMAX, JCMAX, KCMAX, CVAIR,
     >                        PREFIX, LENGTH, IFLCNT, NUNIT, TIME,
     >                        MPI_DOUBLE_PRECISION, MPI_SUM,
     >                        MPI_COMM_WORLD
      USE VARIABLE_DATA

      IMPLICIT NONE

      CHARACTER (LEN = 80) :: FLNAME

      REAL (KIND = 8), ALLOCATABLE, DIMENSION (:,:,:,:) ::
     >            WBUF, RBUF

      INTEGER :: I, J, K, IG, JG, KG

      IF ( IAM .EQ. 0 ) THEN
         WRITE(FLNAME,'(A,"flow_",I3.3,".dat")')
     >                               PREFIX(1:LENGTH), IFLCNT
         WRITE(6,'(/4X,A,A/)') "WRITING FLOW FILE: ", FLNAME

         OPEN(NUNIT, FILE = FLNAME, FORM = "UNFORMATTED")
         WRITE(NUNIT) IMAX-1, JMAX-1, KMAX-1
         WRITE(NUNIT) (SNGL(TIME),I=1,4)
      ENDIF

      ALLOCATE ( WBUF(IMAX-1,JMAX-1,KMAX-1,5),
     >           RBUF(IMAX-1,JMAX-1,KMAX-1,5) )

      RBUF = 0.0D+00

      DO K = 1, KCMAX
         DO J = 1, JCMAX
            DO I = 1, ICMAX
               IG = I + ISTART - 1
               JG = J + JSTART - 1
               KG = K + KSTART - 1
               RBUF(IG,JG,KG,1) = Q(I,J,K,1,1)
               RBUF(IG,JG,KG,2) = Q(I,J,K,1,1) * U(I,J,K)
               RBUF(IG,JG,KG,3) = Q(I,J,K,1,1) * V(I,J,K)
               RBUF(IG,JG,KG,4) = Q(I,J,K,1,1) * W(I,J,K)
               RBUF(IG,JG,KG,5) = Q(I,J,K,1,1) *
     >                             (CVAIR * T(I,J,K) +
     >           0.5 * (U(I,J,K)**2 + V(I,J,K)**2 + W(I,J,K)**2))
            ENDDO
         ENDDO
      ENDDO

      I = (IMAX - 1) * (JMAX - 1) * (KMAX - 1) * 5

      CALL MPI_REDUCE ( RBUF, WBUF, I, MPI_DOUBLE_PRECISION,
     >                  MPI_SUM, 0, MPI_COMM_WORLD, J )

      IF ( IAM .EQ. 0 ) THEN
         WRITE(NUNIT)
     >       (((SNGL(WBUF(I,J,K,1)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >       (((SNGL(WBUF(I,J,K,2)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >       (((SNGL(WBUF(I,J,K,3)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >       (((SNGL(WBUF(I,J,K,4)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >       (((SNGL(WBUF(I,J,K,5)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1)
         CLOSE(NUNIT)
      ENDIF

      DEALLOCATE ( WBUF, RBUF )

      IF ( IFLCNT .GT. 0 ) RETURN

      IF ( IAM .EQ. 0 ) THEN
         WRITE(FLNAME,'(A,"grid.dat")') PREFIX(1:LENGTH)
         WRITE(6,'(/4X,A,A/)') "WRITING GRID FILE: ", FLNAME

         OPEN(NUNIT, FILE = FLNAME, FORM = "UNFORMATTED")
         WRITE(NUNIT) IMAX-1, JMAX-1, KMAX-1
      ENDIF

      ALLOCATE ( WBUF(IMAX-1,JMAX-1,KMAX-1,3),
     >           RBUF(IMAX-1,JMAX-1,KMAX-1,3) )

      RBUF = 0.0D+00

      DO K = 1, KCMAX
         DO J = 1, JCMAX
            DO I = 1, ICMAX
               IG = I + ISTART - 1
               JG = J + JSTART - 1
               KG = K + KSTART - 1
               RBUF(IG,JG,KG,1) = X(I,J,K)
               RBUF(IG,JG,KG,2) = Y(I,J,K)
               RBUF(IG,JG,KG,3) = Z(I,J,K)
            ENDDO
         ENDDO
      ENDDO

      I = (IMAX - 1) * (JMAX - 1) * (KMAX - 1) * 3

      CALL MPI_REDUCE ( RBUF, WBUF, I, MPI_DOUBLE_PRECISION,
     >                  MPI_SUM, 0, MPI_COMM_WORLD, J )

      IF ( IAM .EQ. 0 ) THEN
         WRITE(NUNIT)
     >       (((SNGL(WBUF(I,J,K,1)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >       (((SNGL(WBUF(I,J,K,2)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >       (((SNGL(WBUF(I,J,K,3)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1)
         CLOSE(NUNIT)
      ENDIF

      DEALLOCATE ( WBUF, RBUF )

      RETURN
      END
