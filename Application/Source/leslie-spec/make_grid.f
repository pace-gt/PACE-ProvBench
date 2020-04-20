! --------------------------------------
!  DEFINE GRID
! --------------------------------------

      REAL (KIND = 8), DIMENSION(:,:,:), ALLOCATABLE :: X, Y, Z
      REAL (KIND = 8), DIMENSION(:,:,:), ALLOCATABLE :: RBLANK

      OPEN(10, FILE = './input.data', FORM = 'FORMATTED')
            READ(10,*)
            READ(10,*)
            READ(10,*)
            READ(10,*) IMAX,JMAX,KMAX
            READ(10,*)
            READ(10,*)
            READ(10,*)
            READ(10,*) XLEN,YLEN,ZLEN
      CLOSE(10)

      PRINT*, NL,IMAX,JMAX,KMAX

      I1 = 1-(NL+1)
      J1 = 1-(NL+1)
      K1 = 1-(NL+1)
      I2 = IMAX+(NL+1)
      J2 = JMAX+(NL+1)
      K2 = KMAX+(NL+1)

      PRINT*, I1,I2,J1,J2,K1,K2

      ALLOCATE( X(I1:I2,J1:J2,K1:K2))
      ALLOCATE( Y(I1:I2,J1:J2,K1:K2))
      ALLOCATE( Z(I1:I2,J1:J2,K1:K2))
      ALLOCATE( RBLANK(I1:I2,J1:J2,K1:K2))

      XMIN = 0.0
      XMAX = XLEN
      DX   = (XMAX - XMIN) / DBLE(IMAX - 1)

      YMIN =  0.0
      YMAX =  YLEN
      DY   = (YMAX - YMIN) / DBLE(JMAX - 1)

      ZMIN =  0.0
      ZMAX =  ZLEN 
      DZ   = (ZMAX - ZMIN) / DBLE(KMAX - 1)

      DO K = K1,K2
         DO J = J1,J2
            DO I = I1,I2
               X(I,J,K) = XMIN + DBLE(I-1) * DX
               Y(I,J,K) = YMIN + DBLE(J-1) * DY
               Z(I,J,K) = ZMIN + DBLE(K-1) * DZ
            ENDDO
         ENDDO
      ENDDO

!----------------------------------------------------------
!     WRITE GRID.DATA FOR THE CODE
!-----------------------------------------------------------

      OPEN(24, FILE = 'GRID.DATA', FORM = 'UNFORMATTED')
      WRITE(24) IMAX, JMAX, KMAX
      DO K = K1,K2
         WRITE(24) ((X(I,J,K),I=I1,I2),J=J1,J2)
      ENDDO
      DO K = K1,K2
         WRITE(24) ((Y(I,J,K),I=I1,I2),J=J1,J2)
      ENDDO
      DO K = K1,K2
         WRITE(24) ((Z(I,J,K),I=I1,I2),J=J1,J2)
      ENDDO
      RBLANK = 1.0
      DO K = K1,K2
         WRITE(24) ((RBLANK(I,J,K),I=I1,I2),J=J1,J2)
      ENDDO
      CLOSE(24)

!------------------------------------
!     WRITE IN FIELD-VIEW FORMAT
!------------------------------------

        I1 = 0
        J1 = 0
        K1 = 0
        I2 = IMAX
        J2 = JMAX
        K2 = KMAX
       OPEN(24, FILE = 'grid.dat', FORM = 'UNFORMATTED')
        WRITE(24) I2-I1+1, J2-J1+1, K2-K1+1
        WRITE(24)
     >  (((REAL(X(I,J,K),KIND=4),I=I1,I2),J=J1,J2),K=K1,K2),
     >  (((REAL(Y(I,J,K),KIND=4),I=I1,I2),J=J1,J2),K=K1,K2),
     >  (((REAL(Z(I,J,K),KIND=4),I=I1,I2),J=J1,J2),K=K1,K2)
       CLOSE(24)

       I1 = 1; I2 = IMAX-1
       J1 = 1; J2 = JMAX-1
       K1 = 1; K2 = KMAX-1
       OPEN(24, FILE = 'grid_post.dat', FORM = 'UNFORMATTED')
        WRITE(24) I2-I1+1, J2-J1+1, K2-K1+1
        WRITE(24)
     >  (((REAL(X(I,J,K),KIND=4),I=I1,I2),J=J1,J2),K=K1,K2),
     >  (((REAL(Y(I,J,K),KIND=4),I=I1,I2),J=J1,J2),K=K1,K2),
     >  (((REAL(Z(I,J,K),KIND=4),I=I1,I2),J=J1,J2),K=K1,K2)
       CLOSE(24)

      STOP
      END

