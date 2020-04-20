      SUBROUTINE UPDATE ( )

      USE GENERAL_DATA, ONLY: CVAIR, RGAIR, M, N,
     >                        ICMAX, JCMAX, KCMAX, ND
      USE VARIABLE_DATA, ONLY: Q, DQ, U, V, W, T, P

      IMPLICIT NONE

      INTEGER :: I, J, K, L, NS
      REAL (KIND = 8) :: KE

      M = 3 - N

      DO K = 1, KCMAX
         DO J = 1, JCMAX
            DO I = 1, ICMAX

               IF ( N .EQ. 1 ) THEN
                  Q(I,J,K,1:5,M) = Q(I,J,K,1:5,N) + DQ(I,J,K,1:5)
               ELSE
                  Q(I,J,K,1:5,M) = 0.5D+00 * (Q(I,J,K,1:5,M) +
     >                            Q(I,J,K,1:5,N) + DQ(I,J,K,1:5))
               ENDIF

               U(I,J,K) = Q(I,J,K,2,M) / Q(I,J,K,1,M)
               V(I,J,K) = Q(I,J,K,3,M) / Q(I,J,K,1,M)
               W(I,J,K) = Q(I,J,K,4,M) / Q(I,J,K,1,M)

               KE = 0.5D+00 * (U(I,J,K) * U(I,J,K) +
     >                         V(I,J,K) * V(I,J,K) +
     >                         W(I,J,K) * W(I,J,K))

               T(I,J,K) = (Q(I,J,K,5,M) / Q(I,J,K,1,M) - KE) / CVAIR
               P(I,J,K) = Q(I,J,K,1,M) * RGAIR * T(I,J,K)
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END
