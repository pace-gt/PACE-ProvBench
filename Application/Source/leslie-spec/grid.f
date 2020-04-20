      SUBROUTINE GRID()

      USE GENERAL_DATA, ONLY: XLEN, YLEN, ZLEN, IMAX, JMAX, KMAX,
     >                        ICMAX, JCMAX, KCMAX,
     >                        ISTART, JSTART, KSTART
      USE VARIABLE_DATA, ONLY: T11, T12, T13,
     >                         T21, T22, T23,
     >                         T31, T32, T33, DS, DS1, VOL,
     >                         SIX, SJX, SKX, SIY, SJY, SKY, X, Y, Z,
     >                         SKX, SKY, SKZ, SIZ, SIY, SJZ, SIJKSQ

      IMPLICIT NONE

      INTEGER :: I1, I2, J1, J2, K1, K2, I, J, K, IG, JG, KG
      REAL (KIND = 8) :: X0, X1, DX, Y0, Y1, DY, Z0, Z1, DZ
      REAL (KIND = 8) :: X2, X3, X4, X5, X6, X7, X8
      REAL (KIND = 8) :: Y2, Y3, Y4, Y5, Y6, Y7, Y8
      REAL (KIND = 8) :: Z2, Z3, Z4, Z5, Z6, Z7, Z8
      REAL (KIND = 8) :: XXI, YXI, ZXI
      REAL (KIND = 8) :: XET, YET, ZET
      REAL (KIND = 8) :: XZT, YZT, ZZT
      REAL (KIND = 8) :: T111, T121, T131, T211, T311, T212, T312, T232
      REAL (KIND = 8) :: T221, T231, T331, T321, T222, T132, T322
      REAL (KIND = 8) :: T112, T122, T213, T133, T233, T223, T323, T313
      REAL (KIND = 8) :: T110, T333, T123, T120, T220, T130, T332, T210
      REAL (KIND = 8) :: T230, T310, T320, T330, T113
      REAL (KIND = 8) :: XF, YF, ZF
      REAL (KIND = 8) :: XF1, YF1, ZF1
      REAL (KIND = 8) :: DXDXI, DYDXI, DZDXI, VOLXI, SXIX, SXIY, SXIZ
      REAL (KIND = 8) :: DXDET, DYDET, DZDET, VOLET, SETX, SETY, SETZ
      REAL (KIND = 8) :: DXDZT, DYDZT, DZDZT, VOLZT, SZTX, SZTY, SZTZ
      REAL (KIND = 8) :: RD, DENOM


      X0 = -XLEN / 2.0
      X1 =  XLEN / 2.0
      DX = (X1 - X0) / DBLE(IMAX - 1)

      Y0 = -YLEN / 2.0
      Y1 =  YLEN / 2.0
      DY = (Y1 - Y0) / DBLE(JMAX - 1)

      Z0 = -ZLEN / 2.0
      Z1 =  ZLEN / 2.0
      DZ = (Z1 - Z0) / DBLE(KMAX - 1)

      DO K = -2, KCMAX+4
         DO J = -2, JCMAX+4
            DO I = -2, ICMAX+4
               IG = I + ISTART - 1
               JG = J + JSTART - 1
               KG = K + KSTART - 1
               X(I,J,K) = X0 + DBLE(IG - 1) * DX
               Y(I,J,K) = Y0 + DBLE(JG - 1) * DY
               Z(I,J,K) = Z0 + DBLE(KG - 1) * DZ
            ENDDO
         ENDDO
      ENDDO

      I1 = -2
      I2 = ICMAX+3

      J1 = -2
      J2 = JCMAX+3

      K1 = -2
      K2 = KCMAX+3

      DO K = K1, K2
        DO J = J1, J2
          DO I = I1, I2

               X1 = X(I,J,K)
               Y1 = Y(I,J,K)
               Z1 = Z(I,J,K)
               X2 = X(I,J+1,K)
               Y2 = Y(I,J+1,K)
               Z2 = Z(I,J+1,K)
               X3 = X(I+1,J+1,K)
               Y3 = Y(I+1,J+1,K)
               Z3 = Z(I+1,J+1,K)
               X4 = X(I+1,J,K)
               Y4 = Y(I+1,J,K)
               Z4 = Z(I+1,J,K)
               X5 = X(I,J,K+1)
               Y5 = Y(I,J,K+1)
               Z5 = Z(I,J,K+1)
               X6 = X(I,J+1,K+1)
               Y6 = Y(I,J+1,K+1)
               Z6 = Z(I,J+1,K+1)
               X7 = X(I+1,J+1,K+1)
               Y7 = Y(I+1,J+1,K+1)
               Z7 = Z(I+1,J+1,K+1)
               X8 = X(I+1,J,K+1)
               Y8 = Y(I+1,J,K+1)
               Z8 = Z(I+1,J,K+1)

               SIX(I,J,K) = 0.5D+00*((Y7-Y4)*(Z8-Z3)-(Y8-Y3)*(Z7-Z4))
               SIY(I,J,K) = 0.5D+00*((Z7-Z4)*(X8-X3)-(Z8-Z3)*(X7-X4))
               SIZ(I,J,K) = 0.5D+00*((X7-X4)*(Y8-Y3)-(X8-X3)*(Y7-Y4))

               SJX(I,J,K) = 0.5D+00*((Y6-Y3)*(Z7-Z2)-(Y7-Y2)*(Z6-Z3))
               SJY(I,J,K) = 0.5D+00*((Z6-Z3)*(X7-X2)-(Z7-Z2)*(X6-X3))
               SJZ(I,J,K) = 0.5D+00*((X6-X3)*(Y7-Y2)-(X7-X2)*(Y6-Y3))

               SKX(I,J,K) = 0.5D+00*((Y8-Y6)*(Z7-Z5)-(Y7-Y5)*(Z8-Z6))
               SKY(I,J,K) = 0.5D+00*((Z8-Z6)*(X7-X5)-(Z7-Z5)*(X8-X6))
               SKZ(I,J,K) = 0.5D+00*((X8-X6)*(Y7-Y5)-(X7-X5)*(Y8-Y6))

               XXI = 0.25D+00*(X4-X1+X8-X5+X7-X6+X3-X2)
               YXI = 0.25D+00*(Y4-Y1+Y8-Y5+Y7-Y6+Y3-Y2)
               ZXI = 0.25D+00*(Z4-Z1+Z8-Z5+Z7-Z6+Z3-Z2)
               XET = 0.25D+00*(X2-X1+X6-X5+X7-X8+X3-X4)
               YET = 0.25D+00*(Y2-Y1+Y6-Y5+Y7-Y8+Y3-Y4)
               ZET = 0.25D+00*(Z2-Z1+Z6-Z5+Z7-Z8+Z3-Z4)
               XZT = 0.25D+00*(X5-X1+X6-X2+X7-X3+X8-X4)
               YZT = 0.25D+00*(Y5-Y1+Y6-Y2+Y7-Y3+Y8-Y4)
               ZZT = 0.25D+00*(Z5-Z1+Z6-Z2+Z7-Z3+Z8-Z4)

               SXIX = 0.25D+00*((Y7-Y4)*(Z8-Z3)-(Y8-Y3)*(Z7-Z4)
     >               +(Y6-Y1)*(Z5-Z2)-(Y5-Y2)*(Z6-Z1))
               SXIY = 0.25D+00*((Z7-Z4)*(X8-X3)-(Z8-Z3)*(X7-X4)
     >               +(Z6-Z1)*(X5-X2)-(Z5-Z2)*(X6-X1))
               SXIZ = 0.25D+00*((X7-X4)*(Y8-Y3)-(X8-X3)*(Y7-Y4)
     >               +(X6-X1)*(Y5-Y2)-(X5-X2)*(Y6-Y1))
               SETX = 0.25D+00*((Y6-Y3)*(Z7-Z2)-(Y7-Y2)*(Z6-Z3)
     >               +(Y5-Y4)*(Z8-Z1)-(Y8-Y1)*(Z5-Z4))
               SETY = 0.25D+00*((Z6-Z3)*(X7-X2)-(Z7-Z2)*(X6-X3)
     >               +(Z5-Z4)*(X8-X1)-(Z8-Z1)*(X5-X4))
               SETZ = 0.25D+00*((X6-X3)*(Y7-Y2)-(X7-X2)*(Y6-Y3)
     >               +(X5-X4)*(Y8-Y1)-(X8-X1)*(Y5-Y4))
               SZTX = 0.25D+00*((Y8-Y6)*(Z7-Z5)-(Y7-Y5)*(Z8-Z6)
     >               +(Y4-Y2)*(Z3-Z1)-(Y3-Y1)*(Z4-Z2))
               SZTY = 0.25D+00*((Z8-Z6)*(X7-X5)-(Z7-Z5)*(X8-X6)
     >               +(Z4-Z2)*(X3-X1)-(Z3-Z1)*(X4-X2))
               SZTZ = 0.25D+00*((X8-X6)*(Y7-Y5)-(X7-X5)*(Y8-Y6)
     >               +(X4-X2)*(Y3-Y1)-(X3-X1)*(Y4-Y2))

               SIJKSQ(I,J,K) = SQRT(SXIX**2 + SXIY**2 + SXIZ**2 +
     >                              SETX**2 + SETY**2 + SETZ**2 +
     >                              SZTX**2 + SZTY**2 + SZTZ**2 )

               VOLXI = ABS(SXIX*XXI)+ABS(SXIY*YXI)+ABS(SXIZ*ZXI)
               VOLET = ABS(SETX*XET)+ABS(SETY*YET)+ABS(SETZ*ZET)
               VOLZT = ABS(SZTX*XZT)+ABS(SZTY*YZT)+ABS(SZTZ*ZZT)
               VOL(I,J,K) = (VOLXI+VOLET+VOLZT)/3.0D+00
          END DO
        END DO
      END DO

      I1 = -2
      I2 = ICMAX+2

      J1 = -2
      J2 = JCMAX+2

      K1 = -2
      K2 = KCMAX+2

      DO K = K1, K2
        DO J = J1, J2
          DO I = I1, I2

               DXDXI  = 0.125D+00*(X(I+2,J+1,K)+X(I+2,J,K)
     >                        -X(I,J+1,K)-X(I,J,K)
     >                        +X(I+2,J+1,K+1)+X(I+2,J,K+1)
     >                        -X(I,J+1,K+1)-X(I,J,K+1))
               DYDXI  = 0.125D+00*(Y(I+2,J+1,K)+Y(I+2,J,K)
     >                        -Y(I,J+1,K)-Y(I,J,K)
     >                        +Y(I+2,J+1,K+1)+Y(I+2,J,K+1)
     >                        -Y(I,J+1,K+1)-Y(I,J,K+1))
               DZDXI  = 0.125D+00*(Z(I+2,J+1,K)+Z(I+2,J,K)
     >                        -Z(I,J+1,K)-Z(I,J,K)
     >                        +Z(I+2,J+1,K+1)+Z(I+2,J,K+1)
     >                        -Z(I,J+1,K+1)-Z(I,J,K+1))
               DXDET=0.5D+00*(X(I+1,J+1,K)-X(I+1,J,K)
     >                   +X(I+1,J+1,K+1)-X(I+1,J,K+1))
               DYDET=0.5D+00*(Y(I+1,J+1,K)-Y(I+1,J,K)
     >                   +Y(I+1,J+1,K+1)-Y(I+1,J,K+1))
               DZDET=0.5D+00*(Z(I+1,J+1,K)-Z(I+1,J,K)
     >                   +Z(I+1,J+1,K+1)-Z(I+1,J,K+1))
               DXDZT=0.5D+00*(X(I+1,J,K+1)-X(I+1,J,K)
     >                   +X(I+1,J+1,K+1)-X(I+1,J+1,K))
               DYDZT=0.5D+00*(Y(I+1,J,K+1)-Y(I+1,J,K)
     >                   +Y(I+1,J+1,K+1)-Y(I+1,J+1,K))
               DZDZT=0.5D+00*(Z(I+1,J,K+1)-Z(I+1,J,K)
     >                   +Z(I+1,J+1,K+1)-Z(I+1,J+1,K))

            T111 =  DYDET * DZDZT - DYDZT * DZDET
            T121 = -DYDXI * DZDZT + DYDZT * DZDXI
            T131 =  DYDXI * DZDET - DYDET * DZDXI
            T211 = -DXDET * DZDZT + DXDZT * DZDET
            T221 =  DXDXI * DZDZT - DXDZT * DZDXI
            T231 = -DXDXI * DZDET + DXDET * DZDXI
            T311 =  DXDET * DYDZT - DXDZT * DYDET
            T321 = -DXDXI * DYDZT + DXDZT * DYDXI
            T331 =  DXDXI * DYDET - DXDET * DYDXI

            DENOM =  (DXDXI * T111 + DXDET * T121
     >                             + DXDZT * T131)
            RD      = 1.0D+00 / DENOM

            T11(I,J,K,1)  = RD * T111
            T12(I,J,K,1)  = RD * T121
            T13(I,J,K,1)  = RD * T131

            T21(I,J,K,1)  = RD * T211
            T22(I,J,K,1)  = RD * T221
            T23(I,J,K,1)  = RD * T231

            T31(I,J,K,1)  = RD * T311
            T32(I,J,K,1)  = RD * T321
            T33(I,J,K,1)  = RD * T331

               DXDXI=0.5D+00*(X(I+1,J+1,K)-X(I,J+1,K)
     >                   +X(I+1,J+1,K+1)-X(I,J+1,K+1))
               DYDXI=0.5D+00*(Y(I+1,J+1,K)-Y(I,J+1,K)
     >                   +Y(I+1,J+1,K+1)-Y(I,J+1,K+1))
               DZDXI=0.5D+00*(Z(I+1,J+1,K)-Z(I,J+1,K)
     >                   +Z(I+1,J+1,K+1)-Z(I,J+1,K+1))
               DXDET=0.125D+00*(X(I+1,J+2,K)+X(I,J+2,K)
     >                     -X(I+1,J,K)-X(I,J,K)
     >                     +X(I+1,J+2,K+1)+X(I,J+2,K+1)
     >                     -X(I+1,J,K+1)-X(I,J,K+1))
               DYDET=0.125D+00*(Y(I+1,J+2,K)+Y(I,J+2,K)
     >                     -Y(I+1,J,K)-Y(I,J,K)
     >                     +Y(I+1,J+2,K+1)+Y(I,J+2,K+1)
     >                     -Y(I+1,J,K+1)-Y(I,J,K+1))
               DZDET=0.125D+00*(Z(I+1,J+2,K)+Z(I,J+2,K)
     >                     -Z(I+1,J,K)-Z(I,J,K)
     >                     +Z(I+1,J+2,K+1)+Z(I,J+2,K+1)
     >                     -Z(I+1,J,K+1)-Z(I,J,K+1))
               DXDZT=0.5D+00*(X(I,J+1,K+1)-X(I,J+1,K)
     >                   +X(I+1,J+1,K+1)-X(I+1,J+1,K))
               DYDZT=0.5D+00*(Y(I,J+1,K+1)-Y(I,J+1,K)
     >                   +Y(I+1,J+1,K+1)-Y(I+1,J+1,K))
               DZDZT=0.5D+00*(Z(I,J+1,K+1)-Z(I,J+1,K)
     >                   +Z(I+1,J+1,K+1)-Z(I+1,J+1,K))

            T112 =  DYDET * DZDZT - DYDZT * DZDET
            T122 = -DYDXI * DZDZT + DYDZT * DZDXI
            T132 =  DYDXI * DZDET - DYDET * DZDXI

            T212 = -DXDET * DZDZT + DXDZT * DZDET
            T222 =  DXDXI * DZDZT - DXDZT * DZDXI
            T232 = -DXDXI * DZDET + DXDET * DZDXI

            T312 =  DXDET * DYDZT - DXDZT * DYDET
            T322 = -DXDXI * DYDZT + DXDZT * DYDXI
            T332 =  DXDXI * DYDET - DXDET * DYDXI

            DENOM =  (DXDXI * T112 + DXDET * T122
     >                             + DXDZT * T132)
            RD      = 1.0D+00 / DENOM

            T11(I,J,K,2)  = RD * T112
            T12(I,J,K,2)  = RD * T122
            T13(I,J,K,2)  = RD * T132
      
            T21(I,J,K,2)  = RD * T212
            T22(I,J,K,2)  = RD * T222
            T23(I,J,K,2)  = RD * T232

            T31(I,J,K,2)  = RD * T312
            T32(I,J,K,2)  = RD * T322
            T33(I,J,K,2)  = RD * T332

            DXDXI=0.5D+00*(X(I+1,J,K+1)-X(I,J,K+1)
     >                +X(I+1,J+1,K+1)-X(I,J+1,K+1))
            DYDXI=0.5D+00*(Y(I+1,J,K+1)-Y(I,J,K+1)
     >                +Y(I+1,J+1,K+1)-Y(I,J+1,K+1))
            DZDXI=0.5D+00*(Z(I+1,J,K+1)-Z(I,J,K+1)
     >                +Z(I+1,J+1,K+1)-Z(I,J+1,K+1))
            DXDET=0.5D+00*(X(I,J+1,K+1)-X(I,J,K+1)
     >                +X(I+1,J+1,K+1)-X(I+1,J,K+1))
            DYDET=0.5D+00*(Y(I,J+1,K+1)-Y(I,J,K+1)
     >                +Y(I+1,J+1,K+1)-Y(I+1,J,K+1))
            DZDET=0.5D+00*(Z(I,J+1,K+1)-Z(I,J,K+1)
     >                +Z(I+1,J+1,K+1)-Z(I+1,J,K+1))
            DXDZT=0.125D+00*(X(I,J+1,K+2)+X(I,J,K+2)
     >                  -X(I,J+1,K)-X(I,J,K)
     >                  +X(I+1,J+1,K+2)+X(I+1,J,K+2)
     >                  -X(I+1,J+1,K)-X(I+1,J,K))
            DYDZT=0.125D+00*(Y(I,J+1,K+2)+Y(I,J,K+2)
     >                  -Y(I,J+1,K)-Y(I,J,K)
     >                  +Y(I+1,J+1,K+2)+Y(I+1,J,K+2)
     >                  -Y(I+1,J+1,K)-Y(I+1,J,K))
            DZDZT=0.125D+00*(Z(I,J+1,K+2)+Z(I,J,K+2)
     >                  -Z(I,J+1,K)-Z(I,J,K)
     >                  +Z(I+1,J+1,K+2)+Z(I+1,J,K+2)
     >                  -Z(I+1,J+1,K)-Z(I+1,J,K))

            T113 =  DYDET * DZDZT - DYDZT * DZDET
            T123 = -DYDXI * DZDZT + DYDZT * DZDXI
            T133 =  DYDXI * DZDET - DYDET * DZDXI
            T213 = -DXDET * DZDZT + DXDZT * DZDET
            T223 =  DXDXI * DZDZT - DXDZT * DZDXI
            T233 = -DXDXI * DZDET + DXDET * DZDXI
            T313 =  DXDET * DYDZT - DXDZT * DYDET
            T323 = -DXDXI * DYDZT + DXDZT * DYDXI
            T333 =  DXDXI * DYDET - DXDET * DYDXI

               DENOM = (DXDXI * T113 + DXDET * T123
     >                               + DXDZT * T133)
               RD      = 1.0D+00 / DENOM

            T11(I,J,K,3)  = RD * T113
            T12(I,J,K,3)  = RD * T123
            T13(I,J,K,3)  = RD * T133

            T21(I,J,K,3)  = RD * T213
            T22(I,J,K,3)  = RD * T223
            T23(I,J,K,3)  = RD * T233

            T31(I,J,K,3)  = RD * T313
            T32(I,J,K,3)  = RD * T323
            T33(I,J,K,3)  = RD * T333

          END DO 
        END DO
      END DO 

      I1 = -2
      I2 = ICMAX+2

      J1 = -2
      J2 = JCMAX+2

      K1 = -2
      K2 = KCMAX+2

      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2

               DXDXI  = 0.25D+00*(X(I+1,J,K)-X(I,J,K)
     >                      +X(I+1,J,K+1)-X(I,J,K+1)
     >                      +X(I+1,J+1,K)-X(I,J+1,K)
     >                      +X(I+1,J+1,K+1)-X(I,J+1,K+1))
               DYDXI  = 0.25D+00*(Y(I+1,J,K)-Y(I,J,K)
     >                      +Y(I+1,J,K+1)-Y(I,J,K+1)
     >                      +Y(I+1,J+1,K)-Y(I,J+1,K)
     >                      +Y(I+1,J+1,K+1)-Y(I,J+1,K+1))
               DZDXI  = 0.25D+00*(Z(I+1,J,K)-Z(I,J,K)
     >                      +Z(I+1,J,K+1)-Z(I,J,K+1)
     >                      +Z(I+1,J+1,K)-Z(I,J+1,K)
     >                      +Z(I+1,J+1,K+1)-Z(I,J+1,K+1))
               DXDET  = 0.25D+00*(X(I,J+1,K)-X(I,J,K)
     >                      +X(I,J+1,K+1)-X(I,J,K+1)
     >                      +X(I+1,J+1,K)-X(I+1,J,K)
     >                      +X(I+1,J+1,K+1)-X(I+1,J,K+1))
               DYDET  = 0.25D+00*(Y(I,J+1,K)-Y(I,J,K)
     >                      +Y(I,J+1,K+1)-Y(I,J,K+1)
     >                      +Y(I+1,J+1,K)-Y(I+1,J,K)
     >                      +Y(I+1,J+1,K+1)-Y(I+1,J,K+1))
               DZDET  = 0.25D+00*(Z(I,J+1,K)-Z(I,J,K)
     >                      +Z(I,J+1,K+1)-Z(I,J,K+1)
     >                      +Z(I+1,J+1,K)-Z(I+1,J,K)
     >                      +Z(I+1,J+1,K+1)-Z(I+1,J,K+1))
               DXDZT  = 0.25D+00*(X(I,J,K+1)-X(I,J,K)
     >                      +X(I,J+1,K+1)-X(I,J+1,K)
     >                      +X(I+1,J,K+1)-X(I+1,J,K)
     >                      +X(I+1,J+1,K+1)-X(I+1,J+1,K))
               DYDZT  = 0.25D+00*(Y(I,J,K+1)-Y(I,J,K)
     >                      +Y(I,J+1,K+1)-Y(I,J+1,K)
     >                      +Y(I+1,J,K+1)-Y(I+1,J,K)
     >                      +Y(I+1,J+1,K+1)-Y(I+1,J+1,K))
               DZDZT  = 0.25D+00*(Z(I,J,K+1)-Z(I,J,K)
     >                      +Z(I,J+1,K+1)-Z(I,J+1,K)
     >                      +Z(I+1,J,K+1)-Z(I+1,J,K)
     >                      +Z(I+1,J+1,K+1)-Z(I+1,J+1,K))

               T110 =  DYDET * DZDZT - DYDZT * DZDET
               T120 = -DYDXI * DZDZT + DYDZT * DZDXI
               T130 =  DYDXI * DZDET - DYDET * DZDXI
               T210 = -DXDET * DZDZT + DXDZT * DZDET
               T220 =  DXDXI * DZDZT - DXDZT * DZDXI
               T230 = -DXDXI * DZDET + DXDET * DZDXI
               T310 =  DXDET * DYDZT - DXDZT * DYDET
               T320 = -DXDXI * DYDZT + DXDZT * DYDXI
               T330 =  DXDXI * DYDET - DXDET * DYDXI

               DENOM =  (DXDXI * T110 + DXDET * T120
     >                                + DXDZT * T130)
                  RD      = 1.0D+00 / DENOM

            END DO
         END DO
      END DO

      I1 = -2
      I2 = ICMAX+2

      J1 = -2
      J2 = JCMAX+2

      K1 = -2
      K2 = KCMAX+2

      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2

               XF = 0.25D+00 * (X(I+1,J,K) + X(I+1,J+1,K) + 
     >                      X(I+1,J,K+1) + X(I+1,J+1,K+1))
               YF = 0.25D+00 * (Y(I+1,J,K) + Y(I+1,J+1,K) + 
     >                      Y(I+1,J,K+1) + Y(I+1,J+1,K+1))
               ZF = 0.25D+00 * (Z(I+1,J,K) + Z(I+1,J+1,K) + 
     >                      Z(I+1,J,K+1) + Z(I+1,J+1,K+1))
               XF1= 0.25D+00 * (X(I,J,K) + X(I,J+1,K) + 
     >                      X(I,J,K+1) + X(I,J+1,K+1))
               YF1= 0.25D+00 * (Y(I,J,K) + Y(I,J+1,K) + 
     >                      Y(I,J,K+1) + Y(I,J+1,K+1))
               ZF1= 0.25D+00 * (Z(I,J,K) + Z(I,J+1,K) + 
     >                      Z(I,J,K+1) + Z(I,J+1,K+1))

               DX = XF - XF1
               DY = YF - YF1
               DZ = ZF - ZF1
               DS(I,J,K,1) = SQRT(DX*DX + DY*DY + DZ*DZ)
            END DO
            DO I = I1, I2-1
               DS1(I,J,K,1) = 1.0D+00/(DS(I,J,K,1)+DS(I+1,J,K,1))
            END DO
         END DO
      END DO

      DO K = K1, K2
         DO I = I1, I2
            DO J = J1, J2

               XF = 0.25D+00 * (X(I,J+1,K) + X(I+1,J+1,K) + 
     >                      X(I,J+1,K+1) + X(I+1,J+1,K+1))
               YF = 0.25D+00 * (Y(I,J+1,K) + Y(I+1,J+1,K) + 
     >                      Y(I,J+1,K+1) + Y(I+1,J+1,K+1))
               ZF = 0.25D+00 * (Z(I,J+1,K) + Z(I+1,J+1,K) + 
     >                      Z(I,J+1,K+1) + Z(I+1,J+1,K+1))
               XF1= 0.25D+00 * (X(I,J,K) + X(I+1,J,K) + 
     >                      X(I,J,K+1) + X(I+1,J,K+1))
               YF1= 0.25D+00 * (Y(I,J,K) + Y(I+1,J,K) + 
     >                      Y(I,J,K+1) + Y(I+1,J,K+1))
               ZF1= 0.25D+00 * (Z(I,J,K) + Z(I+1,J,K) + 
     >                      Z(I,J,K+1) + Z(I+1,J,K+1))

               DX = XF - XF1
               DY = YF - YF1
               DZ = ZF - ZF1
               DS(I,J,K,2) = SQRT(DX*DX + DY*DY + DZ*DZ)
            END DO
            DO J = J1, J2-1
               DS1(I,J,K,2) = 1.0D+00/(DS(I,J,K,2)+DS(I,J+1,K,2))
            END DO
         END DO
      END DO

      DO J = J1, J2
         DO I = I1, I2
            DO K = K1, K2

               XF = 0.25D+00 * (X(I,J,K+1) + X(I+1,J,K+1) +
     >                      X(I,J+1,K+1) + X(I+1,J+1,K+1))
               YF = 0.25D+00 * (Y(I,J,K+1) + Y(I+1,J,K+1) +
     >                      Y(I,J+1,K+1) + Y(I+1,J+1,K+1))
               ZF = 0.25D+00 * (Z(I,J,K+1) + Z(I+1,J,K+1) +
     >                      Z(I,J+1,K+1) + Z(I+1,J+1,K+1))
               XF1= 0.25D+00 * (X(I,J,K) + X(I+1,J,K) +
     >                      X(I,J+1,K) + X(I+1,J+1,K))
               YF1= 0.25D+00 * (Y(I,J,K) + Y(I+1,J,K) +
     >                      Y(I,J+1,K) + Y(I+1,J+1,K))
               ZF1= 0.25D+00 * (Z(I,J,K) + Z(I+1,J,K) +
     >                      Z(I,J+1,K) + Z(I+1,J+1,K))

               DX = XF - XF1
               DY = YF - YF1
               DZ = ZF - ZF1
               DS(I,J,K,3) = SQRT(DX*DX + DY*DY + DZ*DZ)
            END DO
            DO K = K1, K2-1
               DS1(I,J,K,3) = 1.0D+00/(DS(I,J,K,3)+DS(I,J,K+1,3))
            END DO
         END DO
      END DO

      RETURN
      END
