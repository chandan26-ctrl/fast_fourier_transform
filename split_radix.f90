!****************************************************************************
! *   FILE         = split_radix.f90                                        *
! *   AUTHOR       = CHANDAN KUMAR (CHANDANKR@IITKGP.AC.IN)                 *
! *   INSTITUTE    = INDIAN INSTITUTE OF TECHNOLOGY (IIT), KHARAGPUR        *
!****************************************************************************
! *  THIS PROGRAM IS DISTRIBUTED IN A HOPE THAT IT WILL BE USEFUL.          *
!****************************************************************************
      PROGRAM SPLIT_RADIX_FFT
      IMPLICIT NONE
      INTEGER , PARAMETER :: N=8
      DOUBLE PRECISION , DIMENSION(N):: X, Y
      INTEGER :: I,J,M
      DOUBLE PRECISION :: NN

       DO I = 1, N
         X(I)=I-1
         Y(I)=0.0D0
       END DO
!INPUT DATA IN ARRAYS X (REAL PART) AND Y (IMAGINARY PART)
!N=2^M
       NN=N
       M=LOG(NN)/LOG(2.0D0)
       
       CALL FFT(X,Y,N,M)
       
        PRINT*, 'THIS SEQUENCE IS NOT PROPER...'
        PRINT*, 'REAL AND IMAGINARY'
        
        DO I = 1, N
           PRINT*, X(I), Y(I),'i'
        END DO

       
      
       END PROGRAM
       
       SUBROUTINE FFT(X,Y,N,M)
       IMPLICIT NONE
       INTEGER :: N2, N4, IS, ID, I0, I1, I2, I3, N, M, J, K
       DOUBLE PRECISION :: E, A, A3, CC1,SS1, CC3, SS3
       DOUBLE PRECISION :: R1, X(N), Y(N), R2, S1, S2, S3
        N2 = 2*N
        DO  10 K = 1, M-1
            N2 = N2/2
            N4 = N2/4
            E  = 6.283185307179586D0/N2
            A = 0.0D0
            DO  20 J = 1, N4
                A3  = 3.0D0*A
                CC1 = DCOS(A)
                SS1 = DSIN(A)
                CC3 = DCOS(A3)
                SS3 = DSIN(A3)
                A   = J*E
                IS  = J
                ID  = 2*N2
 40             DO 30 I0 = IS, N-1, ID
                    I1 = I0 + N4
                    I2 = I1 + N4
                    I3 = I2 + N4
                    R1    = X(I0) - X(I2)
                    X(I0) = X(I0) + X(I2)
                    R2    = X(I1) - X(I3)
                    X(I1) = X(I1) + X(I3)
                    S1    = Y(I0) - Y(I2)
                    Y(I0) = Y(I0) + Y(I2)
                    S2    = Y(I1) - Y(I3)
                    Y(I1) = Y(I1) + Y(I3)
                    S3    = R1 - S2
                    R1    = R1 + S2
                    S2    = R2 - S1
                    R2    = R2 + S1
                    X(I2) = R1*CC1 - S2*SS1
                    Y(I2) =-S2*CC1 - R1*SS1
                    X(I3) = S3*CC3 + R2*SS3
                    Y(I3) = R2*CC3 - S3*SS3
 30             CONTINUE
                IS = 2*ID - N2 + J
                ID = 4*ID
                IF (IS.LT.N) GOTO 40
 20         CONTINUE
 10     CONTINUE
        IS = 1
        ID = 4
 50     DO 60 I0 = IS, N, ID
            I1    = I0 + 1
            R1    = X(I0)
            X(I0) = R1 + X(I1)
            X(I1) = R1 - X(I1)
            R1    = Y(I0)
            Y(I0) = R1 + Y(I1)
 60    Y(I1) = R1 - Y(I1)
            IS = 2*ID - 1
            ID = 4*ID
        IF (IS.LT.N) GOTO 50
        
        RETURN
        END SUBROUTINE
 


