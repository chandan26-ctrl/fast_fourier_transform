!****************************************************************************
! *   FILE         = cooley_tukey_radix2.f90                                *
! *   AUTHOR       = CHANDAN KUMAR (CHANDANKR@IITKGP.AC.IN)                 *
! *   INSTITUTE    = INDIAN INSTITUTE OF TECHNOLOGY (IIT), KHARAGPUR        *
!****************************************************************************
! *  THIS PROGRAM IS DISTRIBUTED IN A HOPE THAT IT WILL BE USEFUL.          *
!****************************************************************************
      PROGRAM COOLEY_TUKEY_FFT
      IMPLICIT NONE
      INTEGER , PARAMETER :: N=8
      DOUBLE PRECISION , DIMENSION(N):: X, Y
      INTEGER :: I,J
      INTEGER :: N2, M, N1, K,L
      DOUBLE PRECISION :: E,A,C,S, XT,YT,NN
    
     

       DO I = 1, N
         X(I)=I-1
         Y(I)=0.0D0
       END DO
!INPUT DATA IN ARRAYS X (REAL PART) AND Y (IMAGINARY PART)
!N=2^M
       NN=N
       M=LOG(NN)/LOG(2.0D0)
       PRINT*, M
       DO I = 1, N
           
           PRINT*, X(I), Y(I),'i'
        END DO

       N2=N

       DO K = 1, M
          N1=N2
          N2=N2/2
          E=6.28318531D0/N1
          A=0.0D0
          DO J=1, N2
             C=DCOS(A)
             S=-DSIN(A)
             A=E*J
             DO I=J,N,N1
                L=I+N2
                XT=X(I)-X(L)
                X(I)=X(I)+X(L)
                YT=Y(I)-Y(L)
                Y(I)=Y(I)+Y(L)
                X(L)=C*XT-S*YT
                Y(L)=C*YT+S*XT
              END DO
           END DO
        END DO
       
        PRINT*, 'THIS SEQUENCE IS NOT PROPER...'
        PRINT*, 'REAL AND IMAGINARY'
        DO I = 1, N
           
           PRINT*, X(I), Y(I),'I'
        END DO

       
      
       END PROGRAM


