CB Routine replaced entirely by a new optimized version                            23DEC04.4360
CB      SUBROUTINE DCPL3DWND(IB,U,V,UA,VA,RPSD,IL,JL,KL,ILX,JLX)                   DCPL3DWND.1
CB#       include <parame.incl>                                                    DCPL3DWND.2
CB      DIMENSION U (MIX,MJX,MKX),V (MIX,MJX,MKX),                                 DCPL3DWND.3
CB     1          UA(MIX,MJX,MKX),VA(MIX,MJX,MKX),RPSD(MIX,MJX)                    DCPL3DWND.4
CBC                                                                                DCPL3DWND.5
CBC_FLIC_RUNPAD(2)                                                                 DCPL3DWND.6
CBcmic$ do all autoscope                                                           DCPL3DWND.7
CBc$doacross                                                                       DCPL3DWND.8
CBc$& local(i,j,k)                                                                 DCPL3DWND.9
CBc$omp parallel do default(shared)                                                DCPL3DWND.10
CBc$omp&private(i,j,k)                                                             DCPL3DWND.11
CB      DO J=1,JL                                                                  DCPL3DWND.12
CB        DO K=1,KL                                                                DCPL3DWND.13
CB          DO I=1,IL                                                              DCPL3DWND.14
CB            U(I,J,K)=UA(I,J,K)*RPSD(I,J)                                         DCPL3DWND.15
CB            V(I,J,K)=VA(I,J,K)*RPSD(I,J)                                         DCPL3DWND.16
CB          ENDDO                                                                  DCPL3DWND.17
CB        ENDDO                                                                    DCPL3DWND.18
CB      ENDDO                                                                      DCPL3DWND.19
CBC                                                                                DCPL3DWND.20
CB      IF(IB.GT.2)THEN                                                            DCPL3DWND.21
CBc$omp parallel                                                                   05DEC01.451
CBc$omp&private(i,k)                                                               05DEC01.452
CBc$        IF(OMP_GET_THREAD_NUM().EQ.0)THEN                                      05DEC01.453
CB            DO K=1,KL                                                            DCPL3DWND.29
CB              DO I=1,IL                                                          DCPL3DWND.30
CB                IF(U(I,1,K).LT.0.)THEN                                           DCPL3DWND.31
CB                  U(I,1,K)=U(I,2,K)                                              DCPL3DWND.32
CB                  V(I,1,K)=V(I,2,K)                                              DCPL3DWND.33
CB                ENDIF                                                            DCPL3DWND.34
CB              ENDDO                                                              DCPL3DWND.35
CB            ENDDO                                                                DCPL3DWND.36
CBc$        ENDIF                                                                  05DEC01.454
CBc$        IF(OMP_GET_THREAD_NUM().EQ.OMP_GET_NUM_THREADS()-1)THEN                05DEC01.455
CB            DO K=1,KL                                                            DCPL3DWND.39
CB              DO I=1,IL                                                          DCPL3DWND.40
CB                IF(U(I,JL,K).GT.0.)THEN                                          DCPL3DWND.41
CB                  U(I,JL,K)=U(I,JLX,K)                                           DCPL3DWND.42
CB                  V(I,JL,K)=V(I,JLX,K)                                           DCPL3DWND.43
CB                ENDIF                                                            DCPL3DWND.44
CB              ENDDO                                                              DCPL3DWND.45
CB            ENDDO                                                                DCPL3DWND.46
CBc$        ENDIF                                                                  05DEC01.456
CBc$omp end parallel                                                               05DEC01.457
CBcmic$ do all autoscope                                                           05DEC01.458
CBc$doacross                                                                       05DEC01.459
CBc$& local(i,j,k)                                                                 05DEC01.460
CBc$omp parallel do default(shared)                                                05DEC01.461
CBc$omp&private(i,j,k)                                                             05DEC01.462
CB        DO J=1,JL                                                                05DEC01.463
CB          DO K=1,KL                                                              DCPL3DWND.48
CB            IF(V(1,J,K).LT.0.)THEN                                               DCPL3DWND.49
CB              V(1,J,K)=V(2,J,K)                                                  DCPL3DWND.50
CB              U(1,J,K)=U(2,J,K)                                                  DCPL3DWND.51
CB            ENDIF                                                                DCPL3DWND.52
CBC                                                                                DCPL3DWND.53
CB            IF(V(IL,J,K).GT.0.)THEN                                              DCPL3DWND.54
CB              V(IL,J,K)=V(ILX,J,K)                                               DCPL3DWND.55
CB              U(IL,J,K)=U(ILX,J,K)                                               DCPL3DWND.56
CB            ENDIF                                                                DCPL3DWND.57
CB          ENDDO                                                                  DCPL3DWND.58
CB        ENDDO                                                                    DCPL3DWND.59
CB      ENDIF                                                                      DCPL3DWND.60
CB      RETURN                                                                     DCPL3DWND.61
CB      END                                                                        DCPL3DWND.62
                                                                                   DCPL3DWND.63
                                                                                   23DEC04.4361
                                                                                   23DEC04.4362
      SUBROUTINE DCPL3DWND( IB, U3, V3, UC, VC, UA, VA, RPSD, MSFD,                23DEC04.4363
     &                     IL, JL, KL, ILX, JLX )                                  23DEC04.4364
      IMPLICIT NONE                                                                23DEC04.4365
#       include <parame.incl>                                                      23DEC04.4366
                                                                                   23DEC04.4367
C........   Arguments                                                              23DEC04.4368
                                                                                   23DEC04.4369
      INTEGER  IB,IL,JL,KL,ILX,JLX                                                 23DEC04.4370
      REAL     U3(MIX,MJX,MKX), V3(MIX,MJX,MKX)                                    23DEC04.4371
      REAL     UC(MIX,MJX,MKX), VC(MIX,MJX,MKX)                                    23DEC04.4372
      REAL     UA(MIX,MJX,MKX), VA(MIX,MJX,MKX)                                    23DEC04.4373
      REAL     RPSD(MIX,MJX), MSFD(MIX,MJX)                                        23DEC04.4374
                                                                                   23DEC04.4375
C........   Local Variables                                                        23DEC04.4376
                                                                                   23DEC04.4377
      INTEGER  I, J,K                                                              23DEC04.4378
C................................................................                  23DEC04.4379
C_FLIC_RUNPAD(2)                                                                   23DEC04.4380
cmic$ do all autoscope                                                             23DEC04.4381
c$doacross                                                                         23DEC04.4382
c$& local(i,j,k)                                                                   23DEC04.4383
c$omp parallel do default(shared)                                                  23DEC04.4384
c$omp&private(i,j,k)                                                               23DEC04.4385
      DO J=1,JL                                                                    23DEC04.4386
                                                                                   23DEC04.4387
          DO K=1,KL                                                                23DEC04.4388
          DO I=1,IL                                                                23DEC04.4389
            UC(I,J,K)=UA(I,J,K)                                                    23DEC04.4390
            VC(I,J,K)=VA(I,J,K)                                                    23DEC04.4391
            UA(I,J,K)=UA(I,J,K)*MSFD(I,J)                                          23DEC04.4392
            VA(I,J,K)=VA(I,J,K)*MSFD(I,J)                                          23DEC04.4393
            U3(I,J,K)=UA(I,J,K)*RPSD(I,J)                                          23DEC04.4394
            V3(I,J,K)=VA(I,J,K)*RPSD(I,J)                                          23DEC04.4395
          ENDDO                                                                    23DEC04.4396
          ENDDO                                                                    23DEC04.4397
                                                                                   23DEC04.4398
          IF ( IB.GT.2 ) THEN                                                      23DEC04.4399
                                                                                   23DEC04.4400
                  IF ( J .EQ. 1 ) THEN                                             23DEC04.4401
                DO K=1,KL                                                          23DEC04.4402
                DO I=1,IL                                                          23DEC04.4403
                   IF(U3(I,1,K).LT.0.)THEN                                         23DEC04.4404
                      U3(I,1,K)=UA(I,2,K)*RPSD(I,2)                                23DEC04.4405
                      V3(I,1,K)=VA(I,2,K)*RPSD(I,2)                                23DEC04.4406
                   END IF                                                          23DEC04.4407
                ENDDO                                                              23DEC04.4408
                ENDDO                                                              23DEC04.4409
             ELSE IF ( J .EQ. JL ) THEN                                            23DEC04.4410
                DO K=1,KL                                                          23DEC04.4411
                DO I=1,IL                                                          23DEC04.4412
                   IF(U3(I,JL,K).LT.0.)THEN                                        23DEC04.4413
                      U3(I,JL,K)=UA(I,JLX,K)*RPSD(I,JLX)                           23DEC04.4414
                      V3(I,JL,K)=VA(I,JLX,K)*RPSD(I,JLX)                           23DEC04.4415
                   END IF                                                          23DEC04.4416
                ENDDO                                                              23DEC04.4417
                ENDDO                                                              23DEC04.4418
             END IF                                                                23DEC04.4419
                                                                                   23DEC04.4420
             DO K=1,KL                                                             23DEC04.4421
                IF(VA(1,J,K).LT.0.)THEN                                            23DEC04.4422
                   V3(1,J,K)=V3(2,J,K)                                             23DEC04.4423
                   U3(1,J,K)=U3(2,J,K)                                             23DEC04.4424
                ENDIF                                                              23DEC04.4425
                IF(VA(IL,J,K).GT.0.)THEN                                           23DEC04.4426
                   V3(IL,J,K)=V3(ILX,J,K)                                          23DEC04.4427
                   U3(IL,J,K)=U3(ILX,J,K)                                          23DEC04.4428
                ENDIF                                                              23DEC04.4429
             ENDDO                                                                 23DEC04.4430
                                                                                   23DEC04.4431
          END IF                                                                   23DEC04.4432
                                                                                   23DEC04.4433
      ENDDO     !  end parallel loop on J                                          23DEC04.4434
                                                                                   23DEC04.4435
      RETURN                                                                       23DEC04.4436
      END                                                                          23DEC04.4437
