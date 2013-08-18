#ifndef MPP1
      SUBROUTINE RHO_MLT(FIN,IXI,JXI,KXI,PS,FOUT,IXO,JXO,KXO)                  
      IMPLICIT NONE                                                             
C                                                                               
C     PURPOSE  MULTIPLY INPUT FIELD BY DENSITY TO CHANGE UNITS FROM
C     #/KG TO #/M**3 FROM SUBROUTINE OUTTAP FOR OUTPUT PURPOSES ONLY.
C                                                                               
#     include <parame.incl>                                                    
#     include <point3d.incl>
#     include <param3.incl>                                                    
#     include <nonhyd.incl>
      INTEGER IXI,JXI,KXI,IXO,JXO,KXO,ISMALL,JSMALL,KSMALL                      
      INTEGER I,J,K                                                             
      REAL      FIN(IXI,JXI,KXI),FOUT(IXO,JXO,KXO),                             
     1          RHO(IXI,JXI,KXI),PS(MIX,MJX)                                                
C                                                                               
C     ... USE SMALLEST DIMENSION SIZES                                          
C                                                                               
      ISMALL=MIN0(IXI,IXO)                                                      
      JSMALL=MIN0(JXI,JXO)                                                      
      KSMALL=MIN0(KXI,KXO)                                                      
C                                                                               
C   CALCULATE DENSITY
C                                                                               
c$omp parallel do default(shared)
c$omp&private(i,j,k)
      DO 100 J=1,JSMALL
        DO 100 K=1,KSMALL
          DO 100 I=1,ISMALL
            RHO(I,J,K)=(PS(I,J)*(A(K)*PS(I,J)+PTOP)*1.E3
     1        +PPA(I,J,K))/(R*TA(I,J,K))
  100     CONTINUE
C
c$omp parallel do default(shared)                                               
c$omp&private(i,j,k)                                                            
      DO 200 J=1,JSMALL                                                         
        DO 200 K=1,KSMALL                                                       
          DO 200 I=1,ISMALL                                                     
            FOUT(I,J,K)=FIN(I,J,K)*RHO(I,J,K)
  200     CONTINUE                                                              
      RETURN                                                                    
      END                                                                       

#else
      SUBROUTINE RHO_MLT(FIN,IXI,JXI,KXI,PS,FOUT,IXO,JXO,KXO)                  
      IMPLICIT NONE                                                             
C                                                                               
C     PURPOSE  MULTIPLY INPUT FIELD BY DENSITY TO CHANGE UNITS FROM
C     #/KG TO #/M**3 FROM SUBROUTINE OUTTAP FOR OUTPUT PURPOSES ONLY.
C                                                                               
#     include <parame.incl>                                                    
#     include <point3d.incl>
#     include <param3.incl>                                                    
#     include <nonhyd.incl>
      INTEGER IXI,JXI,KXI,IXO,JXO,KXO,ISMALL,JSMALL,KSMALL                      
      INTEGER I,J,K                                                             
      REAL      FIN(MIX,MJX,MKX),FOUT(MIX,MJX,MKX),                             
     1          RHO(MIX,MJX,MKX),PS(MIX,MJX)                                                
C                                                                               
C     ... USE SMALLEST DIMENSION SIZES                                          
C                                                                               
      ISMALL=IXO                                                      
      JSMALL=JXO                                                      
      KSMALL=KXO                                                      
C                                                                               
C   CALCULATE DENSITY
C                                                                               

      DO 100 K=1,KSMALL
        DO 100 J=1,JSMALL
          DO 100 I=1,ISMALL
            RHO(I,J,K)=(PS(I,J)*(A(K)*PS(I,J)+PTOP)*1.E3
     1        +PPA(I,J,K))/(R*TA(I,J,K))
  100     CONTINUE
C
      DO 200 K=1,KSMALL                                                       
        DO 200 J=1,JSMALL                                                         
          DO 200 I=1,ISMALL                                                     
            FOUT(I,J,K)=FIN(I,J,K)*RHO(I,J,K)
  200     CONTINUE                                                              
      RETURN                                                                    
      END                                                                       

#endif

