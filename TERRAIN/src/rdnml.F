      PROGRAM RDNML                                                  
C                                                                       
C   PURPOSE :  READ TERRAIN'S NAMELISTS AND OUTPUT THE DATA RESOLUTION
C              TYPES (NTYPE) FOR FTP SCRIPT 
C   COMMENTS :                                                          
C                                                                       
      IMPLICIT NONE

      REAL     PHIC, XLONC, AEXP, PSI
      LOGICAL  IEXP
      CHARACTER*6 IPROJ
      NAMELIST /MAPBG/ PHIC, XLONC, IEXP, AEXP, IPROJ, PSI
      INTEGER MAXNES, NTYPE(10), NESTIX(10), NESTJX(10), 
     -        NESTI(10),NESTJ(10),NUMNC(10),NSTTYP(10)
      REAL    DIS(10), RID(10)
      NAMELIST /DOMAINS/ NESTIX, NESTJX, NESTI, NESTJ, DIS, RID,
     -                   NUMNC, NTYPE, MAXNES, NSTTYP
      INTEGER IPRINT, ISMTHTR, VEGTYPE
      LOGICAL IFTER, IFANAL, IFEZFUG, IFTFUG, IFFUDG, IPRNTD, IPRTHT,
     -        IPRTLU, IPFLAG, IFILL, ISURF, IWRITE, DATASW, VSPLOT,
     -        VEGONLY, VEGESOIL, G30SD, IEXTRA, IBNDRY, LSMDATA
      REAL    FIN(10), TRUELAT1, TRUELAT2
      NAMELIST /OPTN/ IPRINT, IPRNTD, IFFUDG, IFTFUG, IFEZFUG,
     -                IPRTHT, IFANAL,  IFTER,  IFILL,     FIN,   
     -                DATASW,ISMTHTR,VEGESOIL,   
     -               VEGONLY,  VSPLOT, IEXTRA, G30SD,
     -               LSMDATA, VEGTYPE,
     -                IBNDRY,   ISURF, IWRITE, IPFLAG, IPRTLU,
     -              TRUELAT1,TRUELAT2 

      INTEGER M, ITER, NEWLAND, SOIL, ILOW, I2MIN, I30SEC
                                                                        
C  INITIALIZE SOME VALUES
   
      DO M=1,10
         NTYPE(M) = 0
      END DO
      IFTER   = .FALSE.
      VEGONLY = .FALSE.
      VEGESOIL= .FALSE.
      G30SD   = .FALSE.
      LSMDATA = .FALSE.
      VEGTYPE = 0
      ITER    = 0
      NEWLAND = 0
      SOIL    = 0
      ILOW    = 0
      I2MIN   = 0
      I30SEC  = 0
 
      PSI = 0.
      IBNDRY = .FALSE.
      ISURF  = .FALSE.
      IWRITE = .FALSE.
      IPFLAG = .FALSE.
      IPRTLU = .FALSE.
      TRUELAT1=91.
      TRUELAT2=91.
                                                                        
C  READ IN NAMELIST: MAPBG, DOMAINS, and OPTN
                                                                        
      READ(*,NML=MAPBG)                                                    
      READ(*,NML=DOMAINS)                                                  
      READ(*,NML=OPTN)                                                  
C
C TURN DATA TYPE VALUES INTO 1/0
C
      DO M=1,MAXNES
         IF(NTYPE(M).LE.4) THEN
            ILOW   =1
         ELSE IF(NTYPE(M).EQ.5) THEN
            I2MIN  =1
         ELSE IF(NTYPE(M).EQ.6) THEN
            I30SEC =1
         END IF
      END DO
 
      IF (IFTER)   ITER    = 1
      IF (LSMDATA) SOIL    = 1
      IF (VEGTYPE.EQ.1) NEWLAND = 1
      IF (VEGTYPE.EQ.2) NEWLAND = 2
      IF (.NOT.IFTER) THEN
          SOIL = 0
          NEWLAND = 0
      END IF
 
      WRITE (*,'(6(I2,1x))') ILOW,I2MIN,I30SEC,ITER,NEWLAND,SOIL
                                                                        
      STOP                                                           
      END                                                              
