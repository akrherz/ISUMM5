      SUBROUTINE UNPACK_ON84(NBF,MXB,DATA,DATDIM,NERR)
!
! UNPACKS ON84 DATA WHICH HAS BEEN READ BY RON84.
! NBF IS UNMODIFIED BUFFER AS READ BY RDON84. DATA IS ARRAY TO CONTAIN
!  DATA WHICH MUST BE DIMENSION APPROPRIATE TO THE GRID BEING UNPACKED.
!  NERR IS STATUS RETURN WHERE NONZERO VALUE INDICATES THAT NBF HAS
!  BEEN CHANGED BY USER AFTER READ OR THEIR WAS A BAD READ.
!

      INTEGER MXB
      DIMENSION NBF(MXB)
      INTEGER DATDIM
      REAL DATA(DATDIM)
      INTEGER IDAT(145*37)

#include <con84.incl>

      INTEGER NERR, ICSM, I, J, IOFF, IK, JK, ISKP, II, IDT
!
      
!

      NERR=0
      CALL GBYTES(NBF,IDAT,0,16,0,24)
      ICSM=0
      DO I=1,24
         ICSM=IEOR(ICSM,IDAT(I)) !KWM
      ENDDO

      IF(NK .NE. 1 .AND. NK .NE. 2) THEN
!
!        CHANGE FROM (1,1) NW TO (1,1) SW FOR ECMWF FGGE GRIDS.
!
         IOFF=384
         IK=NIDIM
         JK=NJDIM
         ISKP=0
         DO JJ=1,JK
            J = (JK-JJ)+1
            II=(JK-J)*IK+1
            CALL GBYTES(NBF,IDAT(II),IOFF,16,ISKP,IK)
            IOFF=IOFF+16*IK
         ENDDO
      ELSE
         CALL GBYTES(NBF,IDAT,384,16,0,NJ)
      ENDIF
!
      DO I =1,NJ
         IDT= IOR(IDAT(I),0) !KWM
         ICSM=IEOR(ICSM,IDT) !KWM
         IF(IDT .GE. 32768) IDT=IDT-65536
         DATA(I)=FLOAT(IDT)*2.**(NSCL-15)+BASE
      ENDDO

      RETURN
      END
