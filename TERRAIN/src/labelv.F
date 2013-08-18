      SUBROUTINE LABELV                                                          LABELV.1
# include <paramesv.incl>                                                        LABELV.2
# include <vs_cmn2.incl>                                                         LABELV.3
C  USE CONSTANT SPACING OF CHARACTER                                             LABELV.4
#ifdef NCARG                                                                     LABELV.5
C Black color box line:
          CALL LBSETI ('CBL', 4)
C Red color label:
          CALL LBSETI ('CLB',16)

      CALL PCSETR('CS',1.25)                                                     LABELV.6
                                                                                 LABELV.7
      CALL SFSETI ('TYPE OF FILL',0)                                             LABELV.8
      CALL LBLBAR (0,.1,0.90,.05,.1,NVC,.5,.5,LBVG,0,LLBV3,NVC,1)                LABELV.9
#endif                                                                           LABELV.10
      RETURN                                                                     LABELV.11
      END                                                                        LABELV.12
                                                                                 LABELV.13
                                                                                 LABELV.14
