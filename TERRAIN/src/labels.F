      SUBROUTINE LABELS                                                          LABELS.1
#  include <paramesv.incl>                                                       LABELS.2
#  include <vs_cmn2.incl>                                                        LABELS.3
C  USE CONSTANT SPACING OF CHARACTER                                             LABELS.4
#ifdef NCARG                                                                     LABELS.5
C Black color box line:
          CALL LBSETI ('CBL', 4)
C Red color label:
          CALL LBSETI ('CLB',16)

      CALL PCSETR('CS',1.25)                                                     LABELS.6
                                                                                 LABELS.7
      CALL SFSETI ('TYPE OF FILL',0)                                             LABELS.8
      CALL LBLBAR (0,.1,0.90,.05,.1,NSC,.5,.5,LBSL,0,LLBS3,NSC,1)                LABELS.9
#endif                                                                           LABELS.10
      RETURN                                                                     LABELS.11
      END                                                                        LABELS.12
                                                                                 LABELS.13
                                                                                 LABELS.14
