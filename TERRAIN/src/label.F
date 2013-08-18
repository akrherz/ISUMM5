      SUBROUTINE LABEL                                                           LABEL.1
      CHARACTER*17 LLB(13)                                                       LABEL.2
      CHARACTER*2 LLB2(13)                                                       LABEL.3
      CHARACTER*8 LLB3(13)                                                       LABEL.4
      DIMENSION LND(13)                                                          LABEL.5
      DATA LLB / 'URBAN            ' ,                                           LABEL.6
     *           'ARGRICULTURE     ' ,                                           LABEL.7
     *            'RANGE-GRASSLAND  ' ,                                          LABEL.8
     *            'DECIDUOUS FOREST ' ,                                          LABEL.9
     *            'CONIFEROUS FOREST' ,                                          LABEL.10
     *            'FOREST/WET LAND  ' ,                                          LABEL.11
     *            'WATER            ' ,                                          LABEL.12
     *            'MARSH OR WET LAND' ,                                          LABEL.13
     *            'DESERT           ' ,                                          LABEL.14
     *            'TUNDRA           ' ,                                          LABEL.15
     *            'PERMANENT ICE    ' ,                                          LABEL.16
     *            'TROPICAL FOREST  ' ,                                          LABEL.17
     *            'SAVANNAH         ' /                                          LABEL.18
      DATA LLB3 / ' 1 URBAN' ,                                                   LABEL.19
     *            ' 2 ARGRI' ,                                                   LABEL.20
     *            ' 3 R-GLD' ,                                                   LABEL.21
     *            ' 4 D FOR' ,                                                   LABEL.22
     *            ' 5 C FOR' ,                                                   LABEL.23
     *            ' 6 F/WET' ,                                                   LABEL.24
     *            ' 7 WATER' ,                                                   LABEL.25
     *            ' 8 WET L' ,                                                   LABEL.26
     *            ' 9 DESER' ,                                                   LABEL.27
     *            '10 TUNDR' ,                                                   LABEL.28
     *            '11 P ICE' ,                                                   LABEL.29
     *            '12 T FOR' ,                                                   LABEL.30
     *            '13 SAVAN' /                                                   LABEL.31
      DATA LND /18,19,20,21,22,23,24,25,26,27,28,29,30/                          LABEL.32
      DATA LLB2/' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',                         LABEL.33
     *          ' 9','10','11','12','13' /                                       LABEL.34
#ifdef NCARG                                                                     LABEL.35
C Black color box line:
          CALL LBSETI ('CBL', 4)
C Red color label:
          CALL LBSETI ('CLB',16)

C  USE CONSTANT SPACING OF CHARACTER                                             LABEL.36
                                                                                 LABEL.37
      CALL PCSETR('CS',1.25)                                                     LABEL.38
                                                                                 LABEL.39
      CALL SFSETI ('TYPE OF FILL',0)                                             LABEL.40
      CALL LBLBAR (0,.1,0.90,.05,.1,13,.5,.5,LND,0,LLB3,13,1)                    LABEL.41
#endif                                                                           LABEL.42
      RETURN                                                                     LABEL.43
      END                                                                        LABEL.44
