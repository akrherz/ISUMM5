      SUBROUTINE DFCLRS                                                          DFCLRS.1
      DIMENSION COLOR(3,13)                                                      DFCLRS.2
                                                                                 DFCLRS.3
      DATA COLOR /0.65  , 0.25 , 0.00 ,    ! URBAN                               DFCLRS.4
     *             0.50  , 0.50 , 0.25 ,   ! AGRICULTURE                         DFCLRS.5
     *             0.25  , 0.75 , 0.12 ,   ! RANGE-GRASSLAND                     DFCLRS.6
     *             0.25  , 0.38 , 0.00 ,   ! DECIDUOUS FOREST                    DFCLRS.7
     *             0.00  , 0.38 , 0.00 ,   ! CONIFEROUS FOREST                   DFCLRS.8
     *             0.00  , 0.38 , 0.38 ,   ! MIXED FOREST AND WET LAND           DFCLRS.9
     *             0.00  , 0.25 , 0.50 ,   ! WATER                               DFCLRS.10
     *             0.00  , 0.25 , 0.25 ,   ! MARSH OR WET LAND                   DFCLRS.11
     *             1.00  , 1.00 , 0.63 ,   ! DESERT                              DFCLRS.12
     *             0.40  , 0.60 , 0.40 ,   ! TUNDRA                              DFCLRS.13
     *             1.00  , 1.00 , 1.00 ,   ! PERMANENT ICE                       DFCLRS.14
     *             0.00  , 1.00 , 0.00 ,   ! TROPICAL OR SUB TROPICAL FO         DFCLRS.15
     *             0.25  , 0.75 , 0.12 /   ! SAVANNAH                            DFCLRS.16
C  SET COLOR INDEX                                                               DFCLRS.17
#ifdef NCARG                                                                     DFCLRS.18
      DO 10 I=1,13                                                               DFCLRS.19
      CALL GSCR(1,I,COLOR(1,I),COLOR(2,I),COLOR(3,I))                            DFCLRS.20
  10  CONTINUE                                                                   DFCLRS.21
#endif                                                                           DFCLRS.22
      RETURN                                                                     DFCLRS.23
      END                                                                        DFCLRS.24
