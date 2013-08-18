C M4 Macro definitions 
define(COMM_3PT_NE,
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(ne,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_3PT_SW,
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(sw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_4PT_STAR,
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_5PT,
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(sw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(se,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_9PT,
      call rsl_build_message(n2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(sw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s2,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_8PT_SQ,
      call rsl_build_message(nw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(ne,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(sw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(se,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_8PT_STAR,
      call rsl_build_message(n2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s2,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_9PT_STAR,
      call rsl_build_message(n2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(sw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s2,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_10PT_STAR,
      call rsl_build_message(n2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(nw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(ne,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(sw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s2,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
define(COMM_12PT_STAR,
      call rsl_build_message(n2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(nw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(n1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(ne,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(w1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(e2,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(sw,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s1,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(se,RSL_REAL,$1,$2,decomp,glen$3,llen$3)
      call rsl_build_message(s2,RSL_REAL,$1,$2,decomp,glen$3,llen$3))
CCCCCCCCCCC
C patch Mar 5, 1998 to optimize stencil comms on some stencils
C this can be removed once RSL is updated
#define PERF_PATCH


      subroutine define_comms(inest)
      implicit none

#     include <rsl.inc>
#     include <rslcom.inc>
#     include <parame.incl>
#     include <param2.incl>
#     include <addr0.incl>
#     include <point2d.incl>
#     include <point3d.incl>
#     include <dusolve1.incl>
#define FULL_DECLARE
#     include <mpp_dusolve3.incl>
#     include <various.incl>
#     include <nonhyd.incl>
#     include <nhtens.incl>
#     include <nhcnst.incl>
#     include <nonhydb.incl>
#     include <varia.incl>
#     include <navypb.incl>
#     include <fddagd.incl>
#     include <rpstar.incl>
#     include <soil.incl>
#     include <soilp.incl>
#     include <pbltke.incl>

      REAL  PR1, RHO1
      COMMON /STATEVAR/PR1(MIX,MJX,MKX), RHO1(MIX,MJX,MKX)
      REAL TBP(MIXNH,MJXNH,MKXNH)
      COMMON /STATEV2/TBP
C F is changed to F1 to avoid collision with an included definition of a different F
      REAL E,F1,ESTORE,CSNEST
      COMMON /SOUNDL/E(MIX,MJX,KXP1),F1(MIX,MJX,KXP1),
     $               ESTORE(-3:MIX+4,-3:MJX+4),
     $               CSNEST(20)

C added for v2.r6
      REAL THA
      DIMENSION THA(MIX,MJX,MKX)
      COMMON /THA_COM/ THA

      INTEGER ICOARS,NUMNES,MSIZE
      COMMON /MP_INITNEST/ ICOARS,NUMNES,MSIZE

      integer did
c arrays used by rsl message definition routines
      integer msg,messages(168)
      integer n1,s1,e1,w1,n2,s2,e2,w2
      integer nw,ne,sw,se
      integer llens(2)

      integer inest, icrs
      logical domoist

      integer i, k, in

#include <packdown.incl>

      CALL ADDRX1C(IAXALL(1,INEST))
      CALL MP_INITDOMAIN(INEST)

c build a set of messages and a stencil

      did = (domains(inest))
      decomp(1) = RSL_NORTHSOUTH                
      decomp(2) = RSL_EASTWEST
      decomp(3) = RSL_NOTDECOMPOSED             
      llen(1) = mix                             
      llen(2) = mjx                             
      llen(3) = mkx                             
      glen(1) = il
      glen(2) = jl
      glen(3) = mkx

      llenk(1) = mix                             
      llenk(2) = mjx                             
      llenk(3) = mkx+1
      glenk(1) = il
      glenk(2) = jl
      glenk(3) = mkx+1

      llens(1) = mix+8
      llens(2) = mjx+8

C Define Message for initial code
      call rsl_create_message(msg)            



      call rsl_build_message(msg,RSL_REAL,ps0,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xmuu(1,1,inest),
     $                       2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xnuu(1,1,inest),
     $                       2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xmut(1,1,inest),
     $                       2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xnut(1,1,inest),
     $                       2,decomp,glen,llen)
C this ordering is from point2d
      call rsl_build_message(msg,RSL_REAL,psa,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,psb,2,decomp,glen,llen)

      call rsl_build_message(msg,RSL_REAL,rpsa,2,decomp,glen,llen) 
      call rsl_build_message(msg,RSL_REAL,rpsb,2,decomp,glen,llen) 
      call rsl_build_message(msg,RSL_REAL,rpsc,2,decomp,glen,llen) 

      call rsl_build_message(msg,RSL_REAL,pdota,2,decomp,glen,llen)   ! added r9
      call rsl_build_message(msg,RSL_REAL,pdotb,2,decomp,glen,llen)   ! added r9
      call rsl_build_message(msg,RSL_REAL,rpdota,2,decomp,glen,llen)  ! added r9
      call rsl_build_message(msg,RSL_REAL,rpdotb,2,decomp,glen,llen)  ! added r9

      call rsl_build_message(msg,RSL_REAL,dpsdxm,2,decomp,glen,llen)  ! added v3
      call rsl_build_message(msg,RSL_REAL,dpsdym,2,decomp,glen,llen)  ! added v3
      call rsl_build_message(msg,RSL_REAL,dmdy,2,decomp,glen,llen)  ! added v3
      call rsl_build_message(msg,RSL_REAL,dmdx,2,decomp,glen,llen)  ! added v3
      call rsl_build_message(msg,RSL_REAL,ex,2,decomp,glen,llen)  ! added v3
      call rsl_build_message(msg,RSL_REAL,srx,2,decomp,glen,llen)  ! added v3
      call rsl_build_message(msg,RSL_REAL,crx,2,decomp,glen,llen)  ! added v3

      call rsl_build_message(msg,RSL_REAL,rainc,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,rainnc,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,f,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,msfx,2,decomp,glen,llen)  
      call rsl_build_message(msg,RSL_REAL,msfd,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,ht,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,xlat,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xlong,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xland,2,decomp,glen,llen)  ! added 980120
      call rsl_build_message(msg,RSL_REAL,alb,2,decomp,glen,llen)    ! added 980120
      call rsl_build_message(msg,RSL_REAL,thc,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,shc,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,znt,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,tmn,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,tss,2,decomp,glen,llen)    ! added 19991220
      call rsl_build_message(msg,RSL_REAL,mavail,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,emiss,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,zol,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,hol,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,mol,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,pbl,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,regime,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,hfx,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,qfx,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,ust,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,prw,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,tga,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,tgb,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,satbrt,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,psaini,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,rainp,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,gsw,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,glw,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,capg,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,snowc,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,ef,2,decomp,glen,llen)    ! added 980121
      call rsl_build_message(msg,RSL_REAL,cosrot,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,sinrot,2,decomp,glen,llen)
cjm added 1/16/96  -- this takes care of the "jone problem" in solve3 that
cjm made it necessary to never decompose boundary on a nest.
      call rsl_build_message(msg,RSL_REAL,t0,3,decomp,glen,llen)

C Moved from sten_b to sten_init in release 9 because these are now being
C initialized in rdinit and in initnest.  JM 980430
      call rsl_build_message(msg,RSL_REAL,pr0,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,rho0,3,decomp,glen,llen)
C added 20030718 - Cb
      if (iexsi.ge.1) then
        call rsl_build_message(msg,RSL_REAL,seaice,2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,tgsi,2,decomp,glen,llen)
      endif

      call rsl_build_message(msg,RSL_REAL,ua,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,va,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,ub,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,vb,3,decomp,glen,llen)

      do i = 1,24
        messages(i) = msg
      enddo

      call rsl_create_stencil(sten_init(inest))
      call rsl_describe_stencil(did,sten_init(inest),RSL_24PT,messages)


C BUILD STEN_A
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C STEN_A
C
C        8 PT SQ   8 PT STAR   9 PT STAR     12 PT STAR      24 PT
C                      +            +             +       + + + + +
C         + + +        +            +           + + +     + + + + +
C         + * +    + + * + +    + + * + +     + + + + +   + + * + +
C         + + +        +          + +           + + +     + + + + +
C                      +            +             +       + + + + +
C
C ua                                                          x        ! extra pts for bdyval
C va                                                          x        ! extra pts for bdyval
C ub                                                          x
C vb                                                          x
C ta                                                          x
C tb                                                          x
C wa                                                          x
C wb                                                          x
C ppa                                                         x
C ppb                                                         x
C qva                                                         x
C qvb                                                         x
C
C qca                                                         x
C qcb                                                         x
C qra                                                         x
C qrb                                                         x
C
C qia                                                         x
C qib                                                         x
C qnia                                                        x
C qnib                                                        x
C
C qga                                                         x
C qgb                                                         x
C qnca                                                        x
C qncb                                                        x
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      call rsl_create_message(msg)            

      call rsl_build_message(msg,RSL_REAL,ua,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,va,3,decomp,glen,llen)

      call rsl_build_message(msg,RSL_REAL,ub,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,vb,3,decomp,glen,llen)

      call rsl_build_message(msg,RSL_REAL,ta,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,tb,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,wa,3,decomp,glenk,llenk)
      call rsl_build_message(msg,RSL_REAL,wb,3,decomp,glenk,llenk)
      call rsl_build_message(msg,RSL_REAL,ppa,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,ppb,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,qva,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,qvb,3,decomp,glen,llen)
      IF(IMOIST(INEST).EQ.2)THEN
        call rsl_build_message(msg,RSL_REAL,qca,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qcb,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qra,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qrb,3,decomp,glen,llen)
        IF(IMPHYS(INEST).GE.5)THEN
          call rsl_build_message(msg,RSL_REAL,qia,3,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,qib,3,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,qnia,3,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,qnib,3,decomp,glen,llen)
          IF(IMPHYS(INEST).GE.6)THEN
            call rsl_build_message(msg,RSL_REAL,qga,3,decomp,glen,llen)
            call rsl_build_message(msg,RSL_REAL,qgb,3,decomp,glen,llen)
            call rsl_build_message(msg,RSL_REAL,qnca,3,decomp,glen,llen)
            call rsl_build_message(msg,RSL_REAL,qncb,3,decomp,glen,llen)
          ENDIF
        ENDIF
      ENDIF
  
      messages(1) =    msg
      messages(2) =          msg
      messages(3) =                 msg
      messages(4) =                        msg
      messages(5) =                               msg
      messages(6) =    msg
      messages(7) =          msg
      messages(8) =                 msg
      messages(9) =                        msg
      messages(10) =                              msg
      messages(11) =   msg
      messages(12) =         msg
      messages(13) =                       msg
      messages(14) =                              msg
      messages(15) =   msg
      messages(16) =         msg
      messages(17) =                msg
      messages(18) =                       msg
      messages(19) =                              msg
      messages(20) =   msg
      messages(21) =         msg
      messages(22) =                msg
      messages(23) =                       msg
      messages(24) =                              msg

      call rsl_create_stencil(sten_a(inest))
      call rsl_describe_stencil(did,sten_a(inest),RSL_24PT,messages)

 
C BUILD STEN_B
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C STEN_B
C
C          SW                                                      
C                      +            +             +       + + + + +
C                      +            +           + + +     + + + + +
C         + *      + + * + +    + + * + +     + + + + +   + + * + +
C         + +          +          + +           + + +     + + + + +
C                      +            +             +       + + + + +
C
C ucd      x
C vcd      x
C u3dten    x
C v3dten    x
C
C this exists to put the cross point wind tendencies from pbl back
C onto the dot grid.  At the point that sten_b is called, ucd and vcd
C contain these tendencies
      call rsl_create_message(msg)            
      call rsl_build_message(msg,RSL_REAL,ucd,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,vcd,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,u3dten,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,v3dten,3,decomp,glen,llen)
      messages(1) =    RSL_INVALID
      messages(2) =          RSL_INVALID
      messages(3) =                 RSL_INVALID
      messages(4) =    msg
      messages(5) =                 RSL_INVALID
      messages(6) =    msg
      messages(7) =          msg
      messages(8) =                 RSL_INVALID
      call rsl_create_stencil(sten_b(inest))
      call rsl_describe_stencil(did,sten_b(inest),RSL_8PT,messages)

C BUILD STEN_C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C STEN_C
C
C          SW                                                      
C                      +            +             +       + + + + +
C                      +            +           + + +     + + + + +
C         + *      + + * + +    + + * + +     + + + + +   + + * + +
C         + +          +          + +           + + +     + + + + +
C                      +            +             +       + + + + +
C
C u3dten    x
C v3dten    x
C
C this exists to put the cross point wind tendencies from pbl back
C onto the dot grid.  At the point that sten_b is called, ucd and vcd
C contain these tendencies
      call rsl_create_message(msg)            
      call rsl_build_message(msg,RSL_REAL,u3dten,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,v3dten,3,decomp,glen,llen)
      messages(1) =    RSL_INVALID
      messages(2) =          RSL_INVALID
      messages(3) =                 RSL_INVALID
      messages(4) =    msg
      messages(5) =                 RSL_INVALID
      messages(6) =    msg
      messages(7) =          msg
      messages(8) =                 RSL_INVALID
      call rsl_create_stencil(sten_c(inest))
      call rsl_describe_stencil(did,sten_c(inest),RSL_8PT,messages)

C BUILD STEN_D
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C STEN_D
C
C        4 PT STAR   8 PT STAR
C                        +
C           +            +
C         + * +      + + * + +
C           +            +
C                        +
C
C tkea      x             
C tkeb                   x
C
C this exists for TKE calculation in Gayno-Seaman PBL
      call rsl_create_message(n1)
      call rsl_create_message(n2)
      call rsl_create_message(w1)
      call rsl_create_message(w2)
      call rsl_create_message(e1)
      call rsl_create_message(e2)
      call rsl_create_message(s1)
      call rsl_create_message(s2)
      COMM_4PT_STAR(tkea,3)
      COMM_8PT_STAR(tkeb,3)

      messages(1) =                 n2
      messages(2) =     RSL_INVALID
      messages(3) =                 n1
      messages(4) =                        RSL_INVALID 
      messages(5) =    w2
      messages(6) =          w1
      messages(7) =                        e1
      messages(8) =                               e2
      messages(9) =     RSL_INVALID
      messages(10) =                 s1
      messages(11) =                       RSL_INVALID
      messages(12) =                 s2

      call rsl_create_stencil( sten_d(inest) )
      call rsl_describe_stencil(did,sten_d(inest),RSL_12PT,messages)

C Define Message preceding sounda
      call rsl_create_message(w1)
      call rsl_create_message(sw)
      call rsl_create_message(s1)

      COMM_3PT_SW(pp3d,3)
      COMM_3PT_SW(t3d,3)

      messages(1) =          RSL_INVALID
      messages(2) =                 RSL_INVALID
      messages(3) =                        RSL_INVALID
      messages(4) =          w1
      messages(5) =                        RSL_INVALID
      messages(6) =          sw
      messages(7) =                 s1
      messages(8) =                        RSL_INVALID

      call rsl_create_stencil(sten_sa(inest))
      call rsl_describe_stencil(did,sten_sa(inest),RSL_8PT,messages)

C sjh Define Message preceding soundb
      call rsl_create_message(n1)            
      call rsl_create_message(ne)            
      call rsl_create_message(e1)            

      COMM_3PT_NE(u3d,3)
      COMM_3PT_NE(v3d,3)

      messages(1) =          RSL_INVALID
      messages(2) =                 n1
      messages(3) =                        ne
      messages(4) =          RSL_INVALID
      messages(5) =                        e1
      messages(6) =          RSL_INVALID
      messages(7) =                 RSL_INVALID
      messages(8) =                        RSL_INVALID

      call rsl_create_stencil(sten_sb(inest))
      call rsl_describe_stencil(did,sten_sb(inest),RSL_8PT,messages)

C sten_upr
      call rsl_create_message(msg)
      call rsl_build_message(msg,RSL_REAL,
     $                       estore(1,1),
     $                       2,decomp,glen,llens)
      do i = 1,168
        messages(i) = msg
      enddo
      call rsl_create_stencil(sten_upr(inest))
      call rsl_describe_stencil(did,sten_upr(inest),RSL_168PT,messages)

C sten_sint_init   ---------------------------------
      call rsl_create_message(msg)
      call rsl_build_message(msg,RSL_REAL,ht,2,decomp,glen,llen)
      if ( inhyd.eq.0 ) then
        write(0,*)'HYDRO NOT SUPPORTED IN MPP CODE YET'
        stop
      else
        call rsl_build_message(msg,RSL_REAL,ps0,2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,t0,3,decomp,glen,llen)
      endif
      call rsl_build_message(msg,RSL_REAL,psa,2,decomp,glen,llen)
C hscr1 contains psa_dot in initnest; be careful -- hscr1 and hscr2
C are the same memory for all nests.
      call rsl_build_message(msg,RSL_REAL,hscr1,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,hscr2,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,msfx,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,msfd,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,f,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xlat,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,xlong,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,snowc,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,tga,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,tmn,2,decomp,glen,llen)
C added 19991220
      call rsl_build_message(msg,RSL_REAL,tss,2,decomp,glen,llen)
C added 20030718 - Cb
      if (iexsi.ge.1) then
        call rsl_build_message(msg,RSL_REAL,seaice,2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,tgsi,2,decomp,glen,llen)
      endif
C added 980121
      call rsl_build_message(msg,RSL_REAL,rainc,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,rainnc,2,decomp,glen,llen)
C added 000901
      call rsl_build_message(msg,RSL_REAL,mavail,2,decomp,glen,llen)
c 3d fields
      call rsl_build_message(msg,RSL_REAL,ta,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,qva,3,decomp,glen,llen)
c are we doing imoist on *any* nest?
      domoist = .false.
      do in = 1, maxnes
         if ( imoist(in) .eq. 2 ) domoist = .true.
      enddo
      if (domoist) then
        call rsl_build_message(msg,RSL_REAL,qca,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qra,3,decomp,glen,llen)
      endif
      if ( iice .eq. 1 ) then
        call rsl_build_message(msg,RSL_REAL,qia,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qnia,3,decomp,glen,llen)
        if ( iiceg .eq. 1 ) then
          call rsl_build_message(msg,RSL_REAL,qga,3,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,qnca,3,decomp,glen,llen)
        endif
      endif
      if ( inav .eq. 1 ) then
        call rsl_build_message(msg,RSL_REAL,turke,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pqwsb,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlsb,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlqwb,3,decomp,glen,llen)
      endif
      if ( ibltyp(inest) .eq. 6 ) then
        call rsl_build_message(msg,RSL_REAL,tkea,3,decomp,glenk,llenk)
        call rsl_build_message(msg,RSL_REAL,tkeb,3,decomp,glenk,llenk)
      endif
      if ( isoil .eq. 1 ) then
        do k=1,mlx
          call rsl_build_message(msg,RSL_REAL,tsla(1,1,k),
     +                           2,decomp,glen,llen)
        enddo
      endif
      if ( isoil .eq. 2 ) then
        call rsl_build_message(msg,RSL_REAL,VEGFRC,
     +                         2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,XVEGMN,
     +                         2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,CMCA,
     +                         2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,SNOA,
     +                         2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,SNOWHA,
     +                         2,decomp,glen,llen)
        do k=1,mlx
          call rsl_build_message(msg,RSL_REAL,STCA(1,1,k),
     +                           2,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,SMCA(1,1,k),
     +                           2,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,SWCA(1,1,k),
     +                           2,decomp,glen,llen)
        enddo
      endif
      if ( inhyd.eq.1 ) then
        call rsl_build_message(msg,RSL_REAL,ppa,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,wa,3,decomp,glenk,llenk)
      endif
      call rsl_build_message(msg,RSL_REAL,ua,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,va,3,decomp,glen,llen)

c IOR is set in the makefile
#if (IOR == 1)
      do i = 1, 8
        messages(i) = msg
      enddo
      call rsl_create_stencil(sten_sint_init(inest))
      call rsl_describe_stencil( did,sten_sint_init(inest),
     +                           RSL_8PT,messages)
#else
#if (IOR == 2)
      do i = 1, 24
        messages(i) = msg
      enddo
      call rsl_create_stencil(sten_sint_init(inest))
      call rsl_describe_stencil( did,sten_sint_init(inest),
     +                           RSL_24PT,messages)
#else
      if(rsl_myproc.eq.0)write(0,*)'IOR not defined in the compile.  Stopping.'
      stop
#endif
#endif

c
c sten_sint   ---------------------------------
c
      call rsl_create_message( msg )
c 2d fields
      call rsl_build_message(msg,RSL_REAL,ht,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,psa,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,hscr1,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,hscr2,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,hscr3,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,snowc,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,tga,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,tmn,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,rainc_sav,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,rainnc_sav,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,mavail_sav,2,decomp,glen,llen)
C added 20030718 - Cb
      if (iexsi.ge.1) then
        call rsl_build_message(msg,RSL_REAL,seaice,2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,tgsi,2,decomp,glen,llen)
      endif
C added 20001025
      if ( isoil .eq. 2 .or. isoil .eq. 3 ) then
        do k=1,mlx
          call rsl_build_message(msg,RSL_REAL,stca_sav(1,1,k),
     +                           2,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,smca_sav(1,1,k),
     +                           2,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,swca_sav(1,1,k),
     +                           2,decomp,glen,llen)
        enddo
      endif
c 3d fields
      call rsl_build_message(msg,RSL_REAL,ta,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,ta_sav(1,1,1,inest)
     $                       ,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,qva,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,qva_sav(1,1,1,inest)
     $                       ,3,decomp,glen,llen)
c are we doing imoist on *any* nest?
      domoist = .false.
      do in = 1, maxnes
         if ( imoist(in) .eq. 2 ) domoist = .true.
      enddo
      if (domoist) then
        call rsl_build_message(msg,RSL_REAL,qca,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qca_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qra,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qra_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
      endif
      if (iice .eq. 1) then
        call rsl_build_message(msg,RSL_REAL,qia,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qia_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qnia,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qnia_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
        if (iiceg .eq. 1) then
        call rsl_build_message(msg,RSL_REAL,qga,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qga_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qnca,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qnca_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
        endif
      endif
      if ( ibltyp(inest) .eq. 6 ) then
        call rsl_build_message(msg,RSL_REAL,tkea,3,decomp,glenk,llenk)
        call rsl_build_message(msg,RSL_REAL,tkea_sav(1,1,1)
     $                        ,3,decomp,glenk,llenk)
      endif
      if ( inhyd.eq.1 ) then
        call rsl_build_message(msg,RSL_REAL,ppa,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,ppa_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,wa,3,decomp,glenk,llenk)
        call rsl_build_message(msg,RSL_REAL,wa_sav(1,1,1,inest)
     $                        ,3,decomp,glenk,llenk)
      endif
      call rsl_build_message(msg,RSL_REAL,ua,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,ua_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,va,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,va_sav(1,1,1,inest)
     $                        ,3,decomp,glen,llen)

c IOR is set in the makefile
#if (IOR == 1)
      do i = 1, 8
        messages(i) = msg
      enddo
      call rsl_create_stencil(sten_sint(inest))
      call rsl_describe_stencil(did,sten_sint(inest),RSL_8PT,messages)
#else
#if (IOR == 2)
      do i = 1, 24
        messages(i) = msg
      enddo
      call rsl_create_stencil(sten_sint(inest))
      call rsl_describe_stencil(did,sten_sint(inest),RSL_24PT,messages)
#else
      if(rsl_myproc.eq.0)write(0,*)'Define_data3: IOR not defined.'
      stop
#endif
#endif

c
c sten_sint_lbdy   ---------------------------------
c
      call rsl_create_message( msg )
c 2d fields
      call rsl_build_message(msg,RSL_REAL,tss,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,tmn,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,snowc,2,decomp,glen,llen)
      if ( isoil .ge. 2 ) then
       call rsl_build_message(msg,RSL_REAL,xice,2,decomp,glen,llen)
      endif
C added 20030718 - Cb
      if (iexsi.ge.1) then
        call rsl_build_message(msg,RSL_REAL,seaice,2,decomp,glen,llen)
      endif
      do i = 1, 24
        messages(i) = msg
      enddo
      call rsl_create_stencil(sten_sint_lbdy(inest))
      call rsl_describe_stencil(did,sten_sint_lbdy(inest),RSL_24PT,
     +                          messages)

c
c sten_movedot_c   ---------------------------
c
      call rsl_create_message( msg )
      call rsl_build_message( msg,RSL_REAL,ucd,3,decomp,glen,llen )
      call rsl_build_message( msg,RSL_REAL,vcd,3,decomp,glen,llen )
      call rsl_build_message( msg,RSL_REAL,ucc,3,decomp,glen,llen )
      call rsl_build_message( msg,RSL_REAL,vcc,3,decomp,glen,llen )
      messages( 1) =          RSL_INVALID
      messages( 2) =                 RSL_INVALID
      messages( 3) =                        msg
      messages( 4) =          RSL_INVALID
      messages( 5) =                        RSL_INVALID
      messages( 6) =          RSL_INVALID
      messages( 7) =                 RSL_INVALID
      messages( 8) =                        RSL_INVALID
      call rsl_create_stencil( sten_movedot_c(inest) )
      call rsl_describe_stencil(did,
     +           sten_movedot_c(inest),RSL_8PT,messages)



#if 0
C sten_movedot_ini
      call rsl_create_message(msg)
      call rsl_build_message(msg,RSL_REAL,tmp,2,decomp,glen,llen)
      do i = 1,24
        messages(i) = msg
      enddo
      call rsl_create_stencil(sten_movedot_ini(inest))
      call rsl_describe_stencil(did,
     +        sten_movedot_ini(inest),RSL_24PT,messages)
#endif

#if 1
c
c sten_smth_ns   ---------------------------------
c
      call rsl_create_message( msg )
      call rsl_build_message(msg,RSL_REAL,pp3d,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,w3d,3,decomp,glenk,llenk)

      call rsl_build_message(msg,RSL_REAL,t3d,3,decomp,glen,llen)
      if ( idry(1) .eq. 0 ) then
       call rsl_build_message(msg,RSL_REAL,qv3d,3,decomp,glen,llen)
       if ( domoist ) then
        call rsl_build_message(msg,RSL_REAL,qr3d,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qc3d,3,decomp,glen,llen)
        if ( iice .eq. 1 ) then
          call rsl_build_message(msg,RSL_REAL,qi3d,3,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,qni3d,3,decomp,glen,llen)
          if ( iiceg .eq. 1 ) then
            call rsl_build_message(msg,RSL_REAL,qg3d,3,decomp,glen,llen)
            call rsl_build_message(msg,RSL_REAL,qnc3d,3,decomp,
     +                             glen,llen)
          endif
        endif
       endif
      endif
      if(inav.eq.1)then
        call rsl_build_message(msg,RSL_REAL,turke_sav,
     +    3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlsb_sav,
     +    3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pqwsb_sav,
     +    3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlqwb_sav,
     +    3,decomp,glen,llen)
      endif
C Add TKEA to stencils
      if ( ibltyp(inest) .eq. 6 ) then
        call rsl_build_message(msg,RSL_REAL,tkea_sav(1,1,1)
     $                        ,3,decomp,glenk,llenk)
      endif

      call rsl_build_message(msg,RSL_REAL,u3d,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,v3d,3,decomp,glen,llen)

      messages( 1) =          msg
      messages( 2) = RSL_INVALID
      messages( 3) =                        RSL_INVALID
      messages( 4) =          msg
      call rsl_create_stencil( sten_smth_ns(inest) )
      call rsl_describe_stencil( did, sten_smth_ns(inest),
     $                           RSL_4PT, messages )
#endif

#if 0
c
c sten_smth_3   ---------------------------------
c               added to support ifeed=3
c
      call rsl_create_message( msg )
      call rsl_build_message(msg,RSL_REAL,ppa,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,wa,3,decomp,glenk,llenk)

      call rsl_build_message(msg,RSL_REAL,ta,3,decomp,glen,llen)
      if ( idry(1) .eq. 0 ) then
       call rsl_build_message(msg,RSL_REAL,qva,3,decomp,glen,llen)
       if ( domoist ) then
        call rsl_build_message(msg,RSL_REAL,qra,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qca,3,decomp,glen,llen)
        if ( iice .eq. 1 ) then
          call rsl_build_message(msg,RSL_REAL,qia,3,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,qnia,3,decomp,glen,llen)
          if ( iiceg .eq. 1 ) then
            call rsl_build_message(msg,RSL_REAL,qga,3,decomp,glen,llen)
            call rsl_build_message(msg,RSL_REAL,qnca,3,decomp,glen,llen)
          endif
        endif
       endif
      endif
      if(inav.eq.1)then
        call rsl_build_message(msg,RSL_REAL,turke,
     +    3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlsb,
     +    3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pqwsb,
     +    3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlqwb,
     +    3,decomp,glen,llen)
      endif
C Add TKEA to stencils
      if ( ibltyp(inest) .eq. 6 ) then
        call rsl_build_message(msg,RSL_REAL,tkea,3,decomp,glenk,llenk)
      endif
      call rsl_build_message(msg,RSL_REAL,ua,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,va,3,decomp,glen,llen)

      messages( 1) = msg
      messages( 2) =          msg
      messages( 3) =                    msg
      messages( 4) = msg
      messages( 5) =                    msg
      messages( 6) = msg
      messages( 7) =          msg
      messages( 8) =                    msg

      call rsl_create_stencil( sten_smth_3(inest) )
      call rsl_describe_stencil( did, sten_smth_3(inest),
     $                           RSL_8PT, messages )
#endif


c
c sten_rainfbk ---------------------------------
c
      call rsl_create_message(msg)
      call rsl_build_message(msg,RSL_REAL,rainc,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,rainnc,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,snowc,2,decomp,glen,llen)    ! added 020827 (Cb)
      if ( isoil .eq. 2 ) then
        call rsl_build_message(msg,RSL_REAL,snoa,2,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,snowha,2,decomp,glen,llen)
      endif
C added 2000/09/01 jm
      call rsl_build_message(msg,RSL_REAL,mavail,2,decomp,glen,llen)
      messages( 1) =          msg
      messages( 2) =                 msg
      messages( 3) =                        msg
      messages( 4) =          msg
      messages( 5) =                        msg
      messages( 6) =          msg
      messages( 7) =                 msg
      messages( 8) =                        msg
      call rsl_create_stencil( sten_rainfbk(inest) )
      call rsl_describe_stencil(did,
     +           sten_rainfbk(inest),RSL_8PT,messages)


cccccccccccccccccccccccccccccccccccccccccccccccccccc
c         added for fdda
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
      call rsl_create_message( msg )
      call rsl_build_message(msg,RSL_REAL,pso,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,psoc,2,decomp,glen,llen)
      messages( 1) = msg
      messages( 2) =          msg
      messages( 3) =                    msg
      messages( 4) = msg
      messages( 5) =                    msg
      messages( 6) = msg
      messages( 7) =          msg
      messages( 8) =                    msg
      call rsl_create_stencil( sten_fdda_0(inest) )
      call rsl_describe_stencil( did, sten_fdda_0(inest),
     +                           RSL_8PT, messages )

      call rsl_create_message( msg )
      call rsl_build_message(msg,RSL_REAL,mpsd,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,psod,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,psbd,2,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,ub,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,ubo,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,uboten,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,vb,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,vbo,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,vboten,3,decomp,glen,llen)
      messages( 1) = RSL_INVALID
      messages( 2) =          msg
      messages( 3) =                    msg
      messages( 4) = RSL_INVALID
      messages( 5) =                    msg
      messages( 6) = RSL_INVALID
      messages( 7) =          RSL_INVALID
      messages( 8) =                    RSL_INVALID

      call rsl_create_stencil( sten_fdda_1(inest) )
      call rsl_describe_stencil( did, sten_fdda_1(inest),
     +                           RSL_8PT, messages )

C stencil for OBJS nudging in ERROB
      call rsl_create_message( msg )
      call rsl_build_message(msg,RSL_REAL,ub,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,vb,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,tb,3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,qvb,3,decomp,glen,llen)
      messages(1) =                 msg
      messages(2) =          msg
      messages(3) =                 msg
      messages(4) =                        msg
      messages(5) =    msg
      messages(6) =          msg
      messages(7) =                        msg
      messages(8) =                               msg
      messages(9) =          msg
      messages(10) =                msg
      messages(11) =                       msg
      messages(12) =                msg
      call rsl_create_stencil( sten_fdobs(inest) )
      call rsl_describe_stencil( did, sten_fdobs(inest),
     +                           RSL_12PT, messages )

cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  end of additions for fdda
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
      return

C This one cannot be defined at the same time as the others because
C we do not know at the time of domain definition if a nest is 
C to be spawned within and which inest number it has.  We are using
C the nests save storage for this domains smoother (see mp_feedbk), since
C the nest is inactive at that time.  This is called at the time the nest
C is declared.

      entry define_smoother_sten(icrs,inest)
        print*,'ENTRY sten_smth_ew: INEST, ICRS ',INEST,ICRS
        print*,'       DECOMP,GLEN,LLEN',DECOMP,GLEN,LLEN
c
c sten_smth_ew   ---------------------------------
c
c    also used for IFEED=3 (as of R12)
c
      domoist = .false.
      do in = 1, maxnes
         if ( imoist(in) .eq. 2 ) domoist = .true.
      enddo

      call rsl_create_message( msg )
      call rsl_build_message(msg,RSL_REAL,ppa_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,wa_sav(1,1,1,inest),
     +  3,decomp,glenk,llenk)
      call rsl_build_message(msg,RSL_REAL,ta_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
      if ( idry(1) .eq. 0 ) then
       call rsl_build_message(msg,RSL_REAL,qva_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
       if ( domoist ) then
        call rsl_build_message(msg,RSL_REAL,qra_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,qca_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
        if ( iice .eq. 1 ) then
          call rsl_build_message(msg,RSL_REAL,qia_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
          call rsl_build_message(msg,RSL_REAL,qnia_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
          if ( iiceg .eq. 1 ) then
            call rsl_build_message(msg,RSL_REAL,qga_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
            call rsl_build_message(msg,RSL_REAL,qnca_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
          endif
        endif
       endif
      endif
      call rsl_build_message(msg,RSL_REAL,ua_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
      call rsl_build_message(msg,RSL_REAL,va_sav(1,1,1,inest),
     +  3,decomp,glen,llen)
      if(inav.eq.1)then
        call rsl_build_message(msg,RSL_REAL,turke,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlsb,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pqwsb,3,decomp,glen,llen)
        call rsl_build_message(msg,RSL_REAL,pthlqwb,3,decomp,glen,llen)
      endif
C Add TKEA to stencils
      if ( ibltyp(inest) .eq. 6 ) then
        call rsl_build_message(msg,RSL_REAL,tkea,3,decomp,glenk,llenk)
      endif

      messages( 1) = msg
      messages( 2) =          msg
      messages( 3) =                    msg
      messages( 4) = msg
      messages( 5) =                    msg
      messages( 6) = msg
      messages( 7) =          msg
      messages( 8) =                    msg
      call rsl_create_stencil( sten_smth_ew(icrs,inest) )
      call rsl_describe_stencil(domains(icrs),sten_smth_ew(icrs,inest),
     $                          RSL_8PT,messages)
      return
      end

