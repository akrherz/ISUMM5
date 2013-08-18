/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/
# define TA 2
# define TAND 3
# define TANGLEEXP 4
# define TARITHIF 5
# define TASGOTO 6
# define TASSIGN 7
# define TAUTOMATIC 8
# define TBACKSPACE 9
# define TBITCON 10
# define TBLANK 11
# define TBLOCK 12
# define TBN 13
# define TBYTE 14
# define TBZ 15
# define TCALL 16
# define TCHARACTER 17
# define TCLOSE 18
# define TCOLON 19
# define TCOMMA 20
# define TCOMMENT 21
# define TCOMMON 22
# define TCOMPGOTO 23
# define TCOMPLEX 24
# define TCONCAT 25
# define TCONTCARD 26
# define TCONTINUE 27
# define TCURRENCY 28
# define TD 29
# define TDATA 30
# define TDCOMPLEX 31
# define TDCON 32
# define TDIMENSION 33
# define TDO 34
# define TDOT 35
# define TDOUBLE 36
# define TDOWHILE 37
# define TE 38
# define TELSE 39
# define TELSEIF 40
# define TEMPTYPAR 41
# define TENCODE 42
# define TDECODE 43
# define TEND 44
# define TENDDO 45
# define TENDFILE 46
# define TENDIF 47
# define TENDMAP 48
# define TENDSTRUCT 49
# define TENDUNION 50
# define TENTRY 51
# define TEOF 52
# define TEOS 53
# define TEQ 54
# define TEQUALS 55
# define TEQUIV 56
# define TEQV 57
# define TEXTERNAL 58
# define TF 59
# define TFALSE 60
# define TFIELD 61
# define TFMTA 62
# define TFMTB 63
# define TFMTD 64
# define TFMTE 65
# define TFMTF 66
# define TFMTG 67
# define TFMTH 68
# define TFMTI 69
# define TFMTL 70
# define TFMTP 71
# define TFMTS 72
# define TFMTT 73
# define TFMTX 74
# define TFORMAT 75
# define TFUNCTION 76
# define TG 77
# define TGE 78
# define TGOTO 79
# define TGT 80
# define THEXCON 81
# define THOLLERITH 82
# define TI 83
# define TICON 84
# define TIF 85
# define TIMPLICIT 86
# define TINCLUDE 87
# define TINQUIRE 88
# define TINTEGER 89
# define TINTRINSIC 90
# define TL 91
# define TLABEL 92
# define TLANGLE 93
# define TLE 94
# define TLET 95
# define TLETTER 96
# define TLOGICAL 97
# define TLOGIF 98
# define TLPAR 99
# define TLT 100
# define TMAP 101
# define TMINUS 102
# define TNAME 103
# define TNAMEEQ 104
# define TNAMELIST 105
# define TNE 106
# define TNEQV 107
# define TNONE 108
# define TNOT 109
# define TO 110
# define TOCTCON 111
# define TOPEN 112
# define TOR 113
# define TP 114
# define TPARAM 115
# define TPAUSE 116
# define TPERCENT 117
# define TPLUS 118
# define TPOINTER 119
# define TPOWER 120
# define TPRINT 121
# define TPROGRAM 122
# define TPUNCH 123
# define TQ 124
# define TRANGLE 125
# define TRCON 126
# define TREAD 127
# define TREAL 128
# define TRECORD 129
# define TRETURN 130
# define TREWIND 131
# define TRPAR 132
# define TS 133
# define TSAVE 134
# define TSCALE 135
# define TSLASH 136
# define TSOUGHTLABEL 137
# define TSP 138
# define TSS 139
# define TSTAR 140
# define TSTATIC 141
# define TSTOP 142
# define TSTRING 143
# define TSTRUCT 144
# define TSUBROUTINE 145
# define TT 146
# define TTHEN 147
# define TTL 148
# define TTO 149
# define TTR 150
# define TTRUE 151
# define TUNDEFINED 152
# define TUNION 153
# define TUNKNOWN 154
# define TWHILE 155
# define TWRITE 156
# define TX 157
# define TXOR 158
# define TZ 159

typedef union  {
    NodePtr node ;
} YYSTYPE;
extern YYSTYPE yylval;
# define TDUMMY 415
# define THIGHEST 416
