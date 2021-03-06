 ;
 ;      ************** NEST AND MOVING NEST OPTIONS ***************
 ;
 LEVIDN =  0,1,1,1,1,1,1,1,1,1,    ; level of nest for each domain
 NUMNC  =  1,1,1,1,1,1,1,1,1,1,    ; ID of mother domain for each nest
 NESTIX = 200,  34,  31,  46,  46,  46,  46,  46,  46,  46,  ; domain size i
 NESTJX = 250,  37,  31,  61,  61,  61,  61,  61,  61,  61,  ; domain size j
 NESTI  =   1,   8,  8,   1,   1,   1,   1,   1,   1,   1,   ; start location i
 NESTJ  =   1,   9,  9,   1,   1,   1,   1,   1,   1,   1,   ; start location i
 XSTNES =   0.,  0.,900.,  0.,  0.,  0.,  0.,  0.,  0.,  0., ; domain initiation
 XENNES =1440.,1440.,1440.,720.,720.,720.,720.,720.,720.,720.; domain termination
 IOVERW =   1,   1,   0,   0,   0,   0,   0,   0,   0,   0,  ; overwrite nest input
 ;          0=interpolate from coarse mesh (for nest domains);
 ;          1=read in domain initial conditions
 ;          2=read in nest terrain file
 IACTIV =   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,  ;
 ;          in case of restart: is this domain active?
 ;
 ;      ************* MOVING NEST OPTIONS ******************
 ;
 IMOVE  =   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; move domain 0,1
 IMOVCO =   1,   1,   1,   1,   1,   1,   1,   1,   1,   1, ; 1st move #
 ;          imovei(j,k)=L, ; I-INCREMENT MOVE (DOMAIN J, MOVE NUMBER K) IS L
 IMOVEI =   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; I move #1
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; I move #2
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; I move #3
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; I move #4
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; I move #5
 IMOVEJ =   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; J move #1
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; J move #2
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; J move #3
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; J move #4
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; J move #5
 IMOVET =   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; time of move #1
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; time of move #2
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; time of move #3
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; time of move #4
            0,   0,   0,   0,   0,   0,   0,   0,   0,   0, ; time of move #5
 IFEED  =  3,     ; no feedback; 9-pt weighted average; 1-pt feedback w/o smoothing / 
                  ; light smoothing / heavy smoothing - 0,1,2,3, and 4
