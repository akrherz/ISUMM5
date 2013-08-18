 ;
 ;
 ;      ************* FORECAST TIME AND TIME STEP ******************
 ;
 TIMAX  = 720.,         ; forecast length in minutes
 TISTEP = 60.,          ; coarse domain DT in model, use 3*DX
 ;
 ;      ************** OUTPUT/RESTART OPTIONS ***************
 ;
 IFREST = .FALSE.,      ; whether this is a restart
    IXTIMR  = 720,      ; restart time in minutes
 IFSAVE = .TRUE.,       ; save data for restart
    SVLAST = .FALSE.,   ; T: only save the last file for restart
                        ; F: save multiple files
    SAVFRQ = 360.,      ; how frequently to save data (in minutes)
 IFTAPE = 1,            ; model output: 0,1
    TAPFRQ = 180.,      ; how frequently to output model results (in minutes)
    BUFFRQ = 0.,        ; how frequently to split model output files (in minutes),
 ;                        ignored if < TAPFRQ
    INCTAP = 1,1,1,1,1,1,1,1,1,1,  ; multipliers of TAPFRQ for outputting
 IFSKIP = .FALSE.,      ; whether to skip input files - DO NOT use this for restart
    CDATEST = '1993-03-13_00:00:00', ; the DATE for the starting file
 IFPRT = 0,             ; sample print out: =1, a lot of print
 PRTFRQ = 720.,         ; Print frequency for sample output (in minutes)
 MASCHK = 180,          ; mass conservation check (KTAU or no. of time steps)
