C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .                              S T A P                              . 
C .                                                                   . 
C .            AN IN-CORE SOLUTION STATIC ANALYSIS PROGRAM            . 
C .                                                                   . 
C . . . . . . . . . . . . . .  . . .  . . . . . . . . . . . . . . . . .      
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /VAR/ NG,MODEX                                             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
C                                                                       
      DIMENSION TIM(5), HED(20)                                         
      DIMENSION IA(1)                                                   
      EQUIVALENCE (A(1),IA(1))                                          
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .   THE FOLLOWING TWO LINES ARE USED TO DETERMINE THE MAXIMUM HIGH  . 
C .   SPEED STORAGE THAT CAN BE USED FOR SOLUTION. TO CHANGE THE HIGH . 
C .   SPEED STORAGE AVAILABLE FOR EXECUTION, CHANGE THE VALUE OF MTOT . 
C .   AND CORRESPONDINGLY COMMON A(MTOT).                             . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON A(10000)                                                   
      MTOT=10000                                                        
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .   DOUBLE PRECISION LINE                                           . 
C .     ITWO = 1 SINGLE PRECISION ARITHMETIC                          . 
C .     ITWO = 2 DOUBLE PRECISION ARITHMETIC                          . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      ITWO=2                                                            
C                                                                       
C     THE FOLLOWING SCRATCH FILES ARE USED                              
C        IELMNT = UNIT STORING ELEMENT DATA                             
C        ILOAD  = UNIT STORING LOAD VECTORS                             
C        IIN    = UNIT USED FOR INPUT                                   
C        IOUT   = UNIT USED FOR OUTPUT                                  
C                                                                       
C     ON SOME MACHINES THESE FILES MUST BE EXPLICITLY OPENED            
C                                                                       
      IELMNT = 1                                                        
      ILOAD = 2                                                         
      IIN = 5                                                           
      IOUT = 6                                                          
C                                                                       
  200 NUMEST=0                                                          
      MAXEST=0                                                          
C     * * * * * * * * * * * * * * * * * * * * * *                       
C                                                                       
C     * * *   I N P U T   P H A S E   * * *                             
C                                                                       
C     * * * * * * * * * * * * * * * * * * * * * *                       
      CALL SECOND (TIM(1))                                              
C                                                                       
C     R E A D   C O N T R O L   I N F O R M A T I O N                   
C
      CALL OPFILE()
      READ (IIN,1000) HED,NUMNP,NUMEG,NLCASE,MODEX                      
      IF (NUMNP.EQ.0) GO TO 800                                         
      WRITE (IOUT,2000) HED,NUMNP,NUMEG,NLCASE,MODEX                    
C                                                                       
C                                                                       
C     R E A D   N O D A L   P O I N T   D A T A                         
C                                                                       
C                                                                       
      N1= 1                                                             
      N2=N1 + 3*NUMNP                                                   
      N2=(N2/2)*2 + 1                                                   
      N3=N2 + NUMNP*ITWO                                                
      N4=N3 + NUMNP*ITWO                                                
      N5=N4 + NUMNP*ITWO                                                
      IF (N5.GT.MTOT) CALL ERROR (N5-MTOT,1)                            
C                                                                       
      CALL INPUT (A(N1),A(N2),A(N3),A(N4),NUMNP,NEQ)                    
C                                                                       
      NEQ1=NEQ + 1                                                      
C                                                                       
C                                                                       
C     C A L C U L A T E   A N D   S T O R E   L O A D   V E C T O R S   
C                                                                       
C                                                                       
      N6=N5 + NEQ*ITWO                                                  
      WRITE (IOUT,2005)                                                 
C                                                                       
      REWIND ILOAD                                                      
C                                                                       
      DO 300 L=1,NLCASE                                                 
C                                                                       
      READ (IIN,1010) LL,NLOAD                                          
C                                                                       
      WRITE (IOUT,2010) LL,NLOAD                                        
      IF (LL.EQ.L) GO TO 310                                            
      WRITE (IOUT,2020)                                                 
      GO TO 800                                                         
  310 CONTINUE                                                          
C                                                                       
      N7=N6 + NLOAD                                                     
      N8=N7 + NLOAD                                                     
      N9=N8 + NLOAD*ITWO                                                
C                                                                       
      IF (N9.GT.MTOT) CALL ERROR (N9-MTOT,2)                            
C                                                                       
      CALL LOADS (A(N5),A(N6),A(N7),A(N8),A(N1),NLOAD,NEQ)              
C                                                                       
  300 CONTINUE                                                          
C                                                                       
C                                                                       
C     R E A D , G E N E R A T E   A N D   S T O R E                     
C     E L E M E N T   D A T A                                           
C                                                                       
C     CLEAR STORAGE                                                     
C                                                                       
      N6=N5 + NEQ                                                       
      N6=(N6/2)*2 + 1                                                   
      DO 10 I=N5,N6                                                     
   10 IA(I)=0                                                           
      IND=1                                                             
C                                                                       
      CALL ELCAL                                                        
C                                                                       
      CALL SECOND (TIM(2))                                              
C     * * * * * * * * * * * * * * * * * * * * * *                       
C                                                                       
C     * * *   S O L U T I O N   P H A S E   * * *                       
C                                                                       
C     * * * * * * * * * * * * * * * * * * * * * *                       
C                                                                       
C     A S S E M B L E   S T I F F N E S S   M A T R I X                 
C                                                                       
C                                                                       
      CALL ADDRES (A(N2),A(N5))                                         
C                                                                       
      MM=NWK/NEQ                                                        
      N3=N2 + NEQ + 1                                                   
      N3=(N3/2)*2 + 1                                                   
      N4=N3 + NWK*ITWO                                                  
      N5=N4 + NEQ*ITWO                                                  
      N6=N5 + MAXEST                                                    
      IF (N6.GT.MTOT) CALL ERROR (N6-MTOT,4)                            
C                                                                       
C     WRITE TOTAL SYSTEM DATA                                           
C                                                                       
      WRITE (IOUT,2025) NEQ,NWK,MK,MM                                   
C                                                                       
C     IN DATA CHECK ONLY MODE WE SKIP ALL FURTHER CALCULATIONS          
C                                                                       
      IF (MODEX.GT.0) GO TO 100                                         
      CALL SECOND (TIM(3))                                              
      CALL SECOND (TIM(4))                                              
      CALL SECOND (TIM(5))                                              
      GO TO 120                                                         
C                                                                       
C     CLEAR STORAGE                                                     
C                                                                       
  100 NNL=NWK + NEQ                                                     
      CALL CLEAR (A(N3),NNL)                                            
C                                                                       
      IND=2                                                             
C                                                                       
      CALL ASSEM (A(N5))                                                
C                                                                       
      CALL SECOND (TIM(3))                                              
C                                                                       
C                                                                       
C     T R I A N G U L A R I Z E   S T I F F N E S S   M A T R I X       
C                                                                       
C                                                                       
      KTR=1                                                             
      CALL COLSOL (A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,KTR)                  
C                                                                       
   35 CALL SECOND (TIM(4))                                              
C                                                                       
      KTR=2                                                             
      IND=3                                                             
C                                                                       
      REWIND ILOAD                                                      
      DO 400 L=1,NLCASE                                                 
C                                                                       
      CALL LOADV (A(N4),NEQ)                                            
C                                                                       
C                                                                       
C     C A L C U L A T I O N   O F   D I S P L A C E M E N T S           
C                                                                       
C                                                                       
      CALL COLSOL (A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,KTR)                  
C                                                                       
      WRITE (IOUT,2015) L                                               
      CALL WRITED (A(N4),A(N1),NEQ,NUMNP)                               
C                                                                       
C                                                                       
C     C A L C U L A T I O N   O F   S T R E S S E S                     
C                                                                       
C                                                                       
      CALL STRESS (A(N5))                                               
C                                                                       
  400 CONTINUE                                                          
C                                                                       
      CALL SECOND (TIM(5))                                              
C                                                                       
C     PRINT SOLUTION TIMES                                              
C                                                                       
  120 TT=0.                                                             
      DO 500 I=1,4                                                      
      TIM(I)=TIM(I+1) - TIM(I)                                          
  500 TT=TT + TIM(I)                                                    
      WRITE (IOUT,2030) HED,(TIM(I),I=1,4),TT                           
C                                                                       
C     READ NEXT ANALYSIS CASE                                           
C                                                                       
      GO TO 200                                                         
C                                                                       
  800 STOP                                                              
C                                                                       
 1000 FORMAT (20A4,/,4I5)                                               
 1010 FORMAT (2I5)                                                      
C                                                                       
 2000 FORMAT (///,' ',20A4,///,                                         
     1    ' C O N T R O L   I N F O R M A T I O N',//,                  
     2    '      NUMBER OF NODAL POINTS',10(' .'),' (NUMNP)  = ',I5,//, 
     3    '      NUMBER OF ELEMENT GROUPS',9(' .'),' (NUMEG)  = ',I5,//,
     4    '      NUMBER OF LOAD CASES',11(' .'),' (NLCASE) = ',I5,//,   
     5    '      SOLUTION MODE ',14(' .'),' (MODEX)  = ',I5,/,          
     6    '         EQ.0, DATA CHECK',/,                                
     7    '         EQ.1, EXECUTION')                                   
 2005 FORMAT (//,' L O A D   C A S E   D A T A')                        
 2010 FORMAT (////,'     LOAD CASE NUMBER',7(' .'),' = ',I5,//,         
     1        '     NUMBER OF CONCENTRATED LOADS . = ',I5)              
 2015 FORMAT (//,' LOAD CASE ',I3)                                      
 2020 FORMAT (' *** ERROR *** LOAD CASES ARE NOT IN ORDER')             
 2025 FORMAT (//,' TOTAL SYSTEM DATA',///,                              
     1       '     NUMBER OF EQUATIONS',14(' .'),'(NEQ) = ',I5,//,      
     2       '     NUMBER OF MATRIX ELEMENTS',11(' .'),'(NWK) = ',I5,//,
     3       '     MAXIMUM HALF BANDWIDTH ',12(' .'),'(MK ) = ',I5,//,  
     4       '     MEAN HALF BANDWIDTH',14(' .'),'(MM ) = ',I5)         
 2030 FORMAT (//,' S O L U T I O N   T I M E   L O G   I N   S E C',//, 
     1 '            FOR PROBLEM',//,' ',20A4,///,                       
     2 '     TIME FOR INPUT PHASE ',14(' .'),' =',F12.2,//,             
     3 '     TIME FOR CALCULATION OF STIFFNESS MATRIX  . . . . =',F12.2,
     4 //,                                                              
     5 '     TIME FOR FACTORIZATION OF STIFFNESS MATRIX  . . . =',F12.2,
     6 //,                                                              
     7 '     TIME FOR LOAD CASE SOLUTIONS ',10(' .'),' =',F12.2,///,    
     8 '      T O T A L   S O L U T I O N   T I M E  . . . . . =',F12.2)
C                                                                       
      END                                                               
      SUBROUTINE ERROR (N,I)                                            
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO PRINT MESSAGES WHEN HIGH-SPEED STORAGE IS EXCEEDED      . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
C                                                                       
      GO TO (1,2,3,4),I                                                 
C                                                                       
    1 WRITE (IOUT,2000)                                                 
      GO TO 6                                                           
    2 WRITE (IOUT,2010)                                                 
      GO TO 6                                                           
    3 WRITE (IOUT,2020)                                                 
      GO TO 6                                                           
    4 WRITE (IOUT,2030)                                                 
C                                                                       
    6 WRITE (IOUT,2050) N                                               
      STOP                                                              
C                                                                       
 2000 FORMAT (//,' NOT ENOUGH STORAGE FOR ID ARRAY AND NODAL POINT ',   
     1        'COORDINATES')                                            
 2010 FORMAT (//,' NOT ENOUGH STORAGE FOR DEFINITION OF LOAD VECTORS')  
 2020 FORMAT (//,' NOT ENOUGH STORAGE FOR ELEMENT DATA INPUT')          
 2030 FORMAT (//,' NOT ENOUGH STORAGE FOR ASSEMBLAGE OF GLOBAL ',       
     1'STRUCTURE STIFFNESS, AND DISPLACEMENT AND STRESS SOLUTION PHASE')
 2050 FORMAT (//,' *** ERROR ***  STORAGE EXCEEDED BY ', I9)            
C                                                                       
      END                                                               
      SUBROUTINE INPUT (ID,X,Y,Z,NUMNP,NEQ)                             
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .      .TO READ, GENERATE, AND PRINT NODAL POINT INPUT DATA         . 
C .      .TO CALCULATE EQUATION NUMBERS AND STORE THEM IN ID ARRRAY   . 
C .                                                                   . 
C .           N=ELEMENT NUMBER                                        . 
C .           ID=BOUNDARY CONDITION CODES (0=FREE,1=DELETED)          . 
C .           X,Y,Z= COORDINATES                                      . 
C .           KN= GENERATION CODE                                     . 
C .                    I.E. INCREMENT ON NODAL POINT NUMBER           . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .   THE PROGRAM STAP IS USED IN SINGLE PRECISION ARITHMETIC ON CRAY . 
C .   EQUIPMENT AND DOUBLE PRECISION ARITHMETIC ON IBM MACHINES,      . 
C .   ENGINEERING WORKSTATIONS AND PCS. DEACTIVATE ABOVE LINE (ALSO   . 
C .   OCCURRING IN OTHER SUBROUTINES) FOR SINGLE PRECISION ARITHMETIC.. 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      DIMENSION X(1),Y(1),Z(1),ID(3,NUMNP)                              
C                                                                       
C     READ AND GENERATE NODAL POINT DATA                                
C                                                                       
      WRITE (IOUT,2000)                                                 
      WRITE (IOUT,2010)                                                 
      WRITE (IOUT,2020)                                                 
      KNOLD=0                                                           
      NOLD=0                                                            
C                                                                       
   10 READ (IIN,1000) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN               
      WRITE (IOUT,2030) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN             
      IF (KNOLD.EQ.0) GO TO 50                                          
      NUM=(N-NOLD)/KNOLD                                                
      NUMN=NUM - 1                                                      
      IF (NUMN.LT.1) GO TO 50                                           
      XNUM=NUM                                                          
      DX=(X(N)-X(NOLD))/XNUM                                            
      DY=(Y(N)-Y(NOLD))/XNUM                                            
      DZ=(Z(N)-Z(NOLD))/XNUM                                            
      K=NOLD                                                            
      DO 30 J=1,NUMN                                                    
      KK=K                                                              
      K=K + KNOLD                                                       
      X(K)=X(KK) + DX                                                   
      Y(K)=Y(KK) + DY                                                   
      Z(K)=Z(KK) + DZ                                                   
      DO 30 I=1,3                                                       
      ID(I,K)=ID(I,KK)                                                  
   30 CONTINUE                                                          
C                                                                       
   50 NOLD=N                                                            
      KNOLD=KN                                                          
      IF (N.NE.NUMNP) GO TO 10                                          
C                                                                       
C     WRITE COMPLETE NODAL DATA                                         
C                                                                       
      WRITE (IOUT,2015)                                                 
      WRITE (IOUT,2020)                                                 
      DO 200 N=1,NUMNP                                                  
  200 WRITE (IOUT,2030) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN             
C                                                                       
C     NUMBER UNKNOWNS                                                   
C                                                                       
      NEQ=0                                                             
      DO 100 N=1,NUMNP                                                  
      DO 100 I=1,3                                                      
      IF (ID(I,N)) 110,120,110                                          
  120 NEQ=NEQ + 1                                                       
      ID(I,N)=NEQ                                                       
      GO TO 100                                                         
  110 ID(I,N)=0                                                         
  100 CONTINUE                                                          
C                                                                       
C     WRITE EQUATION NUMBERS                                            
C                                                                       
      WRITE (IOUT,2040) (N,(ID(I,N),I=1,3),N=1,NUMNP)                   
C                                                                       
      RETURN                                                            
C                                                                       
 1000 FORMAT (4I5,3F10.0,I5)                                            
 2000 FORMAT(//,' N O D A L   P O I N T   D A T A',/)                   
 2010 FORMAT(' INPUT NODAL DATA',//)                                    
 2015 FORMAT(//,' GENERATED NODAL DATA',//)                             
 2020 FORMAT('  NODE',10X,'BOUNDARY',25X,'NODAL POINT',17X,'MESH',/,    
     1' NUMBER     CONDITION  CODES',21X,'COORDINATES',14X,'GENERATING',
     2/,77X,'CODE',/,                                                   
     315X,'X    Y    Z',15X,'X',12X,'Y',12X,'Z',10X,'KN')               
 2030 FORMAT (I5,6X,3I5,6X,3F13.3,3X,I6)                                
 2040 FORMAT(//,' EQUATION NUMBERS',//,'   NODE',9X,                    
     1 'DEGREES OF FREEDOM',/,'  NUMBER',//,                            
     2 '     N',13X,'X    Y    Z',/,(1X,I5,9X,3I5))                     
C                                                                       
      END                                                               
      SUBROUTINE LOADS (R,NOD,IDIRN,FLOAD,ID,NLOAD,NEQ)                 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .      . TO READ NODAL LOAD DATA                                    . 
C .      . TO CALCULATE THE LOAD VECTOR R FOR EACH LOAD CASE AND      . 
C .        WRITE ONTO UNIT ILOAD                                      . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      COMMON /VAR/ NG,MODEX                                             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      DIMENSION R(NEQ),NOD(1),IDIRN(1),FLOAD(1)                         
      DIMENSION ID(3,1)                                                 
C                                                                       
      WRITE (IOUT,2000)                                                 
      READ (IIN,1000) (NOD(I),IDIRN(I),FLOAD(I),I=1,NLOAD)              
      WRITE (IOUT,2010) (NOD(I),IDIRN(I),FLOAD(I),I=1,NLOAD)            
      IF (MODEX.EQ.0) GO TO 900                                         
C                                                                       
      DO 210 I=1,NEQ                                                    
  210 R(I)=0.                                                           
C                                                                       
      DO 220 L=1,NLOAD                                                  
      LN=NOD(L)                                                         
      LI=IDIRN(L)                                                       
      II=ID(LI,LN)                                                      
      IF (II) 220,220,240                                               
  240 R(II)=R(II) + FLOAD(L)                                            
C                                                                       
  220 CONTINUE                                                          
C                                                                       
      WRITE (ILOAD) R                                                   
C                                                                       
  200 CONTINUE                                                          
C                                                                       
  900 RETURN                                                            
C                                                                       
 1000 FORMAT (2I5,F10.0)                                                
 2000 FORMAT (//,'    NODE       DIRECTION      LOAD',/,                
     1        '   NUMBER',19X,'MAGNITUDE')                              
 2010 FORMAT (' ',I6,9X,I4,7X,E12.5)                                    
C                                                                       
      END                                                               
      SUBROUTINE ELCAL                                                  
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO LOOP OVER ALL ELEMENT GROUPS FOR READING,               . 
C .        GENERATING AND STORING THE ELEMENT DATA                    . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      COMMON A(1)                                                       
C                                                                       
      REWIND IELMNT                                                     
      WRITE (IOUT,2000)                                                 
C                                                                       
C     LOOP OVER ALL ELEMENT GROUPS                                      
C                                                                       
      DO 100 N=1,NUMEG                                                  
      IF (N.NE.1) WRITE (IOUT,2010)                                     
C                                                                       
      READ (IIN,1000) NPAR                                              
C                                                                       
      CALL ELEMNT                                                       
C                                                                       
      IF (MIDEST.GT.MAXEST) MAXEST=MIDEST                               
C                                                                       
      WRITE (IELMNT) MIDEST,NPAR,(A(I),I=NFIRST,NLAST)                  
C                                                                       
  100 CONTINUE                                                          
C                                                                       
      RETURN                                                            
C                                                                       
 1000 FORMAT (10I5)                                                     
 2000 FORMAT (//,' E L E M E N T   G R O U P   D A T A',//)             
 2010 FORMAT (' ')                                                      
C                                                                       
      END                                                               
      SUBROUTINE ELEMNT                                                 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO CALL THE APPROPRIATE ELEMENT SUBROUTINE                 . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
C                                                                       
      NPAR1=NPAR(1)                                                     
C                                                                       
      GO TO (1,2,3),NPAR1                                               
C                                                                       
    1 CALL TRUSS                                                        
      GO TO 900                                                         
C                                                                       
C     OTHER ELEMENT TYPES WOULD BE CALLED HERE, IDENTIFYING EACH        
C     ELEMENT TYPE BY A DIFFERENT NPAR(1) PARAMETER                     
C                                                                       
    2 GO TO 900                                                         
C                                                                       
    3 GO TO 900                                                         
C                                                                       
  900 RETURN                                                            
      END                                                               
      SUBROUTINE COLHT (MHT,ND,LM)                                      
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO CALCULATE COLUMN HEIGHTS                                . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      DIMENSION LM(1),MHT(1)                                            
C                                                                       
      LS=100000                                                         
      DO 100 I=1,ND                                                     
      IF (LM(I)) 110,100,110                                            
  110 IF (LM(I)-LS) 120,100,100                                         
  120 LS=LM(I)                                                          
  100 CONTINUE                                                          
C                                                                       
      DO 200 I=1,ND                                                     
      II=LM(I)                                                          
      IF (II.EQ.0) GO TO 200                                            
      ME=II - LS                                                        
      IF (ME.GT.MHT(II)) MHT(II)=ME                                     
 200  CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE ADDRES (MAXA,MHT)                                      
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO CALCULATE ADDRESSES OF DIAGONAL ELEMENTS IN BANDED      . 
C .        MATRIX WHOSE COLUMN HEIGHTS ARE KNOWN                      . 
C .                                                                   . 
C .        MHT  = ACTIVE COLUMN HEIGHTS                               . 
C.         MAXA = ADDRESSES OF DIAGONAL ELEMENTS                      . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      DIMENSION MAXA(*),MHT(*)                                          
C                                                                       
C     CLEAR ARRAY MAXA                                                  
C                                                                       
      NN=NEQ + 1                                                        
      DO 20 I=1,NN                                                      
   20 MAXA(I)=0.0                                                       
C                                                                       
      MAXA(1)=1                                                         
      MAXA(2)=2                                                         
      MK=0                                                              
      IF (NEQ.EQ.1) GO TO 100                                           
      DO 10 I=2,NEQ                                                     
      IF (MHT(I).GT.MK) MK=MHT(I)                                       
   10 MAXA(I+1)=MAXA(I) + MHT(I) + 1                                    
  100 MK=MK + 1                                                         
      NWK=MAXA(NEQ+1) - MAXA(1)                                         
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE CLEAR (A,N)                                            
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO CLEAR ARRAY A                                           . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      DIMENSION A(1)                                                    
      DO 10 I=1,N                                                       
   10 A(I)=0.                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE ASSEM (AA)                                             
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO CALL ELEMENT SUBROUTINES FOR ASSEMBLAGE OF THE          . 
C .        STRUCTURE STIFFNESS MATRIX                                 . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      DIMENSION AA(1)                                                   
C                                                                       
      REWIND IELMNT                                                     
C                                                                       
      DO 200 N=1,NUMEG                                                  
      READ (IELMNT) NUMEST,NPAR,(AA(I),I=1,NUMEST)                      
C                                                                       
      CALL ELEMNT                                                       
C                                                                       
  200 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE ADDBAN (A,MAXA,S,LM,ND)                                
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO ASSEMBLE UPPER TRIANGULAR ELEMENT STIFFNESS INTO        . 
C .        COMPACTED GLOBAL STIFFNESS                                 . 
C .                                                                   . 
C .         A = GLOBAL STIFFNESS                                      . 
C .         S = ELEMENT STIFFNESS                                     . 
C .         ND = DEGREES OF FREEDOM IN ELEMENT STIFFNESS              . 
C .                                                                   . 
C .                   S(1)        S(2)        S(3)        . . .       . 
C .         S   =                 S(ND+1)     S(ND+2)     . . .       . 
C .                                           S(2*ND)     . . .       . 
C .                                                       . . .       . 
C .                                                                   . 
C .                                                                   . 
C .                   A(1)        A(3)        A(6)        . . .       . 
C .         A   =                 A(2)        A(5)        . . .       . 
C .                                           A(4)        . . .       . 
C .                                                       . . .       . 
C .                                                                   . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      DIMENSION A(1),MAXA(1),S(1),LM(1)                                 
C                                                                       
      NDI=0                                                             
      DO 200 I=1,ND                                                     
      II=LM(I)                                                          
      IF (II) 200,200,100                                               
 100  MI=MAXA(II)                                                       
      KS=I                                                              
      DO 220 J=1,ND                                                     
      JJ=LM(J)                                                          
      IF (JJ) 220,220,110                                               
 110  IJ=II - JJ                                                        
      IF (IJ) 220,210,210                                               
 210  KK=MI + IJ                                                        
      KSS=KS                                                            
      IF (J.GE.I) KSS=J + NDI                                           
      A(KK)=A(KK) + S(KSS)                                              
  220 KS=KS + ND - J                                                    
  200 NDI=NDI + ND - I                                                  
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE COLSOL (A,V,MAXA,NN,NWK,NNM,KKK)                       
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO SOLVE FINITE ELEMENT STATIC EQUILIBRIUM EQUATIONS IN    . 
C .        CORE, USING COMPACTED STORAGE AND COLUMN REDUCTION SCHEME  . 
C .                                                                   . 
C .  - - INPUT VARIABLES - -                                          . 
C .        A(NWK)    = STIFFNESS MATRIX STORED IN COMPACTED FORM      . 
C .        V(NN)     = RIGHT-HAND-SIDE LOAD VECTOR                    . 
C .        MAXA(NNM) = VECTOR CONTAINING ADDRESSES OF DIAGONAL        . 
C .                    ELEMENTS OF STIFFNESS MATRIX IN A              . 
C .        NN        = NUMBER OF EQUATIONS                            . 
C .        NWK       = NUMBER OF ELEMENTS BELOW SKYLINE OF MATRIX     . 
C .        NNM       = NN + 1                                         . 
C .        KKK       = INPUT FLAG                                     . 
C .            EQ. 1   TRIANGULARIZATION OF STIFFNESS MATRIX          . 
C .            EQ. 2   REDUCTION AND BACK-SUBSTITUTION OF LOAD VECTOR . 
C .        IOUT      = UNIT USED FOR OUTPUT                           . 
C .                                                                   . 
C .  - - OUTPUT - -                                                   . 
C .        A(NWK)    = D AND L - FACTORS OF STIFFNESS MATRIX          . 
C .        V(NN)     = DISPLACEMENT VECTOR                            . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      DIMENSION A(NWK),V(1),MAXA(1)                                     
C                                                                       
C     PERFORM L*D*L(T) FACTORIZATION OF STIFFNESS MATRIX                
C                                                                       
      IF (KKK-2) 40,150,150                                             
   40 DO 140 N=1,NN                                                     
      KN=MAXA(N)                                                        
      KL=KN + 1                                                         
      KU=MAXA(N+1) - 1                                                  
      KH=KU - KL                                                        
      IF (KH) 110,90,50                                                 
   50 K=N - KH                                                          
      IC=0                                                              
      KLT=KU                                                            
      DO 80 J=1,KH                                                      
      IC=IC + 1                                                         
      KLT=KLT - 1                                                       
      KI=MAXA(K)                                                        
      ND=MAXA(K+1) - KI - 1                                             
      IF (ND) 80,80,60                                                  
   60 KK=MIN0(IC,ND)                                                    
      C=0.                                                              
      DO 70 L=1,KK                                                      
   70 C=C + A(KI+L)*A(KLT+L)                                            
      A(KLT)=A(KLT) - C                                                 
   80 K=K + 1                                                           
   90 K=N                                                               
      B=0.                                                              
      DO 100 KK=KL,KU                                                   
      K=K - 1                                                           
      KI=MAXA(K)                                                        
      C=A(KK)/A(KI)                                                     
      B=B + C*A(KK)                                                     
  100 A(KK)=C                                                           
      A(KN)=A(KN) - B                                                   
  110 IF (A(KN)) 120,120,140                                            
  120 WRITE (IOUT,2000) N,A(KN)                                         
      GO TO 800                                                         
  140 CONTINUE                                                          
      GO TO 900                                                         
C                                                                       
C     REDUCE RIGHT-HAND-SIDE LOAD VECTOR                                
C                                                                       
  150 DO 180 N=1,NN                                                     
      KL=MAXA(N) + 1                                                    
      KU=MAXA(N+1) - 1                                                  
      IF (KU-KL) 180,160,160                                            
  160 K=N                                                               
      C=0.                                                              
      DO 170 KK=KL,KU                                                   
      K=K - 1                                                           
  170 C=C + A(KK)*V(K)                                                  
      V(N)=V(N) - C                                                     
  180 CONTINUE                                                          
C                                                                       
C     BACK-SUBSTITUTE                                                   
C                                                                       
      DO 200 N=1,NN                                                     
      K=MAXA(N)                                                         
  200 V(N)=V(N)/A(K)                                                    
      IF (NN.EQ.1) GO TO 900                                            
      N=NN                                                              
      DO 230 L=2,NN                                                     
      KL=MAXA(N) + 1                                                    
      KU=MAXA(N+1) - 1                                                  
      IF (KU-KL) 230,210,210                                            
  210 K=N                                                               
      DO 220 KK=KL,KU                                                   
      K=K - 1                                                           
  220 V(K)=V(K) - A(KK)*V(N)                                            
  230 N=N - 1                                                           
      GO TO 900                                                         
C                                                                       
  800 STOP                                                              
  900 RETURN                                                            
C                                                                       
 2000 FORMAT (//' STOP - STIFFNESS MATRIX NOT POSITIVE DEFINITE',//,    
     1          ' NONPOSITIVE PIVOT FOR EQUATION ',I8,//,               
     2          ' PIVOT = ',E20.12 )                                    
C                                                                       
      END                                                               
      SUBROUTINE LOADV (R,NEQ)                                          
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO OBTAIN THE LOAD VECTOR                                  . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      DIMENSION R(NEQ)                                                  
C                                                                       
      READ (ILOAD) R                                                    
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE WRITED (DISP,ID,NEQ,NUMNP)                             
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .      TO PRINT DISPLACEMENTS                                       . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      DIMENSION DISP(NEQ),ID(3,NUMNP)                                   
      DIMENSION D(3)                                                    
C                                                                       
C     PRINT DISPLACEMENTS                                               
C                                                                       
      WRITE (IOUT,2000)                                                 
      IC=4                                                              
C                                                                       
      DO 100 II=1,NUMNP                                                 
      IC=IC + 1                                                         
      IF (IC.LT.56) GO TO 105                                           
      WRITE (IOUT,2000)                                                 
      IC=4                                                              
  105 DO 110 I=1,3                                                      
  110 D(I)=0.                                                           
C                                                                       
      DO 120 I=1,3                                                      
      KK=ID(I,II)                                                       
      IL=I                                                              
  120 IF (KK.NE.0) D(IL)=DISP(KK)                                       
C                                                                       
  100 WRITE (IOUT,2010) II,D                                            
C                                                                       
      RETURN                                                            
C                                                                       
 2000 FORMAT (///,' D I S P L A C E M E N T S',//,'  NODE ',10X,        
     1        'X-DISPLACEMENT    Y-DISPLACEMENT    Z-DISPLACEMENT')     
 2010 FORMAT (1X,I3,8X,3E18.6)                                          
C                                                                       
      END                                                               
      SUBROUTINE STRESS (AA)                                            
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .         TO CALL THE ELEMENT SUBROUTINE FOR THE CALCULATION OF     . 
C .         STRESSES                                                  . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /VAR/ NG,MODEX                                             
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      DIMENSION AA(1)                                                   
C                                                                       
C     LOOP OVER ALL ELEMENT GROUPS                                      
C                                                                       
      REWIND IELMNT                                                     
C                                                                       
      DO 100 N=1,NUMEG                                                  
      NG=N                                                              
C                                                                       
      READ (IELMNT) NUMEST,NPAR,(AA(I),I=1,NUMEST)                      
C                                                                       
      CALL ELEMNT                                                       
C                                                                       
  100 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE TRUSS                                                  
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO SET UP STORAGE AND CALL THE TRUSS ELEMENT SUBROUTINE    . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      COMMON A(1)                                                       
C                                                                       
      EQUIVALENCE (NPAR(2),NUME),(NPAR(3),NUMMAT)                       
C                                                                       
      NFIRST=N6                                                         
      IF (IND.GT.1) NFIRST=N5                                           
      N101=NFIRST                                                       
      N102=N101 + NUMMAT*ITWO                                           
      N103=N102 + NUMMAT*ITWO                                           
      N104=N103 + 6*NUME                                                
      N105=N104 + 6*NUME*ITWO                                           
      N106=N105 + NUME                                                  
      NLAST=N106                                                        
C                                                                       
      IF (IND.GT.1) GO TO 100                                           
      IF (NLAST.GT.MTOT) CALL ERROR (NLAST-MTOT,3)                      
      GO TO 200                                                         
  100 IF (NLAST.GT.MTOT) CALL ERROR (NLAST-MTOT,4)                      
C                                                                       
  200 MIDEST=NLAST - NFIRST                                             
C                                                                       
      CALL RUSS (A(N1),A(N2),A(N3),A(N4),A(N4),A(N5),A(N101),A(N102),   
     1 A(N103),A(N104),A(N105))                                         
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE RUSS (ID,X,Y,Z,U,MHT,E,AREA,LM,XYZ,MATP)               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .        TRUSS ELEMENT SUBROUTINE                                   . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      REAL A                                                            
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /VAR/ NG,MODEX                                             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              
      COMMON A(1)                                                       
C                                                                       
      DIMENSION X(1),Y(1),Z(1),ID(3,1),E(1),AREA(1),LM(6,1),            
     1          XYZ(6,1),MATP(1),U(1),MHT(1)                            
      DIMENSION S(21),ST(6),D(3)                                        
C                                                                       
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)       
      ND=6                                                              
C                                                                       
      GO TO (300,610,800),IND                                           
C                                                                       
C                                                                       
C     R E A D   A N D   G E N E R A T E   E L E M E N T                 
C     I N F O R M A T I O N                                             
C                                                                       
C     READ MATERIAL INFORMATION                                         
C                                                                       
  300 WRITE (IOUT,2000) NPAR1,NUME                                      
      IF (NUMMAT.EQ.0) NUMMAT=1                                         
      WRITE (IOUT,2010) NUMMAT                                          
C                                                                       
      WRITE (IOUT,2020)                                                 
      DO 10 I=1,NUMMAT                                                  
      READ (IIN,1000) N,E(N),AREA(N)                                    
   10 WRITE (IOUT,2030) N,E(N),AREA(N)                                  
C                                                                       
C     READ ELEMENT INFORMATION                                          
C                                                                       
      WRITE (IOUT,2040)                                                 
      N=1                                                               
  100 READ (IIN,1020) M,II,JJ,MTYP,KG                                   
      IF (KG.EQ.0) KG=1                                                 
  120 IF (M.NE.N) GO TO 200                                             
      I=II                                                              
      J=JJ                                                              
      MTYPE=MTYP                                                        
      KKK=KG                                                            
C                                                                       
C     SAVE ELEMENT INFORMATION                                          
C                                                                       
  200 XYZ(1,N)=X(I)                                                     
      XYZ(2,N)=Y(I)                                                     
      XYZ(3,N)=Z(I)                                                     
C                                                                       
      XYZ(4,N)=X(J)                                                     
      XYZ(5,N)=Y(J)                                                     
      XYZ(6,N)=Z(J)                                                     
C                                                                       
      MATP(N)=MTYPE                                                     
C                                                                       
      DO 390 L=1,6                                                      
  390 LM(L,N)=0                                                         
      DO 400 L=1,3                                                      
      LM(L,N)=ID(L,I)                                                   
  400 LM(L+3,N)=ID(L,J)                                                 
C                                                                       
C     UPDATE COLUMN HEIGHTS AND BANDWIDTH                               
C                                                                       
      CALL COLHT (MHT,ND,LM(1,N))                                       
C                                                                       
      WRITE (IOUT,2050) N,I,J,MTYPE                                     
      IF (N.EQ.NUME) GO TO 900                                          
      N=N + 1                                                           
      I=I + KKK                                                         
      J=J + KKK                                                         
      IF (N.GT.M) GO TO 100                                             
      GO TO 120                                                         
C                                                                       
C                                                                       
C     A S S E M B L E  S T U C T U R E  S T I F F N E S S  M A T R I X  
C                                                                       
C                                                                       
  610 DO 500 N=1,NUME                                                   
      MTYPE=MATP(N)                                                     
      XL2=0.                                                            
      DO 505 L=1,3                                                      
      D(L)=XYZ(L,N) - XYZ(L+3,N)                                        
  505 XL2=XL2 + D(L)*D(L)                                               
      XL=SQRT(XL2)                                                      
      XX=E(MTYPE)*AREA(MTYPE)*XL                                        
      DO 510 L=1,3                                                      
      ST(L)=D(L)/XL2                                                    
  510 ST(L+3)=-ST(L)                                                    
C                                                                       
      KL=0                                                              
      DO 600 L=1,6                                                      
      YY=ST(L)*XX                                                       
      DO 600 K=L,6                                                      
      KL=KL + 1                                                         
  600 S(KL)=ST(K)*YY                                                    
      CALL ADDBAN (A(N3),A(N2),S,LM(1,N),ND)                            
  500 CONTINUE                                                          
      GO TO 900                                                         
C                                                                       
C                                                                       
C     S T R E S S  C A L C U L A T I O N S                              
C                                                                       
C                                                                       
  800 IPRINT=0                                                          
      DO 830 N=1,NUME                                                   
      IPRINT=IPRINT + 1                                                 
      IF (IPRINT.GT.50) IPRINT=1                                        
      IF (IPRINT.EQ.1) WRITE (IOUT,2060) NG                             
      MTYPE=MATP(N)                                                     
      XL2=0.                                                            
      DO 820 L=1,3                                                      
      D(L) = XYZ(L,N) - XYZ(L+3,N)                                      
  820 XL2=XL2 + D(L)*D(L)                                               
      DO 814 L=1,3                                                      
      ST(L)=(D(L)/XL2)*E(MTYPE)                                         
  814 ST(L+3)=-ST(L)                                                    
      STR=0.0                                                           
      DO 806 L=1,3                                                      
      I=LM(L,N)                                                         
      IF (I.LE.0) GO TO 807                                             
      STR=STR + ST(L)*U(I)                                              
  807 J=LM(L+3,N)                                                       
      IF (J.LE.0) GO TO 806                                             
      STR=STR + ST(L+3)*U(J)                                            
  806 CONTINUE                                                          
      P=STR*AREA(MTYPE)                                                 
      WRITE (IOUT,2070) N,P,STR                                         
  830 CONTINUE                                                          
C                                                                       
  900 RETURN                                                            
C                                                                       
 1000 FORMAT (I5,2F10.0)                                                
 1010 FORMAT (2F10.0)                                                   
 1020 FORMAT (5I5)                                                      
 2000 FORMAT (' E L E M E N T   D E F I N I T I O N',///,               
     1        ' ELEMENT TYPE ',13(' .'),'( NPAR(1) ) . . =',I5,/,       
     2        '     EQ.1, TRUSS ELEMENTS',/,                            
     3        '     EQ.2, ELEMENTS CURRENTLY',/,                        
     4        '     EQ.3, NOT AVAILABLE',//,                            
     5        ' NUMBER OF ELEMENTS.',10(' .'),'( NPAR(2) ) . . =',I5,//)
 2010 FORMAT (' M A T E R I A L   D E F I N I T I O N',///,             
     1        ' NUMBER OF DIFFERENT SETS OF MATERIAL',/,                
     2        ' AND CROSS-SECTIONAL  CONSTANTS ',                       
     3                           4(' .'),'( NPAR(3) ) . . =',I5,//)     
 2020 FORMAT ('  SET       YOUNG''S     CROSS-SECTIONAL',/,             
     1        ' NUMBER     MODULUS',10X,'AREA',/,                       
     2        15X,'E',14X,'A')                                          
 2030 FORMAT (/,I5,4X,E12.5,2X,E14.6)                                   
 2040 FORMAT (//,' E L E M E N T   I N F O R M A T I O N',///,          
     1        ' ELEMENT     NODE     NODE       MATERIAL',/,            
     2        ' NUMBER-N      I        J       SET NUMBER',/)           
 2050 FORMAT (I5,6X,I5,4X,I5,7X,I5)                                     
 2060 FORMAT (///,' S T R E S S  C A L C U L A T I O N S  F O R  ',     
     1        'E L E M E N T  G R O U P',I4,//,                         
     2        '  ELEMENT',13X,'FORCE',12X,'STRESS',/,                   
     3        '  NUMBER',/)                                             
 2070 FORMAT (1X,I5,11X,E13.6,4X,E13.6)                                 
C                                                                       
      END                                                               
      SUBROUTINE SECOND (TIM)                                           
C                                                                       
C     SUBROUTINE TO OBTAIN TIME                                         
C     THIS SUBROUTINE HAS BEEN USED ON AN IBM RS/6000 WORKSTATION       
C                                                                       
      TIM=0.01*MCLOCK()                                                 
C                                                                       
      RETURN                                                            
      END                                                               