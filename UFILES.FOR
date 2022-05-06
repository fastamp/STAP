C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C+    函数功能: 打开文件
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      SUBROUTINE OPFILE()
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,IPOST,IPOST2
      CHARACTER*256 CPATH
C
      NLEN=0
      NSTA=0
      CALL GET_COMMAND_ARGUMENT (1,CPATH,NLEN,NSTA)
      IF(NLEN.LE.0) GOTO 88
      I=NLEN
      N1=0
      N2=0
10    IF(I.LT.1) GOTO 88
      IF(CPATH(I:I).EQ.'\')GOTO 20  !寻找第一个\ 确定路径 跳出循环
      IF(CPATH(I:I).EQ.'.')N2=I       !后缀名位置
      I=I-1
      GOTO 10
C
20    N1=I
C
      OPEN(UNIT=IIN,  FILE=CPATH)
      OPEN(UNIT=IOUT, FILE=CPATH(1:N2)//'out')
      OPEN(UNIT=IPOST,FILE=CPATH(1:N2)//'vtk')
      OPEN(UNIT=IPOST2,FILE=CPATH(1:N2)//'tmp')
      OPEN(UNIT=IELMNT,FILE=CPATH(1:N2)//'ele',FORM='UNFORMATTED')
      OPEN(UNIT=ILOAD,FILE=CPATH(1:N2)//'lds',FORM='UNFORMATTED')
C
C     以下代码写出后处理文件的头部
C
      REWIND(IPOST)
      REWIND(IPOST2)                !先将后处理文件写出到临时文件,求解完成之后再整理到后处理文件
      WRITE(IPOST2,'(26H# vtk DataFile Version 2.0)')
      WRITE(IPOST2,'(25HUnstructured Grid Example)')
      WRITE(IPOST2,'(5HASCII)')
      WRITE(IPOST2,'(7HDATASET,1X,17HUNSTRUCTURED_GRID)')
C
      RETURN
C
C
88    CONTINUE
      WRITE(*,*)"INPUT ERROR-GIVING WRONG ARGUMENT!"
      STOP
C
      END
C
      SUBROUTINE WRITED (DISP,ID,NEQ,NUMNP)                             
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .      TO PRINT DISPLACEMENTS                                       . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,IPOST,IPOST2
      DIMENSION DISP(NEQ),ID(3,NUMNP)                                   
      DIMENSION D(3)
      CHARACTER*80 CLINE
C
C     更新后处理坐标,删除临时文件
C
      REWIND(IPOST)
      REWIND(IPOST2)
      IT=0
      II=0
10    READ(IPOST2,2030,END=90)CLINE
      IF(CLINE(1:6).EQ.'POINTS') IT=1
      IF(CLINE(1:4).EQ.'CELL') IT=0
C
      IF(IT.LE.1)THEN
        WRITE(IPOST,2030)CLINE
        IF(IT.EQ.1)IT=2
      ELSE
        II=II+1
        READ(CLINE,2040)D
        DO 20 I=1,3
          KK=ID(I,II)
          IL=I
          IF(KK.NE.0) D(IL)=D(IL)+DISP(KK)
20      CONTINUE
        WRITE(IPOST,2050)D
      ENDIF
      GOTO 10
90    CLOSE(IPOST2,STATUS='DELETE')  
C                                                                       
C     PRINT DISPLACEMENTS                                               
C                                                                       
      WRITE (IOUT,2000)                                                 
      IC=4                                                              
C
      WRITE(IPOST,'(10HPOINT_DATA,1X,I8)')NUMNP
      WRITE(IPOST,'(7HVECTORS,1X,12HDISPLACEMENT,1X,5Hfloat)')
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
      WRITE(IPOST,2020)D
C
  100 WRITE (IOUT,2010) II,D
C                                                                       
      RETURN                                                            
C                                                                       
 2000 FORMAT (///,' D I S P L A C E M E N T S',//,'  NODE ',10X,        
     1        'X-DISPLACEMENT    Y-DISPLACEMENT    Z-DISPLACEMENT')     
2010  FORMAT (1X,I3,8X,3E18.6)
2020  FORMAT(E16.9,1X,E16.9,1X,E16.9)
2030  FORMAT(A80)
2040  FORMAT(3E16.9)
2050  FORMAT(E16.9,1X,E16.9,1X,E16.9)                                 
C                                                                       
      END
C