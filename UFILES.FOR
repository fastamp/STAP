C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C+    函数功能: 打开文件
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      SUBROUTINE OPFILE()
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,IPOST
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
      OPEN(UNIT=IOUT, FILE=CPATH(1:N2)//'OUT')
      OPEN(UNIT=IPOST,FILE=CPATH(1:N2)//'vtk')
C      OPEN(UNIT=IELMNT,FILE=CPATH(1:N2)//'ELE')
C      OPEN(UNIT=ILOAD,FILE=CPATH(1:N2)//'LDS')
C
C     以下代码写出后处理文件的头部
C
      REWIND(IPOST)
      WRITE(IPOST,'(26H# vtk DataFile Version 2.0)')
      WRITE(IPOST,'(25HUnstructured Grid Example)')
      WRITE(IPOST,'(5HASCII)')
      WRITE(IPOST,'(7HDATASET,1X,17HUNSTRUCTURED_GRID)')
C
      RETURN
C
C
88    CONTINUE
      WRITE(*,*)"INPUT ERROR-GIVING WRONG ARGUMENT!"
      STOP
C
      END