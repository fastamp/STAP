C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C+
C+    QUADERILATERAL 4 NODES PLATE ELEMENT WITH LINKED INTERPOLATION
C+
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      SUBROUTINE PQ4LIM(ID,X,Y,Z,U,MHT,E,VU,AREA,LM,XYZ,MATP)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(NEAS=4)                   !假设应变矩阵列数
      REAL A        !!!需要特别注意的地方, 当有IMPLICIT REAL*8的地方
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /VAR/ NG,MODEX                                             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,IPOST,IPOST2
      COMMON A(1)
C
      DIMENSION X(1),Y(1),Z(1),ID(3,1),E(1),VU(1),AREA(1),LM(8,1),
     &          XYZ(8,1),MATP(1),U(1),MHT(1)
      DIMENSION IX(4),XG(4,4),WGT(4,4)
C
C     DB: 弯曲弹性矩阵; DS:剪切弹性矩阵
C     HS:    S插值函数
C     HW: W插值函数;  PW: \nabla N_w
C     HWT,PWT: W THETA LINK插值函数及其导数
C     HB,PB: B 插值函数及其导数
C     HC,PC: W\THETA LINK 插值函数
C     SBS: K_BS         SSW: K_SW         STT: K_\THETA\THETA
C     SBT: K_B\THETA    SST: K_S\THETA    SBB: K_BB
C     SSS: K_SS
C     BT:  LN_T;         BB: LN_B
      DIMENSION DB(3,3),DS(2,2),HW(4),PW(2,4),HS(2,4),HB(2,4),
     &          HWT(4),PWT(2,4),SBS(4,4),SSW(4,4),STT(8,8),SBT(4,8),
     &          SST(4,4),SBB(4,4),SSS(4,4),BT(3,8),BB(3,4),SBS0(4,4)
C
C     AJ-(0,0)的雅克比矩阵,   XJ(2,2)-积分点的雅可比矩阵
C     F0-转换矩阵,           XJI(2,2)-积分点雅可比矩阵逆矩阵
      DIMENSION AG(2,2),F0(3,3),AJ(2,2),XJ(2,2),XJI(2,2)
C+++++高斯积分点
      DATA XG/   0.D0,   0.D0,   0.D0,   0.D0,   -.5773502691896D0,     
     1 .5773502691896D0,   0.D0,   0.D0,   -.7745966692415D0,   0.D0,   
     2 .7745966692415D0,   0.D0,   -.8611363115941D0,                   
     3 -.3399810435849D0,   .3399810435849D0,   .8611363115941D0 /      
C+++++高斯积分权重
      DATA WGT /  2.D0,   0.D0,   0.D0,   0.D0,   1.D0,   1.D0,         
     1 0.D0,   0.D0,   .5555555555556D0,   .8888888888889D0,            
     2 .5555555555556D0,   0.D0,   .3478548451375D0,   .6521451548625D0,
     3 .6521451548625D0,   .3478548451375D0 /   
C
      DATA SBS0/1.77777777777778D0,0.D0,0.D0,0.D0,0.D0,
     &         1.77777777777778D0,0.D0,0.D0,0.D0,0.D0,0.3555555555556D0,
     &         0.D0,0.D0,0.D0,0.D0,0.3555555555556D0/
C
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)
C
      ND=12                               !单元自由度数
      NINT=3                              !积分方式: 1-单点积分; 2-2*2积分; 3-3*3积分; 4-4*4积分
      CALL ZEROD(DB,9)
      CALL ZEROD(DS,4)
C
      GOTO(100,200,300)IND
C
C     stage1: 数据输入阶段
C
100   WRITE(IOUT,2000)NPAR1,NUME
      WRITE(IPOST2,'(5HCELLS,1X,I8,1X,I8)')NUME,NUME*4+NUME
      IF(NUMMAT.EQ.0) NUMMAT=1
      WRITE(IOUT,2010) NUMMAT
C
      !读取材料信息
      WRITE(IOUT,2020)
      DO 10 I=1,NUMMAT
      READ(IIN,1000)N,E(N),VU(N),AREA(N)
      WRITE(IOUT,2030)N,E(N),AREA(N)
10    CONTINUE
C
      !读取单元信息
      WRITE(IOUT,2040)
20    READ(IIN,1010)N,(IX(J),J=1,4),MTYP
      DO 21 I=1,4
      L=IX(I)           !实际节点号
      J=2*I-1           !自由度
      K=J+1             !自由度
      XYZ(J,N)=X(L)
      XYZ(K,N)=Y(L)
21    CONTINUE
C
      IF(MTYP.LE.0)MTYP=1
      MATP(N)=MTYP
C
      !生成LM数组并计算列高
      DO 30 I=1,8
      LM(I,N)=0
30    CONTINUE
      DO 31 I=1,4
      L=IX(I)
      J=2*I-1
      K=J+1
      LM(J,N)=ID(1,L)   !生成LM数组
      LM(K,N)=ID(2,L) 
31    CONTINUE
      CALL COLHT(MHT,ND,LM(1,N))         !计算列高
C
      !写出后处理
      WRITE(IOUT,2050)N,(IX(J),J=1,4),MTYP
      WRITE(IPOST2,'(I8,1X,I8,1X,I8,1X,I8,1X,I8)')4,(IX(I)-1,I=1,4)        !VTK文件
      IF (N.EQ.NUME)THEN
        WRITE(IPOST2,'(10HCELL_TYPES,1X,I8)')NUME
        DO II=1,NUME
          WRITE(IPOST2,'(I8)')9
        ENDDO
        GOTO 900                          !完成单元信息读取 退出循环
      ENDIF
C
      GOTO 20                             !继续读取单元信息
C
      GOTO 900
C
C     stage2: 计算和组装刚度矩阵
C
200   CALL ZEROD(BB,12)
      DO 290 N=1,NUME
      IMAT=MATP(N)
      YM=E(IMAT)
      PR=VU(IMAT)
      THIC=AREA(IMAT)
      D=YM*THIC*THIC*THIC/12.0/(1.D0-PR*PR)
      DB(1,1)=D                           !弯曲弹性矩阵
      DB(2,1)=PR*D
      DB(1,2)=DB(2,1)
      DB(2,2)=DB(1,1)
      DB(3,3)=0.5D0*(1.D0-PR)*D
      G=0.5D0*YM/(1.D0+PR)*THIC*0.833333333333333333333D0
      DS(1,1)=G                           !剪切弹性矩阵
      DS(2,2)=G
C
C     计算转换矩阵F0
      !计算(0,0)点的雅可比矩阵AJ
      AJ(1,1)=0.25D0*(XYZ(1,N)-XYZ(3,N)-XYZ(5,N)+XYZ(7,N))
      AJ(2,1)=0.25D0*(XYZ(1,N)+XYZ(3,N)-XYZ(5,N)-XYZ(7,N))
      AJ(1,2)=0.25D0*(XYZ(2,N)-XYZ(4,N)-XYZ(6,N)+XYZ(8,N))
      AJ(2,2)=0.25D0*(XYZ(2,N)+XYZ(4,N)-XYZ(6,N)-XYZ(8,N))
      HJ=AJ(1,1)*AJ(2,2)-AJ(1,2)*AJ(2,1) !DET J0
C
      !K_BS矩阵计算
      DO 201 I=1,4
      DO 201 J=1,4
      SBS(I,J)=SBS0(I,J)*HJ
201   CONTINUE
C
C     计算单元刚度矩阵
      CALL ZEROD(SSW,16)
      CALL ZEROD(STT,64)
      CALL ZEROD(SBT,16)
      DO 280 LX=1,NINT
      RI=XG(LX,NINT)
      DO 280 LY=1,NINT
      SI=XG(LY,NINT)
      CALL SHAPPQ4L(XYZ(1,N),XJ,XJI,HW,PW,HWT,PWT,DET,RI,SI,NEL,IOUT)         !计算型函数及其导数             
      WT=WGT(LX,NINT)*WGT(LY,NINT)*THIC*DET
      !计算K_SW矩阵
      HS(1,1)=XJ(1,1)                     !N_s
      HS(2,1)=XJ(1,2)
      HS(1,2)=XJ(2,1)
      HS(2,2)=XJ(2,2)
      HS(1,3)=XJ(1,1)*SI  
      HS(2,3)=XJ(1,2)*SI 
      HS(1,4)=XJ(2,1)*RI 
      HS(2,4)=XJ(2,2)*RI
      DO 210 I=1,4
      DO 210 J=1,4                        !只计算上三角矩阵
      STIFF=0.D0
      DO 205 K=1,2
205   STIFF=STIFF+HS(K,I)*PW(K,J)
      SSW(I,J)=SSW(I,J)+STIFF*WT
210   CONTINUE          !结束K_SW
C
      !计算K_TT矩阵
      DO 230 I=1,4
      J2=I*2
      J1=J2-1
      BT(1,J1)= 0.D0
      BT(2,J1)=-PW(2,I)
      BT(3,J1)=-PW(1,I)
      BT(1,J2)= PW(1,I)
      BT(2,J2)= 0.D0
      BT(3,J2)= PW(2,I)
230   CONTINUE
      DO 235 I=1,8
      DO 235 J=1,8
      STIFF=0.D0
      DO 232 K=1,3
      DO 232 L=1,3
232   STIFF=STIFF+BT(K,I)*DB(K,L)*BT(L,J)
      STT(I,J)=STT(I,J)+STIFF*WT
235   CONTINUE
C
      !计算K_BT矩阵
      BB(1,3)= XJ(2,2)*XJI(1,2)
      BB(2,3)=-XJ(2,1)*XJI(2,2)
      BB(3,3)=-XJ(2,1)*XJI(1,2) + XJ(2,2)*XJI(2,2)   
      BB(1,4)=-XJ(1,2)*XJI(1,1)
      BB(2,4)= XJ(1,1)*XJI(2,1)
      BB(3,4)= XJ(1,1)*XJI(1,1) - XJ(1,2)*XJI(2,1)  
      DO 245 I=1,4
      DO 245 J=1,4
      STIFF=0.D0
      DO 240 K=1,3
      DO 240 L=1,3
240   STIFF=STIFF+BB(K,I)*DB(K,L)*BT(L,J) 
      SBT(I,J)=SBT(I,J)+STIFF*WT 
245   CONTINUE
C
      !计算K_ST矩阵
      DO 255 I=1,4
      DO 255 J=1,4
      STIFF=0.D0
      DO 250 K=1,2
250   STIFF=STIFF+HS(K,I)*(PWT(K,J)+)
C
280   CONTINUE          !结束积分点循环
C
290   CONTINUE          !结束单元循环
C
C     stage3: 回代计算单元应力
C
300   CONTINUE
C
900   RETURN
1000  FORMAT(I,3F)
1010  FORMAT(6I)
2000  FORMAT(' E L E M E N T   D E F I N I T I O N',///,               
     &        ' ELEMENT TYPE ',13(' .'),'( NPAR(1) ) . . =',I5,/,       
     &        '     EQ.1, TRUSS ELEMENTS',/,                            
     &        '     EQ.2, QUADS ELEMENTS',/,                        
     &        '     EQ.3, NOT AVAILABLE',//,                            
     &        ' NUMBER OF ELEMENTS.',10(' .'),'( NPAR(2) ) . . =',I5,//)
2010  FORMAT (' M A T E R I A L   D E F I N I T I O N',///,             
     &        ' NUMBER OF DIFFERENT SETS OF MATERIAL',/,                
     &        ' AND CROSS-SECTIONAL  CONSTANTS ',                       
     &                           4(' .'),'( NPAR(3) ) . . =',I5,//)
2020  FORMAT ('  SET       YOUNG''S     CROSS-SECTIONAL',/,             
     &        ' NUMBER     MODULUS',10X,'AREA',/,                       
     &        15X,'E',14X,'A')
2030  FORMAT (/,I5,4X,E12.5,2X,E14.6)
2040  FORMAT (//,' E L E M E N T   I N F O R M A T I O N',///,          
     &        ' ELEMENT     NODE    NODE    NODE    NODE  MATERIAL',/,            
     &        ' NUMBER-N      I       J       K       L  SET NUMBER',/)
2050  FORMAT (I8,1X,4I8,2X,I8)
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C
C+    计算2D板单元: PQ4LIM 的插值函数及其导数
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      SUBROUTINE SHAPPQ4L(XX,XJ,XJI,HW,PW,HWT,PWT,DET,R,S,NEL,IOUT)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XX(2,4),B(4,8),H(4),P(2,4),XJ(2,2),XJI(2,2)
      DIMENSION HW(4),PW(2,4),HWT(4),PWT(2,4)
C
      RP = 1.D0+R
      SP = 1.D0+S
      RM = 1.D0-R
      SM = 1.D0-S
C
C     2D-Q4位移插值函数
      H(1)=0.25D0*RP *SP
      H(2)=0.25D0*RM *SP
      H(3)=0.25D0*RM *RM 
      H(4)=0.25D0*RP *SM
C     
C     2D-Q4位移形函数对自然坐标系的导数
      P(1,1)=0.25D0*SP              !\PARTIAL N_I/\PARTIAL \XI
      P(1,2)=-P(1,1)
      P(1,3)=-0.25D0*SM
      P(1,4)=-P(1,3)
      P(2,1)=0.25D0*RP              !\PARTIAL N_I/\PARTIAL \ETA
      P(2,2)=0.25D0*RM 
      P(2,3)=-P(2,2)
      P(2,4)=-P(2,1)
C
C     点(R,S)位置的雅可比矩阵
10    DO 30 I=1,2
      DO 30 J=1,2
      DUM=0.D0
      DO 20 K=1,4
20    DUM=DUM+P(I,K)*XX(J,K)
      XJ(I,J)=DUM                   !\PARTIAL X_J/\PARTIAL \XI_I
30    CONTINUE
C
C     点(R,S)位置的雅可比矩阵行列式
      DET=XJ(1,1)*XJ(2,2)-XJ(1,2)*XJ(2,1)
      IF(DET.GT.1.0D-8)GOTO 40
      WRITE(IOUT,2000)NEL 
      GOTO 800
C
C     雅可比矩阵的逆矩阵
40    DUM=1.D0/DET
      XJI(1,1)= XJ(2,2)*DUM 
      XJI(2,1)=-XJ(2,1)*DUM
      XJI(1,2)=-XJ(1,2)*DUM
      XJI(2,2)= XJ(1,1)*DUM
C
C     计算插值函数及其对全局坐标系的导数
      !横向位移及角位移插值函数及其导数
      HW(1)=H(1)
      HW(2)=H(2)
      HW(3)=H(3)
      HW(4)=H(4)
      DO 60 I=1,2
      DO 60 J=1,4
      DUM=0.D0
      DO 50 K=1,2
50    DUM=DUM+XJI(I,K)*P(K,J)
      PW(I,J)=DUM
60    CONTINUE
      !角位移连接插值函数及其导数
      RM2=1.D0-R*R 
      RP2=1.D0+R*R 
      SM2=1.D0-S*S 
      SP2=1.D0+S*S 
      HWT(1)=0.0625D0*RM2*SM              !N_W\THETA
      HWT(2)=0.0625D0*RP*SM2 
      HWT(3)=0.0625D0*RM2*SP 
      HWT(4)=0.0625D0*RM*SM2
      P(1,1)=-0.1250D0 * R  *SM           !\PARTIAL (N)_J/\PARTIAL \XI_I
      P(1,2)= 0.0625D0 * SM2
      P(1,3)=-0.1250D0 * R  *SP 
      P(1,4)=-0.0625D0 * SM2
      P(2,1)=-0.0625D0 * RM2
      P(2,2)=-0.1250D0 * RP *S
      P(2,3)= 0.0625D0 * RM2
      P(2,4)=-0.1250D0 * RM *S
      DO 80 I=1,2
      DO 80 J=1,4
      DUM=0.D0
      DO 70 K=1,2
70    DUM=DUM+XJI(I,K)*P(K,J)
      PWT(I,J)=DUM                        !\PARTIAL (N)_J/\PARTIAL X_I
80    CONTINUE
C
      GOTO 900
800   STOP
900   RETURN
C
 2000 FORMAT (//,' *** ERROR *** ',
     1    ' ZERO OR NEGATIVE JACOBIAN DETERMINANT FOR ELEMENT (',I8,')')
C
      END