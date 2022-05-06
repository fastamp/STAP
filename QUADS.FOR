C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C+    二维问题接口程序
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      SUBROUTINE TODMFE
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,IPOST,IPOST2 
      COMMON A(1)                                                       
C
C     NUME: 单元数, NUMMAT: 材料数, NETYP: 单元类型 1-4节点位移法
C                                                2-4节点不协调
      EQUIVALENCE (NPAR(2),NUME),(NPAR(3),NUMMAT),(NPAR(4),NETYP)
C
      NFIRST=N6
      IF (IND.GT.1) NFIRST=N5
      N101=NFIRST                         !材料数组-E
      N102=N101 + NUMMAT*ITWO             !材料数组-泊松比
      N103=N102 + NUMMAT*ITWO             !材料数组-Area
      N104=N103 + NUMMAT*ITWO             !LM数组->ne*ndof*nend 二维空间的QUAD单元
      N105=N104 + 8*NUME                  !单元节点坐标向量 ne*ndof*nend nend:单元节点数
      N106=N105 + 8*NUME*ITWO             !单元材料索引
      N107=N106 + NUME
      NLAST=N107
C
      IF (IND.GT.1) GO TO 100                                           
      IF (NLAST.GT.MTOT) CALL ERROR (NLAST-MTOT,3)                      
      GO TO 200                                                         
  100 IF (NLAST.GT.MTOT) CALL ERROR (NLAST-MTOT,4)                      
C                                                                       
  200 MIDEST=NLAST - NFIRST
C
      IF(NETYP.LE.0)NETYP=1               !默认四节点位移法单元
      GOTO(10,20),NETYP
C
C     标准位移法4节点程序
C
10    CALL QUADS(A(N1),A(N2),A(N3),A(N4),A(N4),A(N5),A(N101),A(N102),
     &           A(N103),A(N104),A(N105),A(N106))
      GOTO 900
C
C
20    CALL QUADW(A(N1),A(N2),A(N3),A(N4),A(N4),A(N5),A(N101),A(N102),
     &           A(N103),A(N104),A(N105),A(N106))
      GOTO 900
C
900   RETURN
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C+    2D-QUAD单元
C+    ID: 单元自由度数组
C+    X,Y,Z: 节点坐标(Z忽略)
C+    U: 位移
C+    MHT: 列高数组
C+    E,VU,AREA: 材料数组-弹性模量, 泊松比, 面积
C+    LM: 连接关系数组
C+    XYZ: 单元节点坐标数组
C+    MATP: 单元材料索引数组
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      SUBROUTINE QUADS(ID,X,Y,Z,U,MHT,E,VU,AREA,LM,XYZ,MATP)
      IMPLICIT REAL*8(A-H,O-Z)
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
      DIMENSION IX(4)
      DIMENSION D(4,4),B(4,8),S(8,8),XG(4,4),WGT(4,4),DB(4),SW(36),
     &          STS(4),STR(4)
C
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
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)
      ND=8        !单元自由度数
      ITYPE=2                             !ITYPE: 0-轴对称; 1-平面应变; 2-平面应力
      NINT=2                              !积分方式: 1-单点积分; 2-2*2积分; 3-3*3积分; 4-4*4积分
C
      GOTO(100,200,300),IND
C
C     stage1: 数据输入阶段
C
100   WRITE(IOUT,2000) NPAR1,NUME
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
200   DO 190 N=1,NUME                     !对单元进行循环计算刚度矩阵
      IMAT=MATP(N)
      YM=E(IMAT)
      PR=VU(IMAT)
      THIC=AREA(IMAT)
      F=YM/(1.+PR)                                                      
      G=F*PR/(1.-2.*PR)                                                 
      H=F + G                                                           
C                                                                       
      !平面应变
      D(1,1)=H                                                          
      D(1,2)=G                                                          
      D(1,3)=0.                                                         
      D(2,1)=G                                                          
      D(2,2)=H                                                          
      D(2,3)=0.                                                         
      D(3,1)=0.                                                         
      D(3,2)=0.                                                         
      D(3,3)=F/2.                                                       
      IF (ITYPE.EQ.1) THEN                                              
      THIC=1.                                                           
      GOTO 120                                                          
      ENDIF                                                             
C                                                                       
      !轴对称                                                                       
      D(1,4)=G                                                          
      D(2,4)=G                                                          
      D(3,4)=0.                                                         
      D(4,1)=G                                                          
      D(4,2)=G                                                          
      D(4,3)=0.                                                         
      D(4,4)=H                                                          
      IF (ITYPE.EQ.0) GO TO 120                                          
C                                                                       
      !平面应力 
      DO 110 I=1,3                                                       
      XA=D(I,4)/D(4,4)                                                   
      DO 110 J=I,3                                                       
      D(I,J)=D(I,J) - D(4,J)*XA                                          
      D(J,I)=D(I,J)
110   CONTINUE
C
      !计算单元刚度矩阵
120   DO 130 J=1,8                  !初始化单元刚度矩阵
      DO 130 I=J,8
      S(I,J)=0.D0
130   CONTINUE
C
      IST=3
      IF(ITYPE.EQ.0)IST=4           !轴对称的弹性阵是4*4
      DO 180 LX=1,NINT
      RI=XG(LX,NINT)
      DO 180 LY=1,NINT
      SI=XG(LY,NINT)
      CALL STDM(XYZ(1,N),B,DET,RI,SI,XBAR,N,ITYPE,IOUT)   !计算形函数的导数B和雅克比行列式DET
      IF (ITYPE.GT.0) XBAR=THIC
      WT=WGT(LX,NINT)*WGT(LY,NINT)*XBAR*DET
      !开始刚度矩阵计算
      DO 170 J=1,8                  !开始刚度矩阵计算
      DO 140 K=1,IST                !计算DB=D*B矩阵
      DB(K)=0.0
      DO 140 L=1,IST
      DB(K)=DB(K)+D(K,L)*B(L,J)
140   CONTINUE
C
      DO 160 I=J,8
      STIFF=0.D0
      DO 150 K=1,IST
      STIFF=STIFF+B(K,I)*DB(K)
150   CONTINUE
      S(I,J)=S(I,J)+STIFF*WT
160   CONTINUE
C
170   CONTINUE                      !结束当前积分点刚度矩阵分量计算: J
C
180   CONTINUE                      !结束积分点循环: LX,LY
C
      !组装单元刚度矩阵
      IJ=0
      DO 181 J=1,8
      DO 181 I=J,8
      IJ=IJ+1
      SW(IJ)=S(I,J)
181   CONTINUE
      CALL ADDBAN(A(N3),A(N2),SW,LM(1,N),ND)
190   CONTINUE                      !结束单元循环:N
C
      GOTO 900
C
C
C     stage3: 回代求解计算应力
C
300   IPRINT=0
      WRITE(IPOST,'(9HCELL_DATA,1X,I8)')NUME 
      WRITE(IPOST,'(7HVECTORS,1X,8HSTRESSii,1X,5HFLOAT)')            !正应力
      DO 290 N=1,NUME
      IPRINT=IPRINT+1
      IF(IPRINT.GT.50)IPRINT=1
      IF(IPRINT.EQ.1)WRITE (IOUT,2060) NG
C
C     计算弹性矩阵
C
      IMAT=MATP(N)
      YM=E(IMAT)
      PR=VU(IMAT)
      THIC=AREA(IMAT)
      F=YM/(1.+PR)                                                      
      G=F*PR/(1.-2.*PR)                                                 
      H=F + G                                                           
C                                                                       
      !平面应变
      D(1,1)=H                                                          
      D(1,2)=G                                                          
      D(1,3)=0.                                                         
      D(2,1)=G                                                          
      D(2,2)=H                                                          
      D(2,3)=0.                                                         
      D(3,1)=0.                                                         
      D(3,2)=0.                                                         
      D(3,3)=F/2.                                                       
      IF (ITYPE.EQ.1) THEN                                              
      THIC=1.                                                           
      GOTO 220                                                          
      ENDIF                                                             
C                                                                       
      !轴对称                                                                       
      D(1,4)=G                                                          
      D(2,4)=G                                                          
      D(3,4)=0.                                                         
      D(4,1)=G                                                          
      D(4,2)=G                                                          
      D(4,3)=0.                                                         
      D(4,4)=H                                                          
      IF (ITYPE.EQ.0) GO TO 220                                          
C                                                                       
      !平面应力 
      DO 210 I=1,3                                                       
      XA=D(I,4)/D(4,4)                                                   
      DO 210 J=I,3                                                       
      D(I,J)=D(I,J) - D(4,J)*XA                                          
      D(J,I)=D(I,J)
210   CONTINUE
C
C     计算积分点应力
C
220   IST=3
      IF(ITYPE.EQ.0)IST=4           !轴对称的弹性阵是4*4
      DO 221 I=1,IST
      STR(I)=0.D0                   !计算单元内平均应力
221   CONTINUE
      DO 280 LX=1,NINT
      RI=XG(LX,NINT)
      DO 280 LY=1,NINT
      SI=XG(LY,NINT)
      CALL STDM(XYZ(1,N),B,DET,RI,SI,XBAR,NEL,ITYPE,IOUT)   !计算形函数的导数B和雅克比行列式DET
      IF (ITYPE.GT.0) XBAR=THIC
      WT=WGT(LX,NINT)*WGT(LY,NINT)*XBAR*DET
      !计算积分点应力分量
      DO 230 I=1,IST
      STS(I)=0.D0                   !积分点应力初始化
230   CONTINUE
C
      DO 270 J=1,8                  !开始刚度矩阵计算
      I=LM(J,N)                     !节点自由度
      IF(I.LE.0)GOTO 270
      DO 240 K=1,IST                !计算DB=D*B矩阵
      DB(K)=0.0
      DO 240 L=1,IST
      DB(K)=DB(K)+D(K,L)*B(L,J)
240   CONTINUE
C
      DO 250 K=1,IST
      STS(K)=STS(K)+DB(K)*U(I)
250   CONTINUE
C
270   CONTINUE                      !结束当前积分点刚度矩阵分量计算: J
C
      WRITE(IOUT,2070)N,LX,LY,STS
      DO 271 I=1,IST                !将各个积分点的应力累加,最后求平均输出到vtk文件中
      STR(I)=STR(I)+STS(I)
271   CONTINUE
280   CONTINUE                      !结束积分点循环: LX,LY
      DO 281 I=1,IST
      STR(I)=STR(I)/NINT/NINT
281   CONTINUE
      WRITE(IPOST,'(3E16.9)')(STR(I),I=1,3)
290   CONTINUE
C
      GOTO 900
C
900   RETURN
C
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
2060  FORMAT (///,' S T R E S S  C A L C U L A T I O N S  F O R  ',     
     &        'E L E M E N T  G R O U P',I4,//,                         
     &        '  ELEMENT',5X,'LOCX',5X,'LOCY',6X,'STRESS-X',6X,                   
     &        'STRESS-Y',5X,'STRESS-XY',6X,'STRESS-Z',/,'  NUMBER',/) 
2070  FORMAT (1X,I8,1X,I8,1X,I8,1X,E13.6,1X,E13.6,1X,E13.6,1X,E13.6)
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C+
C+    2D-QUAD WILSON INCOMPATIBLE ELEMENT
C+
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
      SUBROUTINE QUADW(ID,X,Y,Z,U,MHT,E,VU,AREA,LM,XYZ,MATP)
      IMPLICIT REAL*8(A-H,O-Z)
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
      DIMENSION IX(4)
      DIMENSION D(4,4),B(4,8),S(8,8),XG(4,4),WGT(4,4),DB(4),SW(36),
     &          STS(4),STR(4)
C
C     AJ-(0,0)的雅克比矩阵\partial x/\partial \xi
C     F0-转换矩阵F;     BM-修正的B矩阵
C     AH-假设应变法矩阵H; AG-假设应变法矩阵G
C     GA-假设应变法矩阵gamma
      DIMENSION AJ(2,2),F0(3,3),BM(4,8),AH(4,4),GA(4,4)
C
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
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)
      ND=8        !单元自由度数
      ITYPE=2                             !ITYPE: 0-轴对称; 1-平面应变; 2-平面应力
      NINT=2                              !积分方式: 1-单点积分; 2-2*2积分; 3-3*3积分; 4-4*4积分
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
200   DO 190 N=1,NUME
      IMAT=MATP(N)
      YM=E(IMAT)
      PR=VU(IMAT)
      THIC=AREA(IMAT)
      F=YM/(1.+PR)                                                      
      G=F*PR/(1.-2.*PR)                                                 
      H=F + G                                                           
C                                                                       
      !平面应变
      D(1,1)=H                                                          
      D(1,2)=G                                                          
      D(1,3)=0.                                                         
      D(2,1)=G                                                          
      D(2,2)=H                                                          
      D(2,3)=0.                                                         
      D(3,1)=0.                                                         
      D(3,2)=0.                                                         
      D(3,3)=F/2.                                                       
      IF (ITYPE.EQ.1) THEN                                              
      THIC=1.                                                           
      GOTO 120    
      ENDIF                                                             
C                                                                       
      !轴对称                                                                       
      D(1,4)=G                                                          
      D(2,4)=G                                                          
      D(3,4)=0.                                                         
      D(4,1)=G                                                          
      D(4,2)=G                                                          
      D(4,3)=0.                                                         
      D(4,4)=H                                                          
      IF (ITYPE.EQ.0) GO TO 120 
C         
      !平面应力 
      DO 110 I=1,3                                                       
      XA=D(I,4)/D(4,4)                                                   
      DO 110 J=I,3                                                       
      D(I,J)=D(I,J) - D(4,J)*XA                                          
      D(J,I)=D(I,J)
110   CONTINUE
C
      !计算(0,0)点的雅克比阵及其行列式
      AJ(1,1)=0.25D0*(XYZ(1,N)-XYZ(3,N)-XYZ(5,N)+XYZ(7,N))
      AJ(1,2)=0.25D0*(XYZ(2,N)-XYZ(4,N)-XYZ(6,N)+XYZ(8,N))
      AJ(2,1)=0.25D0*(XYZ(1,N)+XYZ(3,N)-XYZ(5,N)-XYZ(7,N))
      AJ(2,2)=0.25D0*(XYZ(2,N)+XYZ(4,N)-XYZ(6,N)-XYZ(8,N))
C
      F0(1,1)=AJ(1,1)*AJ(1,1)
      F0(1,2)=AJ(2,1)*AJ(1,2)
      F0(1,3)=2.D0*AJ(1,1)*AJ(1,2)
      F0(2,1)=F0(1,2)
      F0(2,2)=AJ(2,2)*AJ(2,2)
      F0(2,3)=2.D0*AJ(2,1)*AJ(2,2)
      F0(3,1)=AJ(1,1)*AJ(2,1)
      F0(3,2)=AJ(1,2)*AJ(2,2)
      F0(3,3)=(AJ(1,1)*AJ(2,2)+AJ(1,2)*AJ(2,1))
C
      !计算单元刚度矩阵
120   DO 130 J=1,8                  !初始化单元刚度矩阵
      DO 130 I=J,8
      S(I,J)=0.D0
130   CONTINUE
      IST=3
      IF(ITYPE.EQ.0)IST=4           !轴对称的弹性阵是4*4
      DO 180 LX=1,NINT
      RI=XG(LX,NINT)
      DO 180 LY=1,NINT
      SI=XG(LY,NINT)
      CALL STDM(XYZ(1,N),B,DET,RI,SI,XBAR,N,ITYPE,IOUT)   !计算形函数的导数B和雅克比行列式DET
      IF (ITYPE.GT.0) XBAR=THIC
      WT=WGT(LX,NINT)*WGT(LY,NINT)*XBAR*DET
C
      DO 170 J=1,8                  !开始刚度矩阵计算
      DO 140 K=1,IST                !计算DB=D*B矩阵
      DB(K)=0.0
      DO 140 L=1,IST
      DB(K)=DB(K)+D(K,L)*B(L,J)
140   CONTINUE 
180   CONTINUE

190   CONTINUE
C
300   CONTINUE
C
900   RETURN
C
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
2060  FORMAT (///,' S T R E S S  C A L C U L A T I O N S  F O R  ',     
     &        'E L E M E N T  G R O U P',I4,//,                         
     &        '  ELEMENT',5X,'LOCX',5X,'LOCY',6X,'STRESS-X',6X,                   
     &        'STRESS-Y',5X,'STRESS-XY',6X,'STRESS-Z',/,'  NUMBER',/) 
2070  FORMAT (1X,I8,1X,I8,1X,I8,1X,E13.6,1X,E13.6,1X,E13.6,1X,E13.6)
C
      END
C
      SUBROUTINE QUAD0(NEL,ITYPE,NINT,THIC,YM,PR,XX,S,IOUT)            
C                                                                       
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .        TO CALCULATE ISOPARAMETRIC QUADRILATERAL ELEMENT STIFFNESS . 
C .        MATRIX FOR AXISYMMETRIC, PLANE STRESS, AND PLANE STRAIN    . 
C .        CONDITIONS                                                 . 
C .                                                                   . 
C .  - - INPUT VARIABLES - -                                          . 
C .        NEL       = NUMBER OF ELEMENT                              . 
C .        ITYPE     = ELEMENT TYPE                                   . 
C .                        EQ.0 = AXISYMMETRIC                        . 
C .                        EQ.1 = PLANE STRAIN                        . 
C .                        EQ.2 = PLANE STRESS                        . 
C .        NINT      = GAUSS NUMERICAL INTEGRATION ORDER              . 
C .        THIC      = THICKNESS OF ELEMENT                           . 
C .        YM        = YOUNG'S MODULUS                                . 
C .        PR        = POISSON'S RATIO                                . 
C .        XX(2,4)   = ELEMENT NODE COORDINATES                       . 
C .        S(8,8)    = STORAGE FOR STIFFNESS MATRIX                   . 
C .        IOUT      = UNIT NUMBER USED FOR OUTPUT                    . 
C .                                                                   . 
C .  - - OUTPUT - -                                                   . 
C .        S(8,8)    = CALCULATED STIFFNESS MATRIX                    . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT REAL*8 (A-H,O-Z)
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .   THIS PROGRAM IS USED IN SINGLE PRECISION ARITHMETIC ON CRAY     . 
C .   EQUIPMENT AND DOUBLE PRECISION ARITHMETIC ON IBM MACHINES,      . 
C .   ENGINEERING WORKSTATIONS AND PCS. DEACTIVATE ABOVE LINE FOR     . 
C .   SINGLE PRECISION ARITHMETIC.                                    . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      DIMENSION D(4,4),B(4,8),XX(2,4),S(8,8),XG(4,4),WGT(4,4),DB(4)     
C                                                                       
C     MATRIX XG STORES GAUSS - LEGENDRE SAMPLING POINTS                 
C                                                                       
      DATA XG/   0.D0,   0.D0,   0.D0,   0.D0,   -.5773502691896D0,     
     1 .5773502691896D0,   0.D0,   0.D0,   -.7745966692415D0,   0.D0,   
     2 .7745966692415D0,   0.D0,   -.8611363115941D0,                   
     3 -.3399810435849D0,   .3399810435849D0,   .8611363115941D0 /      
C                                                                       
C     MATRIX WGT STORES GAUSS - LEGENDRE WEIGHTING FACTORS              
C                                                                       
      DATA WGT /  2.D0,   0.D0,   0.D0,   0.D0,   1.D0,   1.D0,         
     1 0.D0,   0.D0,   .5555555555556D0,   .8888888888889D0,            
     2 .5555555555556D0,   0.D0,   .3478548451375D0,   .6521451548625D0,
     3 .6521451548625D0,   .3478548451375D0 /                           
C                                                                       
C     O B T A I N  S T R E S S - S T R A I N  L A W                     
C                                                                       
      F=YM/(1.+PR)                                                      
      G=F*PR/(1.-2.*PR)                                                 
      H=F + G                                                           
C                                                                       
C     PLANE STRAIN ANALYSIS                                             
C                                                                       
      D(1,1)=H                                                          
      D(1,2)=G                                                          
      D(1,3)=0.                                                         
      D(2,1)=G                                                          
      D(2,2)=H                                                          
      D(2,3)=0.                                                         
      D(3,1)=0.                                                         
      D(3,2)=0.                                                         
      D(3,3)=F/2.                                                       
      IF (ITYPE.EQ.1) THEN                                              
      THIC=1.                                                           
      GO TO 20                                                          
      ENDIF                                                             
C                                                                       
C     AXISYMMETRIC ANALYSIS                                             
C                                                                       
      D(1,4)=G                                                          
      D(2,4)=G                                                          
      D(3,4)=0.                                                         
      D(4,1)=G                                                          
      D(4,2)=G                                                          
      D(4,3)=0.                                                         
      D(4,4)=H                                                          
      IF (ITYPE.EQ.0) GO TO 20                                          
C                                                                       
C     FOR PLANE STRESS ANALYSIS CONDENSE STRESS-STRAIN MATRIX           
C                                                                       
      DO 10 I=1,3                                                       
      A=D(I,4)/D(4,4)                                                   
      DO 10 J=I,3                                                       
      D(I,J)=D(I,J) - D(4,J)*A                                          
   10 D(J,I)=D(I,J)                                                     
C                                                                       
C     C A L C U L A T E  E L E M E N T  S T I F F N E S S               
C                                                                       
   20 DO 30 I=1,8                                                       
      DO 30 J=1,8                                                       
   30 S(I,J)=0.                                                         
      IST=3                                                             
      IF (ITYPE.EQ.0) IST=4                                             
      DO 80 LX=1,NINT                                                   
      RI=XG(LX,NINT)                                                    
      DO 80 LY=1,NINT                                                   
      SI=XG(LY,NINT)                                                    
C                                                                       
C     EVALUATE DERIVATIVE OPERATOR B AND THE JACOBIAN DETERMINANT DET   
C                                                                       
      CALL STDM (XX,B,DET,RI,SI,XBAR,NEL,ITYPE,IOUT)                    
C                                                                       
C     ADD CONTRIBUTION TO ELEMENT STIFFNESS                             
C                                                                       
      IF (ITYPE.GT.0) XBAR=THIC                                         
      WT=WGT(LX,NINT)*WGT(LY,NINT)*XBAR*DET                             
      DO 70 J=1,8                                                       
      DO 40 K=1,IST                                                     
      DB(K)=0.0                                                         
      DO 40 L=1,IST                                                     
   40 DB(K)=DB(K) + D(K,L)*B(L,J)                                       
      DO 60 I=J,8                                                       
      STIFF=0.0                                                         
      DO 50 L=1,IST                                                     
   50 STIFF=STIFF + B(L,I)*DB(L)                                        
   60 S(I,J)=S(I,J) + STIFF*WT                                          
   70 CONTINUE                                                          
   80 CONTINUE                                                          
C                                                                       
      DO 90 J=1,8                                                       
      DO 90 I=J,8                                                       
   90 S(J,I)=S(I,J)                                                     
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE STDM (XX,B,DET,R,S,XBAR,NEL,ITYPE,IOUT)                
C                                                                       
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C .                                                                   . 
C .   P R O G R A M                                                   . 
C .     TO EVALUATE THE STRAIN-DISPLACEMENT TRANSFORMATION MATRIX B   . 
C .     AT POINT (R,S) FOR A QUADRILATERAL ELEMENT                    . 
C .                                                                   . 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      DIMENSION XX(2,4),B(4,8),H(4),P(2,4),XJ(2,2),XJI(2,2)             
C                                                                       
      RP = 1.0 + R                                                      
      SP = 1.0 + S                                                      
      RM = 1.0 - R                                                      
      SM = 1.0 - S                                                      
C                                                                       
C     INTERPOLATION FUNCTIONS                                           
C                                                                       
      H(1) = 0.25* RP* SP                                               
      H(2) = 0.25* RM* SP                                               
      H(3) = 0.25* RM* SM                                               
      H(4) = 0.25* RP* SM                                               
C                                                                       
C     NATURAL COORDINATE DERIVATIVES OF THE INTERPOLATION FUNCTIONS     
C                                                                       
C        1. WITH RESPECT TO R                                           
C                                                                       
      P(1,1) = 0.25* SP             !\partial Ni/\partial \xi
      P(1,2) = - P(1,1)                                                 
      P(1,3) = - 0.25* SM                                               
      P(1,4) = - P(1,3)                                                 
C                                                                       
C        2. WITH RESPECT TO S                                           
C
      P(2,1) = 0.25* RP             !\partial Ni/\partial \eta
      P(2,2) = 0.25* RM                                                 
      P(2,3) = - P(2,2)                                                 
      P(2,4) = - P(2,1)                                                 
C                                                                       
C     EVALUATE THE JACOBIAN MATRIX AT POINT (R,S)                       
C                                                                       
   10 DO 30 I=1,2                                                       
      DO 30 J=1,2                                                       
      DUM = 0.0                                                         
      DO 20 K=1,4                                                       
   20 DUM=DUM + P(I,K)*XX(J,K)                                          
   30 XJ(I,J)=DUM                   !\partial x/\partial \xi
C                                                                       
C     COMPUTE THE DETERMINANT OF THE JACOBIAN MATRIX AT POINT (R,S)     
C                                                                       
      DET = XJ(1,1)* XJ(2,2) - XJ(2,1)* XJ(1,2)                         
      IF (DET.GT.0.00000001) GO TO 40                                   
      WRITE (IOUT,2000) NEL                                             
      GO TO 800                                                         
C                                                                       
C     COMPUTE INVERSE OF THE JACOBIAN MATRIX                            
C                                                                       
   40 DUM=1./DET                                                        
      XJI(1,1) = XJ(2,2)* DUM                                           
      XJI(1,2) =-XJ(1,2)* DUM                                           
      XJI(2,1) =-XJ(2,1)* DUM                                           
      XJI(2,2) = XJ(1,1)* DUM                                           
C                                                                       
C     EVALUATE GLOBAL DERIVATIVE OPERATOR B                             
C                                                                       
      K2=0                                                              
      DO 60 K=1,4                                                       
      K2=K2 + 2                                                         
      B(1,K2-1) = 0.                                                    
      B(1,K2  ) = 0.                                                    
      B(2,K2-1) = 0.                                                    
      B(2,K2  ) = 0.                                                    
      DO 50 I=1,2                                                       
      B(1,K2-1) = B(1,K2-1) + XJI(1,I) * P(I,K)                         
   50 B(2,K2  ) = B(2,K2  ) + XJI(2,I) * P(I,K)                         
      B(3,K2  ) = B(1,K2-1)                                             
   60 B(3,K2-1) = B(2,K2  )                                             
C                                                                       
C     IN CASE OF PLANE STRAIN OR PLANE STRESS ANALYSIS DO NOT INCLUDE   
C     THE NORMAL STRAIN COMPONENT                                       
C                                                                       
      IF (ITYPE.GT.0) GO TO 900                                         
C                                                                       
C     COMPUTE THE RADIUS AT POINT (R,S)                                 
C                                                                       
      XBAR=0.0                                                          
      DO 70 K=1,4                                                       
   70 XBAR=XBAR + H(K)*XX(1,K)                                          
C                                                                       
C     EVALUATE THE HOOP STRAIN-DISPLACEMENT RELATION                    
C                                                                       
      IF (XBAR.GT.0.00000001) GO TO 90                                  
C                                                                       
C     FOR THE CASE OF ZERO RADIUS EQUATE RADIAL TO HOOP STRAIN          
C                                                                       
      DO 80 K=1,8                                                       
   80 B(4,K)=B(1,K)                                                     
      GO TO 900                                                         
C                                                                       
C     NON-ZERO RADIUS                                                   
C                                                                       
   90 DUM=1./XBAR                                                       
      K2=0                                                              
      DO 100 K=1,4                                                      
      K2=K2 + 2                                                         
      B(4,K2  ) = 0.                                                    
  100 B(4,K2-1) = H(K)*DUM                                              
      GO TO 900                                                         
C                                                                       
  800 STOP                                                              
  900 RETURN                                                            
C                                                                       
 2000 FORMAT (//,' *** ERROR *** ',                                     
     1    ' ZERO OR NEGATIVE JACOBIAN DETERMINANT FOR ELEMENT (',I8,')')
C                                                                       
      END                                                               