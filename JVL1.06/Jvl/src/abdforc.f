
      SUBROUTINE BDFORC
C
C...PURPOSE  To calculate the forces on the bodies
C
C...INPUT    ALFA       angle of attack (for stability-axis definition)
C            VINF()     freestream velocity components
C            WROT()     roll,pitch,yaw  rates
C            MACH       Mach number
C            NBODY      number of bodies
C            NL(.)      number of line segments on each body
C            RL(..)     line segment coordinates
C            RADL(.)    body radii
C          
C...OUTPUT   DCPB()     body element loadings
C            CXYZTOT    total force,moment coefficients
C            CNC        span load for each strip
C
C...COMMENTS   
C
      INCLUDE 'AVL.INC'
C
      REAL R(3), RROT(3)
      REAL VEFF(3)    , VROT(3)  ,
     &     VEFF_U(3,NUMAX), VROT_U(3), WROT_U(3)
      REAL DRL(3), ESL(3), 
     &     F(3), F_U(3,NUMAX)
C
      REAL DREF(3)
      REAL FSYM(3), MSYM(3)
C
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
C
C
      BETM = SQRT(1.0 - MACH**2)
C
      SINA = SIN(ALFA)
      COSA = COS(ALFA)
C
      DREF(1) = BREF
      DREF(2) = CREF
      DREF(3) = BREF
C
C---- add on body force contributions
      DO 200 IB = 1, NBODY
C------ clear body forces and moments for accumulation
        DO K = 1, 3
          CFLI(K,IB) = 0.
          CMLI(K,IB) = 0.
          CFLV(K,IB) = 0.
          CMLV(K,IB) = 0.
          DO N=1, NUMAX
            CFLI_U(K,IB,N) = 0.
            CMLI_U(K,IB,N) = 0.
            CFLV_U(K,IB,N) = 0.
            CMLV_U(K,IB,N) = 0.
          ENDDO
          DO N=1, NCONTROL
            CFLI_D(K,IB,N) = 0.
            CMLI_D(K,IB,N) = 0.
            CFLV_D(K,IB,N) = 0.
            CMLV_D(K,IB,N) = 0.
          ENDDO
          DO N=1, NDESIGN
            CFLI_G(K,IB,N) = 0.
            CMLI_G(K,IB,N) = 0.
            CFLV_G(K,IB,N) = 0.
            CMLV_G(K,IB,N) = 0.
          ENDDO
          DO N=1, NVARJET
            CFLI_J(K,IB,N) = 0.
            CMLI_J(K,IB,N) = 0.
            CFLV_J(K,IB,N) = 0.
            CMLV_J(K,IB,N) = 0.
          ENDDO
        ENDDO
C
        DO 205 ILSEG = 1, NL(IB)-1
          L1 = LFRST(IB) + ILSEG - 1
          L2 = LFRST(IB) + ILSEG
C
          L = L1
C
          DRL(1) = (RL(1,L2) - RL(1,L1))/BETM
          DRL(2) =  RL(2,L2) - RL(2,L1)
          DRL(3) =  RL(3,L2) - RL(3,L1)
          DRLMAG = SQRT(DRL(1)**2 + DRL(2)**2 + DRL(3)**2)
          IF(DRLMAG.EQ.0.0) THEN
           DRLMI = 0.0
          ELSE
           DRLMI = 1.0/DRLMAG
          ENDIF
C
          DIA = RADL(L1) + RADL(L2)
          IF(DIA.LE.0.0) THEN
           DINV = 0.0
          ELSE
           DINV = 1.0/DIA
          ENDIF
C
C-------- unit vector along line segment
          ESL(1) = DRL(1) * DRLMI
          ESL(2) = DRL(2) * DRLMI
          ESL(3) = DRL(3) * DRLMI
C
          R(1) = 0.5*(RL(1,L2)+RL(1,L1)) - XYZREF(1)
          R(2) = 0.5*(RL(2,L2)+RL(2,L1)) - XYZREF(2)
          R(3) = 0.5*(RL(3,L2)+RL(3,L1)) - XYZREF(3)
C
          RROT(1) = 0.5*(RL(1,L2)+RL(1,L1)) - XYZREF(1)
          RROT(2) = 0.5*(RL(2,L2)+RL(2,L1)) - XYZREF(2)
          RROT(3) = 0.5*(RL(3,L2)+RL(3,L1)) - XYZREF(3)
C
C-------- go over freestream velocity and rotation components
          CALL CROSS(RROT,WROT,VROT)
C
          VEFF(1) = (VINF(1) + VROT(1))/BETM
          VEFF(2) =  VINF(2) + VROT(2)
          VEFF(3) =  VINF(3) + VROT(3)
C
C-------- set VEFF sensitivities to freestream,rotation components
          DO K = 1, 3
            VEFF_U(1,K) = 0.
            VEFF_U(2,K) = 0.
            VEFF_U(3,K) = 0.
            VEFF_U(K,K) = 1.0
          ENDDO
C
          DO K = 4, 6
            WROT_U(1) = 0.
            WROT_U(2) = 0.
            WROT_U(3) = 0.
            WROT_U(K-3) = 1.0
            CALL CROSS(RROT,WROT_U,VROT_U)
C
            VEFF_U(1,K) = VROT_U(1)
            VEFF_U(2,K) = VROT_U(2)
            VEFF_U(3,K) = VROT_U(3)
          ENDDO
C
C-------- U.es
          US = VEFF(1)*ESL(1) + VEFF(2)*ESL(2) + VEFF(3)*ESL(3)
C
C
          DO K = 1, 3
C---------- velocity projected on normal plane = U - (U.es) es
            UN = VEFF(K) - US*ESL(K)
C
C---------- resulting force
            F(K) = UN*SRC(L)
C
            DO N = 1, NUMAX
              UN_U = VEFF_U(K,N)
     &             - ( VEFF_U(1,N)*ESL(1)
     &                +VEFF_U(2,N)*ESL(2)
     &                +VEFF_U(3,N)*ESL(3))*ESL(K)
              F_U(K,N) = UN *SRC_U(L,N) + UN_U*SRC(L)
            ENDDO
C
C---------- average delta(Cp) on area of body segment
            DCPB(K,L) = F(K) * 2.0 * DINV*DRLMI
          ENDDO
C
          DO K = 1, 3
            IC = ICRS(K)
            JC = JCRS(K)
            CFLI(K,IB) = CFLI(K,IB) + F(K) * 2.0/SREF
            CMLI(K,IB) = CMLI(K,IB) + (  R(IC)*F(JC)
     &                                 - R(JC)*F(IC)) * 2.0/SREF/DREF(K)
C
            DO N = 1, NUMAX
              CFLI_U(K,L,N) = CFLI_U(K,L,N) + F_U(K,N) * 2.0/SREF
              CMLI_U(K,L,N) = CMLI_U(K,L,N) + ( R(IC)*F_U(JC,N)
     &                                        - R(JC)*F_U(IC,N) )
     &                                                * 2.0/SREF/DREF(K)
            ENDDO
          ENDDO
C
 205    CONTINUE
 200  CONTINUE
C
C---- clear total body forces and moments for accumulation
      DO K = 1, 3
        CFBI(K) = 0.
        CMBI(K) = 0.
        DO N=1, NUMAX
          CFBI_U(K,N) = 0.
          CMBI_U(K,N) = 0.
        ENDDO
        DO N=1, NCONTROL
          CFBI_D(K,N) = 0.
          CMBI_D(K,N) = 0.
        ENDDO
        DO N=1, NDESIGN
          CFBI_G(K,N) = 0.
          CMBI_G(K,N) = 0.
        ENDDO
        DO N=1, NVARJET
          CFBI_J(K,N) = 0.
          CMBI_J(K,N) = 0.
        ENDDO
      ENDDO
C
C---- total forces summed from surface forces
      DO 300 IB = 1, NBODY
C------ Double the X,Z forces, zero Y force for a Y symmetric case,
C-       but only if the body isn't on the centerline
        IF(IYSYM.EQ.1 .AND. IBCENT(IB).NE.1) THEN
         FSYM(1) = 2.0
         FSYM(2) = 0.
         FSYM(3) = 2.0
         MSYM(1) = 0.
         MSYM(2) = 2.0
         MSYM(3) = 0.
        ELSE
         FSYM(1) = 1.0
         FSYM(2) = 1.0
         FSYM(3) = 1.0
         MSYM(1) = 1.0
         MSYM(2) = 1.0
         MSYM(3) = 1.0
        ENDIF
C
        DO K = 1, 3
          CFBI(K) = CFBI(K) + FSYM(K)*CFLI(K,IB)
          CMBI(K) = CMBI(K) + MSYM(K)*CMLI(K,IB)
          DO N=1, NUMAX
            CFBI_U(K,N) = CFBI_U(K,N) + FSYM(K)*CFLI_U(K,IB,N)
            CMBI_U(K,N) = CMBI_U(K,N) + MSYM(K)*CMLI_U(K,IB,N)
          ENDDO
          DO N=1, NCONTROL
            CFBI_D(K,N) = CFBI_D(K,N) + FSYM(K)*CFLI_D(K,IB,N)
            CMBI_D(K,N) = CMBI_D(K,N) + MSYM(K)*CMLI_D(K,IB,N)
          ENDDO
          DO N=1, NDESIGN
            CFBI_G(K,N) = CFBI_G(K,N) + FSYM(K)*CFLI_G(K,IB,N)
            CMBI_G(K,N) = CMBI_G(K,N) + MSYM(K)*CMLI_G(K,IB,N)
          ENDDO
          DO N=1, NVARJET
            CFBI_J(K,N) = CFBI_J(K,N) + FSYM(K)*CFLI_J(K,IB,N)
            CMBI_J(K,N) = CMBI_J(K,N) + MSYM(K)*CMLI_J(K,IB,N)
          ENDDO
        ENDDO
 300  CONTINUE
C
      RETURN
      END ! BDFORC
