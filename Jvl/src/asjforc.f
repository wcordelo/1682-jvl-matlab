
      SUBROUTINE SJFORC
C
C...PURPOSE  To calculate the jet forces on the surfaces,
C            by strip and surface.
C
C...INPUT    Global Data in labelled commons, defining configuration
C            ALFA       Angle of attack (for stability-axis definition)
C            VINF()     Freestream velocity components
C            WROT()     Roll,Pitch,Yaw  rates
C            MACH       Mach number
C            NVOR       Number of vortices
C            R1         Coordinates of endpoint #1 of bound vortex
C            R2         Coordinates of endpoint #2 of bound vortex
C            ENV        Normal vector at bound vortex midpoint
C            DX         X-length of vortex lattice panel
C            NVSTRP     No. of vortices in strip
C          
C...OUTPUT   DCP                   Vortex element loadings
C            CXYZTOT                Total force,moment coefficients
C            CDFF                  Far-field drag (Trefftz plane)
C            CxxSURF               Surface force,moment coefficients
C            CxxSTRP               Strip force coefficients
C            CNC                   Span load for each strip
C
C...COMMENTS   
C
      INCLUDE 'AVL.INC'
C
      REAL RR(3), R(3)

      REAL DEN(3), ENTE(3)
      REAL ENTEH(3), ENTEH_D(3,NDMAX), ENTEH_G(3,NGMAX)
      REAL VJ0(3), VJ(3), VJHAT(3), FJET(3)
      REAL VJN_D(NDMAX), VJINV_D(NDMAX),
     &     VJN_G(NGMAX), VJINV_G(NGMAX)
      REAL VJ_D(3,NDMAX), VJHAT_D(3,NDMAX),
     &     VJ_G(3,NGMAX), VJHAT_G(3,NGMAX)
C
      REAL PJET, MJET, NJET, MJET_PJET
      REAL PJET_J(NJMAX), MJET_J(NJMAX)
C
      REAL RJET(3), RROT(3), VROT(3), VEFF(3), VEFF_U(3,6)
      REAL F(3), F_U(3,6), F_D(3,NDMAX), F_G(3,NGMAX), F_J(3,NJMAX)
      REAL CM(3), CM_U(3,6), CM_D(3,NDMAX), CM_G(3,NGMAX), CM_J(3,NJMAX)
C
      REAL ENAVE(3), SPN(3), UDRAG(3), ULIFT(3), VPERP(3), DELR(3),
     &     CLV_U(NUMAX),
     &     CLV_D(NDMAX),
     &     CLV_G(NGMAX),
     &     CLV_J(NJMAX)
C
      REAL FSYM(3), MSYM(3)
C
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
C
      SINA = SIN(ALFA)
      COSA = COS(ALFA)
C
      DO 100 JJET = NSTRIPS+1, NSTRIP
        I1JET  = IJFRST(JJET)
C
C------ TE element of solid-surface strip, from which this jet strip trails
        I = IJETM(I1JET)
C
C------ strip and surface from which this jet strip trails
        J = ISTRPV(I)
        L = ISURFV(I)
C
C------ local strip lift and drag directions
C-       "spanwise" vector is cross product of strip normal with X chordline 
        SPN(1) =  0.0
        SPN(2) =  ENSZ(J)
        SPN(3) = -ENSY(J)
C
C------ Stability axes stream vector defines drag direction
        UDRAG(1) = COSA
        UDRAG(2) = 0.0
        UDRAG(3) = SINA
C
C------ Lift direction is vector product of "stream" and spanwise vector
        CALL CROSS(UDRAG,SPN,ULIFT)
        ULMAG = SQRT(ULIFT(1)**2 + ULIFT(2)**2 + ULIFT(3)**2)
        IF(ULMAG .EQ. 0.0) THEN
          ULIFT(3) = 1.0
         ELSE
          ULIFT(1) = ULIFT(1)/ULMAG
          ULIFT(2) = ULIFT(2)/ULMAG
          ULIFT(3) = ULIFT(3)/ULMAG
        ENDIF
C
C------ strip reference area
        SSTRIP = CHORD(J)*WSTRIP(J)
C
C------ set undeflected jet direction
        VJ0(1) = 1.0
        VJ0(2) = 0.0
        VJ0(3) = 0.0
C
C------ set TE normal vector change
        DEN(1) = 0.
        DEN(2) = 0.
        DEN(3) = 0.
        DO N = 1, NCONTROL
          DEN(1) = DEN(1) + ENC_D(1,I,N)*DELCON(N)
          DEN(2) = DEN(2) + ENC_D(2,I,N)*DELCON(N)
          DEN(3) = DEN(3) + ENC_D(3,I,N)*DELCON(N)
        ENDDO
C
C------ set modified TE normal vector
        ENTE(1) = ENC(1,I) + DEN(1)
        ENTE(2) = ENC(2,I) + DEN(2)
        ENTE(3) = ENC(3,I) + DEN(3)
        ENSQ = ENTE(1)**2 + ENTE(2)**2 + ENTE(3)**2
        IF(ENSQ .EQ. 0.0) THEN
         ENI = 1.0
        ELSE
         ENI = 1.0 / SQRT(ENSQ)
        ENDIF
C
        DO K = 1, 3
          ENTEH(K) = ENTE(K)*ENI
          DO N = 1, NCONTROL
            ENTEH_D(K,N) = ENC_D(K,I,N)*ENI
     &                   - ENTE(K)*( ENTE(1)*ENC_D(1,I,N)
     &                              +ENTE(2)*ENC_D(2,I,N)
     &                              +ENTE(3)*ENC_D(3,I,N) ) * ENI**3
          ENDDO
          DO N = 1, NDESIGN
            ENTEH_G(K,N) = ENC_G(K,I,N)*ENI
     &                   - ENTE(K)*( ENTE(1)*ENC_G(1,I,N)
     &                              +ENTE(2)*ENC_G(2,I,N)
     &                              +ENTE(3)*ENC_G(3,I,N) ) * ENI**3
          ENDDO
        ENDDO
C
        VJN = VJ0(1)*ENTEH(1)
     &      + VJ0(2)*ENTEH(2)
     &      + VJ0(3)*ENTEH(3)
        DO N = 1, NCONTROL
          VJN_D(N) = VJ0(1)*ENTEH_D(1,N)
     &             + VJ0(2)*ENTEH_D(2,N)
     &             + VJ0(3)*ENTEH_D(3,N)
        ENDDO
        DO N = 1, NDESIGN
          VJN_G(N) = VJ0(1)*ENTEH_G(1,N)
     &             + VJ0(2)*ENTEH_G(2,N)
     &             + VJ0(3)*ENTEH_G(3,N)
        ENDDO
C
C------ vector along turned jet
        DO K = 1, 3
          VJ(K) = VJ0(K) - VJN*ENTEH(K)
          DO N = 1, NCONTROL
            VJ_D(K,N) = -VJN_D(N)*ENTEH(K) - VJN*ENTEH_D(K,N)
          ENDDO
          DO N = 1, NDESIGN
            VJ_G(K,N) = -VJN_G(N)*ENTEH(K) - VJN*ENTEH_G(K,N)
          ENDDO
        ENDDO
C
C------ set normalizing factor
        VJSQ = VJ(1)**2 + VJ(2)**2 + VJ(3)**2
        IF(VJSQ .EQ. 0.0) THEN
         VJINV = 1.0
         DO N = 1, NCONTROL
           VJINV_D(N) = 0.
         ENDDO
         DO N = 1, NDESIGN
           VJINV_G(N) = 0.
         ENDDO
        ELSE
         VJINV = 1.0/SQRT(VJSQ)
         DO N = 1, NCONTROL
           VJINV_D(N) = -VJINV/VJSQ
     &                * (  VJ(1)*VJ_D(1,N)
     &                   + VJ(2)*VJ_D(2,N)
     &                   + VJ(3)*VJ_D(3,N) )
         ENDDO
         DO N = 1, NDESIGN
           VJINV_G(N) = -VJINV/VJSQ
     &                * (  VJ(1)*VJ_G(1,N)
     &                   + VJ(2)*VJ_G(2,N)
     &                   + VJ(3)*VJ_G(3,N) )
         ENDDO
        ENDIF
C
C------ unit vector along turned jet
        DO K = 1, 3
          VJHAT(K) = VJ(K)*VJINV
          DO N = 1, NCONTROL
            VJHAT_D(K,N) = VJ_D(K,N)*VJINV + VJ(K)*VJINV_D(N)
          ENDDO
          DO N = 1, NDESIGN
            VJHAT_G(K,N) = VJ_G(K,N)*VJINV + VJ(K)*VJINV_G(N)
          ENDDO
        ENDDO
C
C
C------ accumulate excess momentum/span
        DPJET = 0.0
        DO N = 1, NVARJET
          DPJET = DPJET + DELJET(N)*GJSTRP(JJET,N)
          PJET_J(N) =               GJSTRP(JJET,N)
        ENDDO
C
C------ jet height
        NJET = HJSTRP(JJET,1)
C
C------ jet total momentum/span
        PJET = MAX( DPJET + NJET , 0.0 )
C
C------ calculate mass flow/span
        RHJET = 1.0
        MJET = SQRT(RHJET*NJET*PJET)
        IF(PJET .GT. 0.0) THEN
          MJET_PJET = 0.5*MJET / PJET
        ELSE
          MJET_PJET = 0.0
        ENDIF
C
C------ store strip momentum and mass coefficients
        CJS(J) = PJET*2.0 * WSTRIP(J)
        CQS(J) = MJET     * WSTRIP(J)
C
        DO N = 1, NVARJET
          CJS_J(J,N) = PJET_J(N)*2.0 * WSTRIP(J)
          CQS_J(J,N) = MJET_PJET*PJET_J(N) * WSTRIP(J)
        ENDDO
C
C------ location where jet force is applied
        RJET(1) = RLE(1,J) + CHORD(J)
        RJET(2) = RLE(2,J)
        RJET(3) = RLE(3,J)

cc        RJET(1) = RLE(1,J) + 0.75*CHORD(J)     !@@@
C
C------ set strip jet force
        DO K = 1, 3
          IC = ICRS(K)
          JC = JCRS(K)
C
C-------- local velocity, which sets momentum flowing into propulsor
          VEFF(K) = VINF(K) + RJET(IC)*WROT(JC) - RJET(JC)*WROT(IC)
          VEFF_U(K,1) = 0.
          VEFF_U(K,2) = 0.
          VEFF_U(K,3) = 0.
          VEFF_U(K,K) = 1.0
          VEFF_U(K,IC+3) =                     - RJET(JC)
          VEFF_U(K,JC+3) =    RJET(IC)
          VEFF_U(K, K+3) = 0.
C
C-------- jet force
          F(K)    = -(PJET*VJHAT(K) - MJET*VEFF(K)     )*2.0*WSTRIP(J)
          F_PJET  = -(     VJHAT(K) - MJET_PJET*VINF(K))*2.0*WSTRIP(J)
          F_VJHAT = -(PJET                             )*2.0*WSTRIP(J)
          F_VEFF  = -(              - MJET             )*2.0*WSTRIP(J)
          DO N = 1, NUMAX
            F_U(K,N) = F_VEFF*VEFF_U(K,N)
          ENDDO
          DO N = 1, NCONTROL
            F_D(K,N) = F_VJHAT*VJHAT_D(K,N)
          ENDDO
          DO N = 1, NDESIGN
            F_G(K,N) = F_VJHAT*VJHAT_G(K,N)
          ENDDO
          DO N = 1, NVARJET
            F_J(K,N) = F_PJET*PJET_J(N)
          ENDDO
        ENDDO
C
C------ moment arm 
        R(1) = RJET(1) - XYZREF(1)
        R(2) = RJET(2) - XYZREF(2)
        R(3) = RJET(3) - XYZREF(3)
C
C------ set strip jet force, moment
        DO K = 1, 3
          IC = ICRS(K)
          JC = JCRS(K)
C
          CFSJ(K,J) = F(K)
          CMSJ(K,J) = R(IC)*F(JC) - R(JC)*F(IC)
          DO N=1, NUMAX
            CFSJ_U(K,J,N) = F_U(K,N)
            CMSJ_U(K,J,N) = R(IC)*F_U(JC,N) - R(JC)*F_U(IC,N)
          ENDDO
          DO N=1, NCONTROL
            CFSJ_D(K,J,N) = F_D(K,N)
            CMSJ_D(K,J,N) = R(IC)*F_D(JC,N) - R(JC)*F_D(IC,N)
          ENDDO
          DO N=1, NDESIGN
            CFSJ_G(K,J,N) = F_G(K,N)
            CMSJ_G(K,J,N) = R(IC)*F_G(JC,N) - R(JC)*F_G(IC,N)
          ENDDO
          DO N=1, NVARJET
            CFSJ_J(K,J,N) = F_J(K,N)
            CMSJ_J(K,J,N) = R(IC)*F_J(JC,N) - R(JC)*F_J(IC,N)
          ENDDO
        ENDDO
C
 100  CONTINUE
C
C---- Surface forces and moments summed from strip forces
      DO 200 IS = 1, NSURFS
C------ clear surface forces and moments for accumulation
        DO K = 1, 3
          CFNJ(K,IS) = 0.
          CMNJ(K,IS) = 0.
          DO N=1, NUMAX
            CFNJ_U(K,IS,N) = 0.
            CMNJ_U(K,IS,N) = 0.
          ENDDO
          DO N=1, NCONTROL
            CFNJ_D(K,IS,N) = 0.
            CMNJ_D(K,IS,N) = 0.
          ENDDO
          DO N=1, NDESIGN
            CFNJ_G(K,IS,N) = 0.
            CMNJ_G(K,IS,N) = 0.
          ENDDO
          DO N=1, NVARJET
            CFNJ_J(K,IS,N) = 0.
            CMNJ_J(K,IS,N) = 0.
          ENDDO
        ENDDO
C
        DO 220 JJ = 1, NJ(IS)
          J = JFRST(IS) + JJ-1
          DO K = 1, 3
            CFNJ(K,IS) = CFNJ(K,IS) + CFSJ(K,J)
            CMNJ(K,IS) = CMNJ(K,IS) + CMSJ(K,J)
            DO N=1, NUMAX
              CFNJ_U(K,IS,N) = CFNJ_U(K,IS,N) + CFSJ_U(K,J,N)
              CMNJ_U(K,IS,N) = CMNJ_U(K,IS,N) + CMSJ_U(K,J,N)
            ENDDO
            DO N=1, NCONTROL
              CFNJ_D(K,IS,N) = CFNJ_D(K,IS,N) + CFSJ_D(K,J,N)
              CMNJ_D(K,IS,N) = CMNJ_D(K,IS,N) + CMSJ_D(K,J,N)
            ENDDO
            DO N=1, NDESIGN
              CFNJ_G(K,IS,N) = CFNJ_G(K,IS,N) + CFSJ_G(K,J,N)
              CMNJ_G(K,IS,N) = CMNJ_G(K,IS,N) + CMSJ_G(K,J,N)
            ENDDO
            DO N=1, NVARJET
              CFNJ_J(K,IS,N) = CFNJ_J(K,IS,N) + CFSJ_J(K,J,N)
              CMNJ_J(K,IS,N) = CMNJ_J(K,IS,N) + CMSJ_J(K,J,N)
            ENDDO
          ENDDO
 220    CONTINUE
 200  CONTINUE
C
C
C---- clear surface forces and moments for accumulation
      DO K = 1, 3
        CFTJ(K) = 0.
        CMTJ(K) = 0.
        DO N=1, NUMAX
          CFTJ_U(K,N) = 0.
          CMTJ_U(K,N) = 0.
        ENDDO
        DO N=1, NCONTROL
          CFTJ_D(K,N) = 0.
          CMTJ_D(K,N) = 0.
        ENDDO
        DO N=1, NDESIGN
          CFTJ_G(K,N) = 0.
          CMTJ_G(K,N) = 0.
        ENDDO
        DO N=1, NVARJET
          CFTJ_J(K,N) = 0.
          CMTJ_J(K,N) = 0.
        ENDDO
      ENDDO
C
C---- total forces summed from surface forces
      DO 300 IS = 1, NSURFS
        DO K = 1, 3
          CFTJ(K) = CFTJ(K) + CFNJ(K,IS)
          CMTJ(K) = CMTJ(K) + CMNJ(K,IS)
          DO N=1, NUMAX
            CFTJ_U(K,N) = CFTJ_U(K,N) + CFNJ_U(K,IS,N)
            CMTJ_U(K,N) = CMTJ_U(K,N) + CMNJ_U(K,IS,N)
          ENDDO
          DO N=1, NCONTROL
            CFTJ_D(K,N) = CFTJ_D(K,N) + CFNJ_D(K,IS,N)
            CMTJ_D(K,N) = CMTJ_D(K,N) + CMNJ_D(K,IS,N)
          ENDDO
          DO N=1, NDESIGN
            CFTJ_G(K,N) = CFTJ_G(K,N) + CFNJ_G(K,IS,N)
            CMTJ_G(K,N) = CMTJ_G(K,N) + CMNJ_G(K,IS,N)
          ENDDO
          DO N=1, NVARJET
            CFTJ_J(K,N) = CFTJ_J(K,N) + CFNJ_J(K,IS,N)
            CMTJ_J(K,N) = CMTJ_J(K,N) + CMNJ_J(K,IS,N)
          ENDDO
        ENDDO
C
 300  CONTINUE
C
C---- total jet momentum and mass summed from strip values
      CJT = 0.
      CQT = 0.
      DO N=1, NVARJET
        CJT_J(N) = 0.
        CQT_J(N) = 0.
      ENDDO
C
      DO J = 1, NSTRIPS
        CJT = CJT + CJS(J)
        CQT = CQT + CQS(J)
        DO N=1, NVARJET
          CJT_J(N) = CJT_J(N) + CJS_J(J,N)
          CQT_J(N) = CQT_J(N) + CQS_J(J,N)
        ENDDO
      ENDDO
C
C
C---- Double the X,Z forces, zero Y force for a Y symmetric case
      IF(IYSYM.EQ.1) THEN
       CJT = 2.0 * CJT
       CQT = 2.0 * CQT
       DO N=1, NVARJET
         CJT_J(N) = 2.0 * CJT_J(N)
         CQT_J(N) = 2.0 * CQT_J(N)
       ENDDO
C
       FSYM(1) = 2.0
       FSYM(2) = 0.
       FSYM(3) = 2.0
       MSYM(1) = 0.
       MSYM(2) = 2.0
       MSYM(3) = 0.
C
       DO K = 1, 3
         CFTJ(K) = FSYM(K)*CFTJ(K)
         CMTJ(K) = MSYM(K)*CMTJ(K)
         DO N = 1, NUMAX
           CFTJ_U(K,N) = FSYM(K) * CFTJ_U(K,N)
           CMTJ_U(K,N) = MSYM(K) * CMTJ_U(K,N)
         ENDDO
         DO N = 1, NCONTROL
           CFTJ_D(K,N) = FSYM(K) * CFTJ_D(K,N)
           CMTJ_D(K,N) = MSYM(K) * CMTJ_D(K,N)
         ENDDO
         DO N = 1, NDESIGN
           CFTJ_G(K,N) = FSYM(K) * CFTJ_G(K,N)
           CMTJ_G(K,N) = MSYM(K) * CMTJ_G(K,N)
         ENDDO
         DO N = 1, NVARJET
           CFTJ_J(K,N) = FSYM(K) * CFTJ_J(K,N)
           CMTJ_J(K,N) = MSYM(K) * CMTJ_J(K,N)
         ENDDO
       ENDDO
      ENDIF
C
      RETURN
      END ! SJFORC
