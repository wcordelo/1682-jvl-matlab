      
      SUBROUTINE SFFORC
C
C...PURPOSE  To calculate the forces on the surfaces,
C            by vortex, strip and surface.
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
C
C...COMMENTS   
C
      INCLUDE 'AVL.INC'
C
      REAL RC4(3), RROT(3)
      REAL VEFF(3)    , VROT(3)  ,
     &     VEFF_U(3,6), VROT_U(3), WROT_U(3)
      REAL VPERP(3)
      REAL DL(3), R(3), MH(3)
      REAL VXL(3), VXL_U(3,NUMAX)
      REAL F(3),
     &     F_U(3,NUMAX),
     &     F_D(3,NDMAX),
     &     F_G(3,NGMAX),
     &     F_J(3,NJMAX)
      REAL SPN(3), UDRAG(3), ULIFT(3)
C
      REAL CLV,
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
C***********************************************************************
C...Integrate the forces strip-wise, surface-wise and total-wise
C***********************************************************************
C
C=====================================================================
C--- Calculate strip forces...
C-    normalized to strip reference quantities (strip area, chord)
      DO 100 J = 1, NSTRIP
        ISURF = ISURFS(J)
C
cC------ for jet sheet, skip all force summation
c        IF(IFTYPE(ISURF).EQ.1) GO TO 100
C
        SSTRIP = CHORD(J)*WSTRIP(J)
C
        CR = CHORD(J)
        SR = SSTRIP
C
        XTE1 = RLE1(1,J) + CHORD1(J)
        XTE2 = RLE2(1,J) + CHORD2(J)
C
C------ Define local strip lift and drag directions
C------ The "spanwise" vector is cross product of strip normal with X chordline 
        SPN(1) =  0.0
        SPN(2) =  ENSZ(J)
        SPN(3) = -ENSY(J)
C
        UDRAG(1) = COSA
        UDRAG(2) = 0.0
        UDRAG(3) = SINA
C
C------ Strip lift direction is vector product of "stream" and spanwise vector
        CALL CROSS(UDRAG,SPN,ULIFT)
        ULMAG = SQRT(DOT(ULIFT,ULIFT))
        IF(ULMAG.EQ.0.) THEN
          ULIFT(3) = 1.0
         ELSE
          ULIFT(1) = ULIFT(1)/ULMAG
          ULIFT(2) = ULIFT(2)/ULMAG
          ULIFT(3) = ULIFT(3)/ULMAG
        ENDIF
C
C------ Use the strip 1/4 chord location for strip moments
        RC4(1)  = RLE(1,J) + 0.25*CR
        RC4(2)  = RLE(2,J)
        RC4(3)  = RLE(3,J)
C
C------ clear strip forces and moments for accumulation
        DO K = 1, 3
          CFSI(K,J) = 0.
          CMSI(K,J) = 0.
          CFSV(K,J) = 0.
          CMSV(K,J) = 0.
          DO N=1, NUMAX
            CFSI_U(K,J,N) = 0.
            CMSI_U(K,J,N) = 0.
            CFSV_U(K,J,N) = 0.
            CMSV_U(K,J,N) = 0.
          ENDDO
          DO N=1, NCONTROL
            CFSI_D(K,J,N) = 0.
            CMSI_D(K,J,N) = 0.
            CFSV_D(K,J,N) = 0.
            CMSV_D(K,J,N) = 0.
          ENDDO
          DO N=1, NDESIGN
            CFSI_G(K,J,N) = 0.
            CMSI_G(K,J,N) = 0.
            CFSV_G(K,J,N) = 0.
            CMSV_G(K,J,N) = 0.
          ENDDO
          DO N=1, NVARJET
            CFSI_J(K,J,N) = 0.
            CMSI_J(K,J,N) = 0.
            CFSV_J(K,J,N) = 0.
            CMSV_J(K,J,N) = 0.
          ENDDO
        ENDDO
C
C-------------------------------------------------------------------
C------ sum the forces in the strip as generated by velocity
C-        (freestream + rotation + induced) acting on bound vortex 
        I1  = IJFRST(J)
        NVC = NVSTRP(J)
        DO 40 II = 1, NVC
          I = I1 + (II-1)
C
C-------- element area (for computing dCp)
          SELEM = DXV(I)*WSTRIP(J)
C
C-------- local moment arm vector to vortex midpoint
          R(1) = RV(1,I) - XYZREF(1)
          R(2) = RV(2,I) - XYZREF(2)
          R(3) = RV(3,I) - XYZREF(3)
C
C-------- vector from rotation axes
          RROT(1) = RV(1,I) - XYZREF(1)
          RROT(2) = RV(2,I) - XYZREF(2)
          RROT(3) = RV(3,I) - XYZREF(3)
C
C-------- set total effective velocity = freestream + rotation + induced
          CALL CROSS(RROT,WROT,VROT)
          VEFF(1) = VINF(1) + VROT(1) + WV(1,I)
          VEFF(2) = VINF(2) + VROT(2) + WV(2,I)
          VEFF(3) = VINF(3) + VROT(3) + WV(3,I)
C
C-------- set VEFF sensitivities to freestream,rotation components
          DO K = 1, 3
            VEFF_U(1,K) = WV_U(1,I,K)
            VEFF_U(2,K) = WV_U(2,I,K)
            VEFF_U(3,K) = WV_U(3,I,K)
            VEFF_U(K,K) = 1.0  +  VEFF_U(K,K)
          ENDDO
          DO K = 4, 6
            WROT_U(1) = 0.
            WROT_U(2) = 0.
            WROT_U(3) = 0.
            WROT_U(K-3) = 1.0
            CALL CROSS(RROT,WROT_U,VROT_U)
            VEFF_U(1,K) = VROT_U(1) + WV_U(1,I,K)
            VEFF_U(2,K) = VROT_U(2) + WV_U(2,I,K)
            VEFF_U(3,K) = VROT_U(3) + WV_U(3,I,K)
          ENDDO
C
C-------- Force coefficient on vortex segment is 2(Veff x Gamma)
          DL(1) = RV2(1,I) - RV1(1,I)
          DL(2) = RV2(2,I) - RV1(2,I)
          DL(3) = RV2(3,I) - RV1(3,I)
          CALL CROSS(VEFF, DL, VXL)
          DO N = 1, NUMAX
            CALL CROSS(VEFF_U(1,N), DL, VXL_U(1,N))
          ENDDO
C
C-------- element force-area
          DO K = 1, 3
            F(K) = 2.0*GAM(I)*VXL(K)
            DO N = 1, NUMAX
              F_U(K,N) = 2.0*GAM_U(I,N)*VXL(K) + 2.0*GAM(I)*VXL_U(K,N)
            ENDDO
            DO N = 1, NCONTROL
              F_D(K,N) = 2.0*GAM_D(I,N)*VXL(K)
            ENDDO
            DO N = 1, NDESIGN
              F_G(K,N) = 2.0*GAM_G(I,N)*VXL(K)
            ENDDO
            DO N = 1, NVARJET
              F_J(K,N) = 2.0*GAM_J(I,N)*VXL(K)
            ENDDO
          ENDDO
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C-------- Delta Cp (loading across lifting surface) from vortex, 
C-         for element force output
          FN = DOT(ENV(1,I),F)
          DCP(I) = FN / SELEM
C
          DO N = 1, NUMAX
            FN_U = DOT(ENV(1,I),F_U(1,N))
            DCP_U(I,N) = FN_U / SELEM
          ENDDO
C
          DO N = 1, NCONTROL
            FN_D = DOT(ENV(1,I),F_D(1,N)) + DOT(ENV_D(1,I,N),F)
            DCP_D(I,N) = FN_D / SELEM
          ENDDO
C
          DO N = 1, NDESIGN
            FN_G = DOT(ENV(1,I),F_G(1,N)) + DOT(ENV_G(1,I,N),F)
            DCP_G(I,N) = FN_G / SELEM
          ENDDO
C
          DO N = 1, NVARJET
            FN_J = DOT(ENV(1,I),F_J(1,N))
            DCP_J(I,N) = FN_J / SELEM
          ENDDO
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C-------- skip force accumulation for jet sheet
          IF(IFTYPE(ISURF).EQ.1) GO TO 40
C
C------------------------------------------------------------------------
C-------- accumulate strip forces and moments normalized by strip area
          DO K = 1, 3
            IC = ICRS(K)
            JC = JCRS(K)
C
            CFSI(K,J) = CFSI(K,J) + F(K)
            CMSI(K,J) = CMSI(K,J) + ( R(IC)*F(JC)
     &                               -R(JC)*F(IC))
            DO N=1, NUMAX
              CFSI_U(K,J,N) = CFSI_U(K,J,N) +  F_U(K,N)
              CMSI_U(K,J,N) = CMSI_U(K,J,N) + (R(IC)*F_U(JC,N)
     &                                        -R(JC)*F_U(IC,N))
            ENDDO
            DO N=1, NCONTROL
              CFSI_D(K,J,N) = CFSI_D(K,J,N) +  F_D(K,N)
              CMSI_D(K,J,N) = CMSI_D(K,J,N) + (R(IC)*F_D(JC,N)
     &                                        -R(JC)*F_D(IC,N))
            ENDDO
            DO N=1, NDESIGN
              CFSI_G(K,J,N) = CFSI_G(K,J,N) +  F_G(K,N)
              CMSI_G(K,J,N) = CMSI_G(K,J,N) + (R(IC)*F_G(JC,N)
     &                                        -R(JC)*F_G(IC,N))
            ENDDO
            DO N=1, NVARJET
              CFSI_J(K,J,N) = CFSI_J(K,J,N) +  F_J(K,N)
              CMSI_J(K,J,N) = CMSI_J(K,J,N) + (R(IC)*F_J(JC,N)
     &                                        -R(JC)*F_J(IC,N))
            ENDDO
          ENDDO
C
C------------------------------------------------------------------------
C-------- hinge moments
          DO L = 1, NCONTROL
            R(1) = RV(1,I) - PHINGE(1,J,L)
            R(2) = RV(2,I) - PHINGE(2,J,L)
            R(3) = RV(3,I) - PHINGE(3,J,L)
            CALL CROSS(R,F,MH)
C
            DFAC = DCONTROL(I,L) / (SREF*CREF)
            CHINGE(L) = CHINGE(L) + DOT(MH,VHINGE(1,J,L))*DFAC
C
            DO N = 1, NUMAX
              CALL CROSS(R,F_U(1,N),MH)
              CHINGE_U(L,N) = CHINGE_U(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
            ENDDO
            DO N = 1, NCONTROL
              CALL CROSS(R,F_D(1,N),MH)
              CHINGE_D(L,N) = CHINGE_D(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
            ENDDO
            DO N = 1, NDESIGN
              CALL CROSS(R,F_G(1,N),MH)
              CHINGE_G(L,N) = CHINGE_G(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
            ENDDO
            DO N = 1, NVARJET
              CALL CROSS(R,F_J(1,N),MH)
              CHINGE_J(L,N) = CHINGE_J(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
            ENDDO
          ENDDO
C
   40   CONTINUE
C
C------ for jet sheet, skip trailing legs and all force summation
        IF(IFTYPE(ISURF).EQ.1) GO TO 100
C
C------------------------------------------------------------------------
C------ if no trailing leg forces, skip it
        IF(.NOT.LTRFORCE) GO TO 80
C
C------ Sum forces in the strip as generated by velocity (freestream + rotation)
C-        the parts of trailing legs which lie on the surface
        DO 72 II = 1, NVC
          I = I1 + (II-1)
C
          DO 71 ILEG = 1, 2
C
          IF(ILEG.EQ.1) THEN
C--------- moment vector
           R(1) = 0.5*(RV1(1,I) + XTE1) - XYZREF(1)
           R(2) =      RV1(2,I)         - XYZREF(2)
           R(3) =      RV1(3,I)         - XYZREF(3)
C
C--------- vector from rotation axes
           RROT(1) = 0.5*(RV1(1,I) + XTE1) - XYZREF(1)
           RROT(2) =      RV1(2,I)         - XYZREF(2)
           RROT(3) =      RV1(3,I)         - XYZREF(3)
C
C--------- part of trailing leg lying on surface
           DL(1) = RV1(1,I) - XTE1
           DL(2) = 0.
           DL(3) = 0.
C
          ELSE
C--------- moment vector
           R(1) = 0.5*(RV2(1,I) + XTE2) - XYZREF(1)
           R(2) =      RV2(2,I)         - XYZREF(2)
           R(3) =      RV2(3,I)         - XYZREF(3)
C
C--------- vector from rotation axes
           RROT(1) = 0.5*(RV2(1,I) + XTE2) - XYZREF(1)
           RROT(2) =      RV2(2,I)         - XYZREF(2)
           RROT(3) =      RV2(3,I)         - XYZREF(3)
C
C--------- part of trailing leg lying on surface
           DL(1) = XTE2 - RV2(1,I)
           DL(2) = 0.
           DL(3) = 0.
          ENDIF
C
C-------- set total effective velocity = freestream + rotation
          CALL CROSS(RROT,WROT,VROT)
          VEFF(1) = VINF(1) + VROT(1)
          VEFF(2) = VINF(2) + VROT(2)
          VEFF(3) = VINF(3) + VROT(3)
C
C-------- set VEFF sensitivities to freestream,rotation components
          DO K = 1, 3
            VEFF_U(1,K) = 0.
            VEFF_U(2,K) = 0.
            VEFF_U(3,K) = 0.
            VEFF_U(K,K) = 1.0
          ENDDO
          DO K = 4, 6
            WROT_U(1) = 0.
            WROT_U(2) = 0.
            WROT_U(3) = 0.
            WROT_U(K-3) = 1.0
            CALL CROSS(RROT,WROT_U,VROT_U)
            VEFF_U(1,K) = VROT_U(1)
            VEFF_U(2,K) = VROT_U(2)
            VEFF_U(3,K) = VROT_U(3)
          ENDDO
C
C-------- force-area on vortex segment is 2(Veff x Gamma)
          CALL CROSS (VEFF, DL, VXL)
C
          DO N = 1, NUMAX
            CALL CROSS(VEFF_U(1,N), DL, VXL_U(1,N))
          ENDDO
C
          DO K = 1, 3
            F(K) = 2.0*GAM(I)*VXL(K)
            DO N = 1, NUMAX
              F_U(K,N) = 2.0*GAM_U(I,N)*VXL(K) + 2.0*GAM(I)*VXL_U(K,N)
            ENDDO
            DO N = 1, NCONTROL
              F_D(K,N) = 2.0*GAM_D(I,N)*VXL(K)
            ENDDO
            DO N = 1, NDESIGN
              F_G(K,N) = 2.0*GAM_G(I,N)*VXL(K)
            ENDDO
            DO N = 1, NVARJET
              F_J(K,N) = 2.0*GAM_J(I,N)*VXL(K)
            ENDDO
          ENDDO
C
cC-------- Delta Cp (loading across lifting surface) due to vortex 
c          FN = DOT(ENV(1,I),F)
c          DCP(I) = FN / (DXV(I)*WSTRIP(J))
cC
c          DO N = 1, NUMAX
c            FN_U = DOT(ENV(1,I),F_U(1,N))
c            DCP_U(I,N) = FN_U / (DXV(I)*WSTRIP(J))
c          ENDDO
cC
c          DO N = 1, NCONTROL
c            FN_D = DOT(ENV(1,I),F_D(1,N)) + DOT(ENV_D(1,I,N),F)
c            DCP_D(I,N) = FN_D / (DXV(I)*WSTRIP(J))
c          ENDDO
cC
c          DO N = 1, NDESIGN
c            FN_G = DOT(ENV(1,I),F_G(1,N)) + DOT(ENV_G(1,I,N),F)
c            DCP_G(I,N) = FN_G / (DXV(I)*WSTRIP(J))
c          ENDDO
C
c          DO N = 1, NVARJET
c            FN_J = DOT(ENV(1,I),F_J(1,N)) + DOT(ENV_J(1,I,N),F)
c            DCP_J(I,N) = FN_J / (DXV(I)*WSTRIP(J))
c          ENDDO
C
C

C------------------------------------------------------------------------
C-------- accumulate strip forces and moments normalized by strip area
          DO K = 1, 3
            IC = ICRS(K)
            JC = JCRS(K)
C
            CFSI(K,J) = CFSI(K,J) + F(K)
            CMSI(K,J) = CMSI(K,J) + ( R(IC)*F(JC)
     &                               -R(JC)*F(IC))
            DO N=1, NUMAX
              CFSI_U(K,J,N) = CFSI_U(K,J,N) +  F_U(K,N)
              CMSI_U(K,J,N) = CMSI_U(K,J,N) + (R(IC)*F_U(JC,N)
     &                                        -R(JC)*F_U(IC,N))
            ENDDO
            DO N=1, NCONTROL
              CFSI_D(K,J,N) = CFSI_D(K,J,N) +  F_D(K,N)
              CMSI_D(K,J,N) = CMSI_D(K,J,N) + (R(IC)*F_D(JC,N)
     &                                        -R(JC)*F_D(IC,N))
            ENDDO
            DO N=1, NDESIGN
              CFSI_G(K,J,N) = CFSI_G(K,J,N) +  F_G(K,N)
              CMSI_G(K,J,N) = CMSI_G(K,J,N) + (R(IC)*F_G(JC,N)
     &                                        -R(JC)*F_G(IC,N))
            ENDDO
            DO N=1, NVARJET
              CFSI_J(K,J,N) = CFSI_J(K,J,N) +  F_J(K,N)
              CMSI_J(K,J,N) = CMSI_J(K,J,N) + (R(IC)*F_J(JC,N)
     &                                        -R(JC)*F_J(IC,N))
            ENDDO
          ENDDO
C
CC-------- hinge moments
c          DO L=1, NCONTROL
c            R(1) = RV(1,I) - PHINGE(1,J,L)
c            R(2) = RV(2,I) - PHINGE(2,J,L)
c            R(3) = RV(3,I) - PHINGE(3,J,L)
cC
c            DFAC = DCONTROL(I,L) / (SREF * CREF)
cC
c            CALL CROSS(R,F,MH)
c            CHINGE(L) = CHINGE(L) + DOT(MH,VHINGE(1,J,L))*DFAC
cC
c            DO N = 1, NUMAX
c              CALL CROSS(R,F_U(1,N),MH)
c              CHINGE_U(L,N) = CHINGE_U(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
c            ENDDO
c            DO N = 1, NCONTROL
c              CALL CROSS(R,F_D(1,N),MH)
c              CHINGE_D(L,N) = CHINGE_D(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
c            ENDDO
c            DO N = 1, NDESIGN
c              CALL CROSS(R,F_G(1,N),MH)
c              CHINGE_G(L,N) = CHINGE_G(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
c            ENDDO
c            DO N = 1, NVARJET
c              CALL CROSS(R,F_J(1,N),MH)
c              CHINGE_J(L,N) = CHINGE_J(L,N) + DOT(MH,VHINGE(1,J,L))*DFAC
c            ENDDO
c          ENDDO
C
   71   CONTINUE
   72   CONTINUE
 80     CONTINUE
C
C
C*******************************************************************
C--- Drag terms due to viscous effects
C    Drag forces are assumed to be characterized by velocity at the c/4 
C    point and are assumed to act thru the same point. CD is defined by 
C    user-specified CD(CL) polar.  Drag comes from function lookup on 
C    section polar drag using local lift coefficient.  
C
        IF(LVISC.AND.LVISCSTRP(J)) THEN
C------- viscous force is assumed to be applied at c/4 point
         R(1) = RC4(1) - XYZREF(1)
         R(2) = RC4(2) - XYZREF(2)
         R(3) = RC4(3) - XYZREF(3)
C
C------- onset velocity at strip c/4 = freestream + rotation
         RROT(1) = RC4(1) - XYZREF(1)
         RROT(2) = RC4(2) - XYZREF(2)
         RROT(3) = RC4(3) - XYZREF(3)
         CALL CROSS(RROT,WROT,VROT)
         VEFF(1) = VINF(1) + VROT(1)
         VEFF(2) = VINF(2) + VROT(2)
         VEFF(3) = VINF(3) + VROT(3)
         VA = SQRT(VEFF(1)**2 +VEFF(2)**2 +VEFF(3)**2)
C
C------- set sensitivities to freestream,rotation components
         DO K = 1, 3
           VEFF_U(1,K) = 0.
           VEFF_U(2,K) = 0.
           VEFF_U(3,K) = 0.
         ENDDO
         VEFF_U(1,1) = 1.0
         VEFF_U(2,2) = 1.0
         VEFF_U(3,3) = 1.0
         DO K = 4, 6
           WROT_U(1) = 0.
           WROT_U(2) = 0.
           WROT_U(3) = 0.
           WROT_U(K-3) = 1.0
           CALL CROSS(RROT,WROT_U,VROT_U)
           VEFF_U(1,K) = VROT_U(1)
           VEFF_U(2,K) = VROT_U(2)
           VEFF_U(3,K) = VROT_U(3)
         ENDDO
C
C------- generate cd from stored function using strip cl as parameter
         CLV = ( ULIFT(1)*CFSI(1,J)
     &         + ULIFT(2)*CFSI(2,J)
     &         + ULIFT(3)*CFSI(3,J) ) / SSTRIP
C
         DO N = 1, NUMAX
           CLV_U(N) = ( ULIFT(1)*CFSI_U(1,J,N)
     &                + ULIFT(2)*CFSI_U(2,J,N)
     &                + ULIFT(3)*CFSI_U(3,J,N) ) / SSTRIP
         ENDDO
         DO N = 1, NCONTROL
           CLV_D(N) = ( ULIFT(1)*CFSI_D(1,J,N)
     &                + ULIFT(2)*CFSI_D(2,J,N)
     &                + ULIFT(3)*CFSI_D(3,J,N) ) / SSTRIP
         ENDDO
         DO N = 1, NDESIGN
           CLV_G(N) = ( ULIFT(1)*CFSI_G(1,J,N)
     &                + ULIFT(2)*CFSI_G(2,J,N)
     &                + ULIFT(3)*CFSI_G(3,J,N) ) / SSTRIP
         ENDDO
         DO N = 1, NVARJET
           CLV_J(N) = ( ULIFT(1)*CFSI_J(1,J,N)
     &                + ULIFT(2)*CFSI_J(2,J,N)
     &                + ULIFT(3)*CFSI_J(3,J,N) ) / SSTRIP
         ENDDO
C
         CALL CDCL(J,CLV,CDV,CDV_CLV)
C
         CDV_STRP(J) = CDV
C
         CDA     = CDV    *SSTRIP
         CDA_CLV = CDV_CLV*SSTRIP
C
C------- viscous strip force
         DO K = 1, 3
           CFSV(K,J) = VEFF(K)*VA * CDA
           DO N=1, NUMAX
             CFSV_U(K,J,N) = (VEFF_U(K,N)*(VA + VEFF(K)**2/VA))*CDA
     &                     +  VEFF(K)* VA * CDA_CLV*CLV_U(N)
           ENDDO
           DO N=1, NCONTROL
             CFSV_D(K,J,N) =  VEFF(K)* VA * CDA_CLV*CLV_D(N)
           ENDDO
           DO N=1, NDESIGN
             CFSV_G(K,J,N) =  VEFF(K)* VA * CDA_CLV*CLV_G(N)
           ENDDO
           DO N=1, NVARJET
             CFSV_J(K,J,N) =  VEFF(K)* VA * CDA_CLV*CLV_J(N)
           ENDDO
         ENDDO
C
C------- viscous strip moment
         DO K = 1, 3
           IC = ICRS(K)
           JC = JCRS(K)
           CMSV(K,J) = (R(IC)*CFSV(JC,J) - R(JC)*CFSV(IC,J))
           DO N=1, NUMAX
             CMSV_U(K,J,N) = ( R(IC)*CFSV_U(JC,J,N)
     &                       - R(JC)*CFSV_U(IC,J,N) )
           ENDDO
           DO N=1, NCONTROL
             CMSV_D(K,J,N) = ( R(IC)*CFSV_D(JC,J,N)
     &                       - R(JC)*CFSV_D(IC,J,N) )
           ENDDO
           DO N=1, NDESIGN
             CMSV_G(K,J,N) = ( R(IC)*CFSV_G(JC,J,N)
     &                       - R(JC)*CFSV_G(IC,J,N) )
           ENDDO
           DO N=1, NVARJET
             CMSV_J(K,J,N) = ( R(IC)*CFSV_J(JC,J,N)
     &                       - R(JC)*CFSV_J(IC,J,N) )
           ENDDO
         ENDDO
C
        ELSE
         CDV_STRP(J) =  0.

        ENDIF        
C
  100 CONTINUE
C
C
C---- Surface forces and moments summed from strip forces
      DO 200 IS = 1, NSURFS
C------ clear surface forces and moments for accumulation
        DO K = 1, 3
          CFNI(K,IS) = 0.
          CMNI(K,IS) = 0.
          CFNV(K,IS) = 0.
          CMNV(K,IS) = 0.
          DO N=1, NUMAX
            CFNI_U(K,IS,N) = 0.
            CMNI_U(K,IS,N) = 0.
            CFNV_U(K,IS,N) = 0.
            CMNV_U(K,IS,N) = 0.
          ENDDO
          DO N=1, NCONTROL
            CFNI_D(K,IS,N) = 0.
            CMNI_D(K,IS,N) = 0.
            CFNV_D(K,IS,N) = 0.
            CMNV_D(K,IS,N) = 0.
          ENDDO
          DO N=1, NDESIGN
            CFNI_G(K,IS,N) = 0.
            CMNI_G(K,IS,N) = 0.
            CFNV_G(K,IS,N) = 0.
            CMNV_G(K,IS,N) = 0.
          ENDDO
          DO N=1, NVARJET
            CFNI_J(K,IS,N) = 0.
            CMNI_J(K,IS,N) = 0.
            CFNV_J(K,IS,N) = 0.
            CMNV_J(K,IS,N) = 0.
          ENDDO
        ENDDO
C
        DO 220 JJ = 1, NJ(IS)
          J = JFRST(IS) + JJ-1
          DO K = 1, 3
            CFNI(K,IS) = CFNI(K,IS) + CFSI(K,J)
            CMNI(K,IS) = CMNI(K,IS) + CMSI(K,J)
            CFNV(K,IS) = CFNV(K,IS) + CFSV(K,J)
            CMNV(K,IS) = CMNV(K,IS) + CMSV(K,J)
            DO N=1, NUMAX
              CFNI_U(K,IS,N) = CFNI_U(K,IS,N) + CFSI_U(K,J,N)
              CMNI_U(K,IS,N) = CMNI_U(K,IS,N) + CMSI_U(K,J,N)
              CFNV_U(K,IS,N) = CFNV_U(K,IS,N) + CFSV_U(K,J,N)
              CMNV_U(K,IS,N) = CMNV_U(K,IS,N) + CMSV_U(K,J,N)
            ENDDO
            DO N=1, NCONTROL
              CFNI_D(K,IS,N) = CFNI_D(K,IS,N) + CFSI_D(K,J,N)
              CMNI_D(K,IS,N) = CMNI_D(K,IS,N) + CMSI_D(K,J,N)
              CFNV_D(K,IS,N) = CFNV_D(K,IS,N) + CFSV_D(K,J,N)
              CMNV_D(K,IS,N) = CMNV_D(K,IS,N) + CMSV_D(K,J,N)
            ENDDO
            DO N=1, NDESIGN
              CFNI_G(K,IS,N) = CFNI_G(K,IS,N) + CFSI_G(K,J,N)
              CMNI_G(K,IS,N) = CMNI_G(K,IS,N) + CMSI_G(K,J,N)
              CFNV_G(K,IS,N) = CFNV_G(K,IS,N) + CFSV_G(K,J,N)
              CMNV_G(K,IS,N) = CMNV_G(K,IS,N) + CMSV_G(K,J,N)
            ENDDO
            DO N=1, NVARJET
              CFNI_J(K,IS,N) = CFNI_J(K,IS,N) + CFSI_J(K,J,N)
              CMNI_J(K,IS,N) = CMNI_J(K,IS,N) + CMSI_J(K,J,N)
              CFNV_J(K,IS,N) = CFNV_J(K,IS,N) + CFSV_J(K,J,N)
              CMNV_J(K,IS,N) = CMNV_J(K,IS,N) + CMSV_J(K,J,N)
            ENDDO
          ENDDO
  220   CONTINUE
  200 CONTINUE
C
C---- clear surface forces and moments for accumulation
      DO K = 1, 3
        CFTI(K) = 0.
        CMTI(K) = 0.
        CFTV(K) = 0.
        CMTV(K) = 0.
        DO N=1, NUMAX
          CFTI_U(K,N) = 0.
          CMTI_U(K,N) = 0.
          CFTV_U(K,N) = 0.
          CMTV_U(K,N) = 0.
        ENDDO
        DO N=1, NCONTROL
          CFTI_D(K,N) = 0.
          CMTI_D(K,N) = 0.
          CFTV_D(K,N) = 0.
          CMTV_D(K,N) = 0.
        ENDDO
        DO N=1, NDESIGN
          CFTI_G(K,N) = 0.
          CMTI_G(K,N) = 0.
          CFTV_G(K,N) = 0.
          CMTV_G(K,N) = 0.
        ENDDO
        DO N=1, NVARJET
          CFTI_J(K,N) = 0.
          CMTI_J(K,N) = 0.
          CFTV_J(K,N) = 0.
          CMTV_J(K,N) = 0.
        ENDDO
      ENDDO
C
C---- total forces summed from surface forces
      DO 300 IS = 1, NSURFS
        DO K = 1, 3
          CFTI(K) = CFTI(K) + CFNI(K,IS)
          CMTI(K) = CMTI(K) + CMNI(K,IS)
          CFTV(K) = CFTV(K) + CFNV(K,IS)
          CMTV(K) = CMTV(K) + CMNV(K,IS)
          DO N=1, NUMAX
            CFTI_U(K,N) = CFTI_U(K,N) + CFNI_U(K,IS,N)
            CMTI_U(K,N) = CMTI_U(K,N) + CMNI_U(K,IS,N)
            CFTV_U(K,N) = CFTV_U(K,N) + CFNV_U(K,IS,N)
            CMTV_U(K,N) = CMTV_U(K,N) + CMNV_U(K,IS,N)
          ENDDO
          DO N=1, NCONTROL
            CFTI_D(K,N) = CFTI_D(K,N) + CFNI_D(K,IS,N)
            CMTI_D(K,N) = CMTI_D(K,N) + CMNI_D(K,IS,N)
            CFTV_D(K,N) = CFTV_D(K,N) + CFNV_D(K,IS,N)
            CMTV_D(K,N) = CMTV_D(K,N) + CMNV_D(K,IS,N)
          ENDDO
          DO N=1, NDESIGN
            CFTI_G(K,N) = CFTI_G(K,N) + CFNI_G(K,IS,N)
            CMTI_G(K,N) = CMTI_G(K,N) + CMNI_G(K,IS,N)
            CFTV_G(K,N) = CFTV_G(K,N) + CFNV_G(K,IS,N)
            CMTV_G(K,N) = CMTV_G(K,N) + CMNV_G(K,IS,N)
          ENDDO
          DO N=1, NVARJET
            CFTI_J(K,N) = CFTI_J(K,N) + CFNI_J(K,IS,N)
            CMTI_J(K,N) = CMTI_J(K,N) + CMNI_J(K,IS,N)
            CFTV_J(K,N) = CFTV_J(K,N) + CFNV_J(K,IS,N)
            CMTV_J(K,N) = CMTV_J(K,N) + CMNV_J(K,IS,N)
          ENDDO
        ENDDO
 300  CONTINUE
C
C
C---- Double the X,Z forces, zero Y force for a Y symmetric case
      IF(IYSYM.EQ.1) THEN
       FSYM(1) = 2.0
       FSYM(2) = 0.
       FSYM(3) = 2.0
       MSYM(1) = 0.
       MSYM(2) = 2.0
       MSYM(3) = 0.
C
       DO K = 1, 3
         CFTI(K) = FSYM(K)*CFTI(K)
         CMTI(K) = MSYM(K)*CMTI(K)
         CFTV(K) = FSYM(K)*CFTV(K)
         CMTV(K) = MSYM(K)*CMTV(K)
         DO N = 1, NUMAX
           CFTI_U(K,N) = FSYM(K) * CFTI_U(K,N)
           CMTI_U(K,N) = MSYM(K) * CMTI_U(K,N)
           CFTV_U(K,N) = FSYM(K) * CFTV_U(K,N)
           CMTV_U(K,N) = MSYM(K) * CMTV_U(K,N)
         ENDDO
         DO N = 1, NCONTROL
           CFTI_D(K,N) = FSYM(K) * CFTI_D(K,N)
           CMTI_D(K,N) = MSYM(K) * CMTI_D(K,N)
           CFTV_D(K,N) = FSYM(K) * CFTV_D(K,N)
           CMTV_D(K,N) = MSYM(K) * CMTV_D(K,N)
         ENDDO
         DO N = 1, NDESIGN
           CFTI_G(K,N) = FSYM(K) * CFTI_G(K,N)
           CMTI_G(K,N) = MSYM(K) * CMTI_G(K,N)
           CFTV_G(K,N) = FSYM(K) * CFTV_G(K,N)
           CMTV_G(K,N) = MSYM(K) * CMTV_G(K,N)
         ENDDO
         DO N = 1, NVARJET
           CFTI_J(K,N) = FSYM(K) * CFTI_J(K,N)
           CMTI_J(K,N) = MSYM(K) * CMTI_J(K,N)
           CFTV_J(K,N) = FSYM(K) * CFTV_J(K,N)
           CMTV_J(K,N) = MSYM(K) * CMTV_J(K,N)
         ENDDO
       ENDDO
      ENDIF
C
      RETURN
      END ! SFFORC


      SUBROUTINE FSTRIP(JSTRIP,
     &                  CAXIAL,CNORML, 
     &                  CL_STRP,CD_STRP,
     &                  CLJ_STRP,CDJ_STRP,
     &                  CLT_STRP,CLA_STRP,
     &                  CMC4_STRP,CMLE_STRP,
     &                  CNC_STRP )
C-------------------------------------------------------------------
C     Returns various locally-normalized strip forces and moments
C
C   Input 
C   ------
C      JSTRIP     strip index
C
C   Output
C   ------ 
C      CAXIAL     force coeff in x direction
C      CNORML     force coeff normal to strip, along to ENSY(j),ENSZ(j)
C      CL_STRP    cl referenced to freestream velocity (includes jet force)
C      CD_STRP    cd referenced to freestream velocity (includes jet force)
C      CLJ_STRP   cl_jet referenced to freestream velocity (part of cl)
C      CDJ_STRP   cd_jet referenced to freestream velocity (part of cd)
C      CLT_STRP   cl referenced to span-normal local velocity
C      CLA_STRP   cl referenced to local velocity
C      CMC4_STRP  cm about strip's c/4 point, about spanwise axis
C      CMLE_STRP  cm about strip's LE  point, about spanwise axis
C      CNC_STRP   c*cl_inviscid
C         
C-------------------------------------------------------------------
      INCLUDE 'AVL.INC'
C
      REAL R(3), RC4(3), RROT(3)
      REAL VROT(3), VEFF(3), VPERP(3)
      REAL CFS(3), CMS(3)
C
      REAL SPN(3), UDRAG(3), ULIFT(3)
      REAL DRLE(3)
C
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
C
      J = JSTRIP
C
      CSTRIP = CHORD(J)
      SSTRIP = CHORD(J)*WSTRIP(J)
C
      RC4(1)  = RLE(1,J) + 0.25*CHORD(J)
      RC4(2)  = RLE(2,J)
      RC4(3)  = RLE(3,J)
C
C---- Define local strip lift and drag directions
C-      The "spanwise" vector is cross product of strip normal with X chordline 
      SPN(1) =  0.0
      SPN(2) =  ENSZ(J)
      SPN(3) = -ENSY(J)
C
      COSA = COS(ALFA)
      SINA = SIN(ALFA)
C
      UDRAG(1) = COSA
      UDRAG(2) = 0.0
      UDRAG(3) = SINA
C
C---- Strip lift direction is vector product of "stream" and spanwise vector
      CALL CROSS(UDRAG,SPN,ULIFT)
      ULMAG = SQRT(DOT(ULIFT,ULIFT))
      IF(ULMAG.EQ.0.) THEN
        ULIFT(3) = 1.0
       ELSE
        ULIFT(1) = ULIFT(1)/ULMAG
        ULIFT(2) = ULIFT(2)/ULMAG
        ULIFT(3) = ULIFT(3)/ULMAG
      ENDIF
C
C---- translate moments from XYZREF to c/4 point, to create local strip moments
      R(1) = XYZREF(1) - RC4(1)
      R(2) = XYZREF(2) - RC4(2)
      R(3) = XYZREF(3) - RC4(3)
C
      DO K = 1, 3
        CFS(K) = CFSI(K,J) + CFSV(K,J) + CFSJ(K,J)
        CMS(K) = CMSI(K,J) + CMSV(K,J) + CMSJ(K,J)
      ENDDO
      DO K = 1, 3
        IC = ICRS(K)
        JC = JCRS(K)
        CMS(K) = CMS(K) + R(IC)*CFS(JC) - R(JC)*CFS(IC)
      ENDDO
C
      CAXIAL = CFS(1) / SSTRIP
      CNORML = (ENSY(J)*CFS(2)
     &        + ENSZ(J)*CFS(3)) / SSTRIP
C
      CMC4_STRP = ( ENSZ(J)*CMS(2)
     &            - ENSY(J)*CMS(3) ) / (CSTRIP*SSTRIP)
C
C---- vector at chord reference point from rotation axes
      RROT(1) = XYZREFS(1,J) - XYZREF(1)
      RROT(2) = XYZREFS(2,J) - XYZREF(2)
      RROT(3) = XYZREFS(3,J) - XYZREF(3)
C
C---- set total effective velocity = freestream + rotation
      CALL CROSS(RROT,WROT,VROT)
      VEFF(1) = VINF(1) + VROT(1)
      VEFF(2) = VINF(2) + VROT(2)
      VEFF(3) = VINF(3) + VROT(3)
C
      VSQ = VEFF(1)**2 + VEFF(2)**2 + VEFF(3)**2
      IF(VSQ .EQ. 0.0) THEN
       VSQI = 1.0
      ELSE
       VSQI = 1.0 / VSQ
      ENDIF
C
C---- spanwise and perpendicular velocity components
      VSPAN = VEFF(1)*ESS(1,J) + VEFF(2)*ESS(2,J) + VEFF(3)*ESS(3,J)
      VPERP(1) = VEFF(1) - ESS(1,J)*VSPAN
      VPERP(2) = VEFF(2) - ESS(2,J)*VSPAN
      VPERP(3) = VEFF(3) - ESS(3,J)*VSPAN
C
      VPSQ = VPERP(1)**2 + VPERP(2)**2 + VPERP(3)**2
      IF(VPSQ .EQ. 0.0) THEN
       VPSQI = 1.0
      ELSE
       VPSQI = 1.0 / VPSQ
      ENDIF
C
      CL_STRP = ( ULIFT(1)*CFS(1)
     &          + ULIFT(2)*CFS(2)
     &          + ULIFT(3)*CFS(3) ) / SSTRIP
      CD_STRP = ( UDRAG(1)*CFS(1)
     &          + UDRAG(2)*CFS(2)
     &          + UDRAG(3)*CFS(3) ) / SSTRIP
C
      CLT_STRP = CL_STRP * VPSQI
      CLA_STRP = CL_STRP * VSQI
C
C---- Moment about strip LE midpoint in direction of LE segment
      R(1) = RC4(1) - RLE(1,J)
      R(2) = RC4(2) - RLE(2,J)
      R(3) = RC4(3) - RLE(3,J)
      DRLE(1) = RLE2(1,J) - RLE1(1,J)
      DRLE(2) = RLE2(2,J) - RLE1(2,J)
      DRLE(3) = RLE2(3,J) - RLE1(3,J)
C
      IF(IMAGS(ISURFS(J)).LT.0) THEN 
       DRLE(1) = -DRLE(1)
       DRLE(2) = -DRLE(2)
       DRLE(3) = -DRLE(3)
      ENDIF
      DMAG = SQRT(DRLE(1)**2+DRLE(2)**2+DRLE(3)**2)
      IF(DMAG.EQ.0.0) THEN
       CMLE_STRP = 0.0
      ELSE
       CMLE_STRP =
     &   ( DRLE(1)/DMAG*(CMS(1) + (R(2)*CFS(3) - R(3)*CFS(2)))
     &   + DRLE(2)/DMAG*(CMS(2) + (R(3)*CFS(1) - R(1)*CFS(3)))
     &   + DRLE(3)/DMAG*(CMS(3) + (R(1)*CFS(2) - R(2)*CFS(1))) )
     &   / (CSTRIP*SSTRIP)
      ENDIF
C
      CNC_STRP = ( ENSY(J)*CFSI(2,J)
     &           + ENSZ(J)*CFSI(3,J) ) / WSTRIP(J)
C
C
C---- strip on surface has this strip trailing from it
      CLJ_STRP = ( ULIFT(1)*CFSJ(1,J)
     &           + ULIFT(2)*CFSJ(2,J)
     &           + ULIFT(3)*CFSJ(3,J) ) / SSTRIP
      CDJ_STRP = ( UDRAG(1)*CFSJ(1,J)
     &           + UDRAG(2)*CFSJ(2,J)
     &           + UDRAG(3)*CFSJ(3,J) ) / SSTRIP
C
      RETURN
      END ! FSTRIP
