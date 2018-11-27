C***********************************************************************
C    Module:  asetup.f
C 
C    Copyright (C) 2002 Mark Drela, Harold Youngren
C 
C    This program is free software; you can redistribute it and/or modify
C    it under the terms of the GNU General Public License as published by
C    the Free Software Foundation; either version 2 of the License, or
C    (at your option) any later version.
C
C    This program is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU General Public License for more details.
C
C    You should have received a copy of the GNU General Public License
C    along with this program; if not, write to the Free Software
C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C***********************************************************************

      SUBROUTINE SETUP
C
C...PURPOSE  To set up the vortex lattice calculation.
C
C            Additional geometry data is calculated.
C            An AIC matrix is assembled and solutions
C            obtained for 0 deg and 90 deg angle of attack 
C            (for later superposition in AERO). The matrix
C            defining normalwash at the bound vortex midpoints
C            is calculated. This is used to define the bound vortex
C            normalwash for 0 and 90 deg angles of attack (used 
C            for superposition in AERO).
C            
C...INPUT    Global Data in labelled commons, defining configuration
C          
C...OUTPUT   GAM(i,1..3)  Vortex strengths for unit VINF1,2,3
C            GAM(i,4..6)  Vortex strengths for unit WROT1,2,3
C            W            Normalwash at vortex midpoints for GAM
C
C...COMMENTS   
C
      INCLUDE 'AVL.INC'
      REAL WORK(NVMAX)
C
      AMACH = MACH
      BETM = SQRT(1.0 - AMACH**2)
C
C
      IF(.NOT.LWCG) THEN
       WRITE(*,*) ' Building Wc_Gam AIC matrix...'
       CALL VVOR(BETM,IYSYM,YSYM,IZSYM,ZSYM,VRCORE,
     &           NVOR,RV1,RV2,LSURFV,CHORDV,
     &           NVOR,RC ,    LSURFV,.FALSE.,
     &           WC_GAM,NVMAX)
       LWCG = .TRUE.
      ENDIF
C
C
      IF(.NOT.LAIC) THEN
       WRITE(*,*) ' Building normalwash AIC matrix...'
C
C----- go over solid surfaces
       DO I = 1, NVORS
C------- set up flow tangency Jacobian w.r.t. Gamma
C
C          w.n = -V.n     ;  w(i)  =  Sum_j  WC_GAM(ij) * GAM(j)
C
         DO J = 1, NVOR
           AICN(I,J) = WC_GAM(1,I,J)*ENC(1,I)
     &               + WC_GAM(2,I,J)*ENC(2,I)
     &               + WC_GAM(3,I,J)*ENC(3,I)
         ENDDO
       ENDDO
C
C----- go over jet surfaces
       DO I = NVORS+1, NVOR
C-------- set up jet curvature/loading Jacobian w.r.t. Gamma
C
C          0.5*Cjet * Delta(w).n - Gamma  =  0
C
         ISTRIP = ISTRPV(I)
         DPJET = 0.0
         DO N = 1, NVARJET
           DPJET = DPJET  +  DELJET(N) * GJSTRP(ISTRIP,N)
         ENDDO
C
         IM = IJETM(I)
         DO J = 1, NVOR
           AICN(I,J) =
     &        DPJET * (  (WC_GAM(1,I,J)-WC_GAM(1,IM,J))*ENC(1,I)
     &                 + (WC_GAM(2,I,J)-WC_GAM(2,IM,J))*ENC(2,I)
     &                 + (WC_GAM(3,I,J)-WC_GAM(3,IM,J))*ENC(3,I) )
         ENDDO
         AICN(I,I) = AICN(I,I) - 1.0
       ENDDO
C
CC...Holdover from HPV hydro project for forces near free surface
CC...Eliminates excluded vortices from eqns which are below z=Zsym 
C      CALL MUNGEA
C
       WRITE(*,*) ' Factoring normalwash AIC matrix...'
       CALL LUDCMP(NVMAX,NVOR,AICN,IAPIV,WORK)
C
       LAIC = .TRUE.
      ENDIF
C
C
      IF(.NOT.LSRD) THEN
        WRITE(*,*) ' Building source+doublet strength AIC matrix...'
        CALL SRDSET(BETM,XYZREF,
     &              NBODY,LFRST,NLMAX,
     &              NL,RL,RADL,
     &              SRC_U,DBL_U)
        WRITE(*,*) ' Building source+doublet velocity AIC matrix...'
        NU = 6
        CALL VSRD(BETM,IYSYM,YSYM,IZSYM,ZSYM,SRCORE,
     &            NBODY,LFRST,NLMAX,
     &            NL,RL,RADL,
     &            NU,SRC_U,DBL_U,
     &            NVOR,RC,
     &            WCSRD_U,NVMAX)
        LSRD = .TRUE.
      ENDIF

C
      IF(.NOT.LWVG) THEN
       WRITE(*,*) ' Building bound-vortex velocity matrix...'
       CALL VVOR(BETM,IYSYM,YSYM,IZSYM,ZSYM,VRCORE,
     &           NVOR,RV1,RV2,LSURFV,CHORDV,
     &           NVOR,RV ,    LSURFV,.TRUE.,
     &           WV_GAM,NVMAX)
C
       NU = 6
       CALL VSRD(BETM,IYSYM,YSYM,IZSYM,ZSYM,SRCORE,
     &           NBODY,LFRST,NLMAX,
     &           NL,RL,RADL,
     &           NU,SRC_U,DBL_U,
     &           NVOR,RV ,
     &           WVSRD_U,NVMAX)
C
       LWVG = .TRUE.
      ENDIF
C
      RETURN
      END ! SETUP


 
      SUBROUTINE GUCALC
      INCLUDE 'AVL.INC'
      REAL RROT(3), VUNIT(3), WUNIT(3)
C
C---- compute GAM_U only if necessary
      IF(LGMU) RETURN
C
C---- setup BC's at control points for unit freestream,rotation vectors,
C      and back-substitute to obtain corresponding vortex circulations
C
      DO 10 IU = 1, 3
C------ go over solid-surface elements... set up flow tangency r.h.s.
        DO I = 1, NVORS
          VUNIT(1) = 0.
          VUNIT(2) = 0.
          VUNIT(3) = 0.
          VUNIT(IU) = VUNIT(IU) + 1.0
C
          VUNIT(1) = VUNIT(1) + WCSRD_U(1,I,IU)
          VUNIT(2) = VUNIT(2) + WCSRD_U(2,I,IU)
          VUNIT(3) = VUNIT(3) + WCSRD_U(3,I,IU)
C
          GAM_U(I,IU) = -DOT(ENC(1,I),VUNIT)
        ENDDO
C
C------ go over jet surface elements... no driving r.h.s. term
        DO I = NVORS+1, NVOR
          GAM_U(I,IU) = 0.0
        ENDDO
C
        CALL BAKSUB(NVMAX,NVOR,AICN,IAPIV,GAM_U(1,IU))
 10   CONTINUE
C
      DO 20 IU = 4, 6
C------ go over solid-surface elements... set up flow tangency r.h.s.
        DO I = 1, NVORS
          RROT(1) = RC(1,I) - XYZREF(1)
          RROT(2) = RC(2,I) - XYZREF(2)
          RROT(3) = RC(3,I) - XYZREF(3)
C
          WUNIT(1) = 0.
          WUNIT(2) = 0.
          WUNIT(3) = 0.
          WUNIT(IU-3) = WUNIT(IU-3) + 1.0
          CALL CROSS(RROT,WUNIT,VUNIT)
C
          VUNIT(1) = VUNIT(1) + WCSRD_U(1,I,IU)
          VUNIT(2) = VUNIT(2) + WCSRD_U(2,I,IU)
          VUNIT(3) = VUNIT(3) + WCSRD_U(3,I,IU)
C
          GAM_U(I,IU) = -DOT(ENC(1,I),VUNIT)
        ENDDO
C
C------ go over jet surface elements... no driving r.h.s. term
        DO I = NVORS+1, NVOR
          GAM_U(I,IU) = 0.0
        ENDDO
C
        CALL BAKSUB(NVMAX,NVOR,AICN,IAPIV,GAM_U(1,IU))
   20 CONTINUE
C
      LGMU = .TRUE.
C
      RETURN
      END ! GUCALC


 
      SUBROUTINE GDCALC(NQDEF,LQDEF,ENC_Q,GAM_Q)
      INCLUDE 'AVL.INC'
C
C     Set up linearized equations wrt Q, solve for dGAM/dQ     
C
      LOGICAL LQDEF(*)
      REAL ENC_Q(3,NVMAX,*), GAM_Q(NVMAX,*)
C
C
      REAL RROT(3), VROT(3), VC(3)
C
      IF(NQDEF.EQ.0) RETURN
C
C---- Setup variational BC's at the control points
      DO 100 IQ = 1, NQDEF
C------ don't bother if this control variable is undefined
        IF(.NOT.LQDEF(IQ)) GO TO 100
C
        DO I = 1, NVORS
          RROT(1) = RC(1,I) - XYZREF(1)
          RROT(2) = RC(2,I) - XYZREF(2)
          RROT(3) = RC(3,I) - XYZREF(3)
          CALL CROSS(RROT,WROT,VROT)
C
          DO K = 1, 3
            VC(K) = VINF(K)
     &            + VROT(K)
     &            + WCSRD_U(K,I,1)*VINF(1)
     &            + WCSRD_U(K,I,2)*VINF(2)
     &            + WCSRD_U(K,I,3)*VINF(3)
     &            + WCSRD_U(K,I,4)*WROT(1)
     &            + WCSRD_U(K,I,5)*WROT(2)
     &            + WCSRD_U(K,I,6)*WROT(3)
          ENDDO
C
          GAM_Q(I,IQ) = -DOT(ENC_Q(1,I,IQ),VC)
        ENDDO
C
        DO I = NVORS+1, NVOR
          GAM_Q(I,IQ) = 0.0
        ENDDO
C
C********************************************************************
C...Holdover from HPV hydro project for forces near free surface
C...Eliminates excluded vortex equations for strips with z<Zsym 
ccc      CALL MUNGEB(GAM_Q(1,IQ))
C********************************************************************
C
        CALL BAKSUB(NVMAX,NVOR,AICN,IAPIV,GAM_Q(1,IQ))
 100  CONTINUE
C
      RETURN
      END ! GDCALC


 
      SUBROUTINE GJCALC(NQDEF,DPJET_Q,GAM_Q)
      INCLUDE 'AVL.INC'
C
C     Set up linearized equations wrt Q, solve for dGAM/dQ     
C
      REAL DPJET_Q(NSMAX,*), GAM_Q(NVMAX,*)
C
C
      REAL RROT(3), VROT(3), DWC(3)
C
      IF(NQDEF.EQ.0) RETURN
C
C---- Setup variational BC's at the control points
      DO 100 IQ = 1, NQDEF
C------ don't bother if this control variable is undefined
ccc        IF(.NOT.LQDEF(IQ)) GO TO 100
C
        DO I = 1, NVORS
          GAM_Q(I,IQ) = 0.0
        ENDDO
C
        DO I = NVORS+1, NVOR
cc        RROT(1) = RC(1,I) - XYZREF(1)
cc        RROT(2) = RC(2,I) - XYZREF(2)
cc        RROT(3) = RC(3,I) - XYZREF(3)
cc        CALL CROSS(RROT,WROT,VROT)
          DO K = 1, 3
            DWC(K) =
cc   &               VINF(K)
cc   &             + VROT(K)
     &             + WCSRD_U(K,I,1)*VINF(1)
     &             + WCSRD_U(K,I,2)*VINF(2)
     &             + WCSRD_U(K,I,3)*VINF(3)
     &             + WCSRD_U(K,I,4)*WROT(1)
     &             + WCSRD_U(K,I,5)*WROT(2)
     &             + WCSRD_U(K,I,6)*WROT(3)
          ENDDO
C
          IM = IJETM(I)
cc        RROT(1) = RC(1,IM) - XYZREF(1)
cc        RROT(2) = RC(2,IM) - XYZREF(2)
cc        RROT(3) = RC(3,IM) - XYZREF(3)
cc        CALL CROSS(RROT,WROT,VROT)
          DO K = 1, 3
            DWC(K) = DWC(K)
cc   &             - VINF(K)
cc   &             - VROT(K)
     &             - WCSRD_U(K,IM,1)*VINF(1)
     &             - WCSRD_U(K,IM,2)*VINF(2)
     &             - WCSRD_U(K,IM,3)*VINF(3)
     &             - WCSRD_U(K,IM,4)*WROT(1)
     &             - WCSRD_U(K,IM,5)*WROT(2)
     &             - WCSRD_U(K,IM,6)*WROT(3)
          ENDDO
C
          ISTRIP = ISTRPV(I)
          GAM_Q(I,IQ) = -DPJET_Q(ISTRIP,IQ) * DOT(ENC(1,I),DWC)
C
        ENDDO

C********************************************************************
C...Holdover from HPV hydro project for forces near free surface
C...Eliminates excluded vortex equations for strips with z<Zsym 
ccc      CALL MUNGEB(GAM_Q(1,IQ))
C********************************************************************
C
        CALL BAKSUB(NVMAX,NVOR,AICN,IAPIV,GAM_Q(1,IQ))
 100  CONTINUE
C
      RETURN
      END ! GJCALC



      SUBROUTINE MUNGEA
C
C...PURPOSE  To remove hidden vortex equations in AIC matrix
C          
C...OUTPUT   A(.,.)  AIC matrix with affected rows replaced with 1 on diagonal
C                
C
      INCLUDE 'AVL.INC'
C
      DO 30 J = 1, NSTRIP
        IF (.NOT. LSTRIPOFF(J)) GO TO 30
        I1 = IJFRST(J)
        DO 20 K = 1, NVSTRP(J)
          II = I1+K-1
          DO 10 I = 1, NVOR
            AICN(II,I) = 0.0
   10     CONTINUE
          AICN(II,II) = 1.0
   20   CONTINUE
   30 CONTINUE
C
      RETURN
      END


      SUBROUTINE MUNGEB(B)
C
C...PURPOSE  To remove hidden vortex equations in RHS's
C          
C...OUTPUT   B(.)  RHS vector with affected rows replaced with 0
C
      INCLUDE 'AVL.INC'
      REAL B(NVMAX)
C
      DO 30 J = 1, NSTRIP
        IF (.NOT. LSTRIPOFF(J)) GO TO 30
        I1 = IJFRST(J)
        DO 20 K = 1, NVSTRP(J)
          II = I1+K-1
          B(II) = 0.
   20   CONTINUE
   30 CONTINUE
C
      RETURN
      END


      SUBROUTINE GAMSUM
      INCLUDE 'AVL.INC'
C--------------------------------------------------
C     Sums AIC components to get GAM, SRC, DBL
C--------------------------------------------------
C
C---- Set vortex strengths
      DO I = 1, NVOR
        GAM(I) = GAM_U(I,1)*VINF(1)
     &         + GAM_U(I,2)*VINF(2)
     &         + GAM_U(I,3)*VINF(3)
     &         + GAM_U(I,4)*WROT(1)
     &         + GAM_U(I,5)*WROT(2)
     &         + GAM_U(I,6)*WROT(3)
        DO N = 1, NCONTROL
          GAM(I) = GAM(I) + GAM_D(I,N)*DELCON(N)
        ENDDO
        DO N = 1, NDESIGN
          GAM(I) = GAM(I) + GAM_G(I,N)*DELDES(N)
        ENDDO
        DO N = 1, NVARJET
          GAM(I) = GAM(I) + GAM_J(I,N)*DELJET(N)
        ENDDO
      END DO
C
C---- Set source and doublet strengths
      DO L = 1, NLNODE
        SRC(L) = SRC_U(L,1)*VINF(1)
     &         + SRC_U(L,2)*VINF(2)
     &         + SRC_U(L,3)*VINF(3)
     &         + SRC_U(L,4)*WROT(1)
     &         + SRC_U(L,5)*WROT(2)
     &         + SRC_U(L,6)*WROT(3)
        DO K = 1, 3
          DBL(K,L) = DBL_U(K,L,1)*VINF(1)
     &             + DBL_U(K,L,2)*VINF(2)
     &             + DBL_U(K,L,3)*VINF(3)
     &             + DBL_U(K,L,4)*WROT(1)
     &             + DBL_U(K,L,5)*WROT(2)
     &             + DBL_U(K,L,6)*WROT(3)
        ENDDO
      ENDDO
C
      RETURN
      END ! GAMSUM


      SUBROUTINE VELSUM
      INCLUDE 'AVL.INC'
C--------------------------------------------------
C     Sums AIC components to get WC, WV
C--------------------------------------------------
C
      DO I = 1, NVOR
        DO K = 1, 3
          WC(K,I) = WCSRD_U(K,I,1)*VINF(1)
     &            + WCSRD_U(K,I,2)*VINF(2)
     &            + WCSRD_U(K,I,3)*VINF(3)
     &            + WCSRD_U(K,I,4)*WROT(1)
     &            + WCSRD_U(K,I,5)*WROT(2)
     &            + WCSRD_U(K,I,6)*WROT(3)
          WV(K,I) = WVSRD_U(K,I,1)*VINF(1)
     &            + WVSRD_U(K,I,2)*VINF(2)
     &            + WVSRD_U(K,I,3)*VINF(3)
     &            + WVSRD_U(K,I,4)*WROT(1)
     &            + WVSRD_U(K,I,5)*WROT(2)
     &            + WVSRD_U(K,I,6)*WROT(3)
          DO J = 1, NVOR
            WC(K,I) = WC(K,I) + WC_GAM(K,I,J)*GAM(J)
            WV(K,I) = WV(K,I) + WV_GAM(K,I,J)*GAM(J)
          ENDDO
C
          DO N = 1, NUMAX
            WC_U(K,I,N) = WCSRD_U(K,I,N)
            WV_U(K,I,N) = WVSRD_U(K,I,N)
            DO J = 1, NVOR
              WC_U(K,I,N) = WC_U(K,I,N) + WC_GAM(K,I,J)*GAM_U(J,N)
              WV_U(K,I,N) = WV_U(K,I,N) + WV_GAM(K,I,J)*GAM_U(J,N)
            ENDDO
          ENDDO
C
        ENDDO
      ENDDO
C
      RETURN
      END ! VELSUM



      SUBROUTINE WSENS
      INCLUDE 'AVL.INC'
C---------------------------------------------------
C     Computes induced-velocity sensitivities
C     to control and design changes
C---------------------------------------------------
C
      DO I = 1, NVOR
        DO K = 1, 3
          DO N = 1, NCONTROL
            WC_D(K,I,N) = 0.
            WV_D(K,I,N) = 0.
            DO J = 1, NVOR
              WC_D(K,I,N) = WC_D(K,I,N) + WC_GAM(K,I,J)*GAM_D(J,N)
              WV_D(K,I,N) = WV_D(K,I,N) + WV_GAM(K,I,J)*GAM_D(J,N)
            ENDDO
          ENDDO  
C
          DO N = 1, NDESIGN
            WC_G(K,I,N) = 0.
            WV_G(K,I,N) = 0.
            DO J = 1, NVOR
              WC_G(K,I,N) = WC_G(K,I,N) + WC_GAM(K,I,J)*GAM_G(J,N)
              WV_G(K,I,N) = WV_G(K,I,N) + WV_GAM(K,I,J)*GAM_G(J,N)
            ENDDO
          ENDDO  

          DO N = 1, NVARJET
            WC_J(K,I,N) = 0.
            WV_J(K,I,N) = 0.
            DO J = 1, NVOR
              WC_J(K,I,N) = WC_J(K,I,N) + WC_GAM(K,I,J)*GAM_J(J,N)
              WV_J(K,I,N) = WV_J(K,I,N) + WV_GAM(K,I,J)*GAM_J(J,N)
            ENDDO
          ENDDO  
        ENDDO
      ENDDO
C
      RETURN
      END ! WSENS
