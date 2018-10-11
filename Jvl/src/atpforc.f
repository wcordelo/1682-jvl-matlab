C***********************************************************************
C    Module:  atpforc.f
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

      SUBROUTINE TPFORC
C
C...PURPOSE  To calculate the far-field forces on the configuration using
C            a trefftz plane method.
C
C...INPUT    Geometry data from labelled commons
C            CNC     Strip span loading 
C
C...OUTPUT   CLFF    Total far-field lift
C            CYFF    Total far-field side force
C            CDFF    Total far-field drag
C            SPANEF  Span efficiency
C            DWWAKE  Far-field wake downwash at center of strip 
C
C...COMMENTS   The far-field drag is calculated using the Trefftz
C            plane (kinetic energy integral in the far wake).
C            The span-loading CNC is all that is required, plus
C            geometry data defining the wake position.
C            Since the wakes are just the horseshoe legs extending 
C            downstream from the bound legs, only the Y and Z 
C            coordinates are used. The normalwash in the cross-plane
C            is evaluated over the real and "image" sides.
C
      INCLUDE 'AVL.INC'
C
      REAL NY, NZ
      REAL VY_U(NUMAX), VZ_U(NUMAX), 
     &     VY_D(NDMAX), VZ_D(NDMAX), 
     &     VY_G(NGMAX), VZ_G(NGMAX),
     &     VY_J(NJMAX), VZ_J(NJMAX)
      REAL P(3,3), P_M(3,3), P_A(3,3), P_B(3,3)
      REAL RT1(3,NSMAX),
     &     RT2(3,NSMAX),
     &     RTC(3,NSMAX)
      REAL GAMS(NSMAX), 
     &     GAMS_U(NSMAX,NUMAX),
     &     GAMS_D(NSMAX,NDMAX),
     &     GAMS_G(NSMAX,NGMAX),
     &     GAMS_J(NSMAX,NJMAX)
C
      REAL PJET, MJET, NJET, MJET_PJET
      REAL PJET_J(NJMAX), MJET_J(NJMAX)
C
      REAL JVEC(3),JVECSQ, JVECAI,
     &     JVEC_U(3,NUMAX),
     &     JVEC_D(3,NDMAX),
     &     JVEC_G(3,NGMAX),
     &     JVEC_J(3,NJMAX)
      REAL JHAT(3),
     &     JHAT_U(3,NUMAX),
     &     JHAT_D(3,NDMAX),
     &     JHAT_G(3,NGMAX),
     &     JHAT_J(3,NJMAX)
C
      HPI = 1.0 / (2.0*PI)
C
C---- set Prandtl-Glauert transformation matrix
      ALFAT = 0.
      BETAT = 0.
      CALL PGMAT(AMACH,ALFAT,BETAT,P,P_M,P_A,P_B)
C
      YOFF = 2.0*YSYM
      ZOFF = 2.0*ZSYM
C
      CLFFI = 0.
      CYFFI = 0.
      CDFFI = 0.
      CLFFJ = 0.
      CYFFJ = 0.
      CDFFJ = 0.
      CDFFV = 0.
C
      CJFF = 0.
      CQFF = 0.
      DO N = 1, NUMAX
        CLFFI_U(N) = 0.
        CYFFI_U(N) = 0.
        CDFFI_U(N) = 0.
        CLFFJ_U(N) = 0.
        CYFFJ_U(N) = 0.
        CDFFJ_U(N) = 0.
        CDFFV_U(N) = 0.
      ENDDO
      DO N = 1, NCONTROL
        CLFFI_D(N) = 0.
        CYFFI_D(N) = 0.
        CDFFI_D(N) = 0.
        CLFFJ_D(N) = 0.
        CYFFJ_D(N) = 0.
        CDFFJ_D(N) = 0.
        CDFFV_D(N) = 0.
      ENDDO
      DO N = 1, NDESIGN
        CLFFI_G(N) = 0.
        CYFFI_G(N) = 0.
        CDFFI_G(N) = 0.
        CLFFJ_G(N) = 0.
        CYFFJ_G(N) = 0.
        CDFFJ_G(N) = 0.
        CDFFV_G(N) = 0.
      ENDDO
      DO N = 1, NVARJET
        CLFFI_J(N) = 0.
        CYFFI_J(N) = 0.
        CDFFI_J(N) = 0.
        CLFFJ_J(N) = 0.
        CYFFJ_J(N) = 0.
        CDFFJ_J(N) = 0.
        CDFFV_J(N) = 0.
C
        CJFF_J(N) = 0.
        CQFF_J(N) = 0.
      ENDDO
C
      DO JC = 1, NSTRIPS
        GAMS(JC) = 0.
        DO N = 1, NUMAX
          GAMS_U(JC,N) = 0.
        ENDDO
        DO N = 1, NCONTROL
          GAMS_D(JC,N) = 0.
        ENDDO
        DO N = 1, NDESIGN
          GAMS_G(JC,N) = 0.
        ENDDO
        DO N = 1, NVARJET
          GAMS_J(JC,N) = 0.
        ENDDO
C
        I1  = IJFRST(JC)
        NVC = NVSTRP(JC)
        DO I = I1, I1+NVC-1 
          GAMS(JC) = GAMS(JC) + GAM(I)
          DO N = 1, NUMAX
            GAMS_U(JC,N) = GAMS_U(JC,N) + GAM_U(I,N)
          ENDDO
          DO N = 1, NCONTROL
            GAMS_D(JC,N) = GAMS_D(JC,N) + GAM_D(I,N)
          ENDDO
          DO N = 1, NDESIGN
            GAMS_G(JC,N) = GAMS_G(JC,N) + GAM_G(I,N)
          ENDDO
          DO N = 1, NVARJET
            GAMS_J(JC,N) = GAMS_J(JC,N) + GAM_J(I,N)
          ENDDO
        ENDDO
C
        JJC = JJETS(JC)
        IF(JJC .NE. 0) THEN
C------- add on circulation from h.v.'s on jet sheet
         I1  = IJFRST(JJC)
         NVC = NVSTRP(JJC)
         DO I = I1, I1+NVC-1 
           GAMS(JC) = GAMS(JC) + GAM(I)
           DO N = 1, NUMAX
             GAMS_U(JC,N) = GAMS_U(JC,N) + GAM_U(I,N)
           ENDDO
           DO N = 1, NCONTROL
             GAMS_D(JC,N) = GAMS_D(JC,N) + GAM_D(I,N)
           ENDDO
           DO N = 1, NDESIGN
             GAMS_G(JC,N) = GAMS_G(JC,N) + GAM_G(I,N)
           ENDDO
           DO N = 1, NVARJET
             GAMS_J(JC,N) = GAMS_J(JC,N) + GAM_J(I,N)
           ENDDO
         ENDDO
        ENDIF
C
      ENDDO
C
C---- set x,y,z in wind axes (Y,Z are then in Trefftz plane)
      DO JC = 1, NSTRIPS
C------ last h.v. in strip defines Trefftz plane trace
        IC = IJFRST(JC) + NVSTRP(JC) - 1
        DO K = 1, 3
          RT1(K,JC) = P(K,1)*RV1(1,IC)+P(K,2)*RV1(2,IC)+P(K,3)*RV1(3,IC)
          RT2(K,JC) = P(K,1)*RV2(1,IC)+P(K,2)*RV2(2,IC)+P(K,3)*RV2(3,IC)
          RTC(K,JC) = P(K,1)*RC (1,IC)+P(K,2)*RC (2,IC)+P(K,3)*RC (3,IC)
        ENDDO
      ENDDO
C
C...Find the normal velocity across each strip at the projected control
C   point location
      DO 40 JC = 1, NSTRIPS
        DXT = RT2(1,JC) - RT1(1,JC)
        DYT = RT2(2,JC) - RT1(2,JC)
        DZT = RT2(3,JC) - RT1(3,JC)
        DST = SQRT(DYT*DYT + DZT*DZT)
C
        NY = -DZT / DST
        NZ =  DYT / DST
        YCNTR = RTC(2,JC)
        ZCNTR = RTC(3,JC)
C
        VY = 0.
        VZ = 0.
        DO N = 1, NUMAX
          VY_U(N) = 0.
          VZ_U(N) = 0.
        ENDDO
        DO N = 1, NCONTROL
          VY_D(N) = 0.
          VZ_D(N) = 0.
        ENDDO
        DO N = 1, NDESIGN
          VY_G(N) = 0.
          VZ_G(N) = 0.
        ENDDO
        DO N = 1, NVARJET
          VY_J(N) = 0.
          VZ_J(N) = 0.
        ENDDO
C
C...Sum velocity contributions from wake vortices
        DO 30 JV = 1, NSTRIPS
          DSYZ = SQRT(  (RT2(2,JV)-RT1(2,JV))**2
     &                + (RT2(3,JV)-RT1(3,JV))**2 )
          IF(LSURF(ISURFS(JC)) .EQ. LSURF(ISURFS(JV))) THEN
ccc        RCORE = 0.0001*DSYZ
           RCORE = 0.
          ELSE
           RCORE = MAX( VRCORE*CHORD(JV) , 2.0*VRCORE*DSYZ )
          ENDIF
C
          RCORE = 0.
C
          DY1 = YCNTR - RT1(2,JV)
          DY2 = YCNTR - RT2(2,JV)
          DZ1 = ZCNTR - RT1(3,JV)
          DZ2 = ZCNTR - RT2(3,JV)
          RSQ1 = DY1*DY1 + DZ1*DZ1 + RCORE**2
          RSQ2 = DY2*DY2 + DZ2*DZ2 + RCORE**2
          VY = VY + HPI*GAMS(JV)*( DZ1/RSQ1 - DZ2/RSQ2)
          VZ = VZ + HPI*GAMS(JV)*(-DY1/RSQ1 + DY2/RSQ2)
          DO N = 1, NUMAX
            VY_U(N) = VY_U(N) + HPI*GAMS_U(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)
            VZ_U(N) = VZ_U(N) + HPI*GAMS_U(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)
          ENDDO
          DO N = 1, NCONTROL
            VY_D(N) = VY_D(N) + HPI*GAMS_D(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)
            VZ_D(N) = VZ_D(N) + HPI*GAMS_D(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)
          ENDDO
          DO N = 1, NDESIGN
            VY_G(N) = VY_G(N) + HPI*GAMS_G(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)
            VZ_G(N) = VZ_G(N) + HPI*GAMS_G(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)
          ENDDO
          DO N = 1, NVARJET
            VY_J(N) = VY_J(N) + HPI*GAMS_J(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)
            VZ_J(N) = VZ_J(N) + HPI*GAMS_J(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)
          ENDDO
C
          IF(IZSYM.NE.0) THEN
            DY1 = YCNTR -       RT1(2,JV)
            DY2 = YCNTR -       RT2(2,JV)
            DZ1 = ZCNTR - (ZOFF-RT1(3,JV))
            DZ2 = ZCNTR - (ZOFF-RT2(3,JV))
CCC         DZ1 = ZCNTR - (ZOFF-RT1(3,JV)+ALFA*RT1(1,JV))
CCC         DZ2 = ZCNTR - (ZOFF-RT2(3,JV)+ALFA*RT2(1,JV))
            RSQ1 = DY1*DY1 + DZ1*DZ1
            RSQ2 = DY2*DY2 + DZ2*DZ2
            VY = VY - HPI*GAMS(JV)*( DZ1/RSQ1 - DZ2/RSQ2)*IZSYM
            VZ = VZ - HPI*GAMS(JV)*(-DY1/RSQ1 + DY2/RSQ2)*IZSYM
            DO N = 1, NUMAX
              VY_U(N) = VY_U(N)
     &          - HPI*GAMS_U(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IZSYM
              VZ_U(N) = VZ_U(N)
     &          - HPI*GAMS_U(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IZSYM
            ENDDO
            DO N = 1, NCONTROL
              VY_D(N) = VY_D(N)
     &          - HPI*GAMS_D(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IZSYM
              VZ_D(N) = VZ_D(N)
     &          - HPI*GAMS_D(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IZSYM
            ENDDO
            DO N = 1, NDESIGN
              VY_G(N) = VY_G(N)
     &          - HPI*GAMS_G(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IZSYM
              VZ_G(N) = VZ_G(N)
     &          - HPI*GAMS_G(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IZSYM
            ENDDO
            DO N = 1, NVARJET
              VY_J(N) = VY_J(N)
     &          - HPI*GAMS_J(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IZSYM
              VZ_J(N) = VZ_J(N)
     &          - HPI*GAMS_J(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IZSYM
            ENDDO
          ENDIF 
C
          IF(IYSYM.NE.0) THEN
            DY1 = YCNTR - (YOFF-RT1(2,JV))
            DY2 = YCNTR - (YOFF-RT2(2,JV))
            DZ1 = ZCNTR -       RT1(3,JV)
            DZ2 = ZCNTR -       RT2(3,JV)
            RSQ1 = DY1*DY1 + DZ1*DZ1
            RSQ2 = DY2*DY2 + DZ2*DZ2
            VY = VY - HPI*GAMS(JV)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM
            VZ = VZ - HPI*GAMS(JV)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM
            DO N = 1, NUMAX
              VY_U(N) = VY_U(N)
     &          - HPI*GAMS_U(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM
              VZ_U(N) = VZ_U(N)
     &          - HPI*GAMS_U(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM
            ENDDO
            DO N = 1, NCONTROL
              VY_D(N) = VY_D(N)
     &          - HPI*GAMS_D(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM
              VZ_D(N) = VZ_D(N)
     &          - HPI*GAMS_D(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM
            ENDDO
            DO N = 1, NDESIGN
              VY_G(N) = VY_G(N)
     &          - HPI*GAMS_G(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM
              VZ_G(N) = VZ_G(N)
     &          - HPI*GAMS_G(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM
            ENDDO
            DO N = 1, NVARJET
              VY_J(N) = VY_J(N)
     &          - HPI*GAMS_J(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM
              VZ_J(N) = VZ_J(N)
     &          - HPI*GAMS_J(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM
            ENDDO
C
            IF(IZSYM.NE.0) THEN
              DY1 = YCNTR - (YOFF-RT1(2,JV))
              DY2 = YCNTR - (YOFF-RT2(2,JV))
              DZ1 = ZCNTR - (ZOFF-RT1(3,JV))
              DZ2 = ZCNTR - (ZOFF-RT2(3,JV))
CCC           DZ1 = ZCNTR - (ZOFF-RT1(3,JV)+ALFA*RT1(1,JV))
CCC           DZ2 = ZCNTR - (ZOFF-RT2(3,JV)+ALFA*RT2(1,JV))
              RSQ1 = DY1*DY1 + DZ1*DZ1
              RSQ2 = DY2*DY2 + DZ2*DZ2
              VY = VY + HPI*GAMS(JV)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM*IZSYM
              VZ = VZ + HPI*GAMS(JV)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM*IZSYM
              DO N = 1, NUMAX
                VY_U(N) = VY_U(N)
     &            - HPI*GAMS_U(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM*IZSYM
                VZ_U(N) = VZ_U(N)
     &            - HPI*GAMS_U(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM*IZSYM
              ENDDO
              DO N = 1, NCONTROL
                VY_D(N) = VY_D(N)
     &            - HPI*GAMS_D(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM*IZSYM
                VZ_D(N) = VZ_D(N)
     &            - HPI*GAMS_D(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM*IZSYM
              ENDDO
              DO N = 1, NDESIGN
                VY_G(N) = VY_G(N)
     &            - HPI*GAMS_G(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM*IZSYM
                VZ_G(N) = VZ_G(N)
     &            - HPI*GAMS_G(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM*IZSYM
              ENDDO
              DO N = 1, NVARJET
                VY_J(N) = VY_J(N)
     &            - HPI*GAMS_J(JV,N)*( DZ1/RSQ1 - DZ2/RSQ2)*IYSYM*IZSYM
                VZ_J(N) = VZ_J(N)
     &            - HPI*GAMS_J(JV,N)*(-DY1/RSQ1 + DY2/RSQ2)*IYSYM*IZSYM
              ENDDO
            ENDIF
          ENDIF
C
   30   CONTINUE
C
C...Trefftz-plane drag is kinetic energy in crossflow
        DWWAKE(JC) = -(NY*VY + NZ*VZ)
C
        CLFFI = CLFFI + 2.0*GAMS(JC)*          DYT
        CYFFI = CYFFI - 2.0*GAMS(JC)* DZT         
        CDFFI = CDFFI +     GAMS(JC)*(DZT*VY - DYT*VZ)
        DO N = 1, NUMAX
          CLFFI_U(N) = CLFFI_U(N) + 2.0*GAMS_U(JC,N)*DYT
          CYFFI_U(N) = CYFFI_U(N) - 2.0*GAMS_U(JC,N)*DZT
          CDFFI_U(N) = CDFFI_U(N)
     &              + (  GAMS_U(JC,N)*(DZT*VY      - DYT*VZ     )
     &                 + GAMS(JC)    *(DZT*VY_U(N) - DYT*VZ_U(N)) )
        ENDDO
        DO N = 1, NCONTROL
          CLFFI_D(N) = CLFFI_D(N) + 2.0*GAMS_D(JC,N)*DYT
          CYFFI_D(N) = CYFFI_D(N) - 2.0*GAMS_D(JC,N)*DZT
          CDFFI_D(N) = CDFFI_D(N)
     &              + (  GAMS_D(JC,N)*(DZT*VY      - DYT*VZ     )
     &                 + GAMS(JC)    *(DZT*VY_D(N) - DYT*VZ_D(N)) )
        ENDDO
        DO N = 1, NDESIGN
          CLFFI_G(N) = CLFFI_G(N) + 2.0*GAMS_G(JC,N)*DYT
          CYFFI_G(N) = CYFFI_G(N) - 2.0*GAMS_G(JC,N)*DZT
          CDFFI_G(N) = CDFFI_G(N)
     &              + (  GAMS_G(JC,N)*(DZT*VY      - DYT*VZ     )
     &                 + GAMS(JC)    *(DZT*VY_G(N) - DYT*VZ_G(N)) )
        ENDDO
        DO N = 1, NVARJET
          CLFFI_J(N) = CLFFI_J(N) + 2.0*GAMS_J(JC,N)*DYT
          CYFFI_J(N) = CYFFI_J(N) - 2.0*GAMS_J(JC,N)*DZT
          CDFFI_J(N) = CDFFI_J(N)
     &              + (  GAMS_J(JC,N)*(DZT*VY      - DYT*VZ     )
     &                 + GAMS(JC)    *(DZT*VY_J(N) - DYT*VZ_J(N)) )
        ENDDO
C
C
        SSTRIP = CHORD(JC)*WSTRIP(JC)
        CDV = CDV_STRP(JC)
        CDFFV = CDFFV + CDV*SSTRIP
C
C
C------ skip jet sheet calcs if there's no jet sheet
        JJC = JJETS(JC)
        IF(JJC .EQ. 0) GO TO 40
C
C------- accumulate excess momentum
         DPJET = 0.0
         DO N = 1, NVARJET
           DPJET = DPJET + DELJET(N)*GJSTRP(JJC,N)
           PJET_J(N) =               GJSTRP(JJC,N)
         ENDDO
C
C------- jet height
         NJET = HJSTRP(JJC,1)
C
C------- jet total momentum
         PJET = MAX( DPJET + NJET , 0.0 )
C
C------- calculate mass flow
         RHJET = 1.0
         MJET = SQRT(RHJET*NJET*PJET)
         IF(PJET .GT. 0.0) THEN
           MJET_PJET = 0.5*MJET / PJET
         ELSE
           MJET_PJET = 0.0
         ENDIF
C

C------ jet vector in Trefftz Plane
        JVEC(1) = P(1,1)
        JVEC(2) = P(1,2) + VY
        JVEC(3) = P(1,3) + VZ
C
        DO N = 1, NUMAX
          JVEC_U(1,N) = 0.
          JVEC_U(2,N) = VY_U(N)
          JVEC_U(3,N) = VZ_U(N)
        ENDDO
        DO N = 1, NCONTROL
          JVEC_D(1,N) = 0.
          JVEC_D(2,N) = VY_D(N)
          JVEC_D(3,N) = VZ_D(N)
        ENDDO
        DO N = 1, NDESIGN
          JVEC_G(1,N) = 0.
          JVEC_G(2,N) = VY_G(N)
          JVEC_G(3,N) = VZ_G(N)
        ENDDO
        DO N = 1, NVARJET
          JVEC_J(1,N) = 0.
          JVEC_J(2,N) = VY_J(N)
          JVEC_J(3,N) = VZ_J(N)
        ENDDO
C
        JVECSQ = JVEC(1)**2 + JVEC(2)**2 + JVEC(3)**2
        IF(JVECSQ .NE. 0.0) THEN
         JVECAI = 1.0 / SQRT(JVECSQ)
        ELSE
         JVECAI = 1.0
        ENDIF
C
        DO K = 1, 3
          JHAT(K) = JVEC(K)*JVECAI
          DO N = 1, NUMAX
            JHAT_U(K,N) = JVEC_U(K,N)*JVECAI
     &                  - JVEC(K)*( JVEC(1)*JVEC_U(1,N)
     &                            + JVEC(2)*JVEC_U(2,N)
     &                            + JVEC(3)*JVEC_U(3,N) )*JVECAI**3
          ENDDO
          DO N = 1, NCONTROL
            JHAT_D(K,N) = JVEC_D(K,N)*JVECAI
     &                  - JVEC(K)*( JVEC(1)*JVEC_D(1,N)
     &                            + JVEC(2)*JVEC_D(2,N)
     &                            + JVEC(3)*JVEC_D(3,N) )*JVECAI**3
          ENDDO
          DO N = 1, NDESIGN
            JHAT_G(K,N) = JVEC_G(K,N)*JVECAI
     &                  - JVEC(K)*( JVEC(1)*JVEC_G(1,N)
     &                            + JVEC(2)*JVEC_G(2,N)
     &                            + JVEC(3)*JVEC_G(3,N) )*JVECAI**3
          ENDDO
          DO N = 1, NVARJET
            JHAT_J(K,N) = JVEC_J(K,N)*JVECAI
     &                  - JVEC(K)*( JVEC(1)*JVEC_J(1,N)
     &                            + JVEC(2)*JVEC_J(2,N)
     &                            + JVEC(3)*JVEC_J(3,N) )*JVECAI**3
          ENDDO
        ENDDO
C
C
        CLFFJ = CLFFJ - (PJET*JHAT(3)       )*DST * 2.0
        CYFFJ = CYFFJ - (PJET*JHAT(2)       )*DST * 2.0
        CDFFJ = CDFFJ - (PJET*JHAT(1) - MJET)*DST * 2.0
C
        CJFF  = CJFF  +  PJET                *DST * 2.0
        CQFF  = CQFF  +                 MJET *DST      
C
        DO N = 1, NUMAX
          CLFFJ_U(N) = CLFFJ_U(N) - ( PJET*JHAT_U(3,N) )*DST * 2.0
          CYFFJ_U(N) = CYFFJ_U(N) - ( PJET*JHAT_U(2,N) )*DST * 2.0
          CDFFJ_U(N) = CDFFJ_U(N) - ( PJET*JHAT_U(1,N) )*DST * 2.0
        ENDDO
        DO N = 1, NCONTROL
          CLFFJ_D(N) = CLFFJ_D(N) - ( PJET*JHAT_D(3,N) )*DST * 2.0
          CYFFJ_D(N) = CYFFJ_D(N) - ( PJET*JHAT_D(2,N) )*DST * 2.0
          CDFFJ_D(N) = CDFFJ_D(N) - ( PJET*JHAT_D(1,N) )*DST * 2.0
        ENDDO
        DO N = 1, NDESIGN
          CLFFJ_G(N) = CLFFJ_G(N) - ( PJET*JHAT_G(3,N) )*DST * 2.0
          CYFFJ_G(N) = CYFFJ_G(N) - ( PJET*JHAT_G(2,N) )*DST * 2.0
          CDFFJ_G(N) = CDFFJ_G(N) - ( PJET*JHAT_G(1,N) )*DST * 2.0
        ENDDO
        DO N = 1, NVARJET
          CLFFJ_J(N) = CLFFJ_J(N) - (PJET     *JHAT_J(3,N)
     &                              +PJET_J(N)*JHAT(3)    )*DST*2.0
          CYFFJ_J(N) = CYFFJ_J(N) - (PJET     *JHAT_J(2,N)
     &                              +PJET_J(N)*JHAT(2)    )*DST*2.0
          CDFFJ_J(N) = CDFFJ_J(N) - (PJET     *JHAT_J(1,N)
     &                              +PJET_J(N)*JHAT(1)
     &                              -MJET_PJET*PJET_J(N)  )*DST*2.0
C
          CJFF_J(N) = CJFF_J(N)   +  PJET_J(N)             *DST*2.0
          CQFF_J(N) = CQFF_J(N)   +  MJET_PJET*PJET_J(N)   *DST    
        ENDDO
C
   40 CONTINUE
C
C---- Double the X,Z forces, zero Y force for a Y symmetric case
      IF(IYSYM.EQ.1) THEN
       CLFFI = 2.0 * CLFFI
       CYFFI = 0.0
       CDFFI = 2.0 * CDFFI
       CLFFJ = 2.0 * CLFFJ
       CYFFJ = 0.0
       CDFFJ = 2.0 * CDFFJ
       CDFFV = 2.0 * CDFFV
C
       CJFF = 2.0 * CJFF
       CQFF = 2.0 * CQFF
       DO N = 1, NUMAX
         CLFFI_U(N) = 2.0 * CLFFI_U(N)
         CYFFI_U(N) = 0.0
         CDFFI_U(N) = 2.0 * CDFFI_U(N)
         CLFFJ_U(N) = 2.0 * CLFFJ_U(N)
         CYFFJ_U(N) = 0.0
         CDFFJ_U(N) = 2.0 * CDFFJ_U(N)
         CDFFV_U(N) = 2.0 * CDFFV_U(N)
       ENDDO
       DO N = 1, NCONTROL
         CLFFI_D(N) = 2.0 * CLFFI_D(N)
         CYFFI_D(N) = 0.0
         CDFFI_D(N) = 2.0 * CDFFI_D(N)
         CLFFJ_D(N) = 2.0 * CLFFJ_D(N)
         CYFFJ_D(N) = 0.0
         CDFFJ_D(N) = 2.0 * CDFFJ_D(N)
         CDFFV_D(N) = 2.0 * CDFFV_D(N)
       ENDDO
       DO N = 1, NDESIGN
         CLFFI_G(N) = 2.0 * CLFFI_G(N)
         CYFFI_G(N) = 0.0
         CDFFI_G(N) = 2.0 * CDFFI_G(N)
         CLFFJ_G(N) = 2.0 * CLFFJ_G(N)
         CYFFJ_G(N) = 0.0
         CDFFJ_G(N) = 2.0 * CDFFJ_G(N)
         CDFFV_G(N) = 2.0 * CDFFV_G(N)
       ENDDO
       DO N = 1, NVARJET
         CLFFI_J(N) = 2.0 * CLFFI_J(N)
         CYFFI_J(N) = 0.0
         CDFFI_J(N) = 2.0 * CDFFI_J(N)
         CLFFJ_J(N) = 2.0 * CLFFJ_J(N)
         CYFFJ_J(N) = 0.0
         CDFFJ_J(N) = 2.0 * CDFFJ_J(N)
         CDFFV_J(N) = 2.0 * CDFFV_J(N)
C
         CJFF_J(N) = 2.0 * CJFF_J(N)
         CQFF_J(N) = 2.0 * CQFF_J(N)
       ENDDO
      ENDIF
C
C---- aspect ratio * SREF
      BSQ = BREF**2
C
C---- span efficiency
      IF(CDFFI .EQ. 0.0) THEN
       SPANEF = 0.
       SPANEF_A = 0.
       DO N = 1, NUMAX
         SPANEF_U(N) = 0.
       ENDDO
       DO N = 1, NCONTROL
         SPANEF_D(N) = 0.
       ENDDO
       DO N = 1, NDESIGN
         SPANEF_G(N) = 0.
       ENDDO
       DO N = 1, NVARJET
         SPANEF_J(N) = 0.
       ENDDO
C
      ELSE
       CDACT = CDFFI + CDFFJ + CJFF - 2.0*CQFF
C
       SPANEF = ((CLFFI+CLFFJ)**2 + (CYFFI+CYFFJ)**2)
     &                               / ((PI*BSQ + 2.0*CJFF) * CDACT)
       SPANEF_CL = 2.0*(CLFFI+CLFFJ) / ((PI*BSQ + 2.0*CJFF) * CDACT)
       SPANEF_CY = 2.0*(CYFFI+CYFFJ) / ((PI*BSQ + 2.0*CJFF) * CDACT)
       SPANEF_CD = -SPANEF/CDACT
       SPANEF_CJ = -SPANEF/CDACT - SPANEF/(PI*BSQ + 2.0*CJFF) * 2.0
       SPANEF_CQ = -SPANEF/CDACT * (-2.0)
C
       SPANEV    = ((CLFFI+CLFFJ)**2 + (CYFFI+CYFFJ)**2)
     &                               / (PI*BSQ * CDACT)
       SPANEV_CL = 2.0*(CLFFI+CLFFJ) / (PI*BSQ * CDACT)
       SPANEV_CY = 2.0*(CYFFI+CYFFJ) / (PI*BSQ * CDACT)
       SPANEV_CD = -SPANEV/CDACT
       SPANEV_CJ = -SPANEV/CDACT
       SPANEV_CQ = -SPANEV/CDACT * (-2.0)
C
C
       SPANEF_A = 0.
       SPANEV_A = 0.
       DO N = 1, NUMAX
         SPANEF_U(N) = SPANEF_CL*(CLFFI_U(N)+CLFFJ_U(N))
     &               + SPANEF_CY*(CYFFI_U(N)+CYFFJ_U(N))
     &               + SPANEF_CD*(CDFFI_U(N)+CDFFJ_U(N))
         SPANEV_U(N) = SPANEV_CL*(CLFFI_U(N)+CLFFJ_U(N))
     &               + SPANEV_CY*(CYFFI_U(N)+CYFFJ_U(N))
     &               + SPANEV_CD*(CDFFI_U(N)+CDFFJ_U(N))
       ENDDO
       DO N = 1, NCONTROL
         SPANEF_D(N) = SPANEF_CL*(CLFFI_D(N)+CLFFJ_D(N))
     &               + SPANEF_CY*(CYFFI_D(N)+CYFFJ_D(N))
     &               + SPANEF_CD*(CDFFI_D(N)+CDFFJ_D(N))
         SPANEV_D(N) = SPANEV_CL*(CLFFI_D(N)+CLFFJ_D(N))
     &               + SPANEV_CY*(CYFFI_D(N)+CYFFJ_D(N))
     &               + SPANEV_CD*(CDFFI_D(N)+CDFFJ_D(N))
       ENDDO
       DO N = 1, NDESIGN
         SPANEF_G(N) = SPANEF_CL*(CLFFI_G(N)+CLFFJ_G(N))
     &               + SPANEF_CY*(CYFFI_G(N)+CYFFJ_G(N))
     &               + SPANEF_CD*(CDFFI_G(N)+CDFFJ_G(N))
         SPANEV_G(N) = SPANEV_CL*(CLFFI_G(N)+CLFFJ_G(N))
     &               + SPANEV_CY*(CYFFI_G(N)+CYFFJ_G(N))
     &               + SPANEV_CD*(CDFFI_G(N)+CDFFJ_G(N))
       ENDDO
       DO N = 1, NVARJET
         SPANEF_J(N) = SPANEF_CL*(CLFFI_J(N)+CLFFJ_J(N))
     &               + SPANEF_CY*(CYFFI_J(N)+CYFFJ_J(N))
     &               + SPANEF_CD*(CDFFI_J(N)+CDFFJ_J(N))
     &               + SPANEF_CJ*CJFF_J(N)
     &               + SPANEF_CQ*CQFF_J(N)
         SPANEV_J(N) = SPANEV_CL*(CLFFI_J(N)+CLFFJ_J(N))
     &               + SPANEV_CY*(CYFFI_J(N)+CYFFJ_J(N))
     &               + SPANEV_CD*(CDFFI_J(N)+CDFFJ_J(N))
     &               + SPANEV_CJ*CJFF_J(N)
     &               + SPANEV_CQ*CQFF_J(N)
       ENDDO
      ENDIF
C
      RETURN
      END ! TPFORC



      SUBROUTINE PGMAT(MACH,ALFA,BETA,P,P_M,P_A,P_B)
C-------------------------------------------------------
C     Calculates Prandtl-Glauert transformation matrix.
C      
C      xi      [       ] x
C              [       ]  
C      eta  =  [   P   ] y
C              [       ]
C      zeta    [       ] z
C
C-------------------------------------------------------
C
      REAL MACH, ALFA, BETA
      REAL P(3,3), P_M(3,3), P_A(3,3), P_B(3,3)
C
      BINV = 1.0 / SQRT(1.0 - MACH**2)
      BI_M = MACH * BINV**3
C
      SINA = SIN(ALFA)
      COSA = COS(ALFA)
C
      SINB = SIN(BETA)
      COSB = COS(BETA)
C
C
      P(1,1) =  COSA*COSB*BINV
      P(1,2) =      -SINB*BINV
      P(1,3) =  SINA*COSB*BINV
C
      P(2,1) =  COSA*SINB
      P(2,2) =       COSB
      P(2,3) =  SINA*SINB
C
      P(3,1) = -SINA
      P(3,2) = 0.
      P(3,3) =  COSA
C
C
      P_M(1,1) =  COSA*COSB*BI_M
      P_M(1,2) =      -SINB*BI_M
      P_M(1,3) =  SINA*COSB*BI_M
C
      P_M(2,1) = 0.
      P_M(2,2) = 0.
      P_M(2,3) = 0.
C
      P_M(3,1) = 0.
      P_M(3,2) = 0.
      P_M(3,3) = 0.
C
C
      P_A(1,1) = -SINA*COSB*BINV
      P_A(1,2) = 0.
      P_A(1,3) =  COSA*COSB*BINV
C
      P_A(2,1) = -SINA*SINB
      P_A(2,2) = 0.
      P_A(2,3) =  COSA*SINB
C
      P_A(3,1) = -COSA
      P_A(3,2) = 0.
      P_A(3,3) = -SINA
C
C
      P_B(1,1) = -COSA*SINB*BINV
      P_B(1,2) =      -COSB*BINV
      P_B(1,3) = -SINA*SINB*BINV
C
      P_B(2,1) =  COSA*COSB
      P_B(2,2) =      -SINB
      P_B(2,3) =  SINA*COSB
C
      P_B(3,1) = 0.
      P_B(3,2) = 0.
      P_B(3,3) = 0.
C
      RETURN
      END ! PGMAT
