C***********************************************************************
C    Module:  aoutput.f
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

      SUBROUTINE OUTTOT(LUN)
C
C...PURPOSE  To print out results of the vortex lattice calculation
C            for the input configuration.  
C
C...INPUT    Configuration data for case in labeled commons
C          
C...OUTPUT   Printed output on logical unit LUN
C
      INCLUDE 'AVL.INC'
      CHARACTER*50 SATYPE
      REAL CMSAX(3), ROTSAX(3)
C
 1000 FORMAT (A)
C
      IF (LUN.EQ.0) RETURN
C
C
      CALL GETSA(LNASA_SA,SATYPE,DIR)
C
      CA = COS(ALFA)
      SA = SIN(ALFA)
C
C---- set rates in stability axes
      ROTSAX(1) = WROT(1)*CA + WROT(3)*SA
      ROTSAX(2) = WROT(2)                
      ROTSAX(3) = WROT(3)*CA - WROT(1)*SA
C
C---- set moments in stability axes
      CMSAX(1) = CMT(1)*CA + CMT(3)*SA
      CMSAX(2) = CMT(2) 
      CMSAX(3) = CMT(3)*CA - CMT(1)*SA  
C
C---- inviscid near-field forces
      CDTI = CA*CFTI(1) + SA*CFTI(3)
      CYTI =    CFTI(2)
      CLTI = CA*CFTI(3) - SA*CFTI(1)
C
C---- jet near-field forces
      CDTJ = CA*CFTJ(1) + SA*CFTJ(3)
      CYTJ =    CFTJ(2)
      CLTJ = CA*CFTJ(3) - SA*CFTJ(1)
C
C---- viscous near-field forces
      CDTV = CA*CFTV(1) + SA*CFTV(3)
C
C---- dump it
      WRITE(LUN,200)
      WRITE(LUN,201) 
      WRITE(LUN,202) TITLE(1:60),NSURF,NSTRIP,NVOR
      IF(IYSYM.GT.0) WRITE (*,2034) YSYM
      IF(IYSYM.LT.0) WRITE (*,2035) YSYM
      IF(IZSYM.GT.0) WRITE (*,2036) ZSYM
      IF(IZSYM.LT.0) WRITE (*,2037) ZSYM

      WRITE(LUN,204) SREF,CREF,BREF, 
     &               XYZREF(1), XYZREF(2), XYZREF(3)
C
      WRITE(LUN,205) SATYPE

      WRITE(LUN,218) RTITLE(IRUN)
      WRITE(LUN,220) 
     &  ALFA/DTR, DIR*WROT(1)*BREF/2.0, DIR*ROTSAX(1)*BREF/2.0,
     &  BETA/DTR,     WROT(2)*CREF/2.0,
     &  AMACH   , DIR*WROT(3)*BREF/2.0, DIR*ROTSAX(3)*BREF/2.0
      IF(NVARJET .GT. 0) WRITE(LUN,222) CJT/SREF, CQT/SREF
C
      WRITE (LUN,226)
     & DIR*CFT(1)/SREF, DIR*CMT(1)/(SREF*BREF),DIR*CMSAX(1)/(SREF*BREF),
     &     CFT(2)/SREF,     CMT(2)/(SREF*CREF),
     & DIR*CFT(3)/SREF, DIR*CMT(3)/(SREF*BREF),DIR*CMSAX(3)/(SREF*BREF),
     & CLT/SREF, CLFF/SREF, CYFF/SREF,
     & CDT/SREF, CDFF/SREF, SPANEF,
     & CLTI/SREF, CLFFI/SREF,
     & CLTJ/SREF, CLFFJ/SREF,
     & CDTI/SREF, CDFFI/SREF, SPANEV,
     & CDTJ/SREF, CDFFJ/SREF,
     & CDTV/SREF, CDFFV/SREF
 226  FORMAT (
     & /2X,'CXtot =',F10.5,5X,'Cltot =',F10.5,5X,'Cl''tot =',F10.5
     & /2X,'CYtot =',F10.5,5X,'Cmtot =',F10.5 
     & /2X,'CZtot =',F10.5,5X,'Cntot =',F10.5,5X,'Cn''tot =',F10.5
     &//2X,'CLtot =',F10.5,5X,'CLff  =',F10.5,4X,'CYff  =',F10.5,
     & /2X,'CDtot =',F10.5,5X,'CDff  =',F10.5,4X,'    e =',F10.4,
     &//2X,'CLcir =',F10.5,5X,'CLffc =',F10.5,4X,
     & /2X,'CLjet =',F10.5,5X,'CLffj =',F10.5,4X,
     &//2X,'CDind =',F10.5,5X,'CDffi =',F10.5,4X,'e_vec =',F10.4,
     & /2X,'CDjet =',F10.5,5X,'CDffj =',F10.5,4X,
     & /2X,'CDvis =',F10.5,5X,'CDffv =',F10.5,4X )
C
      WRITE(LUN,*)
      DO K = 1, NCONTROL
        WRITE(LUN,231) DNAME(K), DELCON(K)
      ENDDO

      IF(NDESIGN.GT.0) WRITE(LUN,*)
      DO K = 1, NDESIGN
        WRITE(LUN,231) GNAME(K), DELDES(K)
      ENDDO
C
      IF(NVARJET.GT.0) WRITE(LUN,*)
      DO K = 1, NVARJET
        WRITE(LUN,231) JNAME(K), DELJET(K)
      ENDDO
C
      WRITE(LUN,200)
 200  FORMAT(1X,
     &'---------------------------------------------------------------')
 201  FORMAT(' Vortex Lattice Output -- Total Forces')
 202  FORMAT(/' Configuration: ',A
     &       /5X,'# Surfaces =',I4
     &       /5X,'# Strips   =',I4
     &       /5X,'# Vortices =',I4)
C
 2034 FORMAT(' Y Symmetry: Wall plane   at Ysym =',F10.4)
 2035 FORMAT(' Y Symmetry: Free surface at Ysym =',F10.4)
 2036 FORMAT(' Z Symmetry: Ground plane at Zsym =',F10.4)
 2037 FORMAT(' Z Symmetry: Free surface at Zsym =',F10.4)
C
 204  FORMAT(/2X, 'Sref =',G12.5,3X,'Cref =',G12.5,3X,'Bref =',G12.5
     &       /2X, 'Xref =',G12.5,3X,'Yref =',G12.5,3X,'Zref =',G12.5 )

 205  FORMAT(/1X, A)
 218  FORMAT(/' Run case: ', A)
 220  FORMAT(
     & /2X,'Alpha =',F10.5,5X,'pb/2V =',F10.5,5X,'p''b/2V =',F10.5
     & /2X,'Beta  =',F10.5,5X,'qc/2V =',F10.5
     & /2X,'Mach  =',F10.3,5X,'rb/2V =',F10.5,5X,'r''b/2V =',F10.5)
 222  FORMAT(
     & /2X,'CJtot =',F10.5,5X,'CQtot =',F10.5)
C
 231  FORMAT(3X,A,'=',F10.5)
C
 240  FORMAT (/)
C
      RETURN
      END ! OUTTOT


      SUBROUTINE OUTSURF(LUN)
C
C...PURPOSE  To print out results of the vortex lattice calculation
C            for the input configuration.  
C
C...INPUT    Configuration data for case in labeled commons
C          
C...OUTPUT   Printed output on logical unit LUN
C
      INCLUDE 'AVL.INC'
      CHARACTER*50 SATYPE
      REAL CFN(3), CMN(3), R(3)
C
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
C
      CALL GETSA(LNASA_SA,SATYPE,DIR)
C
      SA = SIN(ALFA)
      CA = COS(ALFA)
C
C========================================================================
C---- Force components from each surface
      WRITE(LUN,200)
 200  FORMAT(1X,
     &'---------------------------------------------------------------')
      WRITE (LUN,210) SATYPE,
     &                SREF,CREF,BREF, 
     &                XYZREF(1), XYZREF(2), XYZREF(3)
      DO IS = 1, NSURF
        DO K = 1, 3
          CFN(K) = CFNI(K,IS) + CFNJ(K,IS) + CFNV(K,IS)
          CMN(K) = CMNI(K,IS) + CMNJ(K,IS) + CMNV(K,IS)
        ENDDO
C
        CDN = CA*CFN(1) + SA*CFN(3)
        CYN =    CFN(2)
        CLN = CA*CFN(3) - SA*CFN(1)
C
        CDNI = CA*CFNI(1,IS) + SA*CFNI(3,IS)
        CDNV = CA*CFNV(1,IS) + SA*CFNV(3,IS)
C
        CALL STRIP(STITLE(IS),NT)
        WRITE (LUN,211) IS,SSURF(IS),
     &      CLN    / SREF,
     &      CDN    / SREF,
     &      CMN(2) / (SREF*CREF),
     &      CYN    / SREF,
     &  DIR*CMN(3) / (SREF*BREF),
     &  DIR*CMN(1) / (SREF*BREF),
     &      CDNI   / SREF,
     &      CDNV   / SREF,
     &      STITLE(IS)(1:NT)
      END DO

cc      WRITE(LUN,212)
  210 FORMAT ( ' Surface Forces (referred to Sref,Cref,Bref',
     &                                   ' about Xref,Yref,Zref)',
     &        /' ',A //
     &      5X,'Sref =',G12.4,   3X,'Cref =',F10.4,3X,'Bref =',F10.4/
     &      5X,'Xref =',2X,F10.4,3X,'Yref =',F10.4,3X,'Zref =',F10.4//
     &     ' n',6X,'Area',6X,'CL',6X,'CD',6X,'Cm',
     &                    6X,'CY',6X,'Cn',6X,'Cl',5X,'CDi',5X,'CDv')
  211 FORMAT (I2,1X,F9.3,8F8.4,3X,A)
  212 FORMAT (/)
C
C
C========================================================================
C--- Surface forces normalized by local reference quantities
      WRITE (LUN,220) 
      DO IS = 1, NSURF
C------ set total surface force and moment
        DO K = 1, 3
          CFN(K) = CFNI(K,IS) + CFNJ(K,IS) + CFNV(K,IS)
          CMN(K) = CMNI(K,IS) + CMNJ(K,IS) + CMNV(K,IS)
        ENDDO
C
C------ rotate forces into stability axes
        CDN = CA*CFN(1) + SA*CFN(3)
        CYN =    CFN(2)
        CLN = CA*CFN(3) - SA*CFN(1)
C
        CDNI = CA*CFNI(1,IS) + SA*CFNI(3,IS)
        CDNV = CA*CFNV(1,IS) + SA*CFNV(3,IS)
C
C------ reference point for surface LE (hinge) moments
C        defined by surface hinge vector direction thru first strip LE point
        IF(IMAGS(IS).GE.0) THEN
         R(1) = -RLE1(1,JFRST(IS))
         R(2) = -RLE1(2,JFRST(IS))
         R(3) = -RLE1(3,JFRST(IS))
        ELSE
         R(1) = -RLE2(1,JFRST(IS))
         R(2) = -RLE2(2,JFRST(IS))
         R(3) = -RLE2(3,JFRST(IS))
        ENDIF
C
C------ set moment about point R(.)
        DO K = 1, 3
          IC = ICRS(K)
          JC = JCRS(K)
          CMN(K) = CMN(K) + R(IC)*CFN(JC) - R(JC)*CFN(IC)
        ENDDO
C
C------ Surface hinge moment defined about hinge vector 
        CMLE = DOT(CMN,ESS(1,IS))
C
        CALL STRIP(STITLE(IS),NT)
        WRITE (LUN,221) IS,
     &            SSURF(IS),CAVESURF(IS),
     &            CLN  / SSURF(IS),
     &            CDN  / SSURF(IS),
     &            CDNV / SSURF(IS),
     &            CMLE / (SSURF(IS)*CAVESURF(IS)),
     &            STITLE(IS)(1:NT)
      END DO
      WRITE(LUN,200)
C
  220 FORMAT (/' Surface Forces (referred to Ssurf, Cave ',
     &         'about root LE on hinge axis)'//
     &        2X,' n',5X,'Ssurf',6X,'Cave',
     &        7X,'cl',7X,'cd',6X,'cdv',4x,'cm_LE')
  221 FORMAT (2X,I2,F10.3,F10.3,4(1X,F8.4),2X,A)
C
      RETURN
      END ! OUTSURF



      SUBROUTINE OUTSTRP(LUN)
C
C...PURPOSE  To print out results of the vortex lattice calculation
C            for the input configuration strip and surface forces.  
C
C...INPUT    Configuration data for case in labeled commons
C          
C...OUTPUT   Printed output on logical unit LUN
C
      INCLUDE 'AVL.INC'
      CHARACTER*50 SATYPE
      REAL CFN(3), CMN(3),
     &     CFS(3), CMS(3),
     &     R(3)
C
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
C
      CALL GETSA(LNASA_SA,SATYPE,DIR)
C
      IF (LUN.EQ.0) RETURN
C
      CA = COS(ALFA)
      SA = SIN(ALFA)
C
C...Print out the results -> Forces by surface and strip
      WRITE(LUN,200)
      WRITE(LUN,210) 
      WRITE(LUN,211) SATYPE
  200 FORMAT(1X,
     &'---------------------------------------------------------------')
  210 FORMAT (' Surface and Strip Forces by surface')
  211 FORMAT (/'  Forces referred to Sref, Cref, Bref ',
     &        'about Xref, Yref, Zref'/
     &         ' ',A)
C
      DO IS = 1, NSURF
        DO K = 1, 3
          CFN(K) = CFNI(K,IS) + CFNJ(K,IS) + CFNV(K,IS)
          CMN(K) = CMNI(K,IS) + CMNJ(K,IS) + CMNV(K,IS)
        ENDDO
C
        CDN = CA*CFN(1) + SA*CFN(3)
        CYN =    CFN(2)
        CLN = CA*CFN(3) - SA*CFN(1)
C
        CDNI = CA*CFNI(1,IS) + SA*CFNI(3,IS)
        CDNV = CA*CFNV(1,IS) + SA*CFNV(3,IS)
C
C
        NS = NJ(IS)
        NV = NK(IS)
        J1 = JFRST(IS)
C
        WRITE (LUN,212) IS,STITLE(IS),NV,NS,J1,SSURF(IS),CAVESURF(IS)
        WRITE (LUN,213)
     &    CLN/SREF, DIR*CMN(1)/(SREF*BREF),
     &    CYN/SREF,     CMN(2)/(SREF*CREF),
     &    CDN/SREF, DIR*CMN(3)/(SREF*BREF),
     &    CDNI/SREF,
     &    CDNV/SREF
C
  212 FORMAT (/2X,'Surface #',I2,5X,A/
     &        5X,'# Chordwise =',I3,3X,'# Spanwise =',I3,
     &        5X,'First strip =',I3/
     &        5X,'Surface area =',F12.6,5X,'  Ave. chord =',F12.6)
  213 FORMAT ( 5X,'CLsurf  =',F10.5,5X,'Clsurf  =',F10.5,
     &        /5X,'CYsurf  =',F10.5,5X,'Cmsurf  =',F10.5,
     &        /5X,'CDsurf  =',F10.5,5X,'Cnsurf  =',F10.5, 
     &        /5X,'CDisurf =',F10.5,5x,'CDvsurf =',F10.5)
C
C
        WRITE (LUN,214) CLN/SREF,CDN/SREF
        WRITE (LUN,216) 
        DO JJ = 1, NS
          J = J1 + JJ-1
          ASTRP = WSTRIP(J)*CHORD(J)
C
          CALL FSTRIP(J,
     &                CAXIAL,CNORML, 
     &                CL_STRP,CD_STRP,
     &                CLJ_STRP,CDJ_STRP,
     &                CLT_STRP,CLA_STRP,
     &                CMC4_STRP,CMLE_STRP,
     &                CNC_STRP )
C
          XCP = 999.
          IF(CL_STRP.NE.0.0) THEN
            XCP = 0.25 - CMC4_STRP/CL_STRP
            XCP = MIN( 99.0, MAX( -99.0 , XCP ) )
          ELSE
            XCP = 0.
          ENDIF
          WRITE (LUN,217)
     &            J,RLE(2,J),CHORD(J),ASTRP,CNCI,DWWAKE(J),
     &            CLT_STRP, CLA_STRP,CD_STRP,CDV_STRP(J),
     &            CMC4_STRP,CMLE_STRP,XCP
        END DO
      END DO
      WRITE(LUN,200)
C
  214 FORMAT (/'  Forces referred to Ssurf, Cave ',
     &         'about hinge axis thru LE'/
     &         5X,'CLsurf  =',F10.5,5X,'CDsurf  =',F10.5/
     &         5X,'Deflect =',F10.5,5X,'CmLEsurf=',F10.5)
C
  216 FORMAT (/' Strip Forces referred to Strip Area, Chord'/
     &        2X,'  j ',5X,'Yle',4X,'Chord',5X,'Area',
     &        5X,'c cl',6X,'ai',6X,'cl_norm',2X,'cl',7X,'cd',7X,
     &        'cdv',4x,'cm_c/4',4x,'cm_LE',2x,'C.P.x/c')
  217 FORMAT (2X,I4,11(1X,F8.4),1X,F8.3)
C
      RETURN
      END ! OUTSTRP


      SUBROUTINE OUTELE(LUN)
      INCLUDE 'AVL.INC'
      CHARACTER*50 SATYPE
      REAL CFN(3), CMN(3),
     &     CFS(3), CMS(3),
     &     R(3)
C
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
C
      CALL GETSA(LNASA_SA,SATYPE,DIR)
C
      CA = COS(ALFA)
      SA = SIN(ALFA)
C
C...Forces on each strip and element (long output, and slow to printout)
      WRITE(LUN,200)
      WRITE(LUN,202)
      WRITE(LUN,205) SATYPE
C
  200 FORMAT(1X,
     &'---------------------------------------------------------------')
  202 FORMAT (' Vortex Strengths (by surface, by strip)')
  205 FORMAT (/'  Forces referred to Sref, Cref, Bref ',
     &        'about Xref, Yref, Zref'/
     &         '  ',A)
C
      DO IS = 1, NSURF
        DO K = 1, 3
          CFN(K) = CFNI(K,IS) + CFNJ(K,IS) + CFNV(K,IS)
          CMN(K) = CMNI(K,IS) + CMNJ(K,IS) + CMNV(K,IS)
        ENDDO
C
        CDN = CA*CFN(1) + SA*CFN(3)
        CYN =    CFN(2)
        CLN = CA*CFN(3) - SA*CFN(1)
C
        CDNI = CA*CFNI(1,IS) + SA*CFNI(3,IS)
        CDNV = CA*CFNV(1,IS) + SA*CFNV(3,IS)
C
C
        NS = NJ(IS)
        NV = NK(IS)
        J1 = JFRST(IS)
C
        WRITE (LUN,212) IS,STITLE(IS),NV,NS,J1,
     &                  SSURF(IS),CAVESURF(IS)
  212   FORMAT (/1X,78('*')/2X,'Surface #',I2,5X,A/
     &        5X,'# Chordwise  =',I3,3X,'# Spanwise =',I3,
     &        3X,'First strip  =',I3/
     &        5X,'Surface area =',F12.6,5X,'  Ave. chord =',F12.6)
C
        WRITE (LUN,213)
     &    CLN/SREF, DIR*CMN(1)/(SREF*BREF),
     &    CYN/SREF,     CMN(2)/(SREF*CREF),
     &    CDN/SREF, DIR*CMN(3)/(SREF*BREF),
     &    CDNI/SREF,
     &    CDNV/SREF
  213   FORMAT ( 5X,'CLsurf  =',F10.5,5X,'Clsurf  =',F10.5,
     &          /5X,'CYsurf  =',F10.5,5X,'Cmsurf  =',F10.5,
     &          /5X,'CDsurf  =',F10.5,5X,'Cnsurf  =',F10.5, 
     &          /5X,'CDisurf =',F10.5,5x,'CDvsurf =',F10.5)
C
        WRITE (LUN,214) CLN/SREF, CDN/SREF
  214   FORMAT (/'  Forces referred to Ssurf, Cave ',
     &         'about hinge axis thru LE'/
     &         5X,'CLsurf  =',F10.5,5X,'CDsurf  =',F10.5/
     &         1X,78('*'))
C
        DO JJ = 1, NS
          J = J1 + JJ-1
          I1 = IJFRST(J)
          ASTRP = WSTRIP(J)*CHORD(J)
          DIHED = -ATAN2(ENSY(J),ENSZ(J))/DTR
          WRITE (LUN,232) J,NV,I1,
     &                    RLE(1,J),CHORD(J),AINC(J)/DTR,
     &                    RLE(2,J),WSTRIP(J),ASTRP,
     &                    RLE(3,J),DIHED
C
          CALL FSTRIP(J,
     &                CAXIAL,CNORML, 
     &                CL_STRP,CD_STRP,
     &                CLJ_STRP,CDJ_STRP,
     &                CLT_STRP,CLA_STRP,
     &                CMC4_STRP,CMLE_STRP,
     &                CNC_STRP )
          WRITE (LUN,233) CL_STRP, CD_STRP, CDV_STRP(J),
     &                    CNORML , CAXIAL, 
     &                    CNCI,    DWWAKE(J),
     &                    CMLE_STRP, CMC4_STRP
          DO II = 1, NV
            I = I1 + (II-1)
            XM = 0.5*(RV1(1,I)+RV2(1,I))
            YM = 0.5*(RV1(2,I)+RV2(2,I))
            ZM = 0.5*(RV1(3,I)+RV2(3,I))
            WRITE (LUN,234) I,XM,YM,ZM,DXV(I),SLOPEC(I),DCP(I)
          END DO
        END DO
      END DO
      WRITE(LUN,200)
C
      RETURN
C
  232 FORMAT (/1X,'Strip #',I3,5X,'# Chordwise =',I3,
     &         3X,'First Vortex =',I3/
     &         4X,'Xle =',F10.5,4X,'Ave. Chord   =',F10.4,
     &         3X,'Incidence  =',F10.4,' deg'/
     &         4X,'Yle =',F10.5,4X,'Strip Width  =',F10.5,
     &         3X,'Strip Area =',F12.6/
     &         4X,'Zle =',F10.5,4X,'Strip Dihed. =',F10.4)
  233 FORMAT (/4X,'cl  =',F10.5,4X,'   cd  =',F10.5,4X,'  cdv =',F10.5,
     &        /4X,'cn  =',F10.5,4X,'   ca  =',F10.5,
     &         4X,'  cnc =',F10.5,4X,'wake dnwsh =',F10.5,
     &        /4X,'cmLE=',F10.5,4X,'cm c/4 =',F10.5,
     &       //4X,'I',8X,'X   ',8X,'Y   ',8X,'Z   ',8X,'DX  ',
     &         6X,'Slope',8X,'dCp')
  234 FORMAT (2X,I3,6(2X,F10.5))
C
      END ! OUTELE



      SUBROUTINE OUTHINGE(LUN)
C
C...PURPOSE  To print out results of the vortex lattice calculation
C            for the input configuration.  
C
C...INPUT    Configuration data for case in labeled commons
C          
C...OUTPUT   Printed output on logical unit LUN
C
      INCLUDE 'AVL.INC'
C
C...Hinge moments for each CONTROL
      WRITE(LUN,200)
 200  FORMAT(1X,
     &'---------------------------------------------------------------')
C
      WRITE (LUN,210) SREF,CREF
  210 FORMAT (
     & ' Control Hinge Moments' /
     & ' (referred to    Sref =',G12.4,   3X,'Cref =',F10.4,')' )
C
      WRITE (LUN,212) 
 212  FORMAT(/' Control          Chinge'
     &       /' ---------------- -----------')
C
      DO N = 1, NCONTROL
        WRITE (LUN,220) DNAME(N),CHINGE(N)
  220   FORMAT (1X,A16,G12.4)
      END DO
C
      WRITE(LUN,200)
C
      RETURN
      END ! OUTHINGE



      SUBROUTINE OUTCNC
C
C...PURPOSE  To write out a CNC loading file
C            for the input configuration strips
C
C...INPUT    Configuration data for case in labeled commons
C          
C...OUTPUT   Printed output on logical unit LUN
C
      INCLUDE 'AVL.INC'
      CHARACTER*1 ANS
      CHARACTER*80 FNAM
      SAVE FNAM
      DATA FNAM /' '/
C
 1000 FORMAT (A)
C
      CALL STRIP(FNAM,NFN)
   10 WRITE(*,2080) FNAM(1:NFN)
 2080 FORMAT('Enter forces output file: ', A)
      READ (*,1000) FNAM
C
      IF(FNAM.NE.' ') THEN
        OPEN(18,FILE=FNAM,STATUS='UNKNOWN',ERR=10)
        REWIND(18)
       ELSE
C-------- just a <return> was entered...
        RETURN
      ENDIF
C
      WRITE(*,2090) 
 2090 FORMAT('Output file Simple(y,cnc,cl) or Full(x,y,z,cnc,cl,c)? ',$)
      READ (*,1000) ANS
      CALL TOUPER(ANS)
C
C...Print out the results -> strip loadings
c     WRITE (18,210) 
      DO J=1, NSTRIP
        CALL FSTRIP(J,
     &                CAXIAL,CNORML, 
     &                CL_STRP,CD_STRP,
     &                CLJ_STRP,CDJ_STRP,
     &                CLT_STRP,CLA_STRP,
     &                CMC4_STRP,CMLE_STRP,
     &                CNC_STRP )
        I = IJFRST(J)
        XM = 0.5*(RV1(1,I)+RV2(1,I))
        YM = 0.5*(RV1(2,I)+RV2(2,I))
        ZM = 0.5*(RV1(3,I)+RV2(3,I))
        CNCM = CNC_STRP
        CLM  = CL_STRP
        CHM  = CHORD(J)
        DYM  = WSTRIP(J)
        ASM  = DYM*CHM
        IF(ANS.EQ.'S') THEN
          WRITE (18,213) YM,CNCM,CLM,CHM
         ELSE
          WRITE (18,212) XM,YM,ZM,CNCM,CLM,CHM,DYM,ASM
        ENDIF
      END DO
      CLOSE(18)
C
  210 FORMAT (//' *** Strip Loadings')
  212 FORMAT (3(F8.3,1X),2(F10.4,1X),2(F8.4,1X),F9.4)
  213 FORMAT (F8.3,1X,3(F10.4,1X))
C
      RETURN
      END ! OUTCNC



      SUBROUTINE DERMATM(LU)
C---------------------------------------------------------
C     Calculates and outputs stability derivative matrix
C     for current ALFA, BETA.
C---------------------------------------------------------
      INCLUDE 'AVL.INC'
      CHARACTER*50 SATYPE
      REAL WROT_RX(3), WROT_RZ(3), WROT_A(3)
C
      CALL GETSA(LNASA_SA,SATYPE,DIR)
C
C---- set freestream velocity components from alpha, beta
      CALL VINFAB
C
C---- calculate forces and sensitivities
      CALL AERO
C
C---- set stability-axes rates (RX,RY,RZ) in terms of body-axes rates
      CA = COS(ALFA)
      SA = SIN(ALFA)
      RX = (WROT(1)*CA + WROT(3)*SA) * DIR
      RY =  WROT(2)
      RZ = (WROT(3)*CA - WROT(1)*SA) * DIR
C
C---- now vice-versa, and set sensitivities (which is what's really needed)
cc    WROT(1)    =  RX*CA - RZ*SA
cc    WROT(2)    =  RY
cc    WROT(3)    =  RZ*CA + RX*SA
C
      WROT_RX(1) = CA     * DIR
      WROT_RX(2) = 0.
      WROT_RX(3) =     SA * DIR
C
      WROT_RZ(1) =    -SA * DIR
      WROT_RZ(2) = 0.
      WROT_RZ(3) = CA     * DIR
C
      WROT_A(1)  = -RX*SA - RZ*CA   !!! = -WROT(3)
      WROT_A(2)  =  0.
      WROT_A(3)  = -RZ*SA + RX*CA   !!! =  WROT(1)
C
C
C---- set force derivatives in stability axes
      CL_AL = CLT_U(1)*VINF_A(1) + CLT_U(4)*WROT_A(1)
     &      + CLT_U(2)*VINF_A(2) + CLT_U(5)*WROT_A(2)
     &      + CLT_U(3)*VINF_A(3) + CLT_U(6)*WROT_A(3) + CLT_A
      CL_BE = CLT_U(1)*VINF_B(1)
     &      + CLT_U(2)*VINF_B(2)
     &      + CLT_U(3)*VINF_B(3)
      CL_RX = CLT_U(4)*WROT_RX(1) + CLT_U(6)*WROT_RX(3)
      CL_RY = CLT_U(5)
      CL_RZ = CLT_U(6)*WROT_RZ(3) + CLT_U(4)*WROT_RZ(1)
C
      CY_AL = CYT_U(1)*VINF_A(1) + CYT_U(4)*WROT_A(1)
     &      + CYT_U(2)*VINF_A(2) + CYT_U(5)*WROT_A(2)
     &      + CYT_U(3)*VINF_A(3) + CYT_U(6)*WROT_A(3)
      CY_BE = CYT_U(1)*VINF_B(1)
     &      + CYT_U(2)*VINF_B(2)
     &      + CYT_U(3)*VINF_B(3)
      CY_RX = CYT_U(4)*WROT_RX(1) + CYT_U(6)*WROT_RX(3)
      CY_RY = CYT_U(5)
      CY_RZ = CYT_U(6)*WROT_RZ(3) + CYT_U(4)*WROT_RZ(1)
C
      CR_AL = CMT_U(1,1)*VINF_A(1) + CMT_U(1,4)*WROT_A(1)
     &      + CMT_U(1,2)*VINF_A(2) + CMT_U(1,5)*WROT_A(2)
     &      + CMT_U(1,3)*VINF_A(3) + CMT_U(1,6)*WROT_A(3)
      CR_BE = CMT_U(1,1)*VINF_B(1)
     &      + CMT_U(1,2)*VINF_B(2)
     &      + CMT_U(1,3)*VINF_B(3)
      CR_RX = CMT_U(1,4)*WROT_RX(1) + CMT_U(1,6)*WROT_RX(3)
      CR_RY = CMT_U(1,5)
      CR_RZ = CMT_U(1,6)*WROT_RZ(3) + CMT_U(1,4)*WROT_RZ(1)
C
      CM_AL = CMT_U(2,1)*VINF_A(1) + CMT_U(2,4)*WROT_A(1)
     &      + CMT_U(2,2)*VINF_A(2) + CMT_U(2,5)*WROT_A(2)
     &      + CMT_U(2,3)*VINF_A(3) + CMT_U(2,6)*WROT_A(3)
      CM_BE = CMT_U(2,1)*VINF_B(1)
     &      + CMT_U(2,2)*VINF_B(2)
     &      + CMT_U(2,3)*VINF_B(3)
      CM_RX = CMT_U(2,4)*WROT_RX(1) + CMT_U(2,6)*WROT_RX(3)
      CM_RY = CMT_U(2,5)
      CM_RZ = CMT_U(2,6)*WROT_RZ(3) + CMT_U(2,4)*WROT_RZ(1)
C
      CN_AL = CMT_U(3,1)*VINF_A(1) + CMT_U(3,4)*WROT_A(1)
     &      + CMT_U(3,2)*VINF_A(2) + CMT_U(3,5)*WROT_A(2)
     &      + CMT_U(3,3)*VINF_A(3) + CMT_U(3,6)*WROT_A(3)
      CN_BE = CMT_U(3,1)*VINF_B(1)
     &      + CMT_U(3,2)*VINF_B(2)
     &      + CMT_U(3,3)*VINF_B(3)
      CN_RX = CMT_U(3,4)*WROT_RX(1) + CMT_U(3,6)*WROT_RX(3)
      CN_RY = CMT_U(3,5)
      CN_RZ = CMT_U(3,6)*WROT_RZ(3) + CMT_U(3,4)*WROT_RZ(1)
C
C
      CALL OUTTOT(LU)
C
      WRITE(LU,7004)
 7004 FORMAT(/' Derivatives...')
C
      WRITE(LU,7006)
 7006 FORMAT(14X, 4X,'           alpha',
     &            4X,'            beta'
     &      /14X, 4X,'----------------',
     &            4X,'----------------')
C
      WRITE(LU,7010) CL_AL, CL_BE
 7010 FORMAT(' z force     |','    CLa =',F11.6,'    CLb =',F11.6)
C
      WRITE(LU,7020) CY_AL, CY_BE
 7020 FORMAT(' y force     |','    CYa =',F11.6,'    CYb =',F11.6)
C
      WRITE(LU,7040) DIR*CR_AL, DIR*CR_BE
 7040 FORMAT(' roll  x mom.|','    Cla =',F11.6,'    Clb =',F11.6)
C
      WRITE(LU,7050) CM_AL, CM_BE
 7050 FORMAT(' pitch y mom.|','    Cma =',F11.6,'    Cmb =',F11.6)
C
      WRITE(LU,7060) DIR*CN_AL, DIR*CN_BE
 7060 FORMAT(' yaw   z mom.|','    Cna =',F11.6,'    Cnb =',F11.6)
C
C
      WRITE(LU,7106)
 7106 FORMAT(/14X, 4X,'    roll rate  p',
     &             4X,'   pitch rate  q',
     &             4X,'     yaw rate  r'
     &       /14X, 4X,'----------------',
     &             4X,'----------------',
     &             4X,'----------------' )
C
      WRITE(LU,7110) CL_RX*2.0/BREF, 
     &               CL_RY*2.0/CREF, 
     &               CL_RZ*2.0/BREF
 7110 FORMAT(' z force     |','    CLp =',F11.6,
     &                        '    CLq =',F11.6,
     &                        '    CLr =',F11.6 )
C
      WRITE(LU,7120) CY_RX*2.0/BREF,
     &               CY_RY*2.0/CREF,
     &               CY_RZ*2.0/BREF
 7120 FORMAT(' y force     |','    CYp =',F11.6,
     &                        '    CYq =',F11.6,
     &                        '    CYr =',F11.6 )
C
      WRITE(LU,7140) DIR*CR_RX*2.0/BREF, 
     &               DIR*CR_RY*2.0/CREF,
     &               DIR*CR_RZ*2.0/BREF
 7140 FORMAT(' roll  x mom.|','    Clp =',F11.6,
     &                        '    Clq =',F11.6,
     &                        '    Clr =',F11.6 )
C
      WRITE(LU,7150) CM_RX*2.0/BREF,
     &               CM_RY*2.0/CREF,
     &               CM_RZ*2.0/BREF
 7150 FORMAT(' pitch y mom.|','    Cmp =',F11.6,
     &                        '    Cmq =',F11.6,
     &                        '    Cmr =',F11.6 )
C
      WRITE(LU,7160) DIR*CN_RX*2.0/BREF,
     &               DIR*CN_RY*2.0/CREF,
     &               DIR*CN_RZ*2.0/BREF
 7160 FORMAT(' yaw   z mom.|','    Cnp =',F11.6,
     &                        '    Cnq =',F11.6,
     &                        '    Cnr =',F11.6 )
C
      IF(NCONTROL.GT.0) THEN
C
      WRITE(LU,8106) (DNAME(K), K, K=1, NCONTROL)
 8106 FORMAT(/14X,20(4X,A12, ' d',I1,' '))
      WRITE(LU,8107) (' ',K=1, NCONTROL)
 8107 FORMAT( 14X,20(3X,A,'----------------'))
C
      WRITE(LU,8110) (' ',K,CLT_D(K)/SREF, K=1, NCONTROL)
 8110 FORMAT(' z force     |',20(A,'  CLd',I1,' =',F11.6))
C
      WRITE(LU,8120) (' ',K,CYT_D(K)/SREF, K=1, NCONTROL)
 8120 FORMAT(' y force     |',20(A,'  CYd',I1,' =',F11.6))
C
      WRITE(LU,8140) (' ',K,DIR*CMT_D(1,K)/(SREF*BREF), K=1, NCONTROL)
 8140 FORMAT(' roll  x mom.|',20(A,'  Cld',I1,' =',F11.6))
C
      WRITE(LU,8150) (' ',K,    CMT_D(2,K)/(SREF*CREF), K=1, NCONTROL)
 8150 FORMAT(' pitch y mom.|',20(A,'  Cmd',I1,' =',F11.6))
C
      WRITE(LU,8160) (' ',K,DIR*CMT_D(3,K)/(SREF*BREF), K=1, NCONTROL)
 8160 FORMAT(' yaw   z mom.|',20(A,'  Cnd',I1,' =',F11.6))
C
      WRITE(LU,8170) (' ',K,    CDFF_D(K)/SREF, K=1, NCONTROL)
 8170 FORMAT(' Trefftz drag|',20(A,'CDffd',I1,' =',F11.6))
C
      WRITE(LU,8180) (' ',K,  SPANEF_D(K), K=1, NCONTROL)
 8180 FORMAT(' span eff.   |',20(A,'   ed',I1,' =',F11.6))
C
      WRITE(LU,*)
      WRITE(LU,*)
C
      ENDIF
C
      IF(NDESIGN.GT.0) THEN
C
      WRITE(LU,8206) (GNAME(K), K, K=1, NDESIGN)
 8206 FORMAT(/14X,20(4X,A12, ' g',I1,' '))
      WRITE(LU,8207) (' ',K=1, NDESIGN)
 8207 FORMAT( 14X,20(3X,A,'----------------'))
C
      WRITE(LU,8210) (' ',K,CLT_G(K)/SREF, K=1, NDESIGN)
 8210 FORMAT(' z force     |',20(A,'  CLg',I1,' =',F11.6))
C
      WRITE(LU,8220) (' ',K,CYT_G(K)/SREF, K=1, NDESIGN)
 8220 FORMAT(' y force     |',20(A,'  CYg',I1,' =',F11.6))
C
      WRITE(LU,8230) (' ',K,DIR*CMT_G(1,K)/(SREF*BREF), K=1, NDESIGN)
 8230 FORMAT(' roll  x mom.|',20(A,'  Clg',I1,' =',F11.6))
C
      WRITE(LU,8240) (' ',K,    CMT_G(2,K)/(SREF*CREF), K=1, NDESIGN)
 8240 FORMAT(' pitch y mom.|',20(A,'  Cmg',I1,' =',F11.6))
C
      WRITE(LU,8250) (' ',K,DIR*CMT_G(3,K)/(SREF*BREF), K=1, NDESIGN)
 8250 FORMAT(' yaw   z mom.|',20(A,'  Cng',I1,' =',F11.6))
C
      WRITE(LU,8260) (' ',K,    CDFF_G(K)/SREF, K=1, NDESIGN)
 8260 FORMAT(' Trefftz drag|',20(A,'CDffg',I1,' =',F11.6))
C
      WRITE(LU,8270) (' ',K,  SPANEF_G(K), K=1, NDESIGN)
 8270 FORMAT(' span eff.   |',20(A,'   eg',I1,' =',F11.6))
C
      WRITE(LU,*)
      WRITE(LU,*)
C
      ENDIF
C
      IF(CL_AL .NE. 0.0) THEN
       XNP = XYZREF(1) - CM_AL/CL_AL
       WRITE(LU,8401) XNP
 8401  FORMAT(/' Neutral point  Xnp =', F11.6)
      ENDIF
C
      IF(ABS(CR_RZ*CN_BE*(2.0/BREF)/(SREF*BREF)**2) .GT. 0.0001) THEN
       BB = CR_BE*CN_RZ / (CR_RZ*CN_BE)
       WRITE(LU,8402) BB 
 8402  FORMAT(/' Clb Cnr / Clr Cnb  =', F11.6,
     &    '    (  > 1 if spirally stable )')
      ENDIF

C
      RETURN
      END ! DERMATM



      SUBROUTINE DERMATS(LU)
C---------------------------------------------------------
C     Calculates and outputs stability derivative matrix
C     for current ALFA, BETA.
C---------------------------------------------------------
      INCLUDE 'AVL.INC'
      CHARACTER*50 SATYPE
      REAL WROT_RX(3), WROT_RZ(3), WROT_A(3)
      REAL CMSAX(3),
     &     CMSAX_A(3),
     &     CMSAX_U(3,NUMAX),
     &     CMSAX_D(3,NDMAX),
     &     CMSAX_G(3,NGMAX)
C
      CALL GETSA(LNASA_SA,SATYPE,DIR)
C
C---- set freestream velocity components from alpha, beta
      CALL VINFAB
C
C---- calculate forces and sensitivities
      CALL AERO
C
C---- set stability-axes rates (RX,RY,RZ) in terms of body-axes rates
      CA = COS(ALFA)
      SA = SIN(ALFA)
C
      RX = (WROT(1)*CA + WROT(3)*SA) * DIR
      RY =  WROT(2)
      RZ = (WROT(3)*CA - WROT(1)*SA) * DIR
C
C---- now vice-versa, and set sensitivities (which is what's really needed)
cc    WROT(1)    =  RX*CA - RZ*SA
cc    WROT(2)    =  RY
cc    WROT(3)    =  RZ*CA + RX*SA
C
      WROT_RX(1) = CA     * DIR
      WROT_RX(2) = 0.
      WROT_RX(3) =     SA * DIR
C
      WROT_RZ(1) =    -SA * DIR
      WROT_RZ(2) = 0.
      WROT_RZ(3) = CA     * DIR
C
      WROT_A(1)  = -RX*SA - RZ*CA   !!! = -WROT(3)
      WROT_A(2)  =  0.
      WROT_A(3)  = -RZ*SA + RX*CA   !!! =  WROT(1)
C
C
      CMSAX(1) = CMT(1)*CA + CMT(3)*SA
      CMSAX(2) = CMT(2)              
      CMSAX(3) = CMT(3)*CA - CMT(1)*SA  
      CMSAX_A(1) = -CMT(1)*SA + CMT(3)*CA
      CMSAX_A(2) = 0.
      CMSAX_A(3) = -CMT(3)*SA - CMT(1)*CA
C
      DO K = 1, 6
        CMSAX_U(1,K) = CMT_U(1,K)*CA + CMT_U(3,K)*SA
        CMSAX_U(2,K) = CMT_U(2,K)              
        CMSAX_U(3,K) = CMT_U(3,K)*CA - CMT_U(1,K)*SA  
      ENDDO
      DO K = 1, NCONTROL
        CMSAX_D(1,K) = CMT_D(1,K)*CA + CMT_D(3,K)*SA
        CMSAX_D(2,K) = CMT_D(2,K)              
        CMSAX_D(3,K) = CMT_D(3,K)*CA - CMT_D(1,K)*SA  
      ENDDO
      DO K = 1, NDESIGN
        CMSAX_G(1,K) = CMT_G(1,K)*CA + CMT_G(3,K)*SA
        CMSAX_G(2,K) = CMT_G(2,K)              
        CMSAX_G(3,K) = CMT_G(3,K)*CA - CMT_G(1,K)*SA  
      ENDDO

C
C---- set force derivatives in stability axes
      CL_AL = CLT_U(1)*VINF_A(1) + CLT_U(4)*WROT_A(1)
     &      + CLT_U(2)*VINF_A(2) + CLT_U(5)*WROT_A(2)
     &      + CLT_U(3)*VINF_A(3) + CLT_U(6)*WROT_A(3) + CLT_A
      CL_BE = CLT_U(1)*VINF_B(1)
     &      + CLT_U(2)*VINF_B(2)
     &      + CLT_U(3)*VINF_B(3)
      CL_RX = CLT_U(4)*WROT_RX(1) + CLT_U(6)*WROT_RX(3)
      CL_RY = CLT_U(5)
      CL_RZ = CLT_U(6)*WROT_RZ(3) + CLT_U(4)*WROT_RZ(1)
C
      CY_AL = CYT_U(1)*VINF_A(1) + CYT_U(4)*WROT_A(1)
     &      + CYT_U(2)*VINF_A(2) + CYT_U(5)*WROT_A(2)
     &      + CYT_U(3)*VINF_A(3) + CYT_U(6)*WROT_A(3)
      CY_BE = CYT_U(1)*VINF_B(1)
     &      + CYT_U(2)*VINF_B(2)
     &      + CYT_U(3)*VINF_B(3)
      CY_RX = CYT_U(4)*WROT_RX(1) + CYT_U(6)*WROT_RX(3)
      CY_RY = CYT_U(5)
      CY_RZ = CYT_U(6)*WROT_RZ(3) + CYT_U(4)*WROT_RZ(1)
C
      CR_AL = CMSAX_U(1,1)*VINF_A(1) + CMSAX_U(1,4)*WROT_A(1)
     &      + CMSAX_U(1,2)*VINF_A(2) + CMSAX_U(1,5)*WROT_A(2)
     &      + CMSAX_U(1,3)*VINF_A(3) + CMSAX_U(1,6)*WROT_A(3)
     &      + CMSAX_A(1)
      CR_BE = CMSAX_U(1,1)*VINF_B(1)
     &      + CMSAX_U(1,2)*VINF_B(2)
     &      + CMSAX_U(1,3)*VINF_B(3)
      CR_RX = CMSAX_U(1,4)*WROT_RX(1) + CMSAX_U(1,6)*WROT_RX(3)
      CR_RY = CMSAX_U(1,5)
      CR_RZ = CMSAX_U(1,6)*WROT_RZ(3) + CMSAX_U(1,4)*WROT_RZ(1)
C
      CM_AL = CMSAX_U(2,1)*VINF_A(1) + CMSAX_U(2,4)*WROT_A(1)
     &      + CMSAX_U(2,2)*VINF_A(2) + CMSAX_U(2,5)*WROT_A(2)
     &      + CMSAX_U(2,3)*VINF_A(3) + CMSAX_U(2,6)*WROT_A(3)
     &      + CMSAX_A(2)
      CM_BE = CMSAX_U(2,1)*VINF_B(1)
     &      + CMSAX_U(2,2)*VINF_B(2)
     &      + CMSAX_U(2,3)*VINF_B(3)
      CM_RX = CMSAX_U(2,4)*WROT_RX(1) + CMSAX_U(2,6)*WROT_RX(3)
      CM_RY = CMSAX_U(2,5)
      CM_RZ = CMSAX_U(2,6)*WROT_RZ(3) + CMSAX_U(2,4)*WROT_RZ(1)
C
      CN_AL = CMSAX_U(3,1)*VINF_A(1) + CMSAX_U(3,4)*WROT_A(1)
     &      + CMSAX_U(3,2)*VINF_A(2) + CMSAX_U(3,5)*WROT_A(2)
     &      + CMSAX_U(3,3)*VINF_A(3) + CMSAX_U(3,6)*WROT_A(3)
     &      + CMSAX_A(3)
      CN_BE = CMSAX_U(3,1)*VINF_B(1)
     &      + CMSAX_U(3,2)*VINF_B(2)
     &      + CMSAX_U(3,3)*VINF_B(3)
      CN_RX = CMSAX_U(3,4)*WROT_RX(1) + CMSAX_U(3,6)*WROT_RX(3)
      CN_RY = CMSAX_U(3,5)
      CN_RZ = CMSAX_U(3,6)*WROT_RZ(3) + CMSAX_U(3,4)*WROT_RZ(1)
C
C
      CALL OUTTOT(LU)
C
      WRITE(LU,7004)
 7004 FORMAT(/' Stability-axis derivatives...')
C
      WRITE(LU,7006)
 7006 FORMAT(/14X, 4X,'           alpha',
     &             4X,'            beta'
     &       /14X, 4X,'----------------',
     &             4X,'----------------')
C
      WRITE(LU,7010) CL_AL/SREF, CL_BE/SREF
 7010 FORMAT(' z'' force CL |' ,'    CLa =',F11.6,'    CLb =',F11.6)
C
      WRITE(LU,7020) CY_AL/SREF, CY_BE/SREF
 7020 FORMAT(' y  force CY |'  ,'    CYa =',F11.6,'    CYb =',F11.6)
C
      WRITE(LU,7040) DIR*CR_AL/(SREF*BREF), DIR*CR_BE/(SREF*BREF)
 7040 FORMAT(' x'' mom.  Cl''|','    Cla =',F11.6,'    Clb =',F11.6)
C
      WRITE(LU,7050)     CM_AL/(SREF*CREF),     CM_BE/(SREF*CREF)
 7050 FORMAT(' y  mom.  Cm |'  ,'    Cma =',F11.6,'    Cmb =',F11.6)
C
      WRITE(LU,7060) DIR*CN_AL/(SREF*BREF), DIR*CN_BE/(SREF*BREF)
 7060 FORMAT(' z'' mom.  Cn''|','    Cna =',F11.6,'    Cnb =',F11.6)
C
C
      WRITE(LU,7106)
 7106 FORMAT(/14X, 4X,'   roll rate  p''',
     &             4X,'  pitch rate  q''',
     &             4X,'    yaw rate  r'''
     &       /14X, 4X,'----------------',
     &             4X,'----------------',
     &             4X,'----------------' )
C
      WRITE(LU,7110) CL_RX*(2.0/BREF)/SREF, 
     &               CL_RY*(2.0/CREF)/SREF, 
     &               CL_RZ*(2.0/BREF)/SREF
 7110 FORMAT(' z'' force CL |','    CLp =',F11.6,
     &                         '    CLq =',F11.6,
     &                         '    CLr =',F11.6 )
C
      WRITE(LU,7120) CY_RX*(2.0/BREF)/SREF,
     &               CY_RY*(2.0/CREF)/SREF,
     &               CY_RZ*(2.0/BREF)/SREF
 7120 FORMAT(' y  force CY |','    CYp =',F11.6,
     &                        '    CYq =',F11.6,
     &                        '    CYr =',F11.6 )
C
      WRITE(LU,7140) DIR*CR_RX*(2.0/BREF)/(SREF*BREF), 
     &               DIR*CR_RY*(2.0/CREF)/(SREF*BREF), 
     &               DIR*CR_RZ*(2.0/BREF)/(SREF*BREF)
 7140 FORMAT(' x'' mom.  Cl''|','    Clp =',F11.6,
     &                        '    Clq =',F11.6,
     &                        '    Clr =',F11.6 )
C
      WRITE(LU,7150)     CM_RX*(2.0/BREF)/(SREF*CREF), 
     &                   CM_RY*(2.0/CREF)/(SREF*CREF), 
     &                   CM_RZ*(2.0/BREF)/(SREF*CREF)
 7150 FORMAT(' y  mom.  Cm |','    Cmp =',F11.6,
     &                        '    Cmq =',F11.6,
     &                        '    Cmr =',F11.6 )
C
      WRITE(LU,7160) DIR*CN_RX*(2.0/BREF)/(SREF*BREF),
     &               DIR*CN_RY*(2.0/CREF)/(SREF*BREF), 
     &               DIR*CN_RZ*(2.0/BREF)/(SREF*BREF)
 7160 FORMAT(' z'' mom.  Cn''|','    Cnp =',F11.6,
     &                        '    Cnq =',F11.6,
     &                        '    Cnr =',F11.6 )
C
      IF(NCONTROL.GT.0) THEN
C
      WRITE(LU,8106) (DNAME(K), K, K=1, NCONTROL)
 8106 FORMAT(/14X,20(4X,A12, ' d',I1,' '))
      WRITE(LU,8107) (' ',K=1, NCONTROL)
 8107 FORMAT( 14X,20(3X,A,'----------------'))
C
      WRITE(LU,8110) (' ',K,CLT_D(K)/SREF, K=1, NCONTROL)
 8110 FORMAT(' z'' force CL |' ,20(A,'  CLd',I1,' =',F11.6))
C
      WRITE(LU,8120) (' ',K,CYT_D(K)/SREF, K=1, NCONTROL)
 8120 FORMAT(' y  force CY |'  ,20(A,'  CYd',I1,' =',F11.6))
C
      WRITE(LU,8140) (' ',K,DIR*CMT_D(1,K)/(SREF*BREF), K=1, NCONTROL)
 8140 FORMAT(' x'' mom.  Cl''|',20(A,'  Cld',I1,' =',F11.6))
C
      WRITE(LU,8150) (' ',K,    CMT_D(2,K)/(SREF*CREF), K=1, NCONTROL)
 8150 FORMAT(' y  mom.  Cm |'  ,20(A,'  Cmd',I1,' =',F11.6))
C
      WRITE(LU,8160) (' ',K,DIR*CMT_D(3,K)/(SREF*BREF), K=1, NCONTROL)
 8160 FORMAT(' z'' mom.  Cn''|',20(A,'  Cnd',I1,' =',F11.6))
C
      WRITE(LU,8170) (' ',K,    CDFF_D(K)/SREF, K=1, NCONTROL)
 8170 FORMAT(' Trefftz drag|'  ,20(A,'CDffd',I1,' =',F11.6))
C
      WRITE(LU,8180) (' ',K,  SPANEF_D(K), K=1, NCONTROL)
 8180 FORMAT(' span eff.   |'  ,20(A,'   ed',I1,' =',F11.6))
C
      WRITE(LU,*)
      WRITE(LU,*)
C
      ENDIF
C
      IF(NDESIGN.GT.0) THEN
C
      WRITE(LU,8206) (GNAME(K), K, K=1, NDESIGN)
 8206 FORMAT(/14X,20(4X,A12, ' g',I1,' '))
      WRITE(LU,8207) (' ',K=1, NDESIGN)
 8207 FORMAT( 14X,20(3X,A,'----------------'))
C
      WRITE(LU,8210) (' ',K,CLT_G(K)/SREF, K=1, NDESIGN)
 8210 FORMAT(' z'' force CL |'  ,20(A,'  CLg',I1,' =',F11.6))
C
      WRITE(LU,8220) (' ',K,CYT_G(K)/SREF, K=1, NDESIGN)
 8220 FORMAT(' y  force CY |'   ,20(A,'  CYg',I1,' =',F11.6))
C
      WRITE(LU,8230) (' ',K,DIR*CMT_G(1,K)/(SREF*BREF), K=1, NDESIGN)
 8230 FORMAT(' x'' mom.  Cl''|' ,20(A,'  Clg',I1,' =',F11.6))
C
      WRITE(LU,8240) (' ',K,    CMT_G(2,K)/(SREF*CREF), K=1, NDESIGN)
 8240 FORMAT(' y  mom.  Cm |'    ,20(A,'  Cmg',I1,' =',F11.6))
C
      WRITE(LU,8250) (' ',K,DIR*CMT_G(3,K)/(SREF*BREF), K=1, NDESIGN)
 8250 FORMAT(' z'' mom.  Cn''|',20(A,'  Cng',I1,' =',F11.6))
C
      WRITE(LU,8260) (' ',K,    CDFF_G(K)/SREF, K=1, NDESIGN)
 8260 FORMAT(' Trefftz drag|',20(A,'CDffg',I1,' =',F11.6))
C
      WRITE(LU,8270) (' ',K,  SPANEF_G(K), K=1, NDESIGN)
 8270 FORMAT(' span eff.   |',20(A,'   eg',I1,' =',F11.6))
C
      WRITE(LU,*)
      WRITE(LU,*)
C
      ENDIF
C
      IF(CL_AL .NE. 0.0) THEN
       XNP = XYZREF(1) - CM_AL/CL_AL
       WRITE(LU,8401) XNP
 8401  FORMAT(/' Neutral point  Xnp =', F11.6)
      ENDIF
C
      IF(ABS(CR_RZ*CN_BE*(2.0/BREF)/(SREF*BREF)**2) .GT. 0.0001) THEN
       BB = CR_BE*CN_RZ / (CR_RZ*CN_BE)
       WRITE(LU,8402) BB 
 8402  FORMAT(/' Clb Cnr / Clr Cnb  =', F11.6,
     &    '    (  > 1 if spirally stable )')
      ENDIF

C
      RETURN
      END ! DERMATS



      SUBROUTINE DERMATB(LU)
C---------------------------------------------------------
C     Calculates and outputs stability derivative matrix
C     in body axes
C---------------------------------------------------------
      INCLUDE 'AVL.INC'
      CHARACTER*50 SATYPE
      REAL WROT_RX(3), WROT_RZ(3), WROT_A(3)
C
      CALL GETSA(LNASA_SA,SATYPE,DIR)
C
C---- set freestream velocity components from alpha, beta
      CALL VINFAB
C
C---- calculate forces and sensitivities
      CALL AERO
C
      CALL OUTTOT(LU)
C
      WRITE(LU,7004)
 7004 FORMAT(/' Geometry-axis derivatives...')
C
C
      WRITE(LU,7006)
 7006 FORMAT(/14X, 4X,'  axial   vel. u',
     &             4X,' sideslip vel. v',
     &             4X,'  normal  vel. w'
     &       /14X, 4X,'----------------',
     &             4X,'----------------',
     &             4X,'----------------' )
C
      WRITE(LU,7010) -    CFT_U(1,1)/SREF,
     &               -DIR*CFT_U(1,2)/SREF,
     &               -    CFT_U(1,3)/SREF
 7010 FORMAT(' x force CX  |','    CXu =',F11.6,
     &                        '    CXv =',F11.6,
     &                        '    CXw =',F11.6 )
C
      WRITE(LU,7020) -DIR*CFT_U(2,1)/SREF,
     &               -    CFT_U(2,2)/SREF,
     &               -DIR*CFT_U(2,3)/SREF
 7020 FORMAT(' y force CY  |','    CYu =',F11.6,
     &                        '    CYv =',F11.6,
     &                        '    CYw =',F11.6 )
C
      WRITE(LU,7030) -    CFT_U(3,1)/SREF,
     &               -DIR*CFT_U(3,2)/SREF,
     &               -    CFT_U(3,3)/SREF
 7030 FORMAT(' z force CZ  |','    CZu =',F11.6,
     &                        '    CZv =',F11.6,
     &                        '    CZw =',F11.6 )
C
      WRITE(LU,7040) -    CMT_U(1,1)/(SREF*BREF),
     &               -DIR*CMT_U(1,2)/(SREF*BREF),
     &               -    CMT_U(1,3)/(SREF*BREF)
 7040 FORMAT(' x mom.  Cl  |','    Clu =',F11.6,
     &                        '    Clv =',F11.6,
     &                        '    Clw =',F11.6 )
C
      WRITE(LU,7050) -DIR*CMT_U(2,1)/(SREF*CREF),
     &               -    CMT_U(2,2)/(SREF*CREF),
     &               -DIR*CMT_U(2,3)/(SREF*CREF)
 7050 FORMAT(' y mom.  Cm  |','    Cmu =',F11.6,
     &                        '    Cmv =',F11.6,
     &                        '    Cmw =',F11.6 )
C
      WRITE(LU,7060) -    CMT_U(3,1)/(SREF*BREF),
     &               -DIR*CMT_U(3,2)/(SREF*BREF),
     &               -    CMT_U(3,3)/(SREF*BREF)
 7060 FORMAT(' z mom.  Cn  |','    Cnu =',F11.6,
     &                        '    Cnv =',F11.6,
     &                        '    Cnw =',F11.6 )
C
C
      WRITE(LU,7106)
 7106 FORMAT(/14X, 4X,'    roll rate  p',
     &             4X,'   pitch rate  q',
     &             4X,'     yaw rate  r'
     &       /14X, 4X,'----------------',
     &             4X,'----------------',
     &             4X,'----------------' )
C
      WRITE(LU,7110)     CFT_U(1,4)*(2.0/BREF)/SREF,
     &               DIR*CFT_U(1,5)*(2.0/CREF)/SREF, 
     &                   CFT_U(1,6)*(2.0/BREF)/SREF
 7110 FORMAT(' x force CX  |','    CXp =',F11.6,
     &                        '    CXq =',F11.6,
     &                        '    CXr =',F11.6 )
C
      WRITE(LU,7120) DIR*CFT_U(2,4)*(2.0/BREF)/SREF,
     &                   CFT_U(2,5)*(2.0/CREF)/SREF,
     &               DIR*CFT_U(2,6)*(2.0/BREF)/SREF
 7120 FORMAT(' y force CY  |','    CYp =',F11.6,
     &                        '    CYq =',F11.6,
     &                        '    CYr =',F11.6 )
C
      WRITE(LU,7130)     CFT_U(3,4)*(2.0/BREF)/SREF,
     &               DIR*CFT_U(3,5)*(2.0/CREF)/SREF,
     &                   CFT_U(3,6)*(2.0/BREF)/SREF
 7130 FORMAT(' z force CZ  |','    CZp =',F11.6,
     &                        '    CZq =',F11.6,
     &                        '    CZr =',F11.6 )
C
      WRITE(LU,7140)     CMT_U(1,4)*(2.0/BREF)/(SREF*BREF),
     &               DIR*CMT_U(1,5)*(2.0/CREF)/(SREF*BREF),
     &                   CMT_U(1,6)*(2.0/BREF)/(SREF*BREF)
 7140 FORMAT(' x mom.  Cl  |','    Clp =',F11.6,
     &                        '    Clq =',F11.6,
     &                        '    Clr =',F11.6 )
C
      WRITE(LU,7150) DIR*CMT_U(2,4)*(2.0/BREF)/(SREF*CREF),
     &                   CMT_U(2,5)*(2.0/CREF)/(SREF*CREF),
     &               DIR*CMT_U(2,6)*(2.0/BREF)/(SREF*CREF)
 7150 FORMAT(' y mom.  Cm  |','    Cmp =',F11.6,
     &                        '    Cmq =',F11.6,
     &                        '    Cmr =',F11.6 )
C
      WRITE(LU,7160)     CMT_U(3,4)*(2.0/BREF)/(SREF*BREF),
     &               DIR*CMT_U(3,5)*(2.0/CREF)/(SREF*BREF),
     &                   CMT_U(3,6)*(2.0/BREF)/(SREF*BREF)
 7160 FORMAT(' z mom.  Cn  |','    Cnp =',F11.6,
     &                        '    Cnq =',F11.6,
     &                        '    Cnr =',F11.6 )
C
      IF(NCONTROL.GT.0) THEN
C
      WRITE(LU,8106) (DNAME(K), K, K=1, NCONTROL)
 8106 FORMAT(/14X,20(4X,A12, ' d',I1,' '))
      WRITE(LU,8107) (' ',K=1, NCONTROL)
 8107 FORMAT( 14X,20(3X,A,'----------------'))
C
      WRITE(LU,8110) (' ',K,DIR*CFT_D(1,K)/SREF, K=1, NCONTROL)
 8110 FORMAT(' x force CX  |',20(A,'  CXd',I1,' =',F11.6))
C
      WRITE(LU,8120) (' ',K,    CFT_D(2,K)/SREF, K=1, NCONTROL)
 8120 FORMAT(' y force CY  |',20(A,'  CYd',I1,' =',F11.6))
C
      WRITE(LU,8130) (' ',K,DIR*CFT_D(3,K)/SREF, K=1, NCONTROL)
 8130 FORMAT(' z force CZ  |',20(A,'  CZd',I1,' =',F11.6))
C
      WRITE(LU,8140) (' ',K,DIR*CMT_D(1,K)/(SREF*BREF), K=1, NCONTROL)
 8140 FORMAT(' x mom.  Cl  |',20(A,'  Cld',I1,' =',F11.6))
C
      WRITE(LU,8150) (' ',K,    CMT_D(2,K)/(SREF*CREF), K=1, NCONTROL)
 8150 FORMAT(' y mom.  Cm  |',20(A,'  Cmd',I1,' =',F11.6))
C
      WRITE(LU,8160) (' ',K,DIR*CMT_D(3,K)/(SREF*BREF), K=1, NCONTROL)
 8160 FORMAT(' z mom.  Cn  |',20(A,'  Cnd',I1,' =',F11.6))
C
      WRITE(LU,*)
      WRITE(LU,*)
C
      ENDIF
C
      IF(NDESIGN.GT.0) THEN
C
      WRITE(LU,8206) (GNAME(K), K, K=1, NDESIGN)
 8206 FORMAT(/14X,20(4X,A12, ' g',I1,' '))
      WRITE(LU,8207) (' ',K=1, NDESIGN)
 8207 FORMAT( 14X,20(3X,A,'----------------'))
C
      WRITE(LU,8210) (' ',K,DIR*CFT_G(1,K)/SREF, K=1, NDESIGN)
 8210 FORMAT(' x force CX  |',20(A,'  CXg',I1,' =',F11.6))
C
      WRITE(LU,8220) (' ',K,    CFT_G(2,K)/SREF, K=1, NDESIGN)
 8220 FORMAT(' y force CY  |',20(A,'  CYg',I1,' =',F11.6))
C
      WRITE(LU,8230) (' ',K,DIR*CFT_G(3,K)/SREF, K=1, NDESIGN)
 8230 FORMAT(' z force CZ  |',20(A,'  CZg',I1,' =',F11.6))
C
      WRITE(LU,8240) (' ',K,DIR*CMT_G(1,K)/(SREF*BREF), K=1, NDESIGN)
 8240 FORMAT(' x mom.  Cl  |',20(A,'  Clg',I1,' =',F11.6))
C
      WRITE(LU,8250) (' ',K,    CMT_G(2,K)/(SREF*CREF), K=1, NDESIGN)
 8250 FORMAT(' y mom.  Cm  |',20(A,'  Cmg',I1,' =',F11.6))
C
      WRITE(LU,8260) (' ',K,DIR*CMT_G(3,K)/(SREF*BREF), K=1, NDESIGN)
 8260 FORMAT(' z mom.  Cn  |',20(A,'  Cng',I1,' =',F11.6))
C
      WRITE(LU,*)
      WRITE(LU,*)
C
      ENDIF
C
      RETURN
      END ! DERMATB



      SUBROUTINE DUMPIT(LU,NF,VINF,ALPHA, BETA,
     &                  OMEGAX, OMEGAY, OMEGAZ,
     &                  CFX, CFY, CFZ, CMX, CMY, CMZ)
C
C--- Writes flow condition header to logical unit
C
       REAL VINF(NF),ALPHA(NF), BETA(NF),
     &     OMEGAX(NF), OMEGAY(NF), OMEGAZ(NF),
     &     CFX(NF), CFY(NF), CFZ(NF), CMX(NF), CMY(NF), CMZ(NF)
C
      DO IF=1, NF
        WRITE(LU,2050) IF, VINF(IF)
        WRITE(LU,2060) ALPHA(IF), BETA(IF),
     &                 OMEGAX(IF), OMEGAY(IF), OMEGAZ(IF)
        WRITE(LU,2070) CFX(IF), CFY(IF), CFZ(IF),
     &                 CMX(IF), CMY(IF), CMZ(IF)
      END DO
C
 2050 FORMAT(/1X,'Flow condition', I3, '    Vinf =', F8.3)
 2060 FORMAT(/1X,6X,'Alpha' ,7X,'Beta',
     &           5X,'Omegax',5X,'Omegay',5X,'Omegaz' /1X,5F11.6)
 2070 FORMAT(/1X,8X,'CFx',8X,'CFy',8X,'CFz',
     &           8X,'CMx',8X,'CMy',8X,'CMz' /1X,6F11.6 / )
C
      RETURN
      END


      SUBROUTINE GETSA(LSA,SATYPE,DIR)
      LOGICAL LSA
      CHARACTER*(*) SATYPE
C
      IF(LSA) THEN
       SATYPE = 'Standard axis orientation,  X fwd, Z down'
       DIR = -1.0
      ELSE
       SATYPE = 'Geometric axis orientation,  X aft, Z up  '
       DIR =  1.0
      ENDIF
C
      RETURN
      END ! GETSA
