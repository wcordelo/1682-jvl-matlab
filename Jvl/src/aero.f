C***********************************************************************
C    Module:  aero.f
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

      SUBROUTINE AERO
      INCLUDE 'AVL.INC'
C
      REAL R(3), VMAG_U(3)
C
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
C
C---------------------------------------------------------
C---- calculate all near field force components
C
C---- inviscid and viscous surface forces
      CALL SFFORC
C
C---- jet forces
      CALL SJFORC
C
C---- body forces
      CALL BDFORC
C
C---------------------------------------------------------
C---- add baseline profile drag force along VINF, applied at XYZREF
      VSQ = VINF(1)**2 + VINF(2)**2 + VINF(3)**2
      VMAG = SQRT(VSQ)
      VMAG_U(1) = VINF(1)/VMAG
      VMAG_U(2) = VINF(2)/VMAG
      VMAG_U(3) = VINF(3)/VMAG
C
      R(1) = XYZREF(1)
      R(2) = XYZREF(2)
      R(3) = XYZREF(3)
C
      CDA = CDREF*SREF
C
      DO K = 1, 3
        CFTV(K)     = CFTV(K)     + CDA*VINF(K)*VMAG
        CFTV_U(K,K) = CFTV_U(K,K) + CDA        *VMAG
        CFTV_U(K,1) = CFTV_U(K,1) + CDA*VINF(K)*VMAG_U(1)
        CFTV_U(K,2) = CFTV_U(K,2) + CDA*VINF(K)*VMAG_U(2)
        CFTV_U(K,3) = CFTV_U(K,3) + CDA*VINF(K)*VMAG_U(3)
C
        IC = ICRS(K)
        JC = JCRS(K)
        RXV = R(IC)*VINF(JC) - R(JC)*VINF(IC)
        CMTV(K)      = CMTV(K)      + CDA*RXV  *VMAG
        CMTV_U(K,IC) = CMTV_U(K,IC) - CDA*R(JC)*VMAG
        CMTV_U(K,JC) = CMTV_U(K,JC) + CDA*R(IC)*VMAG
        CMTV_U(K,1)  = CMTV_U(K,1)  + CDA*RXV  *VMAG_U(1)
        CMTV_U(K,2)  = CMTV_U(K,2)  + CDA*RXV  *VMAG_U(2)
        CMTV_U(K,3)  = CMTV_U(K,3)  + CDA*RXV  *VMAG_U(3)
      ENDDO
C
C---------------------------------------------------------
C---- set total forces and total moments
      DO K = 1, 3
        CFT(K) = CFTI(K) + CFTV(K) + CFTJ(K)
     &         + CFBI(K) + CFBV(K)
        CMT(K) = CMTI(K) + CMTV(K) + CMTJ(K)
     &         + CMBI(K) + CMBV(K)
        DO N = 1, NUMAX
          CFT_U(K,N) = CFTI_U(K,N) + CFTV_U(K,N) + CFTJ_U(K,N)
     &               + CFBI_U(K,N) + CFBV_U(K,N)
          CMT_U(K,N) = CMTI_U(K,N) + CMTV_U(K,N) + CMTJ_U(K,N)
     &               + CMBI_U(K,N) + CMBV_U(K,N)
        ENDDO                                                                      
        DO N = 1, NCONTROL                                            
          CFT_D(K,N) = CFTI_D(K,N) + CFTV_D(K,N) + CFTJ_D(K,N)
     &               + CFBI_D(K,N) + CFBV_D(K,N)
          CMT_D(K,N) = CMTI_D(K,N) + CMTV_D(K,N) + CMTJ_D(K,N)
     &               + CMBI_D(K,N) + CMBV_D(K,N)
        ENDDO                                                                      
        DO N = 1, NDESIGN                                              
          CFT_G(K,N) = CFTI_G(K,N) + CFTV_G(K,N) + CFTJ_G(K,N)
     &               + CFBI_G(K,N) + CFBV_G(K,N)
          CMT_G(K,N) = CMTI_G(K,N) + CMTV_G(K,N) + CMTJ_G(K,N)
     &               + CMBI_G(K,N) + CMBV_G(K,N)
        ENDDO                                                                      
        DO N = 1, NVARJET                                              
          CFT_J(K,N) = CFTI_J(K,N) + CFTV_J(K,N) + CFTJ_J(K,N)
     &               + CFBI_J(K,N) + CFBV_J(K,N)
          CMT_J(K,N) = CMTI_J(K,N) + CMTV_J(K,N) + CMTJ_J(K,N)
     &               + CMBI_J(K,N) + CMBV_J(K,N)
        ENDDO
      ENDDO
C
cC---- translate moments to XYZREF location
c      R(1) = -XYZREF(1)
c      R(2) = -XYZREF(2)
c      R(3) = -XYZREF(3)
c!      CALL MSHIFT(CMT,CFT,R)
c      DO K = 1, 3
c        IC = ICRS(K)
c        JC = JCRS(K)
c        CMT(K) = CMT(K) + R(IC)*CFT(JC) - R(JC)*CFT(IC)
c        DO N = 1, NUMAX
c         CMT_U(K,N) = CMT_U(K,N) + R(IC)*CFT_U(JC,N) - R(JC)*CFT_U(IC,N)
c        ENDDO
c        DO N = 1, NCONTROL
c         CMT_D(K,N) = CMT_D(K,N) + R(IC)*CFT_D(JC,N) - R(JC)*CFT_D(IC,N)
c        ENDDO
c        DO N = 1, NDESIGN
c         CMT_G(K,N) = CMT_G(K,N) + R(IC)*CFT_G(JC,N) - R(JC)*CFT_G(IC,N)
c        ENDDO
c        DO N = 1, NVARJET
c         CMT_J(K,N) = CMT_J(K,N) + R(IC)*CFT_J(JC,N) - R(JC)*CFT_J(IC,N)
c        ENDDO
c      ENDDO
C
C---------------------------------------------------------
C---- set total forces in stability axes
      SA = SIN(ALFA)
      CA = COS(ALFA)
C
      CDT = CA*CFT(1) + SA*CFT(3)
      CYT =    CFT(2)
      CLT = CA*CFT(3) - SA*CFT(1)
C
      CDT_A = -SA*CFT(1) + CA*CFT(3)
      CYT_A = 0.
      CLT_A = -SA*CFT(3) - CA*CFT(1)
C
      DO N = 1, NUMAX
        CDT_U(N) = CA*CFT_U(1,N) + SA*CFT_U(3,N)
        CYT_U(N) =    CFT_U(2,N)
        CLT_U(N) = CA*CFT_U(3,N) - SA*CFT_U(1,N)
      ENDDO
      DO N = 1, NCONTROL
        CDT_D(N) = CA*CFT_D(1,N) + SA*CFT_D(3,N)
        CYT_D(N) =    CFT_D(2,N)
        CLT_D(N) = CA*CFT_D(3,N) - SA*CFT_D(1,N)
      ENDDO
      DO N = 1, NDESIGN
        CDT_G(N) = CA*CFT_G(1,N) + SA*CFT_G(3,N)
        CYT_G(N) =    CFT_G(2,N)
        CLT_G(N) = CA*CFT_G(3,N) - SA*CFT_G(1,N)
      ENDDO
      DO N = 1, NVARJET
        CDT_J(N) = CA*CFT_J(1,N) + SA*CFT_J(3,N)
        CYT_J(N) =    CFT_J(2,N)
        CLT_J(N) = CA*CFT_J(3,N) - SA*CFT_J(1,N)
      ENDDO
C
C================================================================
C---- calculate total farfield (Trefftz-Plane) forces
      CALL TPFORC
C
C---- add baseline profile drag force (this doesn't get YSYM doubling)
      CDFFV = CDFFV + CDREF*SREF
C
C---------------------------------------------------------
C---- set total Trefftz-Plane forces
      CLFF = CLFFI + CLFFJ
      CYFF = CYFFI + CYFFJ
      CDFF = CDFFI + CDFFJ + CDFFV
C
      DO N = 1, NUMAX
        CLFF_U(N) = CLFFI_U(N) + CLFFJ_U(N)
        CYFF_U(N) = CYFFI_U(N) + CYFFJ_U(N)
        CDFF_U(N) = CDFFI_U(N) + CDFFJ_U(N) + CDFFV_U(N)
      ENDDO
      DO N = 1, NCONTROL
        CLFF_D(N) = CLFFI_D(N) + CLFFJ_D(N)
        CYFF_D(N) = CYFFI_D(N) + CYFFJ_D(N)
        CDFF_D(N) = CDFFI_D(N) + CDFFJ_D(N) + CDFFV_D(N)
      ENDDO
      DO N = 1, NDESIGN
        CLFF_G(N) = CLFFI_G(N) + CLFFJ_G(N)
        CYFF_G(N) = CYFFI_G(N) + CYFFJ_G(N)
        CDFF_G(N) = CDFFI_G(N) + CDFFJ_G(N) + CDFFV_G(N)
      ENDDO
      DO N = 1, NVARJET
        CLFF_J(N) = CLFFI_J(N) + CLFFJ_J(N)
        CYFF_J(N) = CYFFI_J(N) + CYFFJ_J(N)
        CDFF_J(N) = CDFFI_J(N) + CDFFJ_J(N) + CDFFV_J(N)
      ENDDO
C
      RETURN
      END ! AERO


      SUBROUTINE VINFAB
C
C...Purpose:  To calculate freestream vector components and sensitivities
C
C...Input:   ALFA       Angle of attack (for stability-axis definition)
C            BETA       Sideslip angle (positive wind on right cheek facing fwd)
C...Output:  VINF(3)    Velocity components of free stream
C            VINF_A(3)  dVINF()/dALFA
C            VINF_B(3)  dVINF()/dBETA
C
      INCLUDE 'AVL.INC'
C
      SINA = SIN(ALFA)
      COSA = COS(ALFA)
      SINB = SIN(BETA)
      COSB = COS(BETA)
C
      VINF(1) =  COSA*COSB
      VINF(2) =      -SINB
      VINF(3) =  SINA*COSB
C
      VINF_A(1) = -SINA*COSB
      VINF_A(2) =  0.
      VINF_A(3) =  COSA*COSB
C
      VINF_B(1) = -COSA*SINB
      VINF_B(2) =      -COSB
      VINF_B(3) = -SINA*SINB
C
      RETURN
      END ! VINFAB
