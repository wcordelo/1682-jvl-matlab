C
      IMPLICIT REAL (A-H,M,O-Z)
C
      INCLUDE 'AINDEX.INC'
C
C...Parameter and Global variables for Athena 3-D vortex lattice code
C
C   Primary array limits
C
C    NVMAX   number of horseshoe vortices
C    NSMAX   number of chord strips
C    NFMAX   number of surfaces
C
C    NLMAX   number of source/doublet line nodes
C    NBMAX   number of bodies
C
C    NUMAX   number of freestream parameters (V,Omega)
C    NDMAX   number of surface-deflection control variables
C    NGMAX   number of design variables
C    NJMAX   number of jet momentum variables
C
C    NRMAX   number of stored run cases
C    NTMAX   number of stored time levels
C
      PARAMETER (NVMAX=2500, 
     &           NSMAX=200, 
     &           NFMAX=30,
     &           NLMAX=500,
     &           NBMAX=20,
     &           NUMAX=6,
     &           NDMAX=20,
     &           NGMAX=20,
     &           NJMAX=8,
     &           NRMAX=25,
     &           NTMAX=5000 )
C
C   Derived array limits
C
C    ITMAX  number of triangles for hidden-line routines
C    IPMAX  number of parameters
C    ICMAX  number of constraints
C    JEMAX  number of eigenmode components
C
      PARAMETER (ITMAX=2*NVMAX,
     &           IVMAX=IVTOT+NDMAX+NJMAX,
     &           ICMAX=ICTOT+NDMAX+NJMAX,
     &           IPMAX=IPTOT,
     &           JEMAX=JETOT )
C
C
C---- unit values, names, and namelengths
      CHARACTER*32 UNCHL,UNCHM,UNCHT,UNCHF,UNCHS,UNCHV,UNCHA,UNCHI,UNCHD
      COMMON /UN_R/UNITL,UNITM,UNITT,UNITF,UNITS,UNITV,UNITA,UNITI,UNITD
      COMMON /UN_C/UNCHL,UNCHM,UNCHT,UNCHF,UNCHS,UNCHV,UNCHA,UNCHI,UNCHD
      COMMON /UN_I/NUL  ,NUM  ,NUT  ,NUF  ,NUS  ,NUV  ,NUA  ,NUI  ,NUD
C
      CHARACTER*80 FILDEF, FRNDEF, FMSDEF, FPRDEF, FEVDEF
      CHARACTER*80 TITLE
      CHARACTER*40 STITLE, BTITLE, RTITLE
      CHARACTER*16 DNAME, GNAME, JNAME
      CHARACTER*12 VARNAM, CONNAM
      CHARACTER*12 VARKEY
      CHARACTER*3  CONKEY
      CHARACTER*10 PARNAM
      CHARACTER*32 PARUNCH
      COMMON /CASE_C/
     & FILDEF,         ! default configuration save file
     & FRNDEF,         ! default run case save file
     & FMSDEF,         ! default mass distribution file
     & FPRDEF,         ! default dimensional parameter file
     & FEVDEF,         ! default eigenvalue save file
     & TITLE,          ! configuration title
     & STITLE(NFMAX),  ! surface title
     & BTITLE(NBMAX),  ! body title
     & RTITLE(NRMAX),  ! run case title
     & DNAME(NDMAX),   ! control variable name
     & GNAME(NGMAX),   ! design  variable name
     & JNAME(NJMAX),   ! jet     variable name
     & VARNAM(IVMAX),  ! variable   name
     & CONNAM(ICMAX),  ! constraint name
     & VARKEY(IVMAX),  ! variable   selection key
     & CONKEY(ICMAX),  ! constraint selection key
     & PARNAM(IPMAX),  ! run case parameter name
     & PARUNCH(IPMAX)  ! run case parameter unit name
C
      COMMON /CASE_I/
     & LUINP,             ! logical unit for configuration file
     & LURUN,             ! logical unit for run case file
     & LUOUT,             ! logical unit for output dump file
     & LUSTD,             ! logical unit for stability deriv. dump file
     & LUSYS,             ! logical unit for dynamic system matrix dump file
     & IYSYM,IZSYM,       ! y,z image symm.  (0=no image, 1=image)
     & NVORS,             ! number of horseshoe vortices on solid surfaces
     & NVOR,              ! number of horseshoe vortices
     & NSTRIPS,           ! number of chordwise strips on solid surfaces
     & NSTRIP,            ! number of chordwise strips
     & NSURFS,            ! number of solid surfaces
     & NSURF,             ! number of surfaces
     & NLNODE,            ! number of body source+doublet line nodes
     & NBODY,             ! number of bodies
     & ICON(IVMAX,NRMAX), ! index of constraint for each variable
     & NVTOT,             ! total number of available variables
     & NCTOT,             ! total number of available constraints
     & NPTOT,             ! total number of available parameters
     & NCONTROL,          ! number of control variables
     & NDESIGN,           ! number of design variables
     & NVARJET,           ! number of jet variables
     & NITMAX,            ! max number of Newton iterations
     & IRUN, NRUN,        ! current run case, number of run cases stored
     & IRUNE,             ! target run case for eigenmode calculations
     & IRUNT,             ! target run case for time march initial state
     & ITRIM(NRMAX),      ! trim type used for run case (if any)
     & NEIGEN(NRMAX),     ! number of valid eigenmodes available for run case
     & NEIGENDAT(NRMAX)   ! number reference data eigenvalues
C
      LOGICAL LGEO,LENC,
     &        LWCG,LAIC,LGMU,LSRD,LWVG,LSOL,LSEN,
     &        LVISC,LMASS,
     &        LCONDEF, LDESDEF, LJETDEF,
     &        LPTOT,LPSURF,LPSTRP,LPELE,LPHINGE,LPDERIV,
     &        LBFORCE,
     &        LNASA_SA, LSA_RATES,
     &        LMWAIT
      LOGICAL LPPAR
      COMMON /CASE_L/
     & LGEO,   ! T if geometry exists
     & LENC,   ! T if all normal vectors are valid
     & LWCG,   ! T if WC_GAM matrix has been generated
     & LAIC,   ! T if AIC matrix for W.n has been generated
     & LGMU,   ! T if unit-freestream GAM_U has been generated
     & LSRD,   ! T if unit source+doublet strengths are computed
     & LWVG,   ! T if WV_GAM matrix has been computed
     & LSOL,   ! T if valid solution exists
     & LSEN,   ! T if valid sensitivities exist
     & LVISC,  ! T if viscous profile drag terms are to be added
     & LMASS,  ! T if mass data has been read in
     & LCONDEF(NDMAX),  ! T if control variable has been declared
     & LDESDEF(NGMAX),  ! T if design  variable has been declared
     & LJETDEF(NJMAX),  ! T if jet     variable has been declared
     & LPTOT,  ! T if total   forces are to be printed
     & LPSURF, ! T if surface forces are to be printed
     & LPSTRP, ! T if strip   forces are to be printed
     & LPELE,  ! T if element forces are to be printed
     & LPHINGE, ! T if hinge moments are to be printed
     & LPDERIV, ! T if stability and control derivs are to be printed
     & LBFORCE, ! T if body forces are to be included in total forces
     & LNASA_SA,    ! T if NASA-std stability axes are to be used
     & LSA_RATES,   ! T if stability-axis rates are to be used
     & LMWAIT,      ! T if mode display is to wait for real time
     & LPPAR(IPMAX)          ! T if parameter value is to be plotted
C
      COMMON /CASE_R/
     & VERSION,          ! AVL version number
     & DTR,     PI,      ! 3.14159/180 ,  3.14159
     & YSYM,    ZSYM,    ! y- and z-locations of symmetry planes
     & ALFA,   BETA,        ! alpha, beta
     & VINF(3),             ! freestream velocies in body axes
     & VINF_A(3),           ! d(Vinf)/d(alpha)
     & VINF_B(3),           ! d(Vinf)/d(beta)
     & WROT(3),             ! rotation rates in body axes
     & DTIMED,              ! time step for eigenmode movie integration
     & PARVAL(IPMAX,NRMAX),  ! parameter values for run cases
     & CONVAL(ICMAX,NRMAX),  ! imposed constraint value
     & DELCON(NDMAX),        ! imposed control variable value
     & DELDES(NGMAX),        ! imposed design  variable value
     & DELJET(NJMAX),        ! imposed jet     variable value
     & WJSPAN(NJMAX),        ! total span over which DELJET() acts
     & SREF,  CREF,  BREF,   ! Area, Chord, Span  reference values
     & XYZREF(3),            ! X,Y,Z location for moments
     & XYZREF0(3),           ! X,Y,Z location for moments (default)
     & MACH,                 ! freestream Mach number
     & MACH0,                ! freestream Mach number (default)
     & CDREF,                ! baseline profile CD
     & CDREF0,               ! baseline profile CD (default)
     & VRCORE,               ! vortex core radius / chord
     & SRCORE                ! source core radius / body radius

      COMMON /FORC_R/
     & CLFF, CYFF, CDFF,  ! Trefftz-plane  CL,CY,CD
     & CLFF_U(NUMAX),CYFF_U(NUMAX),CDFF_U(NUMAX), ! deriv wrt Vinf,Wrot
     & CLFF_D(NDMAX),CYFF_D(NDMAX),CDFF_D(NDMAX), ! deriv wrt control
     & CLFF_G(NGMAX),CYFF_G(NGMAX),CDFF_G(NGMAX), ! deriv wrt design
     & CLFF_J(NJMAX),CYFF_J(NJMAX),CDFF_J(NJMAX), ! deriv wrt jet
C
     & CLFFI, CYFFI, CDFFI,  ! Trefftz-plane  CL,CY,CD
     & CLFFI_U(NUMAX),CYFFI_U(NUMAX),CDFFI_U(NUMAX), ! deriv wrt Vinf,Wrot
     & CLFFI_D(NDMAX),CYFFI_D(NDMAX),CDFFI_D(NDMAX), ! deriv wrt control
     & CLFFI_G(NGMAX),CYFFI_G(NGMAX),CDFFI_G(NGMAX), ! deriv wrt design
     & CLFFI_J(NJMAX),CYFFI_J(NJMAX),CDFFI_J(NJMAX), ! deriv wrt jet
C
     & CLFFJ, CYFFJ, CDFFJ,  ! Trefftz-plane  CL,CY,CD
     & CLFFJ_U(NUMAX),CYFFJ_U(NUMAX),CDFFJ_U(NUMAX), ! deriv wrt Vinf,Wrot
     & CLFFJ_D(NDMAX),CYFFJ_D(NDMAX),CDFFJ_D(NDMAX), ! deriv wrt control
     & CLFFJ_G(NGMAX),CYFFJ_G(NGMAX),CDFFJ_G(NGMAX), ! deriv wrt design
     & CLFFJ_J(NJMAX),CYFFJ_J(NJMAX),CDFFJ_J(NJMAX), ! deriv wrt jet
C
     & CDFFV,
     & CDFFV_U(NUMAX),
     & CDFFV_D(NDMAX),
     & CDFFV_G(NGMAX),
     & CDFFV_J(NJMAX),
C
     & CJFF,
     & CQFF,
     & CJFF_J(NJMAX),
     & CQFF_J(NJMAX),                  ! deriv wrt jet
C
     & SPANEF,           ! span efficiency 
     & SPANEF_A,         ! d(SPANEF)/d(alpha)
     & SPANEF_U(NUMAX),  ! d(SPANEF)/d(beta)
     & SPANEF_D(NDMAX),  ! d(SPANEF)/d(control)
     & SPANEF_G(NGMAX),  ! d(SPANEF)/d(design)
     & SPANEF_J(NJMAX),  ! d(SPANEF)/d(jet)
C
     & SPANEV,           ! span efficiency of vectored jet
     & SPANEV_A,         ! d(SPANEV)/d(alpha)
     & SPANEV_U(NUMAX),  ! d(SPANEV)/d(beta)
     & SPANEV_D(NDMAX),  ! d(SPANEV)/d(control)
     & SPANEV_G(NGMAX),  ! d(SPANEV)/d(design)
     & SPANEV_J(NJMAX),  ! d(SPANEV)/d(jet)
C
     & CDT, CYT, CLT,          ! total CD,CY,CL
     & CDT_A, CYT_A, CLT_A,    ! sensitivities wrt alpha
     & CDT_U(NUMAX),CYT_U(NUMAX),CLT_U(NUMAX),  !sens wrt U,W
     & CDT_D(NDMAX),CYT_D(NDMAX),CLT_D(NDMAX),  !sens wrt control
     & CDT_G(NGMAX),CYT_G(NGMAX),CLT_G(NGMAX),  !sens wrt design
     & CDT_J(NJMAX),CYT_J(NJMAX),CLT_J(NJMAX),  !sens wrt jet
C
     & CFT(3),   ! total Cx,Cy,Cz
     & CMT(3),   ! total Cl,Cm,Cn
     & CFT_U(3,NUMAX), !sens wrt U,W
     & CMT_U(3,NUMAX), !sens wrt U,W
     & CFT_D(3,NDMAX), !sens wrt control
     & CMT_D(3,NDMAX), !sens wrt control
     & CFT_G(3,NGMAX), !sens wrt design
     & CMT_G(3,NGMAX), !sens wrt design
     & CFT_J(3,NJMAX), !sens wrt jet
     & CMT_J(3,NJMAX), !sens wrt jet
C
     & CFTI(3),   ! inviscid Cx,Cy,Cz
     & CMTI(3),   ! inviscid Cl,Cm,Cn
     & CFTI_U(3,NUMAX), !sens wrt U,W
     & CMTI_U(3,NUMAX), !sens wrt U,W
     & CFTI_D(3,NDMAX), !sens wrt control
     & CMTI_D(3,NDMAX), !sens wrt control
     & CFTI_G(3,NGMAX), !sens wrt design
     & CMTI_G(3,NGMAX), !sens wrt design
     & CFTI_J(3,NJMAX), !sens wrt jet
     & CMTI_J(3,NJMAX), !sens wrt jet
C
     & CFTJ(3),   ! jet Cx,Cy,Cz
     & CMTJ(3),   ! jet Cl,Cm,Cn
     & CFTJ_U(3,NUMAX), !sens wrt U,W
     & CMTJ_U(3,NUMAX), !sens wrt U,W
     & CFTJ_D(3,NDMAX), !sens wrt control
     & CMTJ_D(3,NDMAX), !sens wrt control
     & CFTJ_G(3,NGMAX), !sens wrt design
     & CMTJ_G(3,NGMAX), !sens wrt design
     & CFTJ_J(3,NJMAX), !sens wrt jet
     & CMTJ_J(3,NJMAX), !sens wrt jet
C
     & CFTV(3),   ! viscous Cx,Cy,Cz
     & CMTV(3),   ! viscous Cl,Cm,Cn
     & CFTV_U(3,NUMAX), !sens wrt U,W
     & CMTV_U(3,NUMAX), !sens wrt U,W
     & CFTV_D(3,NDMAX), !sens wrt control
     & CMTV_D(3,NDMAX), !sens wrt control
     & CFTV_G(3,NGMAX), !sens wrt design
     & CMTV_G(3,NGMAX), !sens wrt design
     & CFTV_J(3,NJMAX), !sens wrt jet
     & CMTV_J(3,NJMAX), !sens wrt jet
C
     & CTT,   ! total jet thrust
     & CJT,   ! total jet momentum
     & CQT,   ! total jet mass flow
     & CTT_J(NJMAX),
     & CJT_J(NJMAX),
     & CQT_J(NJMAX),
C
     & CFBI(3),  ! total body inviscid CX,CY,CZ
     & CMBI(3),  ! total body inviscid Cl,Cm,Cn
     & CFBI_U(3,NUMAX),
     & CMBI_U(3,NUMAX),
     & CFBI_D(3,NDMAX),
     & CMBI_D(3,NDMAX),
     & CFBI_G(3,NGMAX),
     & CMBI_G(3,NGMAX),
     & CFBI_J(3,NJMAX),
     & CMBI_J(3,NJMAX),
C
     & CFBV(3),  ! total body viscous CX,CY,CZ
     & CMBV(3),  ! total body viscous Cl,Cm,Cn
     & CFBV_U(3,NUMAX),
     & CMBV_U(3,NUMAX),
     & CFBV_D(3,NDMAX),
     & CMBV_D(3,NDMAX),
     & CFBV_G(3,NGMAX),
     & CMBV_G(3,NGMAX),
     & CFBV_J(3,NJMAX),
     & CMBV_J(3,NJMAX),
C
     & CHINGE(NDMAX), 
     & CHINGE_U(NDMAX,NUMAX),
     & CHINGE_D(NDMAX,NDMAX),
     & CHINGE_G(NDMAX,NGMAX),
     & CHINGE_J(NDMAX,NJMAX),
C
     & DCL_A0, DCM_A0,   ! additional default CL_a, CM_a
     & DCL_U0, DCM_U0    ! additional default CL_u, CM_u
C
      COMPLEX EVAL, EVEC, EVALDAT
      COMMON /CASE_Z/ 
     &  EVAL(JEMAX,NRMAX),       ! mode eigenvalue
     &  EVEC(JEMAX,JEMAX,NRMAX), ! mode eigenvector
     &  EVALDAT(JEMAX,NRMAX)     ! mode eigenvalue reference data

      COMMON /TIME_I/
     &  ITLEV,           ! current time level
     &  NTLEV,           ! number of stored time levels
     &  NTSTEPS          ! default number of time steps to run

      COMMON /TIME_R/
     &  DELTAT,                 ! integration time step
     &  TDER(3),                ! time-derivative differencing coefficient
     &  TLEV(NTMAX),            ! time values
     &  TPARS(KPTOT,NTMAX),     ! scalar parameter time trace
     &  TPARV(3,KPVTOT,NTMAX),  ! vector parameter time trace
     &  TPARD(NDMAX,NTMAX)      ! control parameter time traces

      COMMON /MASS_R/
     &   RHO0, GEE0,     ! density, gravity   | from .mass file
     &   XYZMASS0(3),    ! mass centroid      | from .mass file
     &   RMASS0     ,    ! real mass          | from .mass file
     &   RINER0(3,3),    ! real inertia       | from .mass file
     &   AMASS(3,3),     ! apparent mass/rho     | from geometry
     &   AINER(3,3)      ! apparent inertia/rho  | from geometry

      COMMON /SURF_I/  
     & NJ(NFMAX),       ! number of elements along span  in surface
     & NK(NFMAX),       ! number of elements along chord in surface
     & IFRST(NFMAX),    ! index of first element in surface
     & JFRST(NFMAX),    ! index of first strip in surface
     & IMAGS(NFMAX),    ! indicates whether surface is a YDUPlicated one
     & NL(NBMAX),       ! number of source-line nodes in body
     & LFRST(NBMAX),    ! index of first line node in body
     & LSURF(NFMAX),    ! surface index
     & IFTYPE(NFMAX),   ! surface-type index  (0=solid, 1=jet)
     & IBCENT(NBMAX)    ! = 1 if body is on centerline, = 0 otherwise

      COMMON /SURF_R/
     & CFNI(3,NFMAX),  ! surface inviscid CX,CY,CZ
     & CMNI(3,NFMAX),  ! surface inviscid Cl,Cm,Cn
     & CFNI_U(3,NFMAX,NUMAX),
     & CMNI_U(3,NFMAX,NUMAX),
     & CFNI_D(3,NFMAX,NDMAX),
     & CMNI_D(3,NFMAX,NDMAX),
     & CFNI_G(3,NFMAX,NGMAX),
     & CMNI_G(3,NFMAX,NGMAX),
     & CFNI_J(3,NFMAX,NJMAX),
     & CMNI_J(3,NFMAX,NJMAX),
C
     & CFNJ(3,NFMAX),  ! surface jet CX,CY,CZ
     & CMNJ(3,NFMAX),  ! surface jet Cl,Cm,Cn
     & CFNJ_U(3,NFMAX,NUMAX),
     & CMNJ_U(3,NFMAX,NUMAX),
     & CFNJ_D(3,NFMAX,NDMAX),
     & CMNJ_D(3,NFMAX,NDMAX),
     & CFNJ_G(3,NFMAX,NGMAX),
     & CMNJ_G(3,NFMAX,NGMAX),
     & CFNJ_J(3,NFMAX,NJMAX),
     & CMNJ_J(3,NFMAX,NJMAX),
C
     & CFNV(3,NFMAX),  ! surface viscous CX,CY,CZ
     & CMNV(3,NFMAX),  ! surface viscous Cl,Cm,Cn
     & CFNV_U(3,NFMAX,NUMAX),
     & CMNV_U(3,NFMAX,NUMAX),
     & CFNV_D(3,NFMAX,NDMAX),
     & CMNV_D(3,NFMAX,NDMAX),
     & CFNV_G(3,NFMAX,NGMAX),
     & CMNV_G(3,NFMAX,NGMAX),
     & CFNV_J(3,NFMAX,NJMAX),
     & CMNV_J(3,NFMAX,NJMAX),
C
     & CJN(NFMAX),               ! surface CJ
     & CQN(NFMAX),               ! surface CQ
     & CJN_J(NFMAX,NJMAX), 
     & CQN_J(NFMAX,NJMAX),
C
     & CFLI(3,NBMAX),  ! body CX,CY,CZ
     & CMLI(3,NBMAX),  ! body Cl,Cm,Cn
     & CFLI_U(3,NBMAX,NUMAX),
     & CMLI_U(3,NBMAX,NUMAX),
     & CFLI_D(3,NBMAX,NDMAX),
     & CMLI_D(3,NBMAX,NDMAX),
     & CFLI_G(3,NBMAX,NGMAX),
     & CMLI_G(3,NBMAX,NGMAX),
     & CFLI_J(3,NBMAX,NJMAX),
     & CMLI_J(3,NBMAX,NJMAX),
C
     & CFLV(3,NBMAX),  ! body CX,CY,CZ
     & CMLV(3,NBMAX),  ! body Cl,Cm,Cn
     & CFLV_U(3,NBMAX,NUMAX),
     & CMLV_U(3,NBMAX,NUMAX),
     & CFLV_D(3,NBMAX,NDMAX),
     & CMLV_D(3,NBMAX,NDMAX),
     & CFLV_G(3,NBMAX,NGMAX),
     & CMLV_G(3,NBMAX,NGMAX),
     & CFLV_J(3,NBMAX,NJMAX),
     & CMLV_J(3,NBMAX,NJMAX),
C
     & SSURF(NFMAX), CAVESURF(NFMAX),
     & VBODY(NBMAX)
C
C
      COMMON /STRP_I/
     & ISURFS(NSMAX),    ! index of surface which contains strip
     & IJFRST(NSMAX),    ! index of first element in strip 
     & NVSTRP(NSMAX),    ! number of elements in strip
     & JJETS(NSMAX)      ! index of jet strip trailing from surface strip (0 if none)

      LOGICAL LSTRIPOFF,LVISCSTRP,LJ1SECT,LJ2SECT
      COMMON /STRP_L/
     & LSTRIPOFF(NSMAX),  ! T if strip is "turned off" (outside of fluid)
     & LVISCSTRP(NSMAX),  ! T is strip has viscous drag data
     & LJ1SECT(NSMAX),    ! T if station 1 is a section
     & LJ2SECT(NSMAX)     ! T if station 2 is a section

      COMMON /STRP_R/
     & RLE(3,NSMAX),  CHORD(NSMAX),    ! strip c.p. line LE point, chord
     & RLE1(3,NSMAX), CHORD1(NSMAX),   ! strip left  end LE point, chord
     & RLE2(3,NSMAX), CHORD2(NSMAX),   ! strip right end LE point, chord
     & WSTRIP(NSMAX),                  ! strip y-z width
     & TANLE(NSMAX),  TANTE(NSMAX),    ! strip LE,TE sweep slopes
     & CLCD(NUMAX,NSMAX),              ! strip viscous polar
     & SAXFR,                       ! x/c of spanwise axis for Vperp def
     & ESS(3,NSMAX),                ! spanwise unit vector for Vperp def
     & ENSY(NSMAX), ENSZ(NSMAX),    ! strip normal vector in Trefftz-Plane
     & XYZREFS(3,NSMAX),
C
     & CDV_STRP(NSMAX),          ! strip's profile cd
C
     & AINC(NSMAX),              ! strip's incidence twist angle
     & AINC_G(NSMAX,NGMAX),      ! dAINC/dG
C
     & CNCI(NSMAX), DWWAKE(NSMAX),
C
     & CFSI(3,NSMAX),   ! strip forces
     & CMSI(3,NSMAX),   ! strip moments
     & CFSI_U(3,NSMAX,NUMAX), 
     & CMSI_U(3,NSMAX,NUMAX), 
     & CFSI_D(3,NSMAX,NDMAX), 
     & CMSI_D(3,NSMAX,NDMAX), 
     & CFSI_G(3,NSMAX,NGMAX), 
     & CMSI_G(3,NSMAX,NGMAX), 
     & CFSI_J(3,NSMAX,NJMAX), 
     & CMSI_J(3,NSMAX,NJMAX), 
C
     & CFSJ(3,NSMAX),                    ! strip jet x,y,z forces
     & CMSJ(3,NSMAX),                    ! strip jet x,y,z moments
     & CFSJ_U(3,NSMAX,NUMAX),
     & CMSJ_U(3,NSMAX,NUMAX),
     & CFSJ_D(3,NSMAX,NDMAX),
     & CMSJ_D(3,NSMAX,NDMAX),
     & CFSJ_G(3,NSMAX,NGMAX),
     & CMSJ_G(3,NSMAX,NGMAX),
     & CFSJ_J(3,NSMAX,NJMAX),
     & CMSJ_J(3,NSMAX,NJMAX),
C
     & CFSV(3,NSMAX),                    ! strip viscous x,y,z forces
     & CMSV(3,NSMAX),                    ! strip viscous x,y,z moments
     & CFSV_U(3,NSMAX,NUMAX),
     & CMSV_U(3,NSMAX,NUMAX),
     & CFSV_D(3,NSMAX,NDMAX),
     & CMSV_D(3,NSMAX,NDMAX),
     & CFSV_G(3,NSMAX,NGMAX),
     & CMSV_G(3,NSMAX,NGMAX),
     & CFSV_J(3,NSMAX,NJMAX),
     & CMSV_J(3,NSMAX,NJMAX),
C
     & CJS(NSMAX),        ! strip jet momentum 
     & CQS(NSMAX),        ! strip jet mass
     & CJS_J(NSMAX,NJMAX),
     & CQS_J(NSMAX,NJMAX),
C
     & CNCJ(NSMAX),
C
     & HJSTRP(NSMAX,NJMAX),  ! strip jet height
     & GJSTRP(NSMAX,NJMAX)   ! strip jet momentum ceoff / jet variable  (gain)
C
C
      COMMON /VRTX_I/
     & LSURFV(NVMAX),   ! logical index of surface which contains vortex element
     & ISURFV(NVMAX),   ! index of surface which contains vortex element
     & ISTRPV(NVMAX),   ! index of strip   which contains vortex element
     & IJETM(NVMAX)     ! index of element just upstream (for jet only)
C
      COMMON /VRTX_R/
     & RV1(3,NVMAX),    ! h.v. vortex left  points
     & RV2(3,NVMAX),    ! h.v. vortex right points
     & RV(3,NVMAX),     ! h.v. vortex center points
     & RC(3,NVMAX),     ! h.v. control points
     & RS(3,NVMAX),     ! h.v. source points
     & RL(3,NLMAX),RADL(NLMAX),  ! source line node points, body radius
     & DXV(NVMAX),               ! chord of element
     & CHORDV(NVMAX),            ! chord of element-containing strip
     & SLOPEV(NVMAX),            ! camber slopes at h.v. bound leg
     & SLOPEC(NVMAX),            ! camber slopes at c.p.
     & DCONTROL(NVMAX,NDMAX),  ! d(normal angle)/dCONTROL
     & VHINGE(3,NSMAX,NDMAX),  ! hinge vector for CONTROL rot. of normal
     & PHINGE(3,NSMAX,NDMAX),  ! point on hingeline for hinge moment calculation
     & VREFL(NSMAX,NDMAX),  !sign applied to hinge vec. of refl. surface
     & ENC(3,NVMAX),           ! c.p. normal vector
     & ENV(3,NVMAX),           ! h.v. normal vector
     & ENC_D(3,NVMAX,NDMAX),   ! sensitivities
     & ENC_G(3,NVMAX,NGMAX),
     & ENV_D(3,NVMAX,NDMAX),
     & ENV_G(3,NVMAX,NGMAX),
     & DCP(NVMAX),             ! delta(Cp) on vortex element
     & DCP_U(NVMAX,NUMAX),
     & DCP_D(NVMAX,NDMAX),
     & DCP_G(NVMAX,NGMAX),
     & DCP_J(NVMAX,NJMAX),
     & GAM(NVMAX),             ! circulation of h.v. vortex
     & GAM_U(NVMAX,NUMAX),
     & GAM_D(NVMAX,NDMAX),
     & GAM_G(NVMAX,NGMAX),
     & GAM_J(NVMAX,NJMAX),
     & SRC(NLMAX),        ! source  strength of source+doublet line elem
     & DBL(3,NLMAX),      ! doublet strength of source+doublet line elem
     & SRC_U(NLMAX,NUMAX),
     & DBL_U(3,NLMAX,NUMAX),
     & WCSRD_U(3,NVMAX,NUMAX),
     & WVSRD_U(3,NVMAX,NUMAX),
     & DCPB(3,NLMAX)
C
      COMMON /SOLV_I/ 
     & IAPIV(NVMAX)           ! pivot indices for LU solver
      COMMON /SOLV_R/  
     & AMACH,                 ! Mach number at which AIC matrices were computed
     & AICN(NVMAX,NVMAX),     ! normalwash AIC matrix (and VL system matrix)
     & WC_GAM(3,NVMAX,NVMAX), ! c.p. velocity/Gamma influence matrix
     & WV_GAM(3,NVMAX,NVMAX), ! h.v. velocity/Gamma influence matrix
     & WC(3,NVMAX),           ! total induced velocity at c.p.
     & WC_U(3,NVMAX,NUMAX),
     & WC_D(3,NVMAX,NDMAX),
     & WC_G(3,NVMAX,NGMAX),
     & WC_J(3,NVMAX,NJMAX),
     & WV(3,NVMAX),           ! total induced velocity at h.v.
     & WV_U(3,NVMAX,NUMAX),
     & WV_D(3,NVMAX,NDMAX),
     & WV_G(3,NVMAX,NGMAX),
     & WV_J(3,NVMAX,NJMAX) 
C
      LOGICAL
     & LWAKEPLT,   LLOADPLT,   LWAKLPLT,  LHINGEPLT,
     & LBOUNDLEG,  LCHORDLINE, LCAMBER,   LCNTLPTS, 
     & LNRMLPLT,   LAXESPLT,   LVELCPLT,  LWSEGPLT,
     & LRREFPLT,
     & LCLPERPLT,  LDWASHPLT,  LLABSURF,   LTRFORCE,
     & LABEL_SURF, LABEL_STRP, LABEL_VRTX, LABEL_BODY,
     & LPLTNEW,    
     & LPLTSURF,
     & LPLTBODY,
     & LSVMOV
      COMMON /PLOT_L/          ! plotting flags
     & LWAKEPLT,   LLOADPLT,   LWAKLPLT  ,LHINGEPLT,
     & LBOUNDLEG,  LCHORDLINE, LCAMBER,   LCNTLPTS, 
     & LNRMLPLT,   LAXESPLT,   LVELCPLT,  LWSEGPLT,
     & LRREFPLT,
     & LCLPERPLT,  LDWASHPLT,  LLABSURF,   LTRFORCE,
     & LABEL_SURF, LABEL_STRP, LABEL_VRTX, LABEL_BODY,
     & LPLTNEW,    
     & LPLTSURF(NFMAX),
     & LPLTBODY(NBMAX),
     & LSVMOV
      COMMON /PLOT_I/ NTRI, IMARKSURF, NAXANN(3), IRCOLOR(NRMAX)
      COMMON /PLOT_R/ TRI(16,ITMAX),
     &                AXMIN(3),AXMAX(3),AXDEL(3),AXSPAN(3),
     &                GMIN(3),GMAX(3),GMINP(3),GMAXP(3),
     &                VMIN(3),VMAX(3),VMINP(3),VMAXP(3),
     &                DTMOVIE,TMOVIE, SLOMOF,
     &                EPHASE, EIGENF, TMOFAC
