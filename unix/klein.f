      function Fitklein(frq,FitL)
      IMPLICIT REAL*8 (A-H,O-Z)
c-------------------------------------------------------------------------
c Compute GS spectrum of a homogeneous source using Klein's approximations
c for the emission and absorption coeffs (A&A 1987, 183, 341). Replaced
c subroutine "gsy" in "gyrosq" code. -TSB
c
c GDF Jan., 31, 2008
c I modified the original program to eventually adjust it for the purpose of the forward
c fitting modeling. 
c
c 1) Subroutine Calc_GS_Spec.for is extracted and saved in a separate file.
c	It calculates the emissivity and absorption coefficients, refractive indices
c	and polarization coefficients at a given frequency 'frq'. It returns other 
c	calculated parameters. Correspondingly, it has many arguments: 
c	(frq,tauo,taux,em1,em2,ab1,ab2,an1,an2,ath1,ath2)
c
c 2) The cycle calculating emission in 100 different frequencies is put now in the main 
c	program (klein). This program is interactive: two commands can be used currently - 
c	GO CAL - calculates emission with the set of parameters adopted in the main program
c	GO PRT - save the data in the txt file, file name has to be deifined interactively
c
c 3) Two other section 'GET' and 'SAV' are kept for the moment but are not used.
c
c 4) Further development must include: free parameters are sent from fitting unit; two kinds 
c	of the fitting algorithm: intensity only and intensity and polarization
c
c 5) This version of the program allows for different types of the electron distribution 
c	including thermal (TNT), power-law (PLW), double power-law (DPL), and kappa-distribution (KAP)
c	The help/documentation file will include definitions for all these distributions as well as 
c	corresponding plots.
c
c 6) In addition, I plan adding two more fitting functions - power-law distribution over the 
c	momentum modulus (now PLW is the power-law over kinetic energy), and "generic" GS function
c	used for analysis of the OVSA data.
c
c 7) Further development will include adding free-free component in various versions:
c	a) free-free absorption at the source; b) free-free emission at the source; 
c	c) free-free absorption at a "screen" on the line of sight between the source and observer.
c-------------------------------------------------------------------------
      character*12 command
      character*8 out_file, savdat, getdat
      character*3 com, ftype
      byte esc
      real*8 f(100), kappa, kb, anPar(4)
		     real*8 testAr(100,20)
Cc	real*8 FluxO, FluxX
      double precision emx(100), emo(100), abx(100), abo(100), 
     &   phix(100), phio(100), phii(100), phiv(100), phiq(100)
	!	Real*4 phii(100) !,
      data esc /27/
      common /parms/ area, dep, enth, enrl, bmag, theta, 
     &   E0, E1, E2, delta1, delta2, EBR, temp, epsTh, kappa, etr, ftype
      common /gs/ emx, emo, abx, abo, phix, phio, phii, phiv, phiq, sang !, f
       common/thermal/gf,cs,kb,c,ffb,ffp  
      common /AnPar/AnPar  
      		common /NType/IAType 
      		common/Integral/IKE
      		      	common /Accuracy/nAcc  
c----------------------------------------------------------------------
c     
10		continue
c      --------------------------
c      -------------
c ----- GO CALCULATE
c      -------------

	


         iret = 0
	!write(*,*)'iret= ',iret 


	     tauo=0
	     taux=0
	     em1=0
	     em2=0
	     ab1=0
	     ab2=0
	     an1=0
	     an2=0
	     ath1=0
	     ath2=0

Cc	Write(*,*)'Fitklein=',Fitklein,sang,frq
	        
Cc Klein      call calc_gs_spec(frq,tauo,taux,em1,em2,ab1,ab2,an1,an2,ath1,ath2)

        !IAtype = 0 !  1 !0 !0 !1 
        nAcc=2
        IKE=16
        
        !comment out below for Alt beam test
        AnPar(1)=60.
        AnPar(2)=180. !180. !90.
        AnPar(3)=0.4 !0.4 !0.1
        AnPar(4)=10.
        theta_true=theta
         If(theta.gt.86.and.theta.lt.94) theta=86.
      !If(theta.gt.86.and.theta.lt.94) Case_theta=94.
      If(theta.lt.4.and.theta.gt.-3) theta=1.

      call 
     &calc_gs_spec_FK(frq,tauo,taux,em1,em2,ab1,ab2,an1,an2,ath1,ath2)

	   
Cc	write(*,*)'tauo=',tauo,'taux=',taux
	   i=1
	   emo(i)=em1
         emx(i)=em2
         abo(i)=ab1
         abx(i)=ab2
c      ----------------------------
c ----- Calculate flux in each mode
c      ----------------------------
         phio(i)=0.0
         phix(i)=0.0
         tfo=tauo
         tfx=taux
C	tauo=1.5e1
         if (tauo.gt.1.e-6.and.tauo.le.1.5e1) tfo=1.-exp(-tauo)
         if (tauo.gt.1.5e1) tfo=1.
         if (taux.gt.1.e-6.and.taux.le.1.5e1) tfx=1.-exp(-taux)
         if (taux.gt.1.5e1) tfx=1.

C	print*,tfx
         if (ab1.gt.0.0) phio(i)=sang*tfo*em1/ab1
         if (ab2.gt.0.0) phix(i)=sang*tfx*em2/ab2
Cc		write(*,*)'parms=',sang,taux,em2,ab2
c     ------------------
c----- Stokes parameters
c     ------------------
         !phii(i)=phio(i)+phix(i)	
         d_theta=6. !4.
         
         QT_X_coeff=(90.-theta_true+d_theta)/d_theta/2.
         QT_L_coeff=(theta_true-90.+d_theta)/d_theta/2.
            If(theta_true.le.90-d_theta) then
	     Fitklein=phix(1)*1e19  ! To get flux in SFU
	     FitL=phio(1)*1e19
              Else If(theta_true.ge.90+d_theta) then
	     Fitklein=phio(1)*1e19  ! To get flux in SFU
	     FitL=phix(1)*1e19
	     Else 
	     Fitklein=(QT_X_coeff*phix(1)+QT_L_coeff*phio(1))*1e19  ! To get flux in SFU
	     FitL= (QT_L_coeff*phix(1)+QT_X_coeff*phio(1))*1e19 !Fitklein !phix(1)*1e19
	      End If
	
Cc	write(*,*)'Fitklein=',Fitklein,phii(1)
Cc         phii(i)=phio(i)+phix(i)
Cc          phiq(i)=phio(i)*(1.-ath1**2)/(1.+ath1**2)+
Cc      &      phix(i)*(1.-ath2**2)/(1.+ath2**2)
Cc          phiv(i)=2.*(phio(i)*ath1/(1.+ath1**2)+
Cc      &      phix(i)*ath2/(1.+ath2**2))

		!	write(*,*)'parms=',Fitklein,phii(1)*1d19 

Cc 	Call DisFun(iret)


c       -------------
c ----- Call for help
c       -------------
    
c      --------------------
c ----- Trouble in Paradise
c      --------------------

400     continue

c      ------------------------------------
c ----- Formats used currently
c      ------------------------------------
 
7074      Format(1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5)   

 
 1700 format(a8)
 
c-----------------------------------------------------------------------
 2000 format(3x,a3)
 2001 format(a)
 
 2010 format(/,1a,6h[21;1H,17h* SAV file name? ,1a,7h[21;18H,$)
 2020 format(/,1a,6h[21;1H,17h* GET file name? ,1a,7h[21;18H,$)
 2030 format(/,1a,6h[21;1H,17h* TXT file name? ,1a,7h[21;18H,$)
 
 2905 format(1a,6h[21;1H,39h                                       ,1a,
     &6h[21;1H,1h*,$)
 2906 format(/,1a,6h[21;1H, 'Can''t find file named: ', a,1a,
     &7h[21;31H,$)

c----------------------------------------------------------------------
 
c----------------------------------------------------------------------
      return
 999  end
