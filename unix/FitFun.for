!!!!!!!!!!!!!!!!!!!!!! FitFUNCTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      function FitFun(X1,X2) ! FitFun = value which fits y at argument x
            IMPLICIT REAL*8 (A-H,O-Z)
      !Real*8 XNe,gamma1,gamma2,xi,wB,w
	      real*8 kappa !, anPar(4) !f(100),
            data c /2.997925d10/	!speed of light

	     character*3 ftype
      common/VAR/EPS,STEP,NPAR,NX1,NX2,KX
      common/Eminmax/E_min, E_max, T_fix,are_arc,dep_arc      
      common/OptDepth/tau,gaunt,densTh

      common/ARR/FUNDAT(300,9),ERRDAT(300,9),X1dat(300),X2dat(9), ! NX1max=300, NX2max=9
     *PAR(15),DPAR(15),VECT(15),PSCALE(15),PARmin(15),PARmax(15), ! NPARmax=15
     *FUNC(19),AS(19,15)         ! 19=NPARmax+4 (for SIMPLEX)
	     common /parms/ area, dep, enth, enrl, bmag, theta, 
     & E0, E1, E2, delta1, delta2, EBR, temp, epsTh, kappa, etr, ftype
       common/B2Ind/NB2 
       !common /AnPar/AnPar !For Alt anis test 01-11-16
Cc 	External SynFitFun

 	External FitKlein

      
c----------------------------------------------------------------------
c     
c ----- Set some default values
c      ------------------------
            
            DepArcSec=dep_arc !8. !20. !24. !20.
            AreaArcSec= are_arc !4. !DepArcSec*DepArcSec !Par(4) !30.*DepArcSec
            eNrl_tot=0.3e35
            
            !DepArcSec=10. !Altyntsev's event 2012-07-06
            !AreaArcSec=15.*DepArcSec !Altyntsev's event 2012-07-06
            
      area =AreaArcSec*7.27e7*7.27e7 !*100*14 !Par(4) !1e20 !3e18 *Par(4) ! 1.e20
      !area =DepArcSec*7.27e7*7.27e7 !*Par(5)
      dep = DepArcSec*7.27e7 !Par(3)*7.27e7 ! *Par(3) !*Par(1) ! 1e10 !3e9 !*Par(4) !*Par(4) !1.e10
      !temp = 1.e7 !*Par(6) !1.e9
            temp = T_fix !4.e7
      If(Npar.gt.6) temp = 1d6*Par(7) !1.e9
      !2018_June 04 Temporarily vary Temp
      epsTh = 0.05 !04 !25 !0.050
      kappa = 4.0 ! must be gt 1.5
      E0 = Par(8) !1.0
      E1 = E_min !0.02 !Par(6) ! 0.064 !Par(6) ! 0.1 !0.02
      !E2 = Par(6) !5.0 !Par(6) !5. !Par(6) !5. !Par(6) !10.0 !
      E2 = E_max !10d0 !Par(6) !5.0 !Par(6) !5. !Par(6) !5. !Par(6) !10.0 !
      If(Npar.gt.5) E2 = Par(6)
      !2018_June 04 Temporarily fix Emax and vary Emin above
      EBR = 1.0
      delta1 =Par(5) !6-0.02*Par(5)  !5. ! Par(6) !4. !4.0*
      delta2 = delta1 !Par(7) !delta1 !delta1 !Par(1) !6.0 !9.0 !4.0
      enth = 0.1e10*Par(4) !0.25 !Par(5) !3.e10*Par(4) - default value  !0.0
      !tau1=Par(1) !(16.+0.01*Par(1))
      enrl = 1e7*Par(1) !ENrl_tot/area/dep*tau1 !*Par(4) !*Par(3) 
      bmag = 100.*Par(2) !100.*(1.+0.01*Par(2)) !*1.5  !*1.5 !*Par(2)  ! !1.0
      theta = Par(3) !70. !45. !Par(5) !60. !Par(5) ! 45. !80.0
      ftype = 'PLW' !'dpl' !'plp' !''plp' !'kap' !'tnt' !'plg' !'kap' !'tnt' !'thm' !'tnt' !'tng' !'plp' 
      etr = 25.
      
c            !Below: Altyntsev Test parms for beam case
c            delta1 =2.
c     
       !E2 = 3.
c            AnPar(1)=60.
c            AnPar(2)=180. !180. !90.
c        AnPar(3)=Par(6) !0.4 !0.4 !0.1
c        AnPar(4)=10.


CC		Test parameters:
Cc		XNe=1d19*Par(1) !1d19*Par(1)		!Total Number of relativistic electrons normalized 
								!to N(>100 keV)*V=10^33


Cc		xi=Par(3)			!Energy spectral index
Cc		gamma2=Par(4)		!hIGH-ENERGY CUT-OFF
Cc		gamma1=Par(5)		!Low-ENERGY CUT-OFF
		
Cc		Bgauss=200.*Par(2) !Par(5) !250. !Par(4)		!Magnetic field
      DensTh=enth+enrl  !1.e10*Par(6) !Thermal number density in cm^{-3}

CC		Derived parameters
		fpe=9e3*sqrt(DensTh)	!plasma frequency in Hz
      	fpeGHz=fpe/1e9
		fBe=2.8e6*Bmag
		wB=fBe/fpe


				If(X1.le.fpe) then
				FitFun=0
Cc	write(*,*)'x1=',x1,fpe
				Return
				Else
CC	write(*,*)'Fundat=',fundat(11,1)
c				RefInd=sqrt(1.-fpeGHz**2/X1**2)
c	print*,refind
				End If




		frqHz=X1		!X1 - frequency in Hz!!!




Cc      FitFun=SynFitFun(XNe,gamma1,gamma2,xi,wB,w,fpe)
        X2=0
	     FitFun=FitKlein(frqHz,X2)
	!write(*,*) 'X2=  ', X2
	 

      return
      end
