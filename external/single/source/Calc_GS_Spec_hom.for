c----------------------------------------------------------------------
c   CALC_GS_SPEC calculates the spectrum resulting from gyrosynchrotron
c   radiation from a variety of electron energy distribution functions:
c
c      MON = monoenergetic (not implemented)
c      THM = thermal
c      PLW = power-law
c      DPL = double power-law
c      TNT = thermal/nonthermal
c      KAP = kappa function
c      PLP = power-law over momentum
c	   PLG = power-law over Lorentz-factor
c	   TNP = thermal/nonthermal with power-law tail over momentum
c	   TNG = thermal/nonthermal with power-law tail over Lorentz-factor
c
c   The relevant paramters are passed through common.
c   I've not put in support of cos(theta)=0.0 yet.
c
c   This version integrates over energy using Gaussian quadrature
c   for thermal distributions. I'll put in a more efficient integration
c   scheme for power-laws later.
c----------------------------------------------------------------------
c
      subroutine 
     &calc_gs_spec_FK(frq,tauo,taux,em1,em2,ab1,ab2,an1,an2,ath1,ath2)
      Implicit Real*8(A-H,O-Z)
c     &ath2,ffb,ffp)
      !Implicit Real*8(A-H,O-Z)
      character*3 ftype
      byte esc
      integer screen, dtype, INE
      real*8 kb, anPar(4), x(32), w(32), xk(128), wk(128)
      real*8 kappa !f(100),
      double precision emx(100), emo(100), abx(100), abo(100), 
     &   phix(100), phio(100), phii(100), phiv(100), phiq(100)
c      double precision emx(300), emo(300), abx(300), abo(300), 
c     &   phix(300), phio(300), phii(300), phiv(300), phiq(300)     
      data au /1.49599d13/   !Astronomical unit, cm
      data asc2rd /2.0626481d5/	!??
      data ech /4.80288d-10/	!electron charge
      data ems /9.1084d-28/	!electron mass
      data kb /1.38066d-16/	!Boltzman constant
      data pi /3.141592654d0/	!pi-number
      data c /2.997925d10/	!speed of light
      data esc, screen /27, 6/
      common /parms/ area, dep, enth, enrl, bmag, theta, 
     &   E0, E1, E2, delta1, delta2, EBR, temp, eps, kappa, etr, ftype
c     common /gs/ emx, emo, abx, abo, phix, phio, phii, phiv, phiq, sang !, f
      common /gs/ emx, emo, abx, abo, phix, phio, phii, phiv, phiq, sang  
      common /AngNorm/TotMu
      	common /AnPar/AnPar
      	common /NType/nType 
      	common/Integral/IKE
!     	      		common /NType/IAType 


      common/thermal/gf,cs,kb,c,ffbExp,ffpExp
c----------------------------------------------------------------------
      sang=area/au/au			!solid angle of the source as seen from Earth
      cs=cos(theta*pi/180.0)	!cos of view-angle theta, theta - in degrees
      ss=sqrt(1.-cs*cs)		!sin of view-angle theta, theta - in degrees
      er=kb*temp/ems/c/c		!kT/mc^2
      gtr=etr/0.511+1.		!Lorenz-factor of ???
      entot=enrl+enth			!total number of thermal and rel. electrons
      if (ftype.eq.'MON'.or.ftype.eq.'mon') dtype=1
      if (ftype.eq.'THM'.or.ftype.eq.'thm') dtype=2
      if (ftype.eq.'PLW'.or.ftype.eq.'plw') dtype=3
      if (ftype.eq.'DPL'.or.ftype.eq.'dpl') dtype=4
		     if (ftype.eq.'TNT'.or.ftype.eq.'tnt') dtype=5
      if (ftype.eq.'KAP'.or.ftype.eq.'kap') dtype=6
		     if (ftype.eq.'PLP'.or.ftype.eq.'plp') dtype=7
		     if (ftype.eq.'PLG'.or.ftype.eq.'plg') dtype=8
		     if (ftype.eq.'TNP'.or.ftype.eq.'tnp') dtype=9
		     if (ftype.eq.'TNG'.or.ftype.eq.'tng') dtype=10

            TotMu=AnNorm(anPar,nType)
            XEnStep=1.*IKe
            !Open(11,File='Anorm.txt')
             !     Write(11,*)'AnNorm=',TotMu,nType
              !    close(11)
	
c      -----------------------------------------------
c ----- Set some parameters needed for thermal models,
c       including weights and abscissas for 32 point
c       Gaussian quadrature
c      -----------------------------------------------
      if (ftype.eq.'TNT'.or.ftype.eq.'tnt') then 
         !dtype=5
         pth2=er*(2.+ er)
CC!        gamcr=sqrt(1.+pth2/(eps-pth2*(1.-eps))) this expression is potentially divergent!!!
CC! expansion gives:  gamcr=sqrt(1.+pth2/eps*(1+pth2*(1.-eps)/eps)); try simper form:
		gamcr=sqrt(1.+pth2/eps)
		 E1=(gamcr-1.)*0.511
        ! enrl=4.0*pi*enth*(gamcr-1.)*fth(gamcr,er)/(delta1-1.)
        !anor=enrl*(delta1-1.)/(E1**(1.-delta1)-E2**(1.-delta1))
      enrl=enth*(gamcr-1.)*fth(gamcr,er)/(delta1-1.)*
     &(1.-(E2/E1)**(1.-delta1))
         entot=enth+enrl
        
         endif

		     if (ftype.eq.'TNP'.or.ftype.eq.'tnp') then 
         !dtype=5
				pth2=er*(2.+ er)
				deltap=delta1
CC!        gamcr=sqrt(1.+pth2/(eps-pth2*(1.-eps))) this expression is potentially divergent!!!
CC! expansion gives:  gamcr=sqrt(1.+pth2/eps*(1+pth2*(1.-eps)/eps)); try simper form:
		gamcr=sqrt(1.+pth2/eps)
		
		XE1=sqrt(gamcr**2-1.)
	  XE2=sqrt((E2/0.511+1.)**2-1.)
C test	Total=0

        
        ! enrl=4.0*pi*enth*(gamcr-1.)*fth(gamcr,er)/(delta1-1.)
	 !enrl=enth*(gamcr**2-1.)**1.5*fth(gamcr,er)/(deltap-1.)
	 
		      enrl=enth*fth(gamcr,er)/(deltap-3.)/gamcr*(gamcr**2-1.)*
     &(1.-(XE2/XE1)**(3.-deltap)) 
         entot=enth+enrl
         E1=(gamcr-1.)*0.511
         endif

		     if (ftype.eq.'TNG'.or.ftype.eq.'tng') then 
        
				pth2=er*(2.+ er)
				deltag=delta1
CC!        gamcr=sqrt(1.+pth2/(eps-pth2*(1.-eps))) this expression is potentially divergent!!!
CC! expansion gives:  gamcr=sqrt(1.+pth2/eps*(1+pth2*(1.-eps)/eps)); try simper form:
		gamcr=sqrt(1.+pth2/eps)
		 E1=(gamcr-1.)*0.511
        ! enrl=4.0*pi*enth*(gamcr-1.)*fth(gamcr,er)/(delta1-1.)
        gamm2=E2/0.511+1.
        
	 	    enrl=enth*gamcr*fth(gamcr,er)/(deltag-1.)*
     &(1.-(gamm2/gamcr)**(1.-deltag))
         entot=enth+enrl
        
         endif

CCC	write(*,*)'gammma_max=',E1
      if (ftype.eq.'THM'.or.ftype.eq.'thm'.or.
     &	ftype.eq.'TNG'.or.ftype.eq.'tng'.or.
     &	ftype.eq.'TNP'.or.ftype.eq.'tnp'.or.
     &    ftype.eq.'TNT'.or.ftype.eq.'tnt') then 
         pth2=er*(2.+ er)
         gamth=sqrt(1.+pth2)
         a=1.+1.e-6
         b=1.+35.*(gamth-1.)
		if (ftype.eq.'TNT'.or.ftype.eq.'tnt'.or.
     &	ftype.eq.'TNG'.or.ftype.eq.'tng'.or.
     &	ftype.eq.'TNP'.or.ftype.eq.'tnp') b=gamcr 
		!GF added to avoid double 
	!calculation of the integrals over the same energy range with thermal and 
	!nonthermal particles. The same change is made in DisFun Subroutine.
         xm=0.5*(b+a)
         xr=0.5*(b-a)
         call gauleg(a,b,x,w,32)
         endif
         
         
      if (ftype.eq.'KAP'.or.ftype.eq.'kap') then 
         !E1=(kappa-1.5)*kb*temp/1.60207e-6
         E1=4.*kb*temp/1.60207e-6
         gamk=E1/0.511+1.
         a=1.+1.e-6
         b=gamk
         xm=0.5*(b+a)
         xr=0.5*(b-a)
         call gauleg(a,b,x,w,32)

C--------- Calculation of the normalization constant for Kappa Distribution
      e1l=dlog10(E1)
      e2l=dlog10(E2)
			Tot=0
         do 3640 j=1,32
           
            Tot=Tot+w(j)*fkNorm(x(j),er,kappa) !/0.511

		!	Ekin=(x(j)-1)*0.511
             
C         write (*,*) 'Tfrac=',total,enth
		 
 3640     continue
         einc=(e2l-e1l)/XEnStep
         do 3660 j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
           
              Tot=Tot+de*fkNorm(gamma,er,kappa)

C	          write (*,*) 'Total=',total,enth
	
			  
 3660        continue
				Akap=1./Tot

C--------- Calculation of the normalization constant for Kappa Distribution - End
         endif


      gf=ech*bmag/2./pi/ems/c
      pf=sqrt(ech*ech*entot/pi/ems)
      alpha=1.5*gf/pf
      ffp=pf/gf
      ffpExp=ffp
CCC	write(*,*) ffp
      e1l=dlog10(E1)
      e2l=dlog10(E2)
      c1=ech**3/ems/c/c
      c2=ech*(2.*pi)**2
c      ----------------------------------------------
c ----- evaluate emission and absorption coefficients
c       for  100 frequencies between 10 MHz and 1 GHz
c      ----------------------------------------------
 !     do 2000 i=1,100
  !       frq = 10.**((i-1)*0.02+9.0)  !+9.0 means f> 1 GHz in place of 10 MHz (+7.0)
         f = frq
         ffb = frq/gf
         ffbExp=ffb
       !  i10 = 10*(i/10)
        ! if (i.eq.i10) write(6,fmt=3000) esc,esc,frq
c      ------------------------------------------------
c ----- Compute refractive index and pol'n coefficients
c      ------------------------------------------------
         call refr(ffb,ffp,cs,an1,an2,ath1,ath2)
	     If(an2.le.0) an2=1e-8	!GF: to avoid divergence when GSE is calculated
          If(an1.le.0) an1=1e-8	!GF: to avoid divergence when GSE is calculated
         em1=0.
         em2=0.
         ab1=0.
         ab2=0.
         
c      ------------------------------------------------
c ----- Branch to relevant electron energy distribution
c      ------------------------------------------------
         goto (100,200,300,400,500,600,700,800,900,1900) dtype
c      ---------------------------
c ----- Monoenergetic distribution
c      ---------------------------
 100     g1=0.
         g2=0.
         do 150 ll=1,100
            ee=E0+(ll-1)*0.01
            gamma=ee/0.511+1.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               call gsy(gamma,ffb,cs,g1,an,ath1)
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               call gsy(gamma,ffb,cs,g2,an,ath2)
               endif
            em1=g1*bmag*c1
            em2=g2*bmag*c1
            ab1=g1/ffb/ffb/bmag*c2
            ab2=g2/ffb/ffb/bmag*c2
            tauo=enrl*dep*ab1
            taux=enrl*dep*ab2
c           write (11,*) ee,em1,em2,ab1,ab2
 150        continue
         goto 1000
c      ---------------------
c ----- Thermal distribution
c      --------------------- 
 200     do 250 j=1,32
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g1,an,ath1)
               Else
               call gsyA(x(j),ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g2,an,ath2)
               Else
               call gsyA(x(j),ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
            fthm=fth(x(j),er)
            em1=em1+w(j)*g1*fthm*bmag*c1
            em2=em2+w(j)*g2*fthm*bmag*c1
 250        continue
                !Open(11,File='AType.txt')
                 ! Write(11,*)'AType=',nType
                  !close(11)
c      -------------------
c ----- Use Kirchoff's law
c      -------------------
         ab1=em1/(an1*kb*temp*frq*frq/c/c)
         ab2=em2/(an2*kb*temp*frq*frq/c/c)
         tauo=enth*dep*ab1
         taux=enth*dep*ab2
         goto 1000
c      -----------------------
c ----- Power-law distribution
c      -----------------------
       
 300             einc=(e2l-e1l)/XEnStep !(1.*INE) !100. !EnStep !100.
         
c      -----------------------------------------------------------
c ----- enrl represents the TOTAL number density of fast electrons
c       between E1 and E2; anor is the number  per cm**3
c      -----------------------------------------------------------
         anor=enrl*(delta1-1.)/(E1**(1.-delta1)-E2**(1.-delta1))
         	beta=0.5 !sqrt(gamma*gamma-1.)/gamma
	     cpa01=0.5
	     gg1=1.
	     ggd11=0.
	     cpa02=0.5
	     gg2=1.
	     ggd12=0.
        ! do 350 j=1,INE !100
        !IKE=32 !100
       ! Open(9,File='spectrum.dat')
	!Do j=1,100
		!		Write(9,*)Ike,XEnStep
         do j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.

            if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
                If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
C GF						write(*,*) 'Start',an1,an2
            dem1=g1*em**(-delta1)*de*bmag*c1
            dem2=g2*em**(-delta1)*de*bmag*c1
            dab1=g1*em**(-delta1)*de/ffb/ffb/bmag*c2/an1
     &             *((delta1*gamma*(gamma+1.)+2.*gamma*gamma-1.)/gamma
     &/(gamma**2.-1.) -(ann1*beta*cs-cpa01)*ggd11/gg1/beta/beta/gamma)
            dab2=g2*em**(-delta1)*de/ffb/ffb/bmag*c2/an2
     &             *((delta1*gamma*(gamma+1.)+2.*gamma*gamma-1.)/gamma
     &/(gamma**2.-1.) -(ann2*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma)

            em1=em1+dem1
            em2=em2+dem2
            ab1=ab1+dab1
            ab2=ab2+dab2
c      -------------------------------------------
c ----- An aside .... cumul. contributions to flux
c      -------------------------------------------
c            s=frq/(2.8e6*bmag)
c            ex=3.3e-24*(10.**(-0.52*delta1))*bmag*enrl*
c     &         (ss**(-0.43+0.65*delta1))*(s**(1.22-0.9*delta1))
c            tx=2.2e9*(10.**(-0.31*delta1))*(ss**(-0.36-0.06*delta1))
c     &         *(s**(0.5+0.085*delta1))
c            emn=1.38e-16*tx/1.602e-9
            tauo=anor*dep*ab1
            pho=em1/ab1*(1.-exp(-tauo))
            taux=anor*dep*ab2
            phx=em2/ab2*(1.-exp(-taux))
c            write (11,*) s,em,pho,phx
c---------------------------------------------------
            End Do
 350        continue
         tauo=anor*dep*ab1
         taux=anor*dep*ab2
         goto 1000


c      -----------------------
c ----- Power-law distribution over momentum, PLP, tested, works well
c      -----------------------
 700     einc=(e2l-e1l)/XEnStep !100.

 		deltap=delta1
	     XE1=sqrt((E1/0.511+1.)**2-1.)
	     XE2=sqrt((E2/0.511+1.)**2-1.)
C test	Total=0

        anorp=enrl*(deltap-3.)/(XE1**(3.-deltap)-XE2**(3.-deltap))/0.511

c      -----------------------------------------------------------
c ----- enrl represents the TOTAL number density of fast electrons
c       between E1 and E2; anor is the number  per cm**3
c      -----------------------------------------------------------
         !anor=enrl*(delta1-1.)/(E1**(1.-delta1)-E2**(1.-delta1))
         do 750 j=1,IKe !100
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.

            if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               gg1=1.
               ggd11=0
               cpa01=0.5
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               gg2=1.
               ggd12=0
               cpa02=0.5
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
C GF						write(*,*) 'Start',an1,an2
            dem1=g1*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*de*bmag*c1
            dem2=g2*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*de*bmag*c1
            dab1=g1*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*de/ffb/ffb/
     &    bmag*c2/an1*(deltap*gamma/(gamma**2.-1.)
     &-(ann1*beta*cs-cpa01)*ggd11/gg1/beta/beta/gamma)   
!!     &    	*(delta1*gamma*(gamma+1.)+2.*gamma*gamma-1.)
!!     &		/gamma/(gamma**2.-1.)
            dab2=g2*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*de/ffb/ffb/
     &    bmag*c2/an2*(deltap*gamma/(gamma**2.-1.)
     &-(ann2*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma) 
!!     &    *(delta1*gamma*(gamma+1.)+2.*gamma*gamma-1.)
!!     &             /gamma/(gamma**2.-1.)
            em1=em1+dem1
            em2=em2+dem2
            ab1=ab1+dab1
            ab2=ab2+dab2

c---------------------------------------------------
 750        continue
         tauo=anorp*dep*ab1
         taux=anorp*dep*ab2
         goto 1000

c      -----------------------
c ----- Power-law distribution over lorenz-factor gamma, PLG,  done
c      -----------------------
 800     einc=(e2l-e1l)/XEnStep

c      -----------------------------------------------------------
c ----- enrl represents the TOTAL number density of fast electrons
c       between E1 and E2; anor is the number  per cm**3
c      -----------------------------------------------------------

		deltg=delta1
	  gamm1=E1/0.511+1.
	  gamm2=E2/0.511+1.
C test	Total=0

       anorg=enrl*(deltg-1.)/(gamm1**(1.-deltg)-gamm2**(1.-deltg))/0.511
         !anor=enrl*(delta1-1.)/(E1**(1.-delta1)-E2**(1.-delta1))
         do 850 j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.

            if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               gg1=1.
               ggd11=0
               cpa01=0.5
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               gg2=1.
               ggd12=0
               cpa02=0.5
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
C GF						write(*,*) 'Start',an1,an2
            dem1=g1*gamma**(-deltg)*de*bmag*c1
            dem2=g2*gamma**(-deltg)*de*bmag*c1
            dab1=g1*gamma**(-deltg)*de/ffb/ffb/bmag*c2/an1
     &            *(((deltg+2)*(gamma*gamma-1.)+1.)/gamma/(gamma**2.-1.)
     &              -(ann1*beta*cs-cpa01)*ggd11/gg1/beta/beta/gamma)
            dab2=g2*gamma**(-deltg)*de/ffb/ffb/bmag*c2/an2
     &            *(((deltg+2)*(gamma*gamma-1.)+1.)/gamma/(gamma**2.-1.)
     &             -(ann2*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma)
            em1=em1+dem1
            em2=em2+dem2
            ab1=ab1+dab1
            ab2=ab2+dab2

c---------------------------------------------------
 850        continue
         tauo=anorg*dep*ab1
         taux=anorg*dep*ab2
         goto 1000




c      ------------------------------
c ----- Double power-law distribution
c      ------------------------------
 400     einc=(e2l-e1l)/XEnStep !50.
c
c ----- The normalization is kind of funny. One possibility is:
c
c         anor=enrl/(E1**(1.-delta1)/(delta1-1.)
c     &             -EBR**(1.-delta1)/(delta1-1.)
c     &             +EBR**(1.-delta1)/(delta2-1.)
c     &             -EBR**(delta2-delta1)*E2**(1.-delta2)/(delta2-1.)) 
c
c       which again takes enrl to be the total number of fast 
c       electrons between E1 and E2. But since I'm more likely to
c       be interested in departures from a single power-law due to
c       a high-energy component breaking up or down, I'll instead
c       use:
c
         anor1=enrl*(delta1-1.)/(E1**(1.-delta1)-E2**(1.-delta1))
         anor2=anor1*EBR**(delta2-delta1)
         enrlDPL=anor1*(E1**(1.-delta1)-EBR**(1.-delta1))/(delta1-1.)
     &       +anor2*(EBR**(1.-delta2)-E2**(1.-delta2))/(delta2-1.)
         anor=anor1
         delta=delta1
C test	  write (*,*) 'Total=',total,enrl
         do 450 j=1,IKe !50
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            if (em.ge.EBR) then
               anor=anor2
               delta=delta2
               endif
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.
             if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               gg1=1.
               ggd11=0
               cpa01=0.5
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               gg2=1.
               ggd12=0
               cpa02=0.5
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
            em1=em1+g1*em**(-delta)*de*bmag*c1*anor
            em2=em2+g2*em**(-delta)*de*bmag*c1*anor
            ab1=ab1+g1*em**(-delta)*de/ffb/ffb/bmag*c2*anor*
     &  ((delta*gamma*(gamma+1.)+2.*gamma*gamma-1.)/gamma/(gamma**2.-1.)
     &   -(ann1*beta*cs-cpa01)*ggd11/gg1/beta/beta/gamma)/an1
            ab2=ab2+g2*em**(-delta)*de/ffb/ffb/bmag*c2*anor*
     &  ((delta*gamma*(gamma+1.)+2.*gamma*gamma-1.)/gamma/(gamma**2.-1.)
     &   -(ann2*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma)/an2
 450        continue
         tauo=dep*ab1
         taux=dep*ab2
         goto 1000
c      --------------------------------
c ----- Thermal/nonthermal distribution
c      --------------------------------
 500     einc=(e2l-e1l)/XEnStep
         anor=enrl*(delta1-1)/(E1**(1.-delta1)-E2**(1.-delta1))
         do 525 j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               gg1=1.
               ggd11=0
               cpa01=0.5
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               gg2=1.
               ggd12=0
               cpa02=0.5
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
            em1=em1+g1*em**(-delta1)*de*bmag*c1*anor
            em2=em2+g2*em**(-delta1)*de*bmag*c1*anor
            ab1=ab1+g1*em**(-delta1)*de/ffb/ffb/bmag*c2/an1*anor*
     & ((delta1*gamma*(gamma+1.)+2.*gamma*gamma-1.)/gamma/(gamma**2.-1.)
     &          -(ann1*beta*cs-cpa01)*ggd11/gg1/beta/beta/gamma)
            ab2=ab2+g2*em**(-delta1)*de/ffb/ffb/bmag*c2/an2*anor*
     & ((delta1*gamma*(gamma+1.)+2.*gamma*gamma-1.)/gamma/(gamma**2.-1.)
     &         -(ann2*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma)
 525     continue
         do 550 j=1,32
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g1,an,ath1)
               Else
               call gsyA(x(j),ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g2,an,ath2)
               Else
               call gsyA(x(j),ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
            fthm=fth(x(j),er)
            eth1=w(j)*g1*fthm*bmag*c1*enth
            eth2=w(j)*g2*fthm*bmag*c1*enth
            em1=em1+eth1
            em2=em2+eth2
            ab1=ab1+eth1/(an1*kb*temp*frq*frq/c/c)
            ab2=ab2+eth2/(an2*kb*temp*frq*frq/c/c)
 550        continue
         tauo=dep*ab1
         taux=dep*ab2
         goto 1000

c      --------------------------------
c ----- Thermal/nonthermal distribution; TNP: NT - power-law over momentum,  done 
c      --------------------------------
 900     einc=(e2l-e1l)/XEnStep
		deltap=delta1
	     XE1=sqrt((E1/0.511+1.)**2-1.)
	     XE2=sqrt((E2/0.511+1.)**2-1.)
C test	Total=0

        anorp=enrl*(deltap-3.)/(XE1**(3.-deltap)-XE2**(3.-deltap))/0.511
        

         !anor=enrl*(delta1-1)/(E1**(1.-delta1)-E2**(1.-delta1))
         do 925 j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               gg1=1.
               ggd11=0
               cpa01=0.5
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               gg2=1.
               ggd12=0
               cpa02=0.5
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
       em1=em1+g1*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*anorp*de*bmag*c1
       em2=em2+g2*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*anorp*de*bmag*c1

           ab1=ab1+g1*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*anorp*de/
     & ffb/ffb/bmag*c2*(deltap*gamma/(gamma**2.-1.)-(ann1*beta*cs-cpa01)
     &*ggd11/gg1/beta/beta/gamma)/an1
Cc     &*ggd11/gg1)/sqrt(an1)
           ab2=ab2+g2*gamma*(gamma**2-1.)**(-deltap/2.+0.5)*anorp*de/
     & ffb/ffb/bmag*c2*(deltap*gamma/(gamma**2.-1.)-(ann2*beta*cs-cpa02)
     &*ggd12/gg2/beta/beta/gamma)/an2    
c     &*ggd12/gg2)/sqrt(an2)
 925     continue
         do 950 j=1,32
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g1,an,ath1)
               Else
               call gsyA(x(j),ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g2,an,ath2)
               Else
               call gsyA(x(j),ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
            fthm=fth(x(j),er)
            eth1=w(j)*g1*fthm*bmag*c1*enth
            eth2=w(j)*g2*fthm*bmag*c1*enth
            em1=em1+eth1
            em2=em2+eth2
            ab1=ab1+eth1/(an1*kb*temp*frq*frq/c/c)
            ab2=ab2+eth2/(an1*kb*temp*frq*frq/c/c)
 950        continue
         tauo=dep*ab1
         taux=dep*ab2
         goto 1000

c      --------------------------------
c ----- Thermal/nonthermal distribution; TNG: NT - power-law over GAMMA, not done yet!
c      --------------------------------
 1900     einc=(e2l-e1l)/XEnStep
	
		deltg=delta1
	  gamm1=E1/0.511+1.
	  gamm2=E2/0.511+1.
C test	Total=0

       anorg=enrl*(deltg-1.)/(gamm1**(1.-deltg)-gamm2**(1.-deltg))/0.511
	 

         !anor=enrl*(delta1-1)/(E1**(1.-delta1)-E2**(1.-delta1))
         do 1925 j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               gg1=1.
               ggd11=0
               cpa01=0.5
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               gg2=1.
               ggd12=0
               cpa02=0.5
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
       em1=em1+g1*gamma**(-deltg)*anorg*de*bmag*c1
       em2=em2+g2*gamma**(-deltg)*anorg*de*bmag*c1

           ab1=ab1+g1*gamma**(-deltg)*anorg*de/
     & ffb/ffb/bmag*c2*(((deltg+2)*(gamma*gamma-1.)+1.)/gamma/
     & (gamma**2.-1.)-(ann1*beta*cs-cpa01)*ggd11/gg1/beta/beta/gamma)
     &/an1
           ab2=ab2+g2*gamma**(-deltg)*anorg*de/
     &      ffb/ffb/bmag*c2*(((deltg+2)*(gamma*gamma-1.)+1.)
     &   /gamma/(gamma**2.-1.)-(ann2*beta*cs-cpa02)*ggd12/gg2
     &/beta/beta/gamma)/an2
 1925     continue
         do 1950 j=1,32
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g1,an,ath1)
               Else
               call gsyA(x(j),ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g2,an,ath2)
               Else
               call gsyA(x(j),ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
            fthm=fth(x(j),er)
            eth1=w(j)*g1*fthm*bmag*c1*enth
            eth2=w(j)*g2*fthm*bmag*c1*enth
            em1=em1+eth1
            em2=em2+eth2
            ab1=ab1+eth1/(an1*kb*temp*frq*frq/c/c)
            ab2=ab2+eth2/(an2*kb*temp*frq*frq/c/c)
 1950        continue
         tauo=dep*ab1
         taux=dep*ab2
         goto 1000




c      -------------------
c ----- Kappa distribution 
Cc	2008 March 03, GF changed the normalization of the kappa distribution to one numerically 
Cc	determined with help of new function fkNorm(). The normalization constant (Anorm) is 
Cc	calculated in the beginning of the program and the same block is inserted in the DisFun 
Cc	function. Now the total number of electrons equals to the number of thermal electrons, enth.
Cc	Normalization works well in DisFun program, but has yet to be checked in Calc_GS_Spec.
Cc	It has been checked: working properly.
c      -------------------
 600     fr=0.0
         do 620 j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            fkap=fk1(gamma,er,kappa,Akap)/0.511 
            !/0.511 added on March 12, 2009,
            !to match the normalization over energy E to compare
            !with the Kuznetsov DLL !fk(gamma,er,kappa)
	  !  fkap=fk(gamma,er,kappa)
            fr=fr+fkap*de
 620     continue
         enrl=fr*enth
         do 640 j=1,32
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g1,an,ath1)
               Else
               call gsyA(x(j),ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(x(j),ffb,cs,g2,an,ath2)
               Else
               call gsyA(x(j),ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
             
		  fkap=fk1(gamma,er,kappa,Akap)/0.511 
            !/0.511 added on March 12, 2009,
            !to match the normalization over energy E to compare
            !with the Kuznetsov DLL !fk(x(j),er,kappa)
		!    fkap=fk(gamma,er,kappa)

		  em1=em1+w(j)*g1*fkap*bmag*c1
            em2=em2+w(j)*g2*fkap*bmag*c1
            ab1=ab1+g1*fkap*w(j)/ffb/ffb/bmag*c2*
     &   ((kappa+1.)/(kappa-1.5)/er/(1.+(x(j)-1.)/er/(kappa-1.5))
     &-(ann1*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma)

            ab2=ab2+g2*fkap*w(j)/ffb/ffb/bmag*c2*
     &   ((kappa+1.)/(kappa-1.5)/er/(1.+(x(j)-1.)/er/(kappa-1.5))
     &-(ann2*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma)

 640     continue
         einc=(e2l-e1l)/XEnStep
         do 660 j=1,IKe
            el=10.**(e1l+(j-1)*einc)
            eu=10.**(e1l+j*einc)
            em=10.**(e1l+(j-0.5)*einc)
            de=eu-el
            gamma=em/0.511+1.
            g1=0.
            g2=0.
            if (ffb.gt.ffp) then
               an=sqrt(an1)
               ann1=sqrt(an1)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g1,an,ath1)
               gg1=1.
               ggd11=0
               cpa01=0.5
               Else
               call gsyA(gamma,ffb,cs,g1,an,ath1,cpa01,beta,gg1,ggd11)
               End If
               endif
            if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then
               an=sqrt(an2)
               ann2=sqrt(an2)
               If(nType.EQ.0) then
               call gsy(gamma,ffb,cs,g2,an,ath2)
               gg2=1.
               ggd12=0
               cpa02=0.5
               Else
               call gsyA(gamma,ffb,cs,g2,an,ath2,cpa02,beta,gg2,ggd12)
               End If
             endif
            fkap=fk1(gamma,er,kappa,Akap)/0.511 
            !/0.511 added on March 12, 2009,
            !to match the normalization over energy E to compare
            !with the Kuznetsov DLL ! !fk(gamma,er,kappa)
		!	    fkap=fk(gamma,er,kappa)

            em1=em1+de*g1*fkap*bmag*c1
            em2=em2+de*g2*fkap*bmag*c1
            ab1=ab1+g1*fkap*de/ffb/ffb/bmag*c2/an1* !sqrt(an1)*!low frequency test
     &   ((kappa+1.)/(kappa-1.5)/er/(1.+(gamma-1.)/er/(kappa-1.5))
     &-(ann1*beta*cs-cpa01)*ggd11/gg1/beta/beta/gamma)
            ab2=ab2+g2*fkap*de/ffb/ffb/bmag*c2/an2* !sqrt(an2)*!low frequency test
     &   ((kappa+1.)/(kappa-1.5)/er/(1.+(gamma-1.)/er/(kappa-1.5))
     &-(ann2*beta*cs-cpa02)*ggd12/gg2/beta/beta/gamma)
 660        continue
         tauo=enth*dep*ab1
         taux=enth*dep*ab2
         goto 1000
c      -------------------------------
c ----- End of integration over energy
c      -------------------------------
 1000    continue
 2000    continue
 
 
              !Adding free-free contribution
            t32=Temp**1.5

                      phioT=0d0
                        phixT=0d0
                        tfo=0
                        abtho=0
                        emtho=0
          
                          tfo=1.
                     if (ffb.gt.ffp) then !.AND.ab1.gt.1.e-30
                     
          abtho=9.786e-3*enth*enth*(24.573+dlog(Temp/frq))/t32/
     &     (frq)**2/sqrt(an1)
         emtho=an1*abtho*kb*Temp*frq*frq/c/c  
         em1=anor*em1+emtho
         ab1=anor*ab1+abtho

         tauo=tauo+dep*abtho
         
C         if (tauo.gt.1.e-35) then
C       !  if (tauo.gt.0) then
C         em1=tauo/dep/ab1*em1+emtho
C        ab1=tauo/dep+abtho
C        else
         
C  
C                          Open(3,File='Abso_DATA.dat')
C     write(3,*) em1, ab1,tauo,dep,abtho
C	     print*,'em1, ab1,tauo,dep,abtho=',em1, ab1,tauo,dep,abtho
C
C	!Pause
C		close(3)    
C                Pause
C         EndIf
                     
                       EndIf
      
                        tfx=1.
      if (ffb.gt.(sqrt(ffp**2+0.25)+0.5)) then    !.AND.ab2.gt.1.e-30
                    
         abthx=9.786e-3*enth*enth*(24.573+dlog(Temp/frq))/t32/
     &     (frq)**2/sqrt(an2)
         emthx=an2*abthx*kb*Temp*frq*frq/c/c 
         em2=anor*em2+emthx
         ab2=anor*ab2+abthx
         
         taux=taux+dep*abthx
C        
C                   if (taux.gt.1.e-35) then
C        !           if (taux.gt.0) then
C        em2=taux/dep/ab2*em2+emthx
C        ab2=taux/dep+abthx 
C        else
         
C                          Open(3,File='AbsX_DATA.dat')
C     write(3,*) em2, ab2,taux,dep,abthx
C	     print*,' em2, ab2,taux,dep,abthx=', em2, ab2,taux,dep,abthx
C
C	!Pause
C		close(3)    
C                Pause
C         EndIf
                            
        
                    EndIf
C                   
C                          Open(3,File='Test_T_DATA.dat')
C     write(3,*) Temp,enth,frq,kb,c,em1,emtho,ab1,abtho,tauo,dep*abtho
C	     print*,'Temp, frq,em1, emtho=',Temp, frq,em1, emtho
C	!Pause
C		close(3)



 3000 format(/1a,16h[21;1HFrq=      ,1a,6h[21;5H,1pe8.2,$)
      return
      end

      function fth(g,r)
      Implicit Real*8(A-H,O-Z)
      data pi /3.1415926d0/
      a = sqrt(2./pi)/(r**1.5)/(1.+15.*r/8.)
      fth = a*g*sqrt(g**2.-1.)*exp(-(g-1.)/r)
      return
      end
c----------------------------------------------------------------------
c	http://www.lesia.obspm.fr/~moncuque/theseweb/tempioweb/node12.html - kappa distribution

      function fk(g,r,kappa)
      Implicit Real*8(A-H,O-Z)
   !   external gammln
      real*8 kappa
      data pi /3.1415926d0/
      g1 = dexp(gammln(kappa+1.d0))
      g2 = exp(gammln(kappa-0.5))
      ak = g1/g2/(kappa-1.5)**1.5
      a = sqrt(2./pi)*ak*g*sqrt(g*g-1.)/(r**1.5)
      fk = a/(1.+(g-1.)/r/(kappa-1.5))**(kappa+1.)
      return
      end

c----------------------------------------------------------------------
c	http://www.lesia.obspm.fr/~moncuque/theseweb/tempioweb/node12.html - kappa distribution

      function fk1(g,r,kappa,Anorm)
      Implicit Real*8(A-H,O-Z)
      real*8 kappa
      data pi /3.1415926d0/
      
      a = Anorm*g*sqrt(g*g-1.)/(r**1.5)
      fk1 = a/(1.+(g-1.)/r/(kappa-1.5))**(kappa+1.)
      return
      end
c---------------------------------------------------------------------

      function fkNorm(g,r,kappa)
      Implicit Real*8(A-H,O-Z)
      real*8 kappa
      data pi /3.1415926/
      
      a = g*sqrt(g*g-1.)/(r**1.5)
      fkNorm = a/(1.+(g-1.)/r/(kappa-1.5))**(kappa+1.)
      return
      end
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
      subroutine refr(ffb,ffp,cs,an1,an2,ath1,ath2)
      Implicit Real*8(A-H,O-Z)
      !Implicit Real*8(A-H,O-Z)
      !double precision dnum1, dnum2, dknum1, dknum2, anum, ss
      ss=sqrt(1.-cs*cs)
      anum=2.d0*ffp*ffp*(ffp*ffp-ffb*ffb)
      dnum1=+sqrt(ffb**4*ss**4+4.*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     2.d0*ffb**2*(ffp**2-ffb**2)-ffb**2*ss**2
      dnum2=-sqrt(ffb**4*ss**4+4.*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     2.d0*ffb**2*(ffp**2-ffb**2)-ffb**2*ss**2
      an1=1.+anum/dnum1
      an2=1.+anum/dnum2
      aknum=2.d0*ffb*(ffp*ffp-ffb*ffb)*cs
      dknum1=+sqrt(ffb**4*ss**4+4.d0*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     ffb**2*ss**2
      dknum2=-sqrt(ffb**4*ss**4+4.d0*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     ffb**2*ss**2
      ath1=-aknum/dknum1
      ath2=-aknum/dknum2
      return
      end
c------------------------------------------------------------------------
c
      subroutine gsy(gamma,ffb,cs,g12,an,ath)
      Implicit Real*8(A-H,O-Z)
            !double precision ath1, ath2
c
c  Using Klein's approx approach ...
c
      pi=3.1415926d0
      beta=sqrt(gamma*gamma-1.)/gamma
      ss=sqrt(1.-cs*cs)
      cpa0=an*beta*cs
      x0=an*beta*ss*sqrt(1.-cpa0*cpa0)/(1.-an*beta*cpa0*cs)
      s0=gamma*ffb*(1.-an*beta*cpa0*cs)
      eps=1./sqrt(1.-an*an*beta*beta)
      tau=eps*beta*an*ss
      call abf(x0,s0,a,b,f)
      c1=(-f*(1+tau*tau)+ath*cs)**2.
      c2=an*ss*ss*(1.+ath*ath)*a*a
      c3=(z(x0))**(2.*s0)/4./pi
      c4=sqrt(gamma)*((1.+tau*tau)**0.25)*eps**3.
      g12=sqrt(pi*ffb)*c1*c3/c2/c4
      return
      end

c------------------------------------------------------------------------
c
      subroutine gsyA(gamma,ffb,cs,g12,an,ath,cpa0,beta,gg,ggd1)
      Implicit Real*8(A-H,O-Z)
            !double precision Gs !, xMua, xMub, xMuc
           ! Implicit Real*8(A-H,O-Z)
                 real*8 kb
      	common /AngNorm/TotMu
      	common /AnPar/AnPar
      	common /NType/nType
      	common /Accuracy/nAcc      	
      	      common/thermal/gfArt,csArt,kb,c,ffbExp,ffpExp

      	Real*8 anPar(4)
CCCC		  Implicit Real*8(A-H,O-Z) 
c
c  Using Klein's approx approach ...
c
      pi=3.1415926d0
      beta=sqrt(gamma*gamma-1.)/gamma
      ss=sqrt(1.-cs*cs)
      ffp=ffpExp
      cpa00=an*beta*cs !mu_0 from Eq. (4a) Klein
		Gs=-((ffp/ffb)**2*ss/ffb-ffp**2/ffb**4*ath*cs*ss)/(
     &1-1./ffb/ffb-ffp**2/ffb**2*(1.-cs**2/ffb/ffb))

Cc		Included fragment to account for anisotropy; 06 Oct. 2009

		!TestCPA0=100. !! replace!!
			!xMuSt=0.1
		!	MuM=301
		aCorr=0
			      
               If(cs.ge.0) then   
              xMua= -1+1e-2 !-1+1e-2
              xMub=  1-1e-4 !-xMua !-1e-4
              else
               xMua= -1+1e-4 !-1+1e-4
              xMub=  1-1e-2  !1-1e-2
              End If 
              
             ! xMu2=xMua
              !xMu3=xMub
              
              
      !If(nAcc.ne.0) aCorr=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMua)
      !If(nAcc.ne.0) TestQu2=Qu(ath,Gs,an,beta,gamma,cs,ss,ffb,xMu2)
              xTest_a=an*beta*ss*sqrt(1.-xMua*xMua)/(1.-an*beta*xMua*cs)
      Brackets_a=dlog(z(xTest_a))-
     &(aCorr+dLnG(xMua,anPar,nType))/2./an/gamma/ffb/beta/cs
          TestMu_a=xMua/cpa00-1+(1-xMua*xMua)/sqrt(1-xTest_a*xTest_a)*Brackets_a
	!TestMu_2=TestMu_a
	
      !If(nAcc.ne.0) aCorr=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMub)
      !If(nAcc.ne.0) TestQu3=Qu(ath,Gs,an,beta,gamma,cs,ss,ffb,xMu3)
	        xTest_b=an*beta*ss*sqrt(1.-xMub*xMub)/(1.-an*beta*xMub*cs)
	     Brackets_b=dlog(z(xTest_b))-
     &(aCorr+dLnG(xMub,anPar,nType))/2./an/gamma/ffb/beta/cs
	     TestMu_b=xMub/cpa00-1+(1-xMub*xMub)/sqrt(1-xTest_b*xTest_b)*Brackets_b
	!TestMu_3=TestMu_b
            !If(TestMu_a*TestMu_b.gt.0) then
	         !Open(8,File='Test_Mu_Solu.txt')
                !  Write(8,*)'Solution not found',TestMu_a,TestMu_b
                 
        !End If
        
               
        
	
	          Ncount=0
	          epsMu=1e-3 !1e-3 !1e-4
	          If(epsMu.ge.AnPar(3)*AnPar(3)/30.and.nType.EQ.2) 
     &epsMu=AnPar(3)*AnPar(3)/30.
     	          If(epsMu.ge.AnPar(3)*AnPar(3)/30.and.nType.EQ.3) 
     &epsMu=AnPar(3)*AnPar(3)/30.
            	          If(epsMu.ge.AnPar(3)*AnPar(3)/30.and.nType.EQ.4) 
     &epsMu=AnPar(3)*AnPar(3)/30.
            	          If(epsMu.ge.AnPar(3)*AnPar(3)/30.and.nType.EQ.5) 
     &epsMu=AnPar(3)*AnPar(3)/30.
      !epsMu=AnPar(3)*AnPar(3)/30.
    
      
      
            xMuc=0
            aCorr=0
            
      !+++++++++++++++++++++++++++++++++++++++++++
                  Do While ((xMub-xMua).ge.epsMu.and.Ncount.lt.30) !.AND.ffb.gt.3)    
              
              xMuc=(xMub+xMua)/2.
              
Cc      If(nAcc.ne.0.AND.nCount.ge.0) 
Cc     &aCorr=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMuc)
	        xTest_c=an*beta*ss*sqrt(1.-xMuc*xMuc)/(1.-an*beta*xMuc*cs)
	     Brackets_c=dlog(z(xTest_c))-
     &(aCorr+dLnG(xMuc,anPar,nType))/2./an/gamma/ffb/beta/cs
	     TestMu_c=xMuc/cpa00-1+(1-xMuc*xMuc)/sqrt(1-xTest_c*xTest_c)*Brackets_c
      
	     If(TestMu_a*TestMu_c.lt.0) then
	     xMub=xMuc
	     TestMu_b=TestMu_c
      
	     else
	     xMua=xMuc
	     TestMu_a=TestMu_c
      
	     End if
	     !xMuRen=xMuc
	     !TestMuRen=TestMu_c
	     Ncount=Ncount+1
              !        Open(8,File='AMu30.txt')
            !      Write(8,*)'Mu=',xMuc,Ncount
         !         close(8)
	     End Do
Cc             Write(11,*)'Mu Counts',xMuc, Ncount

            cpa0=xMuc !xMu0
            
C========March 19 2010===Test with finding mu root with perturbation theory=======================      
Cc Idea: h'(mu)=h'_0(mu) + (lnQ(mu))'= 0 is solved via iterations:
Cc       h'(mu_0+deMu)=h'_0(mu_0)+deMu*h''_0(mu_0) + (lnQ(mu_0))'= 0
Cc       deMu=-(lnQ(mu_0))'/h''_0(mu_0)
      If(nAcc.gt.0) then
      
Cc Calculate second derivative h'' at xMuc

	
	     x0=an*beta*ss*sqrt(1.-cpa0*cpa0)/(1.-an*beta*cpa0*cs)
          s0=gamma*ffb*(1.-an*beta*cpa0*cs)

	     gg=0
		    ggd1=0
		    ggd2=0
	     call ggMu(cpa0,anPar,gg0,gg1,gg2)
      
	     gg=gg0
		ggd1=gg1
		ggd2=gg2

		Su1=(ggd2*gg-ggd1*ggd1)/gg/gg
	     Fa1=2*gamma*ffb*an*beta*cs/(1-cpa0**2)
		Su2=sqrt(1.-x0*x0)/an/beta/cs
			Su3=2*cpa0*sqrt(1.-x0*x0)/(1-cpa0**2)*(cpa0/an/beta/cs-1.)
			Su4=(an*beta*ss)**2*(cpa0-an*beta*cs)/sqrt(1.-x0*x0)/
     &(1-an*beta*cpa0*cs)**3*(cpa0/an/beta/cs-1.)
				Su5=(cpa0-an*beta*cs)/(1-an*beta*cpa0*cs)*sqrt(1.-x0*x0)

		hd2Mu=Su1 - Fa1*(Su2+Su3+Su4-Su5)
		dNum=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMuc)
		dexMu=-dNum/hd2Mu

Cc    End second derivative h''   
            TestDeMu=abs(dexMu/xMuc)
            Thresh=1e1
            If(TestDeMu.lt.Thresh) cpa0=xMuc+dexMu
      
        
	          ! Open(11,File='Test_lnQu.txt')
Cc	           If(iWhile.gt.1) then
Cc        Write(11,*)'lnQ=',dNum,'xMuc0=',xMuc,'dexMu=',dexMu,'xMuc=',cpa0
Cc     &TestMu_3*TestMu_b,TestQu2*TestQu3,'iWhile=',ffb*gfArt,iWhile
Cc                End If
	  
	           
      
      End If

      
C===========End for Mu iteration=======================      

C==========Root improvement with bisection method      
                  If(nAcc.gt.1.AND.TestDeMu.lt.Thresh) then

                        If(dexMu.lt.0) then
	                    xMub=xMuc
	                    xMua=xMuc+3*dexMu
	                    If(xMua.le.-1) xMua=-0.9999
                    	       else
	                    xMua=xMuc
	                    xMub=xMuc+3*dexMu
	                    If(xMub.ge.1) xMub=0.9999
	                    End if
Cc			           Open(11,File='Test_lnQu.txt')
Cc      Write(11,*)'xMua=',xMua,'xMub=',xMub !,'hd2Mu=',hd2Mu	                    

      aCorr=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMua)
      !If(nAcc.ne.0) TestQu2=Qu(ath,Gs,an,beta,gamma,cs,ss,ffb,xMu2)
              xTest_a=an*beta*ss*sqrt(1.-xMua*xMua)/(1.-an*beta*xMua*cs)
      Brackets_a=dlog(z(xTest_a))-
     &(aCorr+dLnG(xMua,anPar,nType))/2./an/gamma/ffb/beta/cs
	     TestMu_a=xMua/cpa00-1+(1-xMua*xMua)/sqrt(1-xTest_a*xTest_a)*Brackets_a
	!TestMu_2=TestMu_a
	
       aCorr=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMub)
      !If(nAcc.ne.0) TestQu3=Qu(ath,Gs,an,beta,gamma,cs,ss,ffb,xMu3)
	        xTest_b=an*beta*ss*sqrt(1.-xMub*xMub)/(1.-an*beta*xMub*cs)
	     Brackets_b=dlog(z(xTest_b))-
     &(aCorr+dLnG(xMub,anPar,nType))/2./an/gamma/ffb/beta/cs
	     TestMu_b=xMub/cpa00-1+(1-xMub*xMub)/sqrt(1-xTest_b*xTest_b)*Brackets_b
	!TestMu_3=TestMu_b
	
            !If(TestMu_a*TestMu_b.gt.0) then
	         !Open(8,File='Test_Mu_Solu.txt')
                !  Write(8,*)'Solution not found',TestMu_a,TestMu_b
            !End If

      !+++++++++++++++++++++++++++++++++++++++++++
                  Do While ((xMub-xMua).ge.epsMu.and.Ncount.lt.30) !.AND.ffb.gt.3)    
              
              xMuc=(xMub+xMua)/2.
              
      aCorr=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMuc)
	        xTest_c=an*beta*ss*sqrt(1.-xMuc*xMuc)/(1.-an*beta*xMuc*cs)
	     Brackets_c=dlog(z(xTest_c))-
     &(aCorr+dLnG(xMuc,anPar,nType))/2./an/gamma/ffb/beta/cs
	     TestMu_c=xMuc/cpa00-1+(1-xMuc*xMuc)/sqrt(1-xTest_c*xTest_c)*Brackets_c
	
	     If(TestMu_a*TestMu_c.lt.0) then
	     xMub=xMuc
	     TestMu_b=TestMu_c
      
	     else
	     xMua=xMuc
	     TestMu_a=TestMu_c
      
	     End if
	!xMuRen=xMuc
	!TestMuRen=TestMu_c
	     Ncount=Ncount+1
               !        Open(8,File='AMu30.txt')
                !      Write(8,*)'Mu=',xMuc,Ncount
             !         close(8)
	     End Do


                cpa0=xMuc !!!ultimately improved root

      End If
C==========Root improvement with bisection method
   
              
		

	!cpa0=xMuc !xMu0
      x0=an*beta*ss*sqrt(1.-cpa0*cpa0)/(1.-an*beta*cpa0*cs)

Cc		Included fragment to account for anisotropy; 06 Oct. 2009

      s0=gamma*ffb*(1.-an*beta*cpa0*cs)
      !eps=1./sqrt(1.-an*an*beta*beta)
      !tau=eps*beta*an*ss
      call abf(x0,s0,a,b,f)
	     gg=0
		    ggd1=0
		    ggd2=0
	     call ggMu(cpa0,anPar,gg0,gg1,gg2)
      
	     gg=gg0
		    ggd1=gg1
		    ggd2=gg2

		    Su1=(ggd2*gg-ggd1*ggd1)/gg/gg
	     Fa1=2*gamma*ffb*an*beta*cs/(1-cpa0**2)
		Su2=sqrt(1.-x0*x0)/an/beta/cs
			Su3=2*cpa0*sqrt(1.-x0*x0)/(1-cpa0**2)*(cpa0/an/beta/cs-1.)
			Su4=(an*beta*ss)**2*(cpa0-an*beta*cs)/sqrt(1.-x0*x0)/
     &(1-an*beta*cpa0*cs)**3*(cpa0/an/beta/cs-1.)
				Su5=(cpa0-an*beta*cs)/(1-an*beta*cpa0*cs)*sqrt(1.-x0*x0)


                h2Corr=0
                hd2Mu0=Su1 - Fa1*(Su2+Su3+Su4-Su5)
            If(nAcc.ne.0) then
            !If(nAcc.lt.0) then
        ddd=3d-6
        aCorr_1=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMuc-ddd)
        aCorr_2=aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMuc+ddd)
          h2CorrT=  (aCorr_2-aCorr_1)/(2.*ddd)
          If(abs(h2CorrT).lt.abs(hd2Mu0)) h2Corr=h2CorrT
            End If
            
             
            
            
        !hd2Mu_an=Su1 - Fa1*(Su2+Su3+Su4-Su5)
        
		hd2Mu=hd2Mu0+h2Corr


CC          Test of numeric evaluation of the second derivative h''		
		!hd2Mu=-(aCorr_2-aCorr_1)/(xMub-xMua)
		 !Open(8,File='Test_Mu_Solu_1.txt')
          !        Write(8,*)'hd2Mu_an=',hd2Mu_an,'hd2Mu_num=',hd2Mu


        !Gs=-Gs
	     Y02=(-(1-an*beta*cpa0*cs)*F+ath*(cs-an*beta*cpa0)+Gs*ss)**2/
     &(1-an*beta*cpa0*cs)/a/a/(1.+ath*ath)

	
		      
      
      c3=(z(x0))**(2.*s0)*gg/4./pi

      	g12=1./an/ss/ss*ffb*c3*Y02*sqrt(-2*pi/hd2Mu)

Cc      g12=sqrt(pi*ffb)*c1*c3/c2/c4 ! for isotropic case
Cc		Included line below to account for anisotropy; 06 Oct. 2009
	  
      return
      end
      
      function Qu(ath,Gs,an,beta,gamma,cs,ss,ffb,xMu)
      Implicit Real*8(A-H,O-Z)
      	Real*8 nzbz1

         A=0.503297
         B=1.193000
            p16=1.0/6.
            pm23=-2.0/3.

         sa2=1.0-xmu**2
         sa=sqrt(sa2)
         beta_z=beta*xMu
         nzbz1=1.0-an*cs*beta_z
         x=an*ss*beta*sa/nzbz1
         s1mx2=sqrt(1.0-x*x)
         !aLnZ=log(x/(1.0+s1mx2))+s1mx2

 
            s1mx2_3=s1mx2*s1mx2*s1mx2
            s=Gamma*ffb*nzbz1

                a6=s1mx2_3+A/s
                b16=s1mx2_3+B/s
                b2=(1.0-0.2*s**pm23)
                ab=(a6*b16)**p16*b2
                xi=3.0*x*x*s1mx2*(an*cs*beta-xMu)/sa2
                eta=an*cs*beta/s
                xlambda=Gamma*ffb/(6.0*s)

          a_1a=xlambda*(A*eta-xi)/a6
          
          
       Qu=(ath*(cs-an*beta_z)+Gs*ss-ab*nzbz1) !-2.0*a_1a+an*cs*beta/nzbz1
 

 
      return
      end

      
        function aLnQ(ath,Gs,an,beta,gamma,cs,ss,ffb,xMu)
        Implicit Real*8(A-H,O-Z)
      	Real*8 nzbz1

         A=0.503297
         B=1.193000
            p16=1.0/6.
            pm23=-2.0/3.

         sa2=1.0-xmu**2
         sa=sqrt(sa2)
         beta_z=beta*xMu
         nzbz1=1.0-an*cs*beta_z
         x=an*ss*beta*sa/nzbz1
         s1mx2=sqrt(1.0-x*x)
         aLnZ=log(x/(1.0+s1mx2))+s1mx2

 
            s1mx2_3=s1mx2*s1mx2*s1mx2
            s=Gamma*ffb*nzbz1

                a6=s1mx2_3+A/s
                b16=s1mx2_3+B/s
                b2=(1.0-0.2*s**pm23)
                ab=(a6*b16)**p16*b2
                xi=3.0*x*x*s1mx2*(an*cs*beta-xMu)/sa2
                eta=an*cs*beta/s
                xlambda=Gamma*ffb/(6.0*s)

          a_1a=xlambda*(A*eta-xi)/a6
          b_1b=xlambda*(B*eta-xi)/b16+4.0*xlambda*beta*an*cs*(b2-1.0)/b2
          
          
        aLnQ=(2.0*(-ab*(a_1a+b_1b)*nzbz1+ab*an*cs*beta-ath*an*beta)/
     &   (ath*(cs-an*beta_z)+Gs*ss-ab*nzbz1)-2.0*a_1a+an*cs*beta/nzbz1)
 

 
      return
      end
c
	
c
     c
      subroutine abf(x,s,a,b,f)
      Implicit Real*8(A-H,O-Z)
      c1=(1.-x*x)**1.5
      c2=0.2/s**(2./3.)
      a=(c1+0.503297/s)**(1./6.)
      b=(1.-c2)*(c1+1.193/s)**(1./6.)
      f=a*b
      return
      end
c
      function z(x)
      Implicit Real*8(A-H,O-Z)
      c=sqrt(1.-x*x)
      z=x*exp(c)/(1.+c)
      return
      end

c------------------------------------------------------------------------
c      

c------------------------------------------------------------------------
c
      subroutine sear(xc,sgc,x,s)
      Implicit Real*8(A-H,O-Z)
      dimension xc(34),sgc(34)
      i=1
 12   if (xc(i).gt.x) goto 13
      i=i+1
      goto 12
 13   s=(sgc(i)*(x-xc(i-1))+sgc(i-1)*(xc(i)-x))/(xc(i)-xc(i-1))
      return
      end
c-------------------------------------------------------------------------
c
      subroutine minmax(n,x,a,b)
      Implicit Real*8(A-H,O-Z)
      real*8 x(*)
      a=1.e6
      b=0.01
      do i=2,n
         if (x(i).lt.a.and.x(i).gt.-999.) a=x(i)
         if (x(i).gt.b) b=x(i)
         end do
      return
      end
c-------------------------------------------------------------------------
c
      SUBROUTINE GAULEG(X1,X2,X,W,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      !REAL*4 X1,X2,X(N),W(N)
      REAL*8 X(N),W(N)
      PARAMETER (EPS=3.D-14)
      M=(N+1)/2
      XM=0.5D0*(X2+X1)
      XL=0.5D0*(X2-X1)
      DO 12 I=1,M
        Z=COS(3.141592654D0*(I-.25D0)/(N+.5D0))
1       CONTINUE
          P1=1.D0
          P2=0.D0
          DO 11 J=1,N
            P3=P2
            P2=P1
            P1=((2.D0*J-1.D0)*Z*P2-(J-1.D0)*P3)/J
11        CONTINUE
          PP=N*(Z*P1-P2)/(Z*Z-1.D0)
          Z1=Z
          Z=Z1-P1/PP
        IF(ABS(Z-Z1).GT.EPS)GO TO 1
        X(I)=XM-XL*Z
        X(N+1-I)=XM+XL*Z
        W(I)=2.D0*XL/((1.D0-Z*Z)*PP*PP)
        W(N+1-I)=W(I)
12    CONTINUE
      RETURN
      END
c----------------------------------------------------------------------
c
      !REAL 
      FUNCTION GAMMLN(XX)
      IMPLICIT REAL*8 (A-H,O-Z)
      !REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      REAL*8 COF(6) 
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*DLOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+DLOG(STP*SER)
      RETURN
      END

      REAL FUNCTION PSI(XX)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      !REAL*8 TMP
      TMP = -0.5772156649
      DO I=1,1000
         XI = 1.0*I
         TMP = TMP + XX/(XI*(XI+XX))
         END DO
      PSI = TMP
      RETURN
      END







