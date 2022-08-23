!!!!!!!!!!!!!!!!!!!  S P I D E R   SUBROUTINES  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!   DELTA   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculates average error and sensitivities of the fitting to parameters !
      function DELTA(ErrMx,IX1,IX2,RedChi2)
      IMPLICIT REAL*8 (A-H,O-Z)      
      common/VAR/EPS,STEP,NPAR,NX1,NX2,KX
      common/ARR/FUNDAT(300,9),ERRDAT(300,9),X1dat(300),X2dat(9), ! NX1max=300, NX2max=9
     *PAR(15),DPAR(15),VECT(15),PSCALE(15),PARmin(15),PARmax(15), ! NPARmax=15
     *FUNC(19),AS(19,15)  ! 19=NPARmax+4 (for SIMPLEX)
				common/Weight/ErrMean,FunMean,WeightMean

      do 3 K=0,NPAR
      if (K.ne.0) then
         P=PAR(K)
         PAR(K)=P*1.1
      endif
      Y=0.
      DEL=0.
      do 2 I=1,NX1
      !do 2 J=1,NX2
      X1=X1dat(I)
      !X2=X2dat(J)
     
     
			
C		write(*,*)'Delta',FitFun(X1,X2)
      X=residual(FUNDAT(I,1),ERRDAT(I,1),FitFun(X1,X2),
     &FUNDAT(I,2),ERRDAT(I,2),X2)/WeightMean
C			write(*,*)'Delta',Errdat(1:10,1)
C			write(*,*)'FunDat',Fundat(1:10,1)

CCsyn		      X=residual(FUNDAT(I,J),FitFun(X1,X2))
      if (X.gt.DEL.and.K.eq.0) then
         DEL=X
         IX1=I
         IX2=J
      endif
      Y=Y+X
ccspi	write(*,*)nx1,nx2,y
    2 continue
	!RRChi2=Y/(NX1-NPar)
      RRChi2=Y/(2*NX1-NPar) !2*NX1 accounts for dual polarization
      Y=sqrt(Y/NX2/NX1)
      if (K.eq.0) then
         ErrMx=sqrt(DEL)
         AvErr=Y
		RedChi2=RRChi2
      else
         SENS=amax1(100.*abs(Y**2/amax1(AvErr**2,1.E-20)-1.),1.E-20)
         PAR(K)=P
         DPAR(K)=P*sqrt(2./SENS) 
      endif
    3 continue
      DELTA=AvErr
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!   AIM   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine AIM(l)
      IMPLICIT REAL*8 (A-H,O-Z)      
      common/VAR/EPS,STEP,NPAR,NX1,NX2,KX
      common/ARR/FUNDAT(300,9),ERRDAT(300,9),X1dat(300),X2dat(9),
     *PAR(15),DPAR(15),VECT(15),PSCALE(15),PARmin(15),PARmax(15),
     *FUNC(19),AS(19,15)
      Y=0.
      do 1 I=1,NPAR
      PAR(I)=PSCALE(I)*VECT(I)
      if (PAR(I).lt.PARMIN(I)) then ! check for the fitting parameters to
         PAR(I)=PARMIN(I)           !  lie within given boundaries
         VECT(I)=PAR(I)/PSCALE(I)
         AS(L,I)=VECT(I)
      endif
      if (PAR(I).gt.PARMAX(I)) then
         PAR(I)=PARMAX(I)
         VECT(I)=PAR(I)/PSCALE(I)
         AS(L,I)=VECT(I)
      endif
    1 continue
      do 2 I=1,NX1
      !do 2 J=1,NX2
      X1=X1dat(I)
      !X2=X2dat(J)
      Y=Y+residual(FUNDAT(I,1),ERRDAT(I,1),FitFun(X1,X2),
     &FUNDAT(I,2),ERRDAT(I,2),X2) !residual(FUNDAT(I,J),ERRDAT(I,J),FitFun(X1,X2))
ccspi		write(*,*)'PSCALE(I)=',PSCALE(I)

    2 continue
      FUNC(L)=Y
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!  SIMPLEX  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine SIMPLEX ! with shaking
      IMPLICIT REAL*8 (A-H,O-Z)      
      character KEY
      common/VAR/EPS,STEP0,NPAR,NX1,NX2,KX
      common/ARR/FUNDAT(300,9),ERRDAT(300,9),X1dat(300),X2dat(9),
     *PAR(15),DPAR(15),VECT(15),PSCALE(15),PARmin(15),PARmax(15),
     *FUNC(19),AS(19,15)
	     Real*8 PARtest(15),PARguess(15),SpecObs(64),
     & SpecRStep(64),SpecLStep(64)
*    Minimization of chi-squared:
*     NPAR = number of fitting parameters,     s[1..NPAR+4,1..NPAR] = matrix of coordinates of SIMPLEX points,
*     func[1..NPAR+4] = trial function in SIMPLEX points calculated in AIM,
*     sl=func[il] = minimum value of func, sh=func[ih] = maximum value,
*     z = selected dispersion of the func[1..NPAR+4],
*     eps = an estimate of z, Iter = number of iterations
      data A/1./,B/.5/,G/2./
C     if (NPAR.lt.1) then
C        print*,'NPAR=0: SIMPLEX not done.'
C        return
C     endif
		Nshake=0
		PARguess(:)=PAR(:)
		!PARtest(:)=PAR(:)
		PARtest(:)=DPAR(:)
		PAR(:)=DPAR(:)
		

      VALUE=1.0E+20
		     VALUE1=1.0E+20
      STEP=STEP0
		ITMAXtest=0
Cccc	Movie Fit
        Movie=0 !0

      If(Movie.eq.1) then
		     open(39,file='SpecSeqR.dat')
			    open(40,file='SpecSeqL.dat')
			!open(41,file='SpecErrR.dat')
			!	open(42,file='SpecErrL.dat')
		     Write(39,1239)(X1dat(I),I=1,64)
		     Write(39,1239)(FunDat(I,1),I=1,64)
		     Write(40,1239)(X1dat(I),I=1,64)
		     Write(40,1239)(FunDat(I,2),I=1,64)
		     !Write(41,1239)(X1dat(I),I=1,64)
		     Write(39,1239)(ErrDat(I,1),I=1,64)
		     !Write(42,1239)(X1dat(I),I=1,64)
		     Write(40,1239)(ErrDat(I,2),I=1,64)
      End If

C==============End movie
      IDcount=0
      IDcount1=0
      Nfast= 0
      ITmax0=500
      NiterADD= 2 ! 0 !3 !-3
    1 ITmax=ITmax0 !1000
      If(Nshake.EQ.1) Par(:)=PARguess(:)
      If(Nshake.EQ.2) Par(:)=PARguess(:)/3.
            If(Nshake.EQ.3) Par(:)=PARguess(:)*1.5
      If(Nshake.EQ.4) Par(:)=PARguess(:)/5.  
      
      If(Nshake.EQ.5) then
      do iii=1,Npar
       Par(iii)=PARguess(iii)*(3.5+(iii-Npar/2)/2.)/3. !/2.
      endDo
      endIf
      
      If(Nshake.EQ.6) then
      do iii=1,Npar
       Par(iii)=PARguess(iii)*(3.5-(iii-Npar/2)/2.) !/2.
      endDo
      endIf
      
      If(Nshake.EQ.9) then
      do iii=1,Npar
       Par(iii)=PARguess(iii)**((3.5-(iii-Npar/2)/2.)/3.)
      endDo
      endIf
      
      If(Nshake.EQ.10) then
      do iii=1,Npar
       Par(iii)=PARguess(iii)**((3.5+(iii-Npar/2)/2.)/3.)
      endDo
      endIf      
      
      If(Nshake.EQ.1) STEP=STEP0
		    ! Print*,Par(1:Npar)
721   If(IDcount.gt.0.AND.Nshake.GT.1) then
      PAR(:)=PARguess(:)/(1.+(rand(Npar+IDcount1)-0.5)*IDcount1*0.25)	
		     STEP=(STEP0+0.17*IDcount1)/IDcount1
	     !print*,'!!!Step=',Step,'!!!IDcount=',IDcount
	     End If
  111	Continue
      K1=NPAR+1
      K2=NPAR+2
      K3=NPAR+3
      K4=NPAR+4
      ITER=0
      KO=0
      D=1.E10
      do 11 I=1,NPAR
   11 VECT(I)=PAR(I)/PSCALE(I)
      Z=STEP/NPAR/1.4142   ! initial coordinates of SIMPLEX 
      D1=Z*(sqrt(NPAR+1.)+NPAR-1.)
      D2=Z*(sqrt(NPAR+1.)-1.)
      do 12 I=1,NPAR
   12 AS(1,I)=0.
      AS(K1,NPAR)=D1
      do 13 I=2,NPAR
      AS(I,I-1)=D1
      do 13 J=I,NPAR
      AS(I,J)=D2
      AS(J+1,I-1)=AS(I,J)
   13 continue
      do 14 I=1,K1   ! shift of SIMPLEX into initial vect[] 
      do 14 J=1,NPAR
   14 AS(I,J)=VECT(J)+AS(I,J)
      do 17 I=1,K1 ! calculation of the trial function in a SIMPLEX point
      do 16 J=1,NPAR
   16 VECT(J)=AS(I,J)
      call AIM(I)
   17 continue
   18 continue ! start of a forthcoming iteration
      ITER=ITER+1
      SH=FUNC(1)
      IE=1
      SL=SH
      IC=1
      do 19 I=2,K1
      if (FUNC(I).gt.SH) then
         SH=FUNC(I)
         IE=I
      endif
      if (FUNC(I).lt.SH) then
         SL=FUNC(I)
         IC=I
      endif 
   19 continue
      do 21 J=1,NPAR ! search for a center of mass of the SIMPLEX with weight
      SUM=0.
      do 20 I=1,K1
   20 SUM=SUM+AS(I,J)
      AS(K2,J)=(SUM-AS(IE,J))/NPAR
      AS(K3,J)=(1.+A)*AS(K2,J)-A*AS(IE,J)
      VECT(J)=AS(K3,J)
   21 continue
      call AIM(K3)
      if (FUNC(K3).lt.SL) then  
         do 22 J=1,NPAR
         AS(K4,J)=(1.-G)*AS(K2,J)+G*AS(K3,J)
         VECT(J)=AS(K4,J)
   22    continue
         call AIM(K4)
         do 23 J=1,NPAR
        if (FUNC(K4).lt.SL) then
           AS(IE,J)=AS(K4,J)
        else
           AS(IE,J)=AS(K3,J)
        endif
         VECT(J)=AS(IE,J)
   23    continue
        if (FUNC(K4).lt.SL) then
           FUNC(IE)=FUNC(K4)
        else
           FUNC(IE)=FUNC(K3)
        endif
      else
        if (IE.eq.1) then
           SS=FUNC(2)
        else
           SS=FUNC(1)
        endif
         do 24 I=1,K1
         if (FUNC(I).gt.SS.and.I.ne.IE) SS=FUNC(I)
   24    continue
        if (FUNC(K3).gt.SS) then ! choice of branches C or D
           if (FUNC(K3).le.SH) then
              do 25 J=1,NPAR
   25         AS(IE,J)=AS(K3,J) ! squeeze of the reflected point
           endif
            do 26 J=1,NPAR
            AS(K4,J)=AS(IE,J)*B+AS(K2,J)*(1.-B)
            VECT(J)=AS(K4,J)
   26       continue
            call AIM(K4)
           if (FUNC(k4).gt.SH) then ! choice of branches of D or A
              do 27 J=1,NPAR ! reduction of all points
              do 27 I=1,K1
   27         AS(I,J)=.5*(AS(I,J)+AS(IC,J))
              do 29 I=1,K1
              do 28 J=1,NPAR
   28         VECT(J)=AS(I,J)
              call AIM(I)
   29         continue
           else ! choice of branches A or B
              do 30 J=1,NPAR
              AS(IE,J)=AS(K4,J)
              VECT(J)=AS(IE,J)
   30         continue
              FUNC(IE)=FUNC(K4)
          endif
        else
           do 31 J=1,NPAR
           AS(IE,J)=AS(K3,J)
           VECT(J)=AS(IE,J)
   31      continue
           FUNC(IE)=FUNC(K3)
        endif
      endif
      do 32 J=1,NPAR
   32 VECT(J)=AS(K2,J)
      call AIM(K2)
ccspi	print*,(func(I),I=1,19)
ccspi	pause
      SUM=0.
      do 33 I=1,K1
ccspi	write(*,*)sum,npar,FUNC(I),FUNC(K2)
Cc         print*,'FUNC(I)=',FUNC(I),'FUNC(K2)=',FUNC(K2)
C   33 If(FUNC(I).gt.-1e20) SUM=SUM+(FUNC(I)-FUNC(K2))**2

              IDcount=0
	     SUMtest=SUM+(FUNC(I)-FUNC(K2))**2
CC  33  
       If(Sumtest.ge.0) then
       SUM=Sumtest 
Cc            print*,'Sum=',Sum

      D=sqrt(SUM/NPAR)
Cc      print*,'D=',D
        Else
            !If(D.le.0.AND.IDcount1.lt.5) then
       D=sqrt(SUM/NPAR)*K1/(I-0.8) !accounts for missing elements in the SUM
      IDcount=IDcount+1
      IDcount1=IDcount1+1
       !print*,'ID=',IDcount,'ID1=',IDcount1
       !print*,'Parms=',Par(1:Npar)
       !!!Pause !Commented out 2018-Jul-21
       !BELOW !!! Commented out 2018-Jul-21
       ! temporal debug print to file
 121      Format(1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5)
       	!!!open(14,file='Fit_Test_2012.dat') !Commented out 2018-Jul-21
Ccc		writing the original and fitted light curves for a specific set of parameters E,Mu,and S 				
		!!!Do Ifr=1,NX1
	!!!xxxMu=X1dat(Ifr)
	!!!Dum=0
	!!!FitProf=FitFun(xxxMu,dum)  !/100.
Cc	Spectrum=Fit1Data(NE,I,JS) !FunDat(I,1) !
Cc	ErrProf=Err1Data(NE,I,JS)
Cc
Ccc	     Write(9,12)xxxMu,TimeProf,ErrProf,FunDat(I,1),FitProf
	   !!!  Write(14,121)xxxMu,FunDat(Ifr,1),ErrDat(Ifr,1),FitProf
      !FFit(Nx,Ny,I)=FitProf
			!!!End Do
 	!!!Close(14)
       ! end print to file
       !Pause       
                    !go to 721
            End If  !If(Sumtest.gt.0)
33    Continue      
      
      
      if (KO.le.ITER-1) then
CC print     print*,ITER,FUNC(K2)
       KO=ITER
      endif

Cccc	Movie Fit 2
      If(Movie.eq.1) then

				
		Do I=1,NX1
	     xxxMu=X1dat(I)
	     Dum=0
	     SpecRStep(I)=FitFun(xxxMu,dum)  !/100.
	     SpecLStep(I)=dum
			End Do

	     Write(39,1239)(SpecRStep(I),I=1,64)
	     Write(40,1239)(SpecLStep(I),I=1,64)
		     
1239      Format(64(1x,G12.5))
        End If
Cccc	END of Movie Fit 2

		Incr=1000
      if(ITER.ge.ITMAX.AND.(Nfast.gt.0.OR.ITER.le.(Nshake+1)*1000))
     &  then
Cc         print*,CHAR(7),'Do you need N new iterations? (input N or 0):'
         !print*,'Add 1000 new iterations!',Partest(2),FUNC(K2)
      !  read*,I
      ITmax=ITmax0
         ITMAXtest=ITMAXtest+Incr

	     If(FUNC(K2).lt.VALUE1) then
		    VALUE1=FUNC(K2)
	     If(VALUE1.lt.VALUE) then

	     PARtest(:)=PAR(:)
      
	
Cccc	Movie Fit 3
      If(Movie.eq.1) then

				
		Do I=1,NX1
	     xxxMu=X1dat(I)
	     Dum=0
	     SpecRStep(I)=FitFun(xxxMu,dum)  !/100.
	     SpecLStep(I)=dum
			End Do

	     Write(39,1239)(SpecRStep(I),I=1,64)
	     Write(40,1239)(SpecLStep(I),I=1,64)

      End If
Cccc	END of Movie Fit 3


	     End If    !(VALUE1.lt.VALUE)
	     End If    !(FUNC(K2).lt.VALUE1)





Cc	If(D.gt.0) 
      If(Nfast.gt.0.OR.Nshake.gt.ITMAXtest/1000.) then
	!PAR(:)=PAR(:)/(1.+(rand(Nshake)-0.5)*0.75)
	     Else
	     ITmax=ITmax+Incr/2
	     Go To 724
	     End If
C	PAR(4)=90.-PAR(4)
	        ! print*,'Pause!','ITMAXtest=',ITMAXtest

C            if (ITMAXtest.gt.2000.AND.ITMAXtest.le.4000) goto 721
			!if (ITMAXtest.gt.1000*Nshake) then
			!IDcount=1
			!IDcount1=IDcount1+1
			!       print*,'ITmaxID=',IDcount,'ID1=',IDcount1
            !        print*,'Parms=',Par(1:Npar)
			!goto 723
			!end if
			if (ITMAXtest.gt.8000) goto 37

Cc	pause
	  !STEP=STEP*25.*sqrt(1.*ITER)/ITMAXtest
	!Below we test a version with shake of guess parameters:
	!PAR(:)=PARguess(:)/(1.+(rand(Iter-Nshake)-0.5)*(1.+Nshake)*0.25)	
	          !STEP=STEP0*(2.+Nshake+rand(Iter+Nshake))/(1.+Nshake)/I*Iter
Cc	Par(1)=0.3*PARtest(1)
Cc	Par(2)=0.03*PARtest(2)
      goto 1
C	Else if(ITER.ge.ITMAX) then 
C	Print*,'ITmax=',ITMAX
C	ITMAX=ITMAX+Incr
C	!ITMAXtest=ITMAXtest+Incr
C	Print*,'ITmax=',ITMAX
C	Else
      endif		!if (ITER.ge.ITMAX) then
  724      continue      




      if (D.gt.EPS.and.ITER.lt.ITmax) goto 18 ! Repeat
           ! print*,'D=',D,EPS,ITER


      if (D.lt.EPS.and.abs(FUNC(K2)-VALUE).gt.EPS.or.ITER.ge.ITMAX
     &.or.IDcount.gt.0) then !(D.lt.EPS.and.abs(FUNC(K2)-VALUE).gt.EPS.or.ITER.ge.ITMAX) 
        If(FUNC(K2).lt.VALUE.and.FUNC(K2).lt.VALUE1) then
             ! print*,'Func(K2)=',Func(K2),Value,Value1,'Improved!'
              !Pause

	  
	  VALUE=FUNC(K2)
	     PARtest(:)=PAR(:)


Cccc	Movie Fit 4
      If(Movie.eq.1) then

				
		Do I=1,NX1
	     xxxMu=X1dat(I)
	     Dum=0
	     SpecRStep(I)=FitFun(xxxMu,dum)  !/100.
	     SpecLStep(I)=dum
			End Do

	     Write(39,1239)(SpecRStep(I),I=1,64)
	     Write(40,1239)(SpecLStep(I),I=1,64)

      End If
Cccc	END of Movie Fit 4

	     End If

Cc		Temp=(5e8/Par(3))**0.67/1e6
Cc	Dens=1e10*Par(1)
Cc	Write(*,*)'T=',temp,'B=',Par(4),'n=',Dens
Cc	Write(*,*)'xi=',Par(5),'gamma2=',Par(6) !,'n=',Dens
723	  Nshake=Nshake+1
		!print*,' '
         !print*,'To shake!!! Nshake=',Nshake,ITER,FUNC(K2)




Cc         read(*,'(A1)') KEY    ! Always to shake
722			Key='Y'
Cc         STEP=STEP*5.*ITER/ITMAX
		if (Nshake.gt.NPAR+NiterADD) goto 37
C fast mode		if (Nshake.gt.NPAR+1) goto 37
		ITMAXtest=3 !10

Cc test!         STEP=STEP*40.*sqrt(1.*ITER)/ITMAX
Cc         STEP=STEP*Nshake*10.*sqrt(1.*ITER)/ITMAX
            !STEP=STEP*40.*sqrt(1.*ITER)/ITMAX !!! The most recent Step Shake
            STEP=STEP0*(1+1.1*Nshake) !
            !STEP=STEP*10.*sqrt(1.*ITER)/ITMAX
C           print*,' ' 
C           print*,'Shake Step= ', step 
C           print*,' ' 
	     If(Nshake.eq.2.or.Nshake.eq.2.or.Nshake.eq.7.or.Nshake.eq.10) 
     &      PAR(:)=PAR(:)*(1.-0.01*Nshake) !*(rand(Nshake)-0.5))	!PAR(4)=90.-PAR(4) !C fast mode

         if (KEY.ne.'N'.and.KEY.ne.'n') goto 1
      endif		!      if (D.lt.EPS.and.abs(FUNC(K2)-VALUE).gt.EPS.or.ITER.ge.ITMAX) 

   37      continue


	     PAR(:)=	PARtest(:)

Cccc	Movie Fit 5
      If(Movie.eq.1) then

Cc	open(39,file='D:\GS_Modeling\Gelu_Blind_Test_1\DATA\FitRes.dat')
Ccc		writing the original and fitted light curves for a specific set of parameters E,Mu,and S 				
		Do I=1,NX1
	     xxxMu=X1dat(I)
	     Dum=0
	     SpecRStep(I)=FitFun(xxxMu,dum)  !/100.
	     SpecLStep(I)=dum
			End Do

	     Write(39,1239)(SpecRStep(I),I=1,64)
	     Write(40,1239)(SpecLStep(I),I=1,64)
      !End If
Cccc	END of Movie Fit 5
        

    	Close(39)
    	Close(40)
CCCCCCCC	              pause
	      End If

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  DRAWING  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine DRAWING(KPAR) ! Preparing to show the results
      IMPLICIT REAL*8 (A-H,O-Z)      
      real*8 BUFFER(5),BUFFER1(5)
      character CHKX
      common/VAR/EPS,STEP,NPAR,NX1,NX2,KX
      common/ARR/FUNDAT(300,9),ERRDAT(300,9),X1dat(300),X2dat(9),
     *PAR(15),DPAR(15),VECT(15),PSCALE(15),PARmin(15),PARmax(15),
     *FUNC(19),AS(19,15)
C     if (KX.eq.1) CHKX='1'
C     if (KX.eq.2) CHKX='2'
C     X1min=X1dat(1)
C     X1max=X1min
C     do 1 I=2,NX1
C     if (X1dat(I).lt.X1min) X1min=X1dat(I)
C     if (X1dat(I).gt.X1max) X1max=X1dat(I)
C   1 continue
C     X2min=X2dat(1)
C     X2max=X2min
C     do 2 I=2,NX2
C     if (X2dat(I).lt.X2min) X2min=X2dat(I)
C     if (X2dat(I).gt.X2max) X2max=X2dat(I)
C   2 continue
C     open(1,file='SPIDER'//CHKX//'.DAT')
C     open(2,file='SPIDERD'//CHKX//'.DAT')
C     if (KPAR.le.0) then
C       if (KX.eq.2) then
C          ID=(NX1+4)/5
C          do 3 J=1,NX2-1
C          DT=(X2dat(J+1)-X2dat(J))/10.
C          do 3 J1=1,10
C          T=X2dat(J)+J1*DT
C          write(1,'(1P,9E13.5)') T,(FitFun(X1dat(I),T),I=1,NX1,ID)
C   3      T=T+DT
C          do 4 J=1,NX2
C          write(2,'(1P,9E13.5)') X2dat(J),(FUNDAT(I,J),I=1,NX1,ID)
C   4      continue
C       else
C          ID=(NX2+4)/5
C          do 5 I=1,NX1-1
C          DT=(X1dat(I+1)-X1dat(I))/10.
C          do 5 I1=1,10
C          T=X1dat(I)+I1*DT
C          write(1,'(1P,9E13.5)') T,(FitFun(T,X2dat(J)),J=1,NX2,ID)
C   5      T=T+DT
C          do 6 I=1,NX1
C          write(2,'(1P,9E13.5)') X1dat(I),(FUNDAT(I,J),J=1,NX2,ID)
C   6      continue
C       endif
C     else
C        DP=amin1(DPAR(KPAR),abs(PAR(KPAR))/3.)
C        P=PAR(KPAR)
C        PAR(KPAR)=P-2.*DP
C        do 7 K=1,5
C        BUFFER1(K)=PAR(KPAR)
C        BUFFER(K)=Delta(ErrMx,IX1,IX2,RedChi2)
C   7    PAR(KPAR)=PAR(KPAR)+DP
C        PAR(KPAR)=P
C        write(*,'(1P,A8,5E13.5)') '  PAR  =',(BUFFER1(K),K=1,5),
C    *   ' AvErr =',(BUFFER(K),K=1,5)
C       if (KX.eq.2) then
C          T=X2min
C          DT=X2max-X2min
C          ID=(NX1+4)/5
C          do 10 I=1,NX1,ID
C          X2=X2min
C          do 9 J=1,300
C          P=PAR(KPAR)
C          PAR(KPAR)=P-2.*DP
C          do 8 K=1,5
C          BUFFER(K)=FitFun(X1dat(I),X2)
C   8      PAR(KPAR)=PAR(KPAR)+DP
C          PAR(KPAR)=P
C          write(1,'(1P,9E13.5)') T,(BUFFER(K),K=1,5)
C          X2=X2+DT/49.
C   9      T=T+DT/49.
C          do 10 J=1,NX2
C          T=X2dat(J)+DT*(I-1)
C          write(2,'(1P,9E13.5)') T,(FUNDAT(I,J),K=1,5)
C  10      continue
C       else
C          T=X1min
C          DT=X1max-X1min
C          ID=(NX2+4)/5
C          do 13 J=1,NX2,ID
C          X1=X1min
C          do 12 I=1,300
C          P=PAR(KPAR)
C          PAR(KPAR)=P-2.*DP
C          do 11 K=1,5
C          BUFFER(K)=FitFun(X1,X2dat(J))
C  11      PAR(KPAR)=PAR(KPAR)+DP
C          PAR(KPAR)=P
C          write(1,'(1P,9E13.5)') T,(BUFFER(K),K=1,5)
C          X1=X1+DT/49.
C  12      T=T+DT/49.
C          do 13 I=1,NX1
C          T=X1dat(I)+DT*(J-1)
C          write(2,'(1P,9E13.5)') T,(FUNDAT(I,J),K=1,5)
C  13      continue
C       endif
C     endif
C     close(1)
C     close(2)
      return
      end
!!!!!!!!!!!!!!!!!!!  S P I D E R   SUBROUTINES  !!!!!!!!!!!!!!!!!!!!!!!!!!!

