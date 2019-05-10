!!  These three procedures should be adjusted to the concrete situation !!
!!!!!!!!!!!!!!!   MAIN PROGRAM   S P I D E R   !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   The program calls Spider Subroutines: SPIDRSUB.FOR or SPIDER.SUB   !!
!!   The program uses files: SPIDAT (data) and SPIDER.IN (parameters)   !!

Cc					22 Nov 2008
Cc			I will modify the code created in June-July 2005 to study trapping
Cc		of the electrons based on comparison between BATSE HXR data and NoRH 
Cc		spatially resolved light curves. 

Cc			The idea is to use the results of numerical solution of the Fokker-Plank
Cc		Equation (provided by V.F.Melnikov) as input fo the phenomenological 
Cc		treatment in terms of characteristic life-time \tau(E,mu,S) (Fleishman, 2005, Nobe Proc).
Cc		! We do not consider emission here, only fast electron evolution. 

Cc			The GOAL is to derive the cube of the life time as a function of E,mu, and S,
Cc		find an analytical approximation for it, and then use these results for faster modeling 
Cc		of the fast electron evolution in the magnetic traps. This later be implemented into 
Cc		GS simulator to be developed by Gelu Nita and I.

Cc			First step is to implement integration over irregular time grid inherited from 
Cc		Victor's computations to describe trapping. Start from a "blind" approach:
Cc		the injection function has gaussian shape with unknown parameters. 

Cc					25 Nov 2008
Cc			The code works appropriately, when: 1) the injection core is explicitly specified, i.e.,
Cc		two-parameter fitting is used (blind [4- and 3- par] test is unstable, perhapse because 
Cc		the injection profile is not too different from the trapping response); 2) the tail of the 
Cc		time profiles must be cut at the level above second time point at the rise phase; 
Cc		otherwise the time profiles can contain a long tail at low level (I do not know yet its origin).
Cc		The accuracy of fits varies from fit to fit: this means that I must save the parameter errors
Cc		for further fitting of them by model functions.

Cc					07 Dec 2008
Cc			The input has been changed to the array (30,60,60,25) with a constant time resolution of 4 sec.
Cc		25 time steps. Now I plan to perform global test of the fit with the output life-time(29,60,60)
Cc		and Ampl(29,60,60) [E(30) returns zeros]

Cc					05 Jan 2009
Cc			This is a next step fitting process to replace the numeric solution of the F-P equation
Cc		by the life-time. This code reads the array of the life-time(Mu,S,E) and fit the dependence on
Cc		Mu by a model function with a few free parameters functions of S & E





Cc					17 June 2005
Cc			Start to modify the fitting program to find the charachteristic 
Cc		life time of the radio emission as observed along the spatially 
Cc		resolved (in 2d) radio source. Plan: use X-ray light curves as 
Cc		the proxi of the injection profile and fit radio profile to the 
Cc		injection profile convolved with the trapping function

Cc			First step: test - one - four  light curves produced by calculation
Cc		within trapping model (Anis. paper, Astro-ph/0505145, 2005): gaussian 
Cc		injection profile.

Cc			Current version uses calculated radio flux at the foot-point (fourth 
Cc		column of the file 'trap_model.dat') as the injection proxi. Fitting is made 
Cc		for one localized light curve (one of four columns of the above file)
Cc		Q: why the convolution gives relatively poor fit for J=1-3? Although 
Cc		excellent fit for J=4 (self-fit)

Cc				20 June 2005
Cc		Upgrade of Version 1. 20 local light curves produced by model GS_trap_model.
Cc		Expected: life time vs position along the loop. z=0.1 - loop top, 
Cc		z=2 - foot point. We'll study spatial dependence of the life time along the loop
Cc		Accross: thin loop (one pixel).


Cc                06 July 2005
Cc		Start test the code with original data: X-ray BATSE and 17 GHz Nobeyama 
Cc		(provided by Tim Bastian). The data points (X and R) are taken at different 
Cc		times and have different cadence: 1 s for radio and 1.024 s for X-rays.

Cc		This version of the code must take this into accont and determine the life time 
Cc		of the radio emission coming from a few different spatial locations.

cc		We will modify primarily fitting function and reading.

Cc                07 July 2005
Cc		Start to modify to calculate 2D distributions of the life time and response intensity
Cc		We first modify all required arrays for the case of 7 Oct 1992 flare, e.g., 
Cc		AllData(43,40,358) 

Cc				12 July 2005
Cc		Previous version is the final working code for the event 7 Oct 1992 (Stox V fit).
Cc		Now we change the code for 27 Oct 1992, including AllDAta(43x40x358), whic is enough 
Cc		for all four sample bursts

Cc				14 July 2005
Cc		This version is adjusted from 'Oct_27' to the event 23 Dec 93

Cc				21 July 2005
Cc		This version is adjusted from 'Jun_27' back to the event 07 Oct 92:
Cc		more numbers obtained directy from the IDL 'read-write' code

Cc            01 April 2019
Cc    This is a dll version of the code. The most recent change is the ability of the code to 
Cc        accept any combination of polarizations: I, L&R, I&V, I&P

Cc    NO EXPLICIT provision is made for XX or YY only. If true XX(YY) is provided, it muct be supploied twice (as L and R inputs)

Cc    If input is I then only the Fitting Mode I is allowed
Cc    If a combination of two polarization inputs is provided, the user can select a different fitting mode.


      real*8 function Get_Tables(argc, argv) !(ParmIn,Spectrum,ar3) 
                  implicit none
!GCC$ ATTRIBUTES DLLEXPORT :: get_tables
   

            integer, intent(in)                   :: argc
            !integer*8, intent(in)                   :: argc
            integer, intent(in), dimension(argc)  :: argv
            !integer*8, intent(in), dimension(argc)  :: argv
            !External forstr
            
           !call Call_Parms(%val(argv(1)), %val(argv(2)))
           !call forstr(%val(argv(1)), %val(argv(2)))
                    
                 Get_Tables=2.   
                        Open(8,File='Long_input.txt')
       !Write(8,126)'parm=', '      Default value', '    Name, Units'
       Write(8,77)  'Nparms;', 7, '; ;user ;Number of fit Parms' 
       Write(8,77)  'Angular Code;',  0, '; ;data ;0L for PK, 1L for FK'
       Write(8,77)  'Npix;', 128, '; ;data ;Number of pixels sent to dll' 
       !Write(8,77)  'NpicY;', 128, ';Number of pixels along y axes' 
       Write(8,77)'Nfreq;', 24,   '; ;data ;Number of frequencies in the spectrum' 
       Write(8,77)'Fitting Mode;', 1, '; ;user ;Case of the fit: I:1, L&R:2, I&V:3, I&P:4' 
       Write(8,77)'Stokes Data;', 1, '; ;data ;Case of the data: I:1, L&R:2, I&V:3, I&P:4' 
                   close(8)
                  
                        Open(8,File='Real_input.txt')
       !Write(8,126)'parm=', '      Default value', '    Name, Units'
       Write(8,7)  'SIMPLEX Step;', 0.17, ';   ;user',  ';SIMPLEX Step' 
       Write(8,7)  'SIMPLEX EPS;',  1d-6, ';   ;user',  ';SIMPLEX accuracy'
      Write(8,7)'Flux Threshold;',1.,';sfu*GHz',' ; user ;Flux threshold to be fitted' 
      Write(8,7)'Pixel Area;',4.,';arcsec^2',' ;data ;Number of pixels along y axes' 
      Write(8,7)'LOS Depth;',8.,';arcsec ',';user ;LOS depth (assumed)'     
      Write(8,7)'E_min;',0.015d0,';MeV ;user',';Min energy in PLW (assumed)'       
                   close(8)

                       Open(8,File='Parms_input.txt')
       !Write(8,126)'parm=', '      Default value', '    Name, Units'
      Write(8,8)'n_nth;',1d0,';',1.d-4,';',2d3,';','1d7 cm^-3'
     &, ';Non-thermal density' 
       Write(8,8) 'B;',  4d0,';',1.d-2,';',3d1,';', '1d2G '
     &,  ';Magnetic field'
      Write(8,8)'theta;',60.,';',22.d0,';',87d0,';','deg',
     &';Viewing angle to B' 
      Write(8,8)'n_th;',10d0,';',1d-2,';',6d2,';', '1d9 cm^-3', 
     & ';Thermal density'
      Write(8,8)'Delta;',4.5,';',1.7d0,';',15d0,';','No',
     & ';PLW spectral index'     
      Write(8,8)'E_max;',5d0,';',0.1d0,';',1d1,';','MeV',
     &';Max energy in PLW'   
       Write(8,8) 'T_e;',5d0,';',1.5d0,';',6d1,';','MK',';Temperature'        
      Write(8,8)'Res1;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used'       
      Write(8,8)'Res2;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used' 
      Write(8,8)'Res3;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used'       
      Write(8,8)'Res4;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used' 
      Write(8,8)'Res5;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used'       
      Write(8,8)'Res6;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used' 
      Write(8,8)'Res7;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used'       
      Write(8,8)'Res8;',5d0,';',0.2,';',0.2d2,';', ' ',';Not used'
                   close(8)
                   
                       Open(8,File='Parms_out.txt')
       !Write(8,126)'parm=', '      Default value', '    Name, Units'
      Write(8,7)'n_nth;',1d7,';cm^-3'
     &, ';Non-thermal density \pm err' 
       Write(8,7) 'B;',  4d2, ';G '
     &,  ';Magnetic field \pm err'
      Write(8,7)'theta;',60.,';deg',
     & ';Viewing angle to B \pm err' 
      Write(8,7)'n_th;',1d10, ';cm^-3', 
     & ';Thermal density \pm err'
      Write(8,7)'Delta;',4.5,'; ',
     & ';PLW spectral index \pm err'     
      Write(8,7)'E_max;',5d0,';MeV',
     &';Max energy in PLW \pm err'   
       Write(8,7) 'T_e;',5d0,';MK',';Temperature \pm err'        
      Write(8,7)'Chi-2;',1d0, '; ',';Chi-2 or Average Fit Error (Dim.less)'       

                   close(8)
                   



c126      FORMAT(1x,a10,2x,a20,2x,a10,a30)                  
7        FORMAT(1x,a15,2x,G10.3,2x,a10,a40)
8        FORMAT(1x,a11,3(2x,G10.3,a1),2x,a10,a40)
77        FORMAT(1x,a13,2x,G10.3,2x,a40)

                    
	
	    	     !character*8 string(3,29)
	    	     !string(1,1)='area'
	    	     end  



        real*8 function GET_MW_FIT(argc, argv)
            implicit none
            interface
              subroutine SPIDER(N_dat_in,r_inp,ParmsIn,fr_in,spec_in,ArrParms,ErrParms,spec_out)
                INTEGER*8 :: N_dat_in,r_inp,ParmsIn,fr_in,spec_in,ArrParms,ErrParms,spec_out
              end
            end interface
            INTEGER*8:: argc, argv(*)
            


!!            !integer, intent(in)                   :: argc
!!            integer*8, intent(in)                   :: argc ! for x64
!!            !integer, intent(in), dimension(argc)  :: argv
!!            integer*8, intent(in), dimension(argc)  :: argv

       !      real*8 status,test8,cpa0
            !!integer, intent(in)                   :: arg2
           	!open(9,file='D:\GS_Modeling\DLL_Tested\Test.dat')
            !write(9,*)argc
            !integer, intent(in), dimension(argc)  :: argvv
        !call klein4dll_sub(%val(argv(1)), %val(argv(2)), %val(argv(3)))
        !call Get_Parms(%val(argv(1)), %val(argv(2)))
C          common /status/ status
C          common /test8/ test8   !returns Nthreads for test purpose
C                      common /testMu/cpa0 !returns mu_0 for test purpose
          call SPIDER (%val(argv(1)), 
     & %val(argv(2)), %val(argv(3)), %val(argv(4)), %val(argv(5))
     & , %val(argv(6)), %val(argv(7)), %val(argv(8)))
        get_mw_fit=0d0      

C       transfer_sol_Slice(%val(argv(1)), %val(argv(2)), 
C    & %val(argv(3)), %val(argv(4)))
c             call transfer_sol(%val(argv(1)), %val(argv(2)),
c     & %val(argv(3)))

            
            !Get_MW_Slice= cpa0 !status !1
            return
        end   



      !program
      subroutine 
     & SPIDER(N_dat_in,r_inp,ParmsIn,fr_in,spec_in,ArrParms,ErrParms
     &,spec_out) !(xxx_in,yyy_in)  ! Fitting of Func(X1,X2).
      IMPLICIT REAL*8 (A-H,O-Z)
      character KEY
      common/VAR/EPS,STEP,NPAR,NX1,NX2,KX
      common/ARR/FUNDAT(300,9),ERRDAT(300,9),X1dat(300),X2dat(9), ! NX1max=300, NX2max=9
     *PAR(15),DPAR(15),VECT(15),PSCALE(15),PARmin(15),PARmax(15), ! NPARmax=15
     *FUNC(19),AS(19,15)         ! 19=NPARmax+4 (for SIMPLEX)
c	 common/AllDat/AllData(43,40,358) 
	     common/Indecs/Npic,Nfreq
               common/Eminmax/E_min, E_max, T_fix,are_arc,dep_arc
	     common/B2Ind/NB2
	     common /NType/IAType
                     common/Fit_case/N_case_d,N_case_f
C	common/ParGuess/
      !Integer*4 N_dat_in(4)
      Integer*4 N_dat_in(6)
      Real*8 ParGuess(15),r_inp(6),fr_in(N_dat_in(4)),ParmsIn(15,3),
     & ArrParms(N_dat_in(3),8),
     & ErrParms(N_dat_in(3),8),
     & spec_in(N_dat_in(3),N_dat_in(4),4),
     & spec_out(N_dat_in(3),N_dat_in(4),2),  
     & FreqData(N_dat_in(4)),Flcp(N_dat_in(3),N_dat_in(4)),
     & Frcp(N_dat_in(3),N_dat_in(4)),
     & ErrFlcp(N_dat_in(3),N_dat_in(4)),ErrFrcp(N_dat_in(3),N_dat_in(4))
     & ,FFitR(N_dat_in(3),N_dat_in(4)),FFitL(N_dat_in(3),N_dat_in(4))
     &,ArrNrl(N_dat_in(3)),ArrBgauss(N_dat_in(3)),
     & ArrEmax(N_dat_in(3)),ArrAngle(N_dat_in(3))
     &,ArrNth(N_dat_in(3)),ArrDelta1(N_dat_in(3))           
     &,ErrNrl(N_dat_in(3)),ErrBgauss(N_dat_in(3)),
     & ErrEmax(N_dat_in(3)),ErrAngle(N_dat_in(3))
     &,ErrNth(N_dat_in(3)),ErrDelta1(N_dat_in(3)),ErrData(N_dat_in(3)),
     & RChi2(N_dat_in(3))
     &,ArrT(N_dat_in(3)),ErrT(N_dat_in(3))
C		     & FFitR(N_dat_in(3),N_dat_in(4),N_dat_in(5)),
C         & FFitL(N_dat_in(3),N_dat_in(4),N_dat_in(5)),
C

C	     Real*8, ALLOCATABLE :: FreqData(:),Flcp(:,:),Frcp(:,:),
C    &ErrFlcp(:,:),ErrFrcp(:,:),FFitR(:,:),FFitL(:,:)
C    &,ArrNrl(:),ArrBgauss(:),ArrEmax(:),ArrAngle(:)
C    &,ArrNth(:),ArrDelta1(:)           
C    &,ErrNrl(:),ErrBgauss(:),ErrEmax(:),ErrAngle(:)
C    &,ErrNth(:),ErrDelta1(:),ErrData(:),RChi2(:)
C    &,ArrT(:),ErrT(:)
CCC     &,ArrParms(:,:,:),ErrParms(:,:,:)
	 

C		Real*4 ParMu1(30,60),ParMu2(30,60),ParMu3(30,60)
C     &,ErrParMu1(30,60),ErrParMu2(30,60),ErrParMu3(30,60) ! increase dimentions of the arrays later for the 2D case - done on 07 Dec 2008
          NX41=60
          NX2=1
          KX=1
          
C     open(1,file='SPIDER_3.IN')
C     read(1,*) NX41,NX2,NPAR,STEP0,EPS,KX,IAType,Fl_thr,are_arc,dep_arc 
C     
C	  read(1,*)  E_min, E_max, del_min, del_max, T_fix
C
C
C*        NXi - number of Xi data,
C*        NPAR - number of parameters,
C*        STEP - initial step,
C*        EPS - accuracy parameter, 
C*        KX=1 or 2 for displaying the 1st or the 2nd argument,
C      !read(1,*) (PARGuess(I),I=1,NPAR)       ! Initial parameters
C     close(1)
      
          NPAR=N_dat_in(1)
          IAType=N_dat_in(2)
          Npic=N_dat_in(3)
           !NpicY=N_dat_in(4)
          Nfreq=N_dat_in(4)
          N_case_f=N_dat_in(5)
          N_case_d=N_dat_in(6)          
          
        STEP0  =r_inp(1)
        EPS    =r_inp(2)
        Fl_thr =r_inp(3)
        are_arc=r_inp(4)
        dep_arc=r_inp(5)
        E_min  =r_inp(6)
      
      
      ParGuess(:)=ParmsIn(:,1) 
      PARMIN(:)=ParmsIn(:,2)
      PARMAX(:)=ParmsIn(:,3)
      
      E_max=ParGuess(6)
      T_fix=ParGuess(7)*1d6
      
!      If(npar.lt.(5.6)) PARGuess(6)=5.

C     write(*,'(''  NX1  NX2 NPAR   STEP     EPS        KX''/
C    /3I5,1P,2E10.3,0P,I5)') NX41,NX2,NPAR,STEP,EPS,KX
C     write(*,'('' Initial parameters:'',1P,5E10.3)') 
C    &(PARGuess(I),I=1,NPAR)
C    
             
       
C         PARMIN(1)=1.E-4	!Non-thermal density
C         PARMAX(1)=2.E3
C	      PARMIN(2)=1.E-2   !B-field
C         PARMAX(2)=3.E1    ! angle
C     PARMIN(3)= 22. !3. !0.07 ! 1.E-4
C     PARMAX(3)=  87. !135 !177. !8. !1.E2 !-1
CCC**	PARMIN(4)=4.E0
CCC**      PARMAX(4)=8.E0
C
CCC**	PARMIN(5)=1.E-2
CCC**      PARMAX(5)=1.E4
C	     PARMIN(4)=1e-2 !2.E0 Thermal density
C         PARMAX(4)=6e2 !88. !1.78E2
C	  !   PARMIN(5)=1.10E-1 !delta1
C      !   PARMAX(5)=2.E1
C     PARMIN(5)=del_min !1.10E-1 !delta1
C     PARMAX(5)= del_max !2.E1
C	     PARMIN(6)=0.1 !0.003 !4.E0  E_max
C         PARMAX(6)= 10. ! 0.03 !20. !0.5 !176.E0 !2
C	     PARMIN(7)=1.5E-1
C         PARMAX(7)=6.E0 !2
C	     PARMIN(8)=0.2E0
C     PARMAX(8)=16.E0 !2



	     do 1 I=1,NPAR
      PSCALE(I)=ParGuess(I)/2.
    1 continue
	



	
   10 continue

Cc			Start of the fitting: select '0' for automatic fitting - standard mode.
Cc		Later change this to the default value
Cc      print*,'Input number of the manually varying parameter',
Cc     *' or 0 for automatic fitting:'
Cc      read*,KPAR
		Kpar=0
      LMAX=1
C     if (KPAR.ne.0) then
C        LMAX=0
C        write(*,'('' Parameter ('',I2,''):'')') KPAR
C        read*,PAR(KPAR)
C     endif
C     Pause





   
       !Open(8,File='C:\GS_Modeling\Gelu_Blind_Test_1\DATA\Dim_DATA.dat')
C      Open(8,File='Dim_DATA.dat')
C	     Read(8,*) NpicX,NpicY,Nfreq
C	     !print*,'NpicX,NpicY,Nfreq=',NpicX,NpicY,Nfreq
C	!Pause
C		close(8)
C		


C	     Allocate (   FreqData(1:Nfreq),
C    &Flcp(1:Npic,1:Nfreq),Frcp(1:Npic,1:Nfreq),
C    &ErrFlcp(1:Npic,1:Nfreq),ErrFrcp(1:Npic,1:Nfreq)
C    &,ArrNrl(1:Npic),ArrBgauss(1:Npic)
C    &,ArrEmax(1:Npic),ArrAngle(1:Npic)
C    &,ArrNth(1:Npic),ArrDelta1(1:Npic)
C    &,ErrNrl(1:Npic),ErrBgauss(1:Npic)
C    &,ErrEmax(1:Npic),ErrAngle(1:Npic)
C    &,ErrNth(1:Npic),ErrDelta1(1:Npic)
C    &,ErrData(1:Npic),RChi2(1:Npic)
C    & ,FFitR(1:Npic,1:Nfreq),FFitL(1:Npic,1:Nfreq)
C    &,ArrT(1:Npic),ErrT(1:Npic)  )

Cc     &,ArrParms(1:NpicX,1:NpicY,8),ErrParms(1:NpicX,1:NpicY,8))
     
     
     
                !Assigning zeros to all arrays
          ArrNrl=0d0
          ArrBgauss=0d0
          ArrEmax=0d0
          ArrAngle=0d0
          ArrNth= 0d0
          ArrDelta1=0d0
          !If(Npar.lt.6) ArrDelta1=E_max
          ErrNrl= 0d0
          ErrBgauss=0d0
          ErrEmax=0d0
          ErrAngle=0d0
          ErrNth=0d0
          ErrDelta1=0d0
          ErrData=0d0
          RChi2=0d0
          FFitR=0d0
          FFitL=0d0
          ArrT=0d0
          !If(Npar.lt.7) ArrT=T_fix
          ErrT=0d0
          !End of Assigning zeros to all arrays   

		!CALL AllDataSet(freqdata,Flcp,Frcp,ErrFlcp,ErrFrcp) !This reads arrays of the distribution function and 
!This reads arrays of the distribution function and 
						!its parameters E,S,Mu,t
	          freqdata=fr_in
              Flcp(:,:)=spec_in(:,:,1)
              Frcp(:,:)=spec_in(:,:,2)
              ErrFlcp(:,:)=spec_in(:,:,3)
              ErrFrcp(:,:)=spec_in(:,:,4)


C1212      Format(31(1x,G12.5)) !,1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5)
C	open(1,file='TimeProMu.dat')
c	Do Imu=1,60
c	Write(1,1212)xMu(Imu),(Fit1Data(14,IMu,JS1),JS1=31,60)
c	End Do
c	close(1)




*		      open(1,file='SPIDER.IN')
*				read(1,*) NX41,NX2,NPAR,STEP0,EPS,KX
*        NXi - number of Xi data,
*        NPAR - number of parameters,
*        STEP - initial step,
*        EPS - accuracy parameter,
*        KX=1 or 2 for displaying the 1st or the 2nd argument,
*      read(1,*) (PAR(I),I=1,NPAR)       ! Initial parameters
*      close(1)


	     NYmin=1 !17 !20 !23 !28
	     NYmax=Npic !17 !20 !28 !29

	     NXmin=1 !10 !1 !NpicX-2 !16 !1 !NpicX-3 !1 !21 !15 !1 !6 !30 
	     NXmax=Npic !NXmin+10 !NpicX !11 !6 !30 !60
		
			Nx1=Nfreq !64 !30 !
			


         !write(*,*) 'X2=  ', X2dat

C			open(22,file='TErrFastElDens.dat')
C			open(3,file='TErrBgauss.dat')
C			open(4,file='TErrTheta.dat')
C			open(5,file='TErrThElDens.dat')
C			open(15,file='TErrDelta1.dat')
C			open(25,file='TErrEmax.dat')
C			open(27,file='XChi2.dat')
C           open(24,file='TErrT.dat')



		
		Par=ParGuess
		IparCorrX=Npic !100 !102 !18
		IparCorrY=Npic !100
		
C		Do JMu=NMumin, NMumax !25,NMumax ! NMumin, NMumax !36, NJMu !Nx=1,NxTot 1,NJS !32,32 !
C           ! !$OMP PARALLEL DO		
			    Do Nx=NXmin, NXmax  !,4 !12, NEmax !NEmin, NEmax !NJE,NJE !1, 29 !NJE,NJE  28,28 
			    !Do Ny=NYmin, NYmax !,4 !21,NSmax ! NSmin, NSmax
		!Do Nx=NXmin, NXmax  !,4 !12, NEmax !NEmin, NEmax !NJE,NJE !1, 29 !NJE,NJE  28,28 !
		
		

		!If(Nx.lt.IparCorr.OR.Nx.eq.60.OR.Nx.eq.64) then 
		If(Nx.le.IparCorrX) then 
		Par=ParGuess
		DPar=ParGuess
		EndIf
		    
		
		Step=Step0

*		print*,Par,Step
*	Pause



		call SpecPixel(Nx,freqdata,Flcp,Frcp,ErrFlcp,ErrFrcp) !Call the time profile to fit with 
									!the trapping function

	     TestFlux=sum(Fundat)
	     TestFluxR=sum(Fundat(:,1))
	     TestFluxL=sum(Fundat(:,2))
	     StocksV=TestFluxR-TestFluxL
      

      
	     If(StocksV.lt.0) Par(3)=180.-Par(3) !Change trial viewing angle for L polarization
      
		    !print*,'TestFlux=',TestFlux,'Nx #',Nx,'Ny #',Ny  

	     !If(TestFlux.le.22.OR.(TestFlux.ge.23.AND.TestFlux.le.32)) then ! 40 for unfolded model
	     If(TestFlux.le.(Fl_thr)) then ! 40 for unfolded model
	     !If(TestFlux.le.22) then ! 40 for unfolded model
	     !print*,'TestFlux=',TestFlux
          !print*,'TestFlux=',TestFlux,'Nx #',Nx,'Ny #',Ny  
	     go to 2104
	     End If
         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        print*,'TestFlux=',TestFlux,'Nx #',Nx,'Ny #',Ny 
	!Pause
	



Cc		the fitting is being done by calling the SIMPLEX function
                    ix1=0
      do 4 L=0,LMAX
Cc		write(*,*)Fundat(1:10,1)

		

      if (L.eq.1) call SIMPLEX
        
      AvErr=Delta(ErrMx,IX1,IX2,RedChi2)
                        	       
          
      X1=X1dat(IX1)

      !X2=X2dat(IX2)
C     if (NPAR.gt.0) then
C
C        write(*,'('' Parameters:''/(1P,1X,10E11.4))') (PAR(I),I=1,NPAR)
C        write(*,'('' DeltaParam:''/(1P,10E11.4))') (DPAR(I),I=1,NPAR)
C     endif
CCc		write(*,*)Fundat(1:10,1)
C
C
C     write(*,'('' RMS error:'',1PE11.4)') AvErr
C      print*,'  '
C      Write(*,*)'Chi2=',RedChi2
C             print*,'  '
C     write(*,'('' MAX error:'',1PE11.4,'' at X1 ='',E10.3,'', X2 ='',
C    *E10.3)') ErrMx,X1,X2
      !Pause
    4 continue
C     open(1,file='SPIDER.RES')
C     if (NPAR.gt.0) then
C        write(1,'(''Parameters:''/(1P,10E11.4))') (PAR(I),I=1,NPAR)
C        write(1,'(''DeltaParam:''/(1P,10E11.4))') (DPAR(I),I=1,NPAR)
C     endif
C     write(1,'(''RMS error:'',1PE11.4)') AvErr
C     write(1,'(''MAX error:'',1PE11.4,'' at X1 ='',E10.3,'', X2 ='',
C    *E10.3)') ErrMx,X1,X2
C     close(1)
C     call DRAWING(KPAR)
      

	!open(9,file='C:\GS_Modeling\Gelu_Blind_Test_1\DATA\FitRes_2011.dat')
612      Format(1x,G12.5,6(1x,G12.5))	
C	     open(9,file='FitRes_2018.dat')
CCcc		writing the original and fitted light curves for a specific set of parameters E,Mu,and S 				
		Do I=1,NX1
	     xxxMu=X1dat(I)
	     Dum=0
	     FitProf=FitFun(xxxMu,dum)  !/100.
CCc	Spectrum=Fit1Data(NE,I,JS) !FunDat(I,1) !
CCc	ErrProf=Err1Data(NE,I,JS)
CCc
CCcc	     Write(9,12)xxxMu,TimeProf,ErrProf,FunDat(I,1),FitProf
Cc	     Write(9,612)xxxMu,FunDat(I,2),ErrDat(I,2),dum ,
Cc     &FunDat(I,1) ,ErrDat(I,1),FitProf
C    	     Write(9,612)xxxMu,FunDat(I,2)+FunDat(I,1),ErrDat(I,2)+
C    &ErrDat(I,2),dum+FitProf
Cc    &,FunDat(I,2),ErrDat(I,2) !,dum
      FFitR(Nx,I)=FitProf
      FFitL(Nx,I)=dum
C
			End Do
C	    Close(9)
Ccspi 		Pause

ccspi			w=w*1.1
ccspi	End Do
		Ne=Nx
	     Js=Ny
			
		ErrData(NX)=AvErr
		
		 

	     ArrNrl(NX)=1e7*Par(1) !Fast electron number density
	     ArrBgauss(NX)=Par(2)*1e2 !Absolute value of magnetic field
      ArrEmax(NX)=Par(3)	!Theta in Degrees
	     ArrAngle(NX)=Par(4)*0.1e10 !Thermal density
          ArrNth(NX)=Par(5) !delta1
	     ArrDelta1(NX)=Par(6) !Emin
                      ArrT(Nx)=Par(7) 

	     RChi2(NX)=RedChi2
         
          If(Npar.lt.6) ArrDelta1(NX)=E_max
          If(Npar.lt.7) ArrT(NX)=T_fix


        !!!Use of AvErr is needed to form 'weights' of the data points for further fitting or analysis
	!ErrNrl(NX,NY)=1e7*sqrt((AvErr*Par(1))**2+DPAR(1)**2)/4. !*1e16
	!ErrBgauss(NX,NY)=sqrt((AvErr*Par(2))**2+DPAR(2)**2)*1e2/4.
      !ErrEmax(NX,NY)=sqrt((AvErr*Par(3))**2+DPAR(3)**2)/4.	!*7.27e7
	!ErrAngle(NX,NY)=sqrt((AvErr*Par(4))**2+DPAR(4)**2)*0.1e10/4. !Thermal density
      !ErrNth(NX,NY)=sqrt((AvErr*Par(5))**2+DPAR(5)**2)/4. !*1e10
	!ErrDelta1(NX,NY)=sqrt((AvErr*Par(6))**2+DPAR(6)**2)/4. !*1e2
	
	  !Use DPAR only below to estimate fit errors only 
        CorrErr=1.    
          ErrNrl(NX)=1e7*DPAR(1)/CorrErr !*1e16
	     ErrBgauss(NX)=DPAR(2)*1e2/CorrErr
          ErrEmax(NX)=DPAR(3)/CorrErr	!*7.27e7
	     ErrAngle(NX)=DPAR(4)*0.1e10/CorrErr !Thermal density
          ErrNth(NX)=DPAR(5)/CorrErr !*1e10
	     ErrDelta1(NX)=DPAR(6)/CorrErr !*1e2
             ErrT(Nx)=DPar(7)         
             
           If(Npar.lt.6) ErrDelta1(NX)=0d0
           If(Npar.lt.7) ErrT(NX)=0d0                            


	

			

Cc++++++++++++++below we consider the case of symmetric magnetic trap: integration time is reduced
Cc			by the factor of 2: JMu changes 1 to 30 rather than 1 to 60			
			
		

2104		Continue


				Xcm=2.*(Nx-1) !Arcseconds along Y axes
		

C	     If (ArrBgauss(Nx,Ny).gt.1e-2) then
C	     
C			Write(22,*)Xcm,ArrNrl(Nx,Ny),ErrNrl(Nx,Ny) !Fast e density
C			Write(3,*)Xcm,ArrBgauss(Nx,Ny),ErrBgauss(Nx,Ny) !B field
C			Write(4,*)Xcm,ArrEmax(Nx,Ny),ErrEmax(Nx,Ny) !Theta (viewing angle)
C
C			Write(5,*)Xcm,ArrAngle(Nx,Ny),ErrAngle(Nx,Ny)  !Thermal density
C			Write(15,*)Xcm,ArrNth(Nx,Ny),ErrNth(Nx,Ny)      !delta1
C			Write(25,*)Xcm,ArrDelta1(Nx,Ny),ErrDelta1(Nx,Ny) !Emin
C			Write(27,*)Xcm,RChi2(Nx,Ny)
C			Write(24,*)Xcm,ArrT(Nx,Ny),ErrT(Nx,Ny)              
C
C	     End If


        DPar(:)=Par(:)
        If(Nx.le.IparCorrX) DPar=ParGuess


C			Do i=1,Nx1
C		write(*,*)NE !X1dat(i),x1
C	end do
	!If(TestFlux.gt.(3)) pause
			End Do	!Nx
C			End Do	!Ny
C           ! !$OMP END PARALLEL DO



C		Close(22)
C		Close(3)
C		Close(4)
		!Close(5)
c		Close(15)
C		Close(25)
C		Close(27)
C      		Close(24)  

c      open(3,file='c:\GS_Modeling\EOVSA_Model_Fit_Test_1
c     &\Some_Fit_Results\AlSp.dat')
              spec_out(:,:,1)=FFitL(:,:)
              spec_out(:,:,2)=FFitR(:,:)
C       		open(3,file='AllFitSpecR.dat')
C       		open(4,file='AllFitSpecL.dat')
C       Write(3,*)FFitR(:,:,:)
C       Write(4,*)FFitL(:,:,:)
C		Close(3)
C		Close(4)
C		
C			
C			open(2,file='ArrNrl.dat')
C			open(3,file='ArrBgauss.dat')
C			open(4,file='ArrTheta.dat')
C			open(5,file='ArrDensTherm.dat')
C			open(15,file='ArrDelta1.dat')
C			open(25,file='ArrEmin.dat')
C			open(24,file='ArrT.dat')             
C			open(27,file='ArrChi2.dat')
C	     
C			Write(2,*)ArrNrl(:,:)
C			Write(3,*)ArrBgauss(:,:)
C			Write(4,*)ArrEmax(:,:) 
C
C			Write(5,*)ArrAngle(:,:)
C			Write(15,*)ArrNth(:,:)
C			Write(25,*)ArrDelta1(:,:)
C			Write(24,*)ArrT(:,:)              
C			Write(27,*)RChi2(:,:)
C
C
C		Close(2)
C		Close(3)
C		Close(4)
C		Close(5)
C		Close(15)
C		Close(25)
C       		Close(24) 
C               
            ArrParms(:,1)=ArrNrl(:)       !Non-thermal density*1d7
			ArrParms(:,2)=ArrBgauss(:)    !B-field*1e2
			ArrParms(:,3)=ArrEmax(:)      !theta, degrees
            ArrParms(:,4)=ArrAngle(:)     !Thermal density * 1d9
			ArrParms(:,5)=ArrNth(:)       !delta1
			ArrParms(:,6)=ArrDelta1(:)    !E_max, MeV
			ArrParms(:,7)=ArrT(:)         !T *10^7 K           
			ArrParms(:,8)=RChi2(:)        !Fit Chi-2
            
            
            ErrParms(:,1)=ErrNrl(:)       !Non-thermal density*1d7
			ErrParms(:,2)=ErrBgauss(:)    !B-field*1e2
			ErrParms(:,3)=ErrEmax(:)      !theta, degrees
			ErrParms(:,4)=ErrAngle(:)     !Thermal density * 1d9
			ErrParms(:,5)=ErrNth(:)       !delta1
			ErrParms(:,6)=ErrDelta1(:)    !E_max, MeV
			ErrParms(:,7)=ErrT(:)         !T *10^7 K              
			ErrParms(:,8)=ErrData(:)      !Average fit error
                
                

C			open(2,file='ErrNrl.dat')
C			open(3,file='ErrBgauss.dat')
C			open(4,file='ErrTheta.dat')
C			open(5,file='ErrDensTherm.dat')
C			open(15,file='ErrDelta1.dat')
C			open(25,file='ErrEmin.dat')
C           open(24,file='ErrT.dat')
C           
C
C			open(22,file='ErrData.dat')
C
C	     
C			Write(2,*)ErrNrl(:,:)
C			Write(3,*)ErrBgauss(:,:)
C			Write(4,*)ErrEmax(:,:) 
C
C			Write(5,*)ErrAngle(:,:)
C			Write(15,*)ErrNth(:,:)
C			Write(25,*)ErrDelta1(:,:)
C			Write(24,*)ErrT(:,:)             
C
C			Write(22,*)ErrData(:,:)
C
C
C		Close(22)
C
C
C		Close(2)
C		Close(3)
C		Close(4)
C		Close(5)
C		Close(15)
C		Close(25)
C		Close(27)
C       		Close(24) 

		!Npic=Nx*Ny


	
	
*		Do Nx=NXmin, NXmax !12, NEmax !NEmin, NEmax !NJE,NJE !1, 29 !NJE,NJE  28,28 !
*		Xcm=7.327e7*(Nx-1)
*		Do Ny=NYmin, NYmax !21,NSmax ! NSmin, NSmax
*
*	If (ArrBgauss(Nx,Ny).gt.0) then
*	     
*			Write(2,*)Xcm,ArrNrl(Nx,Ny),ErrNrl(Nx,Ny)
*			Write(3,*)Xcm,ArrBgauss(Nx,Ny),ErrBgauss(Nx,Ny)
*			Write(4,*)Xcm,ArrDepth(Nx,Ny),ErrDepth(Nx,Ny) 
*
*			Write(5,*)Xcm,ArrAngle(Nx,Ny),ErrAngle(Nx,Ny)
*			Write(15,*)Xcm,ArrNth(Nx,Ny),ErrNth(Nx,Ny)
*			Write(25,*)Xcm,ArrDelta1(Nx,Ny),ErrDelta1(Nx,Ny)
*	End If
*	End Do
*	End Do





Cc		Do JS= 1,NJS !36,NJMu !
Cc		Do NE=NEmin,NEmax !7,11 !NJE, NJE !1,29 !JE,JE !27,27 !1,1 !29 

		 !Write(2,12)Energy(NE),TauR(NE,JS),ErrTau(NE,JS) !,Err(NE,JS)*TauR(NE,JS) !Alldata(Nx,Ny,20) !   !Ampl(Ltime)	
		 !Write(3,12)Energy(NE),Ampl(NE,JS),ErrAmpl(NE,JS) !,Err(NE,JS)*Ampl(NE,JS)	
		 !Write(2,12)Scm(JS),TauR(NE,JS) !Alldata(Nx,Ny,20) !   !Ampl(Ltime)	
		 !Write(3,12)Scm(JS),Ampl(NE,JS)
Cc		 Write(2,12)xMu(JS),TauR(NE,JS),ErrTau(NE,JS) !Alldata(Nx,Ny,20) !   !Ampl(Ltime)	
Cc		 Write(3,12)xMu(JS),Ampl(NE,JS),ErrAmpl(NE,JS)
Cc	End Do
Cc	End Do


12      Format(1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5)
             !,1x,G12.5,1x,G12.5,1x,G12.5    &,1x,G12.5,1x,G12.5)


      !print*,'To continue? (Y/N)'
      !read(*,'(A1)') KEY
      !if (KEY.ne.'N'.and.KEY.ne.'n') goto 10
	
      return
       !stop
      end








      function RESIDUAL(DAT1,ERR1,FIT1,DAT2,ERR2,FIT2) ! The norm which square is minimizing
            IMPLICIT REAL*8 (A-H,O-Z)
			common/Weight/ErrMean,FunMean,WeightMean
            common/Fit_case/N_case_d,N_case_f
      !RESIDUAL=WeightMean*(DAT-FIT)**2/(ERR+0.4e-1*DAT+1.)**2
      
      If(N_case_d.eq.1) N_case_f=1
      
                             If(N_case_d.EQ.1) then
      If(N_case_f.EQ.1) then      
      !To fit Stokes I, one has to use two lines below:
           RESIDUAL=WeightMean*(DAT1-FIT1+DAT2-FIT2)**2/((ERR1 
     &                    +ERR2)**2)
                      EndIf  
                          EndIf
      
            If(N_case_d.EQ.2) then
                  
      If(N_case_f.EQ.1) then      
      !To fit Stokes I, one has to use the line below:
      RESIDUAL=WeightMean*(DAT1-FIT1+DAT2-FIT2)**2/(ERR1**2+ERR2**2)
 
            EndIf 

      If(N_case_f.EQ.2) then
      RESIDUAL=WeightMean*((DAT1-FIT1)**2/(ERR1)**2 
     &                    +(DAT2-FIT2)**2/(ERR2)**2)
      !Now the polarized (R&L) spectra are fit. 
                  EndIf
    
            If(N_case_f.EQ.3) then
      RESIDUAL=WeightMean*(
     &  (DAT1-FIT1+DAT2-FIT2)**2/(ERR1**2+ERR2**2) 
     & +(DAT1-FIT1-DAT2+FIT2)**2/(ERR1**2+ERR2**2))
      !Now the polarized spectra (I&V) are fit
                  EndIf  
                  
	            If(N_case_f.EQ.4) then
      RESIDUAL=WeightMean*(DAT1-FIT1+DAT2-FIT2)**2/((ERR1)**2 
     &                    +(ERR2)**2)
     &+((DAT1-DAT2)/(DAT1+DAT2+1d-5)-(FIT1-FIT2)/(FIT1+FIT2+1d-5))**2/
     &(((ERR1+1d-5)/(DAT1+1d-5))**2     +((ERR2+1d-5)/(DAT2+1d-5))**2)
c      RESIDUAL=WeightMean*((DAT1-FIT1)**2/(ERR1)**2+ 
c     &(DAT2-FIT2)**2/(ERR2)**2 
c     &+((DAT1-DAT2)/(DAT1+DAT2+0.1)-(FIT1-FIT2)/(FIT1+FIT2+0.1))**2/
c     &(ERR2/DAT2)**2)
      !Now the polarized spectra (I&P) are fit
                  EndIf 
                      EndIf
                      
                      

      return
      end

!!!!!!!!!!!!!!!  The program calls SPIDRSUB  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
