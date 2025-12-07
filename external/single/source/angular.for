      subroutine ggMu(xMu,anPar,gg0,gg1,gg2)
      Implicit Real*8(A-H,O-Z)
      Real*8 anPar(4)
		common /NType/nType
		common /AngNorm/TotMu
	!!! Normalized to 2 because 1/4!pi has already been included in normalization while 
	!!! isotropic distribution was considered
		!nTyp=IFType
		pi=3.14159265
      If((nType.eq.0.or.nType.eq.1.or.nType.gt.5).and.nType.ne.111) then  !Isotropic angular distribution
	          

	     gg0=1.
	     gg1=0
	     gg2=0

	     return 
		 End If
	!write(*,*)'Tot=',Tot
c==========================================================
      If(nType.eq.111) then  !Sin^N loss-cone angular distribution
	
	
	          
	          Cc=cos(pi*AnPar(1)/180.)
	          TTc=acos(Cc)
	          NSi=Int(AnPar(2)+0.1)
	          dMu2=AnPar(3)
	          
	          gg0=1./TotMu
	          gg1=0
	          gg2=0
	          
	        
	 
   
     
         If(abs(xMu).ge.Cc) then 
          Teta=acos(xMu)
         If(xMu.lt.0) Teta=Teta-pi
         
          C1=cos(pi*Teta/2./TTc)
          S1=(sin(pi*Teta/2./TTc))**(NSi-1)
          
            gg0=(sin(pi*Teta/2./TTc))**NSi/TotMu
         gg1=-(pi*NSi/(2.*TTc)/sqrt(1.-xMu*xMu))*C1*S1/TotMu
         
         gg2=(NSi*pi/(8.*(1 - xMu**2)**1.5* TTc*2))*
     &S1/sin(pi*Teta/2/TTc)*(sqrt(1 - xMu**2)*(-2 + NSi)* pi + 
     &sqrt(1 - xMu**2)*NSi*pi*Cos((pi*Teta)/TTc) - 
     &  2 *xMu* TTc* Sin((pi*Teta)/TTc))/TotMu !/gg0
     
        gg2K=-pi**2*NSi/4./TTc**2/(1.-xMu**2)*
     &(1+2*TTc/pi/sqrt(1-xMu**2)*C1/sin(pi*Teta/2./TTc)- 
     &(NSi-1)*C1/sin(pi*Teta/2./TTc)/sin(pi*Teta/2./TTc))*gg0
        !gg2=1.
         end If        
        !Open(9,File='spectrum.dat')
        !write(9,*)gg20,gg2
        !Close(9)
	   					
	      return 
		 End If     !End of Sin^N loss-cone angular distribution
		 


c==========================================================
      If(nType.eq.2) then  !Exponential loss-cone angular distribution
	          Cc=cos(pi*AnPar(1)/180.)
	          	          If(abs(Cc).lt.1e-6) Cc=0
	          dMu2=AnPar(3)
	          
	          gg0=1./TotMu
	          gg1=0
	          gg2=0
         
	   					If(abs(xMu).ge.Cc) then 
			        gg0=1e-35
			 If(xMu.gt.0) then
			  gg0T=exp(-(xMu-Cc)/dMu2)/TotMu
	          If(gg0T.gt.0) gg0=gg0T
			
			 gg1=-gg0/dMu2
	       gg2=gg0/dMu2/dMu2
					EndIf	         
			 
			 If(xMu.lt.0) then
			 gg0T=exp((xMu+Cc)/dMu2)/TotMu
	          If(gg0T.gt.0) gg0=gg0T
			
			 gg1=gg0/dMu2
	       gg2=gg0/dMu2/dMu2	
					EndIf
             
							end If

	      return 
		 End If
		 
c==========================================================
      If(nType.eq.3) then  !Gaussian loss-cone angular distribution
	          Cc=cos(pi*AnPar(1)/180.)
	          If(abs(Cc).lt.1e-6) Cc=0
	          dMu2=AnPar(3)
	          
	          gg0=1./TotMu
	          gg1=0
	          gg2=0
         
	   					If(abs(xMu).ge.Cc) then 
	   					gg0=1e-29

			 
			 If(xMu.gt.0) then
			 gg0T=exp(-(xMu-Cc)**2/dMu2**2)/TotMu
	          If(gg0T.gt.0) gg0=gg0T
			 
			 gg1=-gg0*2*(xMu-Cc)/dMu2/dMu2
	       gg2=-2*gg0/dMu2/dMu2*(1-2*(xMu-Cc)**2/dMu2/dMu2)
					EndIf	         
			 
			 If(xMu.lt.0) then
			  gg0T=exp(-(xMu+Cc)**2/dMu2**2)/TotMu
	          If(gg0T.gt.0) gg0=gg0T
			 
			 gg1=-gg0*2*(xMu+Cc)/dMu2/dMu2
	       gg2=-2*gg0/dMu2/dMu2*(1-2*(xMu+Cc)**2/dMu2/dMu2)	
					EndIf
             
							end If

	      return 
		 End If	
c==========================================================
      If(nType.eq.4) then  !gaussian/beam angular distribution
	          xMu0=cos(pi*AnPar(2)/180.)
	          xMuSt=AnPar(3)
	          
	           gg0=1e-29

	     gg0T=exp(-(xMu-xMu0)*(xMu-xMu0)/xMuSt/xMuSt)/TotMu
	          If(gg0T.gt.1e-29) gg0=gg0T

	     gg1=-2*(xMu-xMu0)/xMuSt/xMuSt*gg0
      gg2=-2/xMuSt/xMuSt*gg0*(1-2*(xMu-xMu0)*(xMu-xMu0)/xMuSt/xMuSt)

	     return 
		 End If
c==========================================================
      If(nType.eq.5) then  !Supergaussian/beam angular distribution
	          xMu0=cos(pi*AnPar(2)/180.)
	          xMuSt=AnPar(3)
	          a4=AnPar(4)

        gg0=1e-35
        !gg1=gg0*gg0
        !gg2=gg0*gg0

        
	     gg0T=exp(-(a4*(xMu-xMu0)**4+(xMu-xMu0)**2)/xMuSt/xMuSt)/TotMu
	          If(gg0T.gt.0) gg0=gg0T ! then
	
	     gg1=-(4*a4*(xMu-xMu0)**3+2*(xMu-xMu0))/xMuSt/xMuSt*gg0
	     gg2=(-(2+12*a4*(xMu-xMu0)**2)/xMuSt/xMuSt+
     &((2*(xMu-xMu0)+4*a4*(xMu-xMu0)**3)/xMuSt/xMuSt)**2)*gg0
                !End If
	     return 
		 End If
		 
		 
	     end
	
	     function dLnG(xMu,anPar,nType)
      IMPLICIT REAL*8 (A-H,O-Z)         
	      !character*3 Atype
	     Real*8 anPar(4)
	      common /AngNorm/TotMu
	 
	     pi=3.14159265
	 
      If((nType.eq.0.or.nType.eq.1.or.nType.gt.5).and.nType.ne.111) then  !Isotropic angular distribution	
	          	          dLnG=0
	              	     Return
	  End If
	  
c==========================================================
      If(nType.eq.111) then  !Sin^N loss-cone angular distribution
	
	
	         
	          Cc=cos(pi*AnPar(1)/180.)
	          TTc=acos(Cc)	          
	          NSi=Int(AnPar(2)+0.1)
	          dMu2=AnPar(3)
	          
	         
	           dLnG=0

     
         If(abs(xMu).ge.Cc) then 
          Teta=acos(xMu)
         If(xMu.lt.0) Teta=Teta-pi
         
          C1=cos(pi*Teta/2./TTc)
          
      dLnG=-(pi*NSi/(2.*TTc)/sqrt(1.-xMu*xMu))*C1/sin(pi*Teta/2./TTc)

          
         end If        
       
        
	   					
	      return 
		 End If     !End of Sin^N loss-cone angular distribution
  
	  
c==========================================================
      If(nType.eq.2) then  !Exponential loss-cone angular distribution
	          Cc=cos(pi*AnPar(1)/180.)
	          If(abs(Cc).lt.1e-6) Cc=0	          
	          dMu2=AnPar(3)
	          
	          dLnG=0
	          
         
	   					If(abs(xMu).ge.Cc) then 
			 
			 If(xMu.gt.0) then
			 dLnG=-1./dMu2
	       	EndIf	         
			 
			 If(xMu.lt.0) then
			 dLnG=1./dMu2
				EndIf
							end If

	      return 
		 End If
		 
c==========================================================
      If(nType.eq.3) then  !Gaussian loss-cone angular distribution
	          Cc=cos(pi*AnPar(1)/180.)
          If(abs(Cc).lt.1e-6) Cc=0	          
	          dMu2=AnPar(3)
	          
	          dLnG=0
	          
         
	   					If(abs(xMu).ge.Cc) then 
			 
			 If(xMu.gt.0) then
			 dLnG=-2*(xMu-Cc)/dMu2/dMu2
	       	EndIf	         
			 
			 If(xMu.lt.0) then
			 dLnG=-2*(xMu+Cc)/dMu2/dMu2
				EndIf
							end If

	      return 
		 End If
	
	  
c==========================================================
	     If(nType.eq.4) then  !gaussian/beam angular distribution
	          xMu0=cos(pi*AnPar(2)/180.)
	          xMuSt=AnPar(3)
	     dLnG=-2*(xMu-xMu0)/xMuSt/xMuSt
	     
	     Return
	     End If

c==========================================================
      If(nType.eq.5) then  !Supergaussian/beam angular distribution
	          xMu0=cos(pi*AnPar(2)/180.)
	          xMuSt=AnPar(3)
	          a4=AnPar(4)

      dLnG=-(4*a4*(xMu-xMu0)**3+2*(xMu-xMu0))/xMuSt/xMuSt
	
	!If(dLnGT.gt.1e-10) dLnG=dLnGT

	     return 
		 End If	 
		 	 End
c==========================================================
	 
	     function AnNorm(anPar,nType)
         Implicit Real*8(A-H,O-Z)
	     !character*3 Atype
	     Real*8 anPar(4)
	 
	     pi=3.14159265
	     !common /AngNorm/TotMu
      If((nType.eq.0.or.nType.eq.1.or.nType.gt.5).and.nType.ne.111) then  !Isotropic angular distribution	

	          	          AnNorm=1.
	              	     Return
	  End If

c==========================================================
      If(nType.eq.111) then  !Sin^N loss-cone angular distribution
	
	      MuMax=101
	      TotMu=0
	          TTc0=pi*AnPar(1)/180.
	          Cc=cos(pi*AnPar(1)/180.)
	          TTc=acos(Cc)
	          NSi=Int(AnPar(2)+0.1)
	          dMu2=AnPar(3)
	          
	          
	         xMu=-1.
	     dMu=2./(MuMax*1.)
	     !dMu=1./(MuMax*1.)
	     !Open(9,File='spectrum.dat')
	     !Do j=1,100
	
	     !End Do
	
				
	     Do Mu=1,MuMax
	          gg=1.
   
     
         If(abs(xMu).ge.Cc) then 
          Teta=acos(xMu)
         If(xMu.lt.0) Teta=Teta-pi
         
          !C1=cos(pi*Teta/2.d0/TTc)
                   
            gg=(sin(pi*Teta/2./TTc))**NSi
C            gg1=-(pi*NSi/(2.*TTc)/sqrt(1.-xMu*xMu))*
C     &C1*(sin(pi*Teta/2./TTc))**(NSi-1)
           end If        
         TotMu=TotMu+dMu*gg
	      xMu=xMu+dMu
C	       Write(9,*)xMu,gg,gg1
          End Do !Mu=1,101
C	      Close(9)
      AnNorm=TotMu/2. !normalized to 2!!
	     !AnNorm=TotMu
       
	   					
	      return 
		 End If     !End of Sin^N loss-cone angular distribution


c==========================================================
	     If(nType.eq.2) then  !Exponential loss-cone angular distribution
	      !nType=3    ;distribution over pitch-angle (sin^N)
        !AnPar[1]=90   ;loss-cone boundary, degrees
        !AnPar[2]=6    ;N (in the sin^N distribution)
        !AnPar[3]=0.1   ;deltaMu for gaussian loss-cone
	     

	     

	     Cc=cos(pi*AnPar(1)/180.)
		          If(abs(Cc).lt.1e-6) Cc=0
	     dMu2=AnPar(3)
		     !If(dMu2.lt.0.01) then
	     !If(MuMax.lt.5./dMu2) MuMax=Int(5./dMu2)
	     If(dMu2.lt.0.0001) then
	     AnNorm=Cc+dMu2
	     else
		     AnNorm=Cc+dMu2*(1.-exp((Cc-1.)/dMu2))
		     end if
	     Return
	      End If

c==========================================================
      If(nType.eq.3) then  !Gaussian loss-cone angular distribution
	      !nType=3    ;distribution over pitch-angle (sin^N)
        !AnPar[1]=90   ;loss-cone boundary, degrees
        !AnPar[2]=6    ;N (in the sin^N distribution)
        !AnPar[3]=0.1   ;deltaMu for gaussian loss-cone
	     
	     Cc=cos(pi*AnPar(1)/180.)
		          If(abs(Cc).lt.1e-6) Cc=0
	     dMu2=AnPar(3)
	     If(dMu2.lt.0.01) then
	     AnNorm=Cc+sqrt(pi)*dMu2/2.
	     !Open(8,File='ATest_Cc.txt')
       !           Write(8,*)Cc,dMu2,Annorm
        !          close(8)
	     return
	     End If
      
	     MuMax=101
	     TotMu=0
	     xMu=0.
	     dMu=1./(MuMax*1.)
	     Do Mu=1,MuMax
      
	
	
	          gg=1.
	          If(abs(xMu).ge.Cc) then 
			gg=0
			 If(xMu.gt.0) then
			  gg0T=exp(-(xMu-Cc)**2/dMu2**2)
	          If(gg0T.gt.0) gg=gg0T
			 					EndIf	         
			 
			 If(xMu.lt.0) then
			 gg0T=exp(-(xMu+Cc)**2/dMu2**2)
	          If(gg0T.gt.0) gg=gg0T
			 
					EndIf
				EndIf
                        
             TotMu=TotMu+dMu*gg
             xMu=xMu+dMu
             End Do !Mu=1,101
             AnNorm=TotMu/1. !normalized to 2!!
          
                 Return
	     End If
	  
c==========================================================
	     If(nType.eq.4) then  !gaussian/beam angular distribution
              !nType=3    ;distribution over pitch-angle (sin^N)
            !AnPar[1]=90   ;loss-cone boundary, degrees
            !AnPar[2]=6    ;N (in the sin^N distribution)
            !AnPar[3]=0.1   ;deltaMu for gaussian loss-cone
             
	     xMu0=cos(pi*AnPar(2)/180.)
	     xMuSt=AnPar(3)
	     If(xMuSt.lt.0.01) then
	     AnNorm=sqrt(pi)*xMuSt/2.
	     return
	     End If
      
	     MuMax=101
	     TotMu=0
	
	
	
	     xMu1=-1.
	     dMu=2./(MuMax*1.)
	     Do Mu=1,MuMax
      
	     gg=0
	     ggT=exp(-(xMu1-xMu0)*(xMu1-xMu0)/xMuSt/xMuSt)
	     If(ggT.gt.1e-30) gg=ggT
	
		 TotMu=TotMu+dMu*gg
		 
		 xMu1=xMu1+dMu
	     End Do !Mu=1,101
	     AnNorm=TotMu/2.
      
             Return
	     End If
      
c==========================================================
	     If(nType.eq.5) then  !Supergaussian/beam angular distribution
              !nType=3    ;distribution over pitch-angle (sin^N)
            !AnPar[1]=90   ;loss-cone boundary, degrees
            !AnPar[2]=6    ;N (in the sin^N distribution)
            !AnPar[3]=0.1   ;deltaMu for gaussian loss-cone
            
	     xMu0=cos(pi*AnPar(2)/180.)
	     xMuSt=AnPar(3)
            

	     a4=AnPar(4)
	     If(xMuSt.lt.0.01) then
	     AnNorm=sqrt(pi)*xMuSt/2.
	     return
	     End If
      
	     MuMax=101
	     TotMu=0
      
	     xMu=-1.
	     dMu=2./(MuMax*1.)
	     Do Mu=1,MuMax
	     gg=0
	     ggT=exp(-(a4*(xMu-xMu0)**4+(xMu-xMu0)**2)/xMuSt/xMuSt)
	     If(ggT.gt.0) gg=ggT
	     TotMu=TotMu+dMu*gg
	     xMu=xMu+dMu
	     End Do !Mu=1,101
	     AnNorm=TotMu/2.
	
	     Return
	      End If	 
	      End