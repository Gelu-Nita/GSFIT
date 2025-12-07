!!!!!!!!!!!!Selecting Spatially resolved spectrum from a given pixel (Jx,Jy) !!!!!!!!!!!!!!!!!!!!!!!!
      subroutine SpecPixel(Jx,FreqD,Flc,Frc,ErrFlc,ErrFrc) ! Selecting from input data (FPData)
      IMPLICIT REAL*8 (A-H,O-Z)      
      common/VAR/EPS,STEP,NPAR,NX1,NX2,KX
      common/ARR/FUNDAT(300,9),ERRDAT(300,9),X1dat(300),X2dat(9),
     *PAR(15),DPAR(15),VECT(15),PSCALE(15),PARmin(15),PARmax(15),
     *FUNC(19),AS(19,15)


C     & ,Times(25) 
		common/Weight/ErrMean,FunMean,WeightMean
			common/Indecs/NpicX,Nfreq
	
		     Real*8 FreqD(Nfreq),
     &Flc(NpicX,Nfreq),Frc(NpicX,Nfreq),
     &ErrFlc(1:NpicX,1:Nfreq),ErrFrc(1:NpicX,1:Nfreq)
      
		ErrMean=0
		FunMean=0
	
		     do 11 I=1,NX1
	
		X1dat(I)=FreqD(I)*1e9		
C		FUNDAT(I,1)=Fit1Data(JE,I,JS)
C		ERRDAT(I,1)=Fit1Data(JE,I,JS) !1e0
		FUNDAT(I,1)=Frc(Jx,I) ! RCP flux
		FUNDAT(I,2)=Flc(Jx,I) ! LCP flux
		ERRDAT(I,1)=(ErrFrc(Jx,I)+2e-2*FUNDAT(I,1)+0.0001) !*0.5 !+0.99e-1*FUNDAT(I,1)- for Altuntsev's flare 
		        !+8.5e-2*FUNDAT(I,1)+1. Gary et al. (2013)
		ERRDAT(I,2)=(ErrFlc(Jx,I) +2e-2*FUNDAT(I,2)+0.0001) !*0.5 !/3.
		
		ErrMean=ErrMean+ERRDAT(I,1)**2 +ERRDAT(I,2)**2			
		FunMean=FunMean+FunDAT(I,1)**2+FunDAT(I,2)**2

   11 continue

		ErrMean=ErrMean/(NX1*2.)
		FunMean=FunMean/(NX1*2.)
			WeightMean=ErrMean/FunMean
      !print*,'ErrMean=',ErrMean,'FunMean=',FunMean,'Weight=',WeightMean
      
      return
      end