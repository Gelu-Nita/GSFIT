;+
; NAME:
;   gsfit_energy_computation
; PURPOSE:
;   Add magnetic and nonthermal electron energy maps to a GSFIT fit-map structure.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   gsfit_energy_computation, maps, header [, emin=emin]
; INPUTS:
;   MAPS: fit-map structure containing the B, ERRB, DELTA, ERRDELTA, N_NTH, and ERRN_NTH parameter maps.
;   HEADER: fit header. HEADER.INFO.RPARMS supplies the source sizes and the default low-energy cutoff.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   EMIN: low-energy cutoff in eV. The default is the fifth real library parameter, converted from keV.
; OUTPUTS:
;   MAPS is updated in place with WB, ERRWB, WNTH, and ERRWNTH, in erg.
; ROUTINES CALLED:
;   None.
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   A spectral index of 2 makes the nonthermal-energy expression divide by zero. The message is reported and the other maps are still written.
; RESTRICTIONS:
;   HEADER.INFO.RPARMS.VALUE must contain the source depth, source length, and low-energy cutoff used by the fit.
; MODIFICATION HISTORY:
;   Written 5-May-2019 Gelu M. Nita (gnita@njit.edu)
;   22-May-2019 GN: Cosmetic changes and new functionality.
;   27-May-2019 GN: Restructured the package.
;-

pro gsfit_energy_computation,maps,header,emin=emin
ds=(header.info.rparms.value)[3]
dl=(header.info.rparms.value)[4]
default,emin,(header.info.rparms.value)[5]*1000
vol=ds*dl*(7.27d7)^3
WB=vol*maps.b.data^2/8./!pi ; B energy density from a pixel
errWB=vol*2*maps.b.data*maps.errb.data/8./!pi
del=maps.delta.data
Wnth=vol*1.6d-9*emin*(del-1)/(del-2)*maps.n_nth.data
errWnth=vol*1.6d-9*emin*sqrt(maps.errdelta.data^2*maps.n_nth.data^2/(del-2)^4+((del-1)/(del-2)*maps.errn_nth.data)^2)
if ~tag_exist(maps,'WB') or ~tag_exist(maps,'Wnth') then begin
  WBmap=maps.b
  WBmap.id='WB'
  WBmap.dataunits='erg'
  WBmap.data=WB
  maps=create_struct(maps,'WB',temporary(WBmap))
  errWBmap=maps.b
  errWBmap.id='errWB'
  errWBmap.dataunits='erg'
  errwBmap.data=errWB
  maps=create_struct(maps,'errWB',temporary(errWBmap))
 
  Wnthmap=maps.b
  Wnthmap.id='Wnth'
  Wnthmap.dataunits='erg'
  Wnthmap.data=Wnth
  maps=create_struct(maps,'Wnth',temporary(Wnthmap))
  errWnthmap=maps.b
  errWnthmap.id='errWnth'
  errWnthmap.dataunits='erg'
  errwnthmap.data=errWnth
  maps=create_struct(maps,'errWnth',temporary(errWnthmap))
  
endif else begin
  maps.WB.data=WB
  maps.errWB.data=errWB
  maps.Wnth.data=Wnth
  maps.errWnth.data=errWnth
endelse

end
