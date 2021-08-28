;+
; NAME:
;    eovsa_fits2map
; PURPOSE:
;    this function can be used to import a sequence of EOVSA fits files 
;    and convert them to a map structure either in Tb or sfu units
; CATEGORY:
;    EOVSA
; CALLING SEQUENCE:
;      maps=eovsa_fits2map(files,flux_threshold=flux_threshold,rms_mask=rms_mask,sfu=sfu,no_rms=no_rms)
;
; INPUTS:
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
; 
; 
; /SFU: to convert the original Tb maps to Solar Flux maps
; 
; FLUX_THRESHOLD: set it to a value representing an user defined minimum integrated flux 
;                 over all bands for a given pixel intensity not to be considered noise. 
;                 The default value is 1 sfu.
; 
; RMS_MASK: provide an rms_mask as a nx*ny bit mask to indicate the pixels that should be included in the RMS computation.
;          if rms_mask is provided, the flux_threshold, if present, is ignored
; 
; ROUTINES CALLED:
; vla_fits2map
;
; OUTPUTS:
; COMMENTS:
; SIDE EFFECTS:
; IF no SFU conversion is requested and no RMS_MASK is provided, 
;       the FLUX_THRESHOLD is interpreted as a FOV-integrated Temperature threshold  (default 1K),
;       which may have no physical interpretation. TO BE REVISED!
; RESTRICTIONS:
; MODIFICATION HISTORY:
;     Written 13-Nov-2020 Gelu M. Nita (gnita@njit.edu)
;     14-Nov-2020 GN: Added functionality to compute data RMS for either XX-YY, Tb or SFU maps
;     15-Nov-2020 GN: Added kyword to skip RMS computation
;     15-July-2021 GN: 
;        Updated RMS computation
;        Updated Tb2sfu conversion to complay with the I=(XX+YY)/2 convention
;        Removed the default option of converting the original data to RR-LL, which was done in the previous versions considering wrong assumptions  
;     
function eovsa_fits2map,files,_extra=_extra
 default,flux_threshold,1
 nfiles=n_elements(files)
 if nfiles eq 0 then return,!null
 dimensions=['Freq','Pol','Time']
 vla_fits2map, files[0], dummy_map
 if ~tag_exist(dummy_map,'stokes')then return,!null
 if ~tag_exist(dummy_map,'freq')then return,!null
 if ~tag_exist(dummy_map,'time')then return,!null
 has_rms=tag_exist(dummy_map,'rms')
 add_prop,dummy_map,dimensions=dimensions
 add_prop,dummy_map,datatype='Brightness Temperature'
 add_prop,dummy_map,dataunit='K'
 if ~has_rms then add_prop,dummy_map,rms=0.0
 dummy_map.data=!values.f_nan
 dummy_map.ID='Missing'
 maps=replicate(dummy_map,nfiles)
 for i=0,nfiles-1 do begin
  vla_fits2map, files[i], amap
  add_prop,amap,dimensions=dimensions
  add_prop,amap,datatype='Brightness Temperature'
  add_prop,amap,dataunit='K'
  if ~has_rms then add_prop,amap,rms=0.0
  maps[i]=amap
 end
 return,eovsa_maps2gsfit(maps,_extra=_extra)
end