;+
; NAME:
;    eovsa_fits2map
; PURPOSE:
;    this function can be used to import a sequence of EOVSA fits files 
;    and convert them to a map structure either in Tb or sfu units
; CATEGORY:
;    EOVSA
; CALLING SEQUENCE:
;      maps=eovsa_fits2map(files,no_iv2rl=no_iv2rl,flux_threshold=flux_threshold,rms_mask=rms_mask,sfu=sfu,no_rms=no_rms)
;
; INPUTS:
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
; 
; /NO_IV2RL: to return original I-V instead of LCP-RCP maps
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
;     14-Nov-2020 GN: Added functionality to compute data RMS for either I-V, RCP-LCP, Tb or SFU maps
;     15-Nov-2020 GN: Added kyword to skip RMS computation
function eovsa_fits2map,files,no_iv2rl=no_iv2rl,flux_threshold=flux_threshold,rms_mask=rms_mask,sfu=sfu,no_rms=no_rms
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
 freq=long(float(maps.freq)*1000)/1000. ; Frequencies within 1 part in 10000 should count as the same
 time=maps.time & time=time[uniq(time,sort(time))] & ntime=n_elements(time)
 freq=freq[uniq(freq,sort(freq))] & nfreq=n_elements(freq)
 stokes=maps.stokes & stokes=stokes[uniq(stokes,sort(stokes))] & npol=n_elements(stokes)
 mapcube=reform(replicate(dummy_map,nfreq,npol,ntime),nfreq,npol,ntime)
 for i=0, nfiles-1 do begin
  time_idx=where(time eq maps[i].time)
  pol_idx=where(stokes eq maps[i].stokes)
  blah = min(abs(freq - maps[i].freq),freq_idx)
  ;freq_idx=where(abs(freq - maps[i].freq) lt 1e-4)
  mapcube[freq_idx,pol_idx,time_idx]=maps[i]
 endfor
 maps=temporary(mapcube)
 if keyword_set(no_iv2rl) or npol eq 1 then goto,skip
 for j=0,ntime-1 do begin
  for i=0,nfreq-1 do begin
    imap=maps[i,0,j]
    vmap=maps[i,1,j]
    maps[i,0,j].data=(imap.data+vmap.data)/2
    maps[i,0,j].STOKES='LCP'
    maps[i,0,j].ID=strcompress(string(vmap.freq,format="('EOVSA LCP',f7.3,' GHz')"))
    maps[i,1,j].data=(imap.data-vmap.data)/2
    maps[i,1,j].STOKES='RCP'
    maps[i,1,j].ID=strcompress(string(imap.freq,format="('EOVSA RCP',f7.3,' GHz')"))
  endfor
 endfor
 skip:
 sz=size(maps[0].data)
 nx=sz[1]
 ny=sz[2]
 
 if ~keyword_set(no_rms) then begin
   if ~has_rms or n_elements(rms_mask) eq n_elements(maps[0].data) then begin
     default,rms_mask,(total(reform(total(reform(maps.data,nx,ny,nfreq,npol,ntime),/nan,3),nx,ny,npol,ntime),3) lt ntime*flux_threshold)
     idx=where(rms_mask)
     for i=0,nfreq-1 do begin
       for j=0,npol-1 do begin
         for k=0,ntime-1 do begin
           mom=moment(maps[i,j,k].data[idx],sdev=sdev,/nan)
           maps[i,j,k].rms=sdev
         endfor
       endfor
     end
   end
 end
 
 ;use /sfu to convert maps from Tb to flux on the fly
 if keyword_set(sfu) then begin
   if maps[0].datatype eq 'Brightness Temperature' then begin
     maps[*].dataunit='sfu'
     maps[*].datatype='Flux'
     sz=size(maps[0].data)
     dim=size(maps,/dim)
     dx=(maps[0].dx)
     dy=(maps[0].dy)
     freq=reform(maps[*,0,0].freq)
     time=reform(maps[0,0,*].time)
     coeff= 1.4568525d026/((dx*dy)*(7.27d7)^2)/freq^2
     coeff_arr = reform(replicate(1,sz[1]*sz[2])#coeff,sz[1],sz[2],dim[0])
     for i=0, n_elements(maps)-1 do maps[i].id=strreplace(maps[i].id,'Tb','')
     for i=0,dim[1]-1 do begin
       for j=0,dim[2]-1 do begin
         maps[*,i,j].data*=1/coeff_arr
         maps[*,i,j].rms*=1/coeff
       endfor
     endfor
   endif
 endif
 
 return,maps
end