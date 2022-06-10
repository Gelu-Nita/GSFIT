;+
; NAME:
;    eovsa_maps2gsfit
; PURPOSE:
;    this function can be used to reformat an EOVSA IDL map structure to o GSFIT compatible format
; CATEGORY:
;    EVSA,GSFIT
; CALLING SEQUENCE:
;      maps=eovsa_fits2map(files,flux_threshold=flux_threshold,rms_mask=rms_mask,sfu=sfu,no_rms=no_rms)
;
; INPUTS:
; a map structure or a filename to restore a map structure from
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
function eovsa_maps2gsfit,maps_in,flux_threshold=flux_threshold,rms_mask=rms_mask,sfu=sfu,no_rms=no_rms
  
  if valid_map(maps_in) then begin
    maps=maps_in
    goto,format_maps
  endif
  
  if ~file_exist(maps_in) then begin
    message, 'A valid map structure or a filename of an IDL file containing such structure is expected! Operation abortd!',/cont
    return,!null
  endif
  ;restores brightness temperature maps and convert them to flux maps
  osav=obj_new('idl_savefile',maps_in)
  names=osav->names()
  valid=0
  for i=0,n_elements(names)-1 do begin
    osav->restore,names[i]
    e=execute('result=size('+names[i]+',/tname)')
    if (result eq 'STRUCT') then begin
      e=execute('eomaps=temporary('+names[i]+')')
    endif
  endfor
  obj_destroy,osav
  if valid_map(eomaps) then begin
    maps=temporary(eomaps)
    goto,format_maps
  endif
 
format_maps: 

if ~valid_map(maps) then begin
  message, 'Error: Restored data is not a valid map structure, operation aborted!',/cont
 return,!null
endif
  
;if tag_exist(maps,'datatype') then begin
;  if not ((maps[0].datatype eq 'Brightness Temperature') or (maps[0].datatype eq 'Flux')) then begin
;    message,'Error: The restored maps are not registered as brightness temperature or flux maps,operation aborted!',/cont
;    return,!null
;  endif
;endif else begin
;  message,'WARNING: The restored maps are not registered as brightness temperature or flux maps. Formatting operation aborted!',/cont
;  return,maps
;endelse

if tag_exist(maps,'datatype') then begin
  if not ((maps[0].datatype eq 'Brightness Temperature') or (maps[0].datatype eq 'Flux')) then begin
    message,'Error: The restored maps are not brightness temperature or flux maps,operation aborted!',/cont
    return,!null
  endif
endif else message,'WARNING: The restored maps are not registered as brightness temperature maps, proceed with caution!',/cont
maps=temporary(maps)
sz=size(maps)
if sz[0] eq 1 then maps=reform(maps,sz[1],1,1)
if sz[0] eq 2 then maps=reform(maps,sz[1],1,sz[2])
dim=size(maps,/dim)
if ~tag_exist(maps,'datatype') then begin
  maps=REP_TAG_VALUE(maps,'Brightness Temperature','datatype')
  maps=REP_TAG_VALUE(maps,'K','dataunits')
  maps=REP_TAG_VALUE(maps,'K','rmsunits')
endif

maps.freq=long(float(maps.freq)*100)/100.
time=maps.time & time=time[uniq(time,sort(time))] & ntime=n_elements(time)
freq=maps.freq & freq=freq[uniq(freq,sort(freq))] & nfreq=n_elements(freq)
stokes=maps.stokes & stokes=stokes[uniq(stokes,sort(stokes))] & npol=n_elements(stokes)
dummy_map=maps[0]
has_rms=tag_exist(dummy_map,'rms')
mapcube=reform(replicate(dummy_map,nfreq,npol,ntime),nfreq,npol,ntime)
for i=0, n_elements(maps)-1 do begin
  time_idx=where(time eq maps[i].time)
  pol_idx=where(stokes eq maps[i].stokes)
  freq_idx=where(freq eq maps[i].freq)
  mapcube[freq_idx,pol_idx,time_idx]=maps[i]
endfor
maps=temporary(mapcube)
skip:
sz=size(maps[0].data)
nx=sz[1]
ny=sz[2]
dim=size(maps,/dim)
dx=(maps[0].dx)
dy=(maps[0].dy)
freq=reform(maps[*,0,0].freq)
;compute sfu2Tb conversion factor
coeff= 1.4568525d026/((dx*dy)*(7.27d7)^2)/freq^2
coeff/=2; Division by two is needed to comply with the 
        ; EOVSA map convention that assumes I=(XX+YY)/2=(RR_LL)/2
coeff_arr = reform(replicate(1,sz[1]*sz[2])#coeff,sz[1],sz[2],dim[0])

;if not already flux maps at this point, convert maps from Tb to flux disegarding the datatype requested, since the flux maps are anywa needed to compute RMS
if maps[0].datatype eq 'Brightness Temperature' then begin
  maps[*].dataunits='sfu'
  maps[*].rmsunits='sfu'
  maps[*].datatype='Flux'
  for i=0, n_elements(maps)-1 do maps[i].id=strreplace(maps[i].id,'Tb','')
  for i=0,dim[1]-1 do begin
    for j=0,dim[2]-1 do begin
      maps[*,i,j].data*=1/coeff_arr
      maps[*,i,j].rms*=1/coeff
    endfor
  endfor
endif

;Here te conversion from I=(RR+LL)/2 EOVSA conventio to I=(RCP+LCP) Gsfit conventio is done
;Also if only XX or YY polariations are present, conversio to I=2XX or I=2YY is performed
if npol eq 1 then begin
  ;Here XX, YY, RR,or LL single polarizations are expected
  ;which are expected to satisfy the EOVSA convention for unoplarized data
  ;I=XX, or I=YY, or I=RR, or I=LL, so relabel data accordingly
  for i=0,n_elements(maps)-1 do begin
    if (maps[i].stokes eq 'XX') or (maps[i].stokes eq 'YY') or (maps[i].stokes eq 'RR') or (maps[i].stokes eq 'LL') then begin
      maps[i].stokes='I'
      maps[i].id=str_replace(str_replace(str_replace(str_replace(maps[i].id,'XX','I'),'YY','I'),'RR','I'),'LL','I')
    end
  end
endif

if npol eq 2 then begin
  ;Here only (XX,YY),(RR,LL), or (I,V) polarization pairs (already alphabeically ordered above) are expected
  ;Depending f the polarizatin pair detected appopriate actio is taken to
  ;convert data to the expected GSFIT convention, i.e.
  ;I=RCP+LCP=(RR+LL)/2
  case stokes[0] of
    'XX': begin
      ;Circular polarization cannot be recovered only from (XX,YY)
      ;so conversion to I is forced
      xx=reform(maps[*,0,*],nfreq,1,ntime)
      yy=reform(maps[*,1,*],nfreq,1,ntime)
      maps=temporary(xx)
      maps.data=maps.data+yy.data
      for i=0,n_elements(maps)-1 do begin
        maps[i].stokes='I'
        maps[i].id=str_replace(maps[i].id,'XX','I')
      end
      npol=1
    end
    'LL': begin
      ;Convert RR-LL folowing EOVSA convention I=(RR+LL)/2 to
      ;circular polarization data following GSFIT convention I=RCP+LCP
      maps.data=maps.data/2
      for i=0,n_elements(maps)-1 do begin
        maps[i].stokes=str_replace(str_replace(maps[i].stokes,'RR','RCP'),'LL','LCP')
        maps[i].id=str_replace(str_replace(maps[i].id,'RR','RCP'),'LL','LCP')
      end
    end
    else:
  endcase
endif

if ~keyword_set(no_rms) then begin
  if ~has_rms or n_elements(rms_mask) eq n_elements(maps[0].data) then begin
    default,rms_mask,(total(total(reform(total(reform(maps.data,nx,ny,nfreq,npol,ntime),/nan,3),nx,ny,npol,ntime),3),3) lt npol*ntime*flux_threshold)
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

; if conversion to sfu has not been explicitely requested by setting the /sfu keyword
; the flux maps are converte back to TB be fore returning the map structure
if ~keyword_set(sfu) then begin
  if maps[0].datatype eq 'Flux' then begin
    maps[*].dataunits='K'
    maps[*].rmsunits='K'
    maps[*].datatype='Brightness Temperature'
    for i=0, n_elements(maps)-1 do maps[i].id=strreplace(maps[i].id,'Tb','')
    for i=0,dim[1]-1 do begin
      for j=0,dim[2]-1 do begin
        maps[*,i,j].data*=coeff_arr
        maps[*,i,j].rms*=coeff
      endfor
    endfor
  endif
endif



return,maps
end