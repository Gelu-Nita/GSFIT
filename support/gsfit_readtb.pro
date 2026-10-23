;+
; NAME:
;   gsfit_readtb
; PURPOSE:
;   Restore a brightness-temperature or flux map cube and arrange it as [frequency, polarization, time].
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   gsfit_readtb, datafile, maps
; INPUTS:
;   DATAFILE: file name of an IDL save file, or a map structure that is already in memory.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   MAPS: named variable that receives the normalized cube. When DATAFILE is itself a map structure, the normalized cube is returned in DATAFILE.
; OUTPUTS:
;   MAPS: flux maps shaped [frequency, polarization, time]. Brightness-temperature input is converted to sfu. A zero RMS cube can be filled from a frequency-dependent noise ratio entered in a form.
; ROUTINES CALLED:
;   gsfit_freqpoltime, valid_map, CW_FORM
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   May open a modal form asking for the noise-ratio intercept and slope when every RMS value is zero.
; RESTRICTIONS:
;   The restored structure must be a map cube whose datatype is Brightness Temperature or Flux. A missing datatype is accepted with a warning.
; MODIFICATION HISTORY:
;   Written 27-May-2019 Gelu M. Nita (gnita@njit.edu)
;   10-Mar-2020 GN: Added polarization functionality.
;   10-Sep-2020 GN: Fixed opening of map cubes with frequency, polarization, and time axes.
;   22-Nov-2020 GN: Updated while adding EOVSA FITS import.
;   25-Jun-2021 GN: Fixed a GX_SIMULATOR dependency.
;   28-Aug-2021 GN: Updated the data-reading routines.
;   10-Jun-2022 GN: Minor bug fixes.
;   2-Nov-2022 GN: Updated run-time error handling.
;   15-Nov-2023 GN: Fixed tag-name consistency.
;   22-Oct-2026 GN: Replace the inline axis logic with gsfit_freqpoltime.
;-

pro gsfit_readtb,datafile,maps
  if valid_map(datafile) then begin
    dim=size(datafile,/dim)
    maps=reform(datafile,dim)
    goto,compute_rms
  endif
  ;restores brightness temperature maps and convert them to flux maps
  osav=obj_new('idl_savefile',datafile)
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
    if tag_exist(eomaps,'datatype') then begin
    if not ((eomaps[0].datatype eq 'Brightness Temperature') or (eomaps[0].datatype eq 'Flux')) then begin
      message,'Error: The restored maps are not brightness temperature or flux maps,operation aborted!',/info
      return
    endif
    endif else message,'WARNING: The restored maps are not registered as brightness temperature maps, proceed with caution!',/info
    maps=temporary(eomaps)
    if ~tag_exist(maps,'datatype') then begin
      maps=REP_TAG_VALUE(maps,'Brightness Temperature','datatype')
      maps=REP_TAG_VALUE(maps,'K','dataunits')
    endif
    maps=gsfit_freqpoltime(maps)
    compute_rms:
    ;temporary
    if max(maps.rms) eq 0 then begin
      desc = [ $
        '0, LABEL, Add user-defined Frequency Dependent Data to Noise Ratio, CENTER', $
        '1, BASE,, COLUMN, FRAME', $
        '0, Float, 100.31, LABEL_TOP=DNR intercept:, WIDTH=6, TAG=a', $
        '0, Float, -3.11, LABEL_TOP=SNR slope:, WIDTH=6, TAG=b', $
        '2, BUTTON, OK, QUIT,TAG=OK']
      a = CW_FORM(desc, /COLUMN,title='Select SNR')
      sz=size(maps)
      for t=0, sz[3]-1 do begin
        for p=0, sz[2]-1 do begin
          for f=0, sz[1]-1 do begin
            map=maps[f,p,t]
            max_data=max(map.data,min=min_data)
            data_range=max_data-min_data
            map.RMS=max_data/(a.a+a.b*map.freq)
;            map.data=map.data+map.rms*randomn(seed,sz[1],sz[2])
            maps[f,p,t]=map
          endfor
        endfor
      endfor
    endif
    ;temporary
    if maps[0].datatype eq 'Brightness Temperature' then begin
      for i=0, n_elements(maps)-1 do maps[i].id=strreplace(maps[i].id,'Tb','')
      if ~tag_exist(maps,'dataunits') then begin
       maps=rem_tag(maps,'dataunit') 
       maps=add_tag(maps,'sfu','dataunits') 
      endif else maps[*].dataunits='sfu'
      if ~tag_exist(maps,'rmsunits') then begin
        maps=rem_tag(maps,'rmsunit')
        maps=add_tag(maps,'sfu','rmsunits')
      endif else maps[*].rmsunits='sfu'
      maps[*].datatype='Flux'
      sz=size(maps[0].data)
      dim=size(maps,/dim)
      dx=(maps.dx)[0]
      dy=(maps.dy)[0]
      freq=reform(maps[*,0,0].freq)
      time=reform(maps[0,0,*].time)
      coeff= 1.4568525d026/((dx*dy)*(7.27d7)^2)/freq^2
      coeff_arr = reform(replicate(1,sz[1]*sz[2])#coeff,sz[1],sz[2],dim[0])
      for i=0,dim[1]-1 do begin
        for j=0,dim[2]-1 do begin
          maps[*,i,j].data*=1/coeff_arr
          maps[*,i,j].rms*=1/coeff
        endfor
      endfor  
    endif  
  endif  
  if valid_map(datafile) then datafile=temporary(maps)
end  