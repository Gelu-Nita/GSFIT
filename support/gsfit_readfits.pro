function gsfit_readfits,files,flux_threshold=flux_threshold,rms_mask=rms_mask,tb=tb
 ;set flux_thresholds to a value representing an user define minimum integrated flux over all bands for a given pixel intensity not to be considered noise
 default,flux_threshold,1
 ;provide an rms_mask as a nx*ny bit mask to indicate the pixels that should be included in RMS computation.
 ;if rms_mask is provided, flux_threshold is ignored
 if n_elements(files) eq 0 then files=dialog_pickfile(filter='*.fit*',title='Select a sequence of EOVSA fits files to import into GSFIT',/read,/multiple)
 if files[0] eq '' then return,!null
 files=files[sort(files)]
 break_file,files,disk_log, dir, filenames, ext,/last
 bands=strmid(filenames,strpos(filenames,'_')+1)
 bands=bands[uniq(bands,sort(bands))]
 nb=n_elements(bands)
 nt=n_elements(files)/nb
 if nb*nt ne n_elements(files) then begin
  message,'Incomplete data set, operation aborted!',/cont
  return,!null
 endif
 files=reform(files,nt,nb)
 vla_fits2map, files[0], map
 has_rms=tag_exist(map,'rms')
 if ~has_rms then add_prop,map,rms=0d
 maps=replicate(map,nb,nt)
 for i=0,nt-1 do begin
  for j=0,nb-1 do begin
    vla_fits2map, files[i,j], map
    add_prop,map,rms=0d
    maps[j,i]=map
  endfor
 endfor
 data=maps.data
 if ~has_rms or n_elements(rms_mask) eq n_elements(maps[0].data) then begin
   default,rms_mask,(total(total(data,/nan,3),3) lt nt*flux_threshold)
   idx=where(rms_mask)
   for i=0,nt-1 do begin
    for j=0,nb-1 do begin
     mom=moment(maps[j,i].data[idx],sdev=sdev,/nan)
     maps[j,i].rms=sdev
    endfor
   endfor
 end
 ;use /tb to convert maps from Tb to flux on the fly
 if keyword_set(tb) then begin
    sz=size(maps.data)
    dx=(maps.dx)[0]
    dy=(maps.dy)[0]
    freq=reform(maps[*,0].freq)
    time=reform(maps[0,*].time)
    coeff= 1.4568525d026/((dx*dy)*(7.27d7)^2)/freq^2
    coeff_arr = reform(replicate(1,sz[1]*sz[2])#coeff,sz[1],sz[2],sz[3])
    for i=0,sz[4]-1 do begin
      maps[*,i].data*=1/coeff_arr
      maps[*,i].rms*=1/coeff
    end   
  endif
 return,maps
end