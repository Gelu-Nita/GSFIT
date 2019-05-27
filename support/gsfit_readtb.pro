pro gsfit_readtb,datafile,maps
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
    maps=temporary(eomaps)
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
end  