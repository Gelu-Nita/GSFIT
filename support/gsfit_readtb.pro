pro gsfit_readtb,datafile,maps
  if valid_map(datafile) then begin
    maps=temporary(datafile)
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
      message,'Error: The restored maps are not brightness temperature or flux maps,operation aborted!',/cont
      return
    endif
    endif else message,'WARNING: The restored maps are not registered as brightness temperature maps, proceed with caution!',/cont
    maps=temporary(eomaps)
    sz=size(maps)
    if sz[0] eq 1 then maps=reform(maps,sz[1],1,1)
    if sz[0] eq 2 then maps=reform(maps,sz[1],sz[2],1)
    dim=size(maps,/dim)
    if ~tag_exist(maps,'datatype') then begin
      maps=REP_TAG_VALUE(maps,'Brightness Temperature','datatype')
      maps=REP_TAG_VALUE(maps,'K','dataunit')
    endif
    if tag_exist(maps,'dimensions') then begin
      dimensions=maps[0].dimensions
      if n_elements(dimensions) ne n_elements(dim) then begin
        message,'Error: Map array dimensions do not match the dimensions tag, operation aborted!',/cont
        return
      endif
       case n_elements(dim) of
        1: begin 
             case dimensions[0] of
              'Freq': maps=reform(maps,dim[0],1,1)
              'Pol': maps=reform(maps,1,dim[0],1)
               else: maps=reform(maps,1,1,dim[0])
             endcase
           end  
        2: begin
            case 1 of
              array_equal(dimensions,['Freq','Pol']): maps=reform(maps,dim[0],dim[1],1)
              array_equal(dimensions,['Freq','Time']): maps=reform(maps,dim[0],1,dim[1])
              else: maps=reform(1,maps,dim[0],dim[1])
            endcase  
           end
        else: begin 
        end   
       endcase
      dimensions=['Freq','Pol','Time']
      dim=size(maps,/dim)
      maps=REP_TAG_VALUE(maps,dimensions,'dimensions')
      maps=reform(maps,dim[0],dim[1],dim[2])
    endif else begin
      ;Here we make some assumptions based on preestablished conventions
      dimensions=['Freq','Pol','Time']
      case n_elements(dim) of
        1: maps=reform(maps,dim[0],1,1)
        2: maps=reform(maps,dim[0],1,dim[1]);backward compatibility with no polariziation freq-time maps
        else: maps=reform(maps[0:dim[0]*dim[1]*dim[2]-1],dim[0],dim[1],dim[2]); Remove unexpected dimmensions, if any!!!
      endcase
      dim=size(maps,/dim)
      maps=REP_TAG_VALUE(maps,dimensions,'dimensions')
      maps=reform(maps,dim[0],dim[1],dim[2])
    endelse
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
            map.data=map.data+map.rms*randomn(seed,sz[1],sz[2])
            maps[f,p,t]=map
          endfor
        endfor
      endfor
    endif
    ;temporary
    if maps[0].datatype eq 'Brightness Temperature' then begin
      for i=0, n_elements(maps)-1 do maps[i].id=strreplace(maps[i].id,'Tb','')
      maps[*].dataunit='sfu'
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
end  