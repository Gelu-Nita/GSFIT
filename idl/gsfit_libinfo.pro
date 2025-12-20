function gsfit_libinfo,libpath,getlib=getlib
  entry_point:
  if keyword_set(getlib) then libpath=gsfit_select_lib()
  if size(libpath,/tname) eq 'STRING' then begin
    if file_test(libpath) then begin
      get_function=!version.os_family eq 'Windows'?['GET_TABLES','GET_MW_FIT']:['get_tables_','get_mw_fit_']
      CATCH, Error_status
      IF Error_status NE 0 THEN BEGIN
        ;try to change case
        get_function=!version.os_family eq 'Windows'?strlowcase(get_function):strupcase(get_function)
        message,'Fit library symbole case mismatch, trying to change it...',/info
        CATCH, /CANCEL
      ENDIF
      res=call_external(libpath, get_function[0], /d_value,/unload)
    endif else  begin
      mesage,'Invalid library path provided, not able to go on....',/info
      return,!NULL
    endelse
  endif else begin
    if file_test('libinfo.inp') then begin
      restore,'libinfo.inp'
      if ~file_test(info.path) then begin
        getlib=1
        goto,entry_point
      endif
      goto,exit_point
    endif else begin
      getlib=1
      goto,entry_point
    endelse
  endelse
  openr,lun,'Long_input.txt',/get,error=err
  if err ne 0 then return,!null
  line=''
  count=0
  WHILE ~ EOF(lun) DO BEGIN
    READF, lun, line
    info=strsplit(line,';',/extract)
    unit=strcompress(info[2],/rem)
    unit=unit eq ''?'':' ('+unit+')'
    if count eq 0 then begin
      nparms={name:strcompress(info[0]),value:fix(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}
    endif else begin
      nparms=[nparms,{name:strcompress(info[0]),value:fix(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER' ?1:0,hint:info[4]}]
    end
    count+=1
  ENDWHILE
  free_lun,lun
  file_delete,'Long_input.txt',/q

  openr,lun,'Real_input.txt',/get,error=error
  if error eq 1 then return,!null
  line=''
  count=0
  WHILE ~ EOF(lun) DO BEGIN
    READF, lun, line
    info=strsplit(line,';',/extract)
    unit=strcompress(info[2],/rem)
    unit=unit eq ''?'':'('+unit+')'
    if count eq 0 then begin
      rparms={name:strcompress(info[0]),value:double(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}
    endif else begin
      rparms=[rparms,{name:strcompress(info[0]),value:double(info[1]),unit:unit,user:strupcase(strcompress(info[3],/rem)) eq 'USER'?1:0,hint:info[4]}]
    end
    count+=1
  ENDWHILE
  free_lun,lun
  file_delete,'Real_input.txt',/q

  openr,lun,'Parms_input.txt',/get,error=error
  if error eq 1 then return,!null
  line=''
  count=0
  WHILE ~ EOF(lun) DO BEGIN
    READF, lun, line
    info=strsplit(line,';',/extract)
    unit=strcompress(info[4],/rem)
    unit=unit eq ''?'':'('+unit+')'
    if count eq 0 then begin
      parms_in={name:strcompress(info[0]),guess:double(info[1]),min:double(info[2]),max:double(info[3]),unit:unit,hint:info[5],user:strmid(strupcase(strcompress(info[0],/rem)),0,3) eq 'RES'?0:1}
    endif else begin
      parms_in=[parms_in,{name:strcompress(info[0]),guess:double(info[1]),min:double(info[2]),max:double(info[3]),unit:unit,hint:info[5],user:strmid(strupcase(strcompress(info[0],/rem)),0,3) eq 'RES'?0:1}]
    end
    count+=1
  ENDWHILE
  free_lun,lun
  file_delete,'Parms_input.txt',/q

  openr,lun,'Parms_out.txt',/get,error=error
  if error eq 1 then return,!null
  line=''
  count=0
  WHILE ~ EOF(lun) DO BEGIN
    READF, lun, line
    info=strsplit(line,';',/extract)
    if count eq 0 then begin
      parms_out={name:strcompress(info[0]),unit:strcompress(info[2],/rem),hint:info[3]}
    endif else begin
      parms_out=[parms_out,{name:strcompress(info[0]),unit:strcompress(info[2],/rem),hint:info[3]}]
    end
    count+=1
  ENDWHILE
  free_lun,lun
  file_delete,'Parms_out.txt',/q
  info={get_function:get_function,nparms:nparms,rparms:rparms,parms_in:parms_in,parms_out:parms_out,path:libpath,rms:4d}
  exit_point:
  save,info,file='libinfo.inp'
  return,info
end