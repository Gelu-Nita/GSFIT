function gsfit_fastcode,array=array
  cdir=curdir()
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    goto,invalid_renderer
  end
  if !version.os_family eq 'Windows' then begin
    renderer=gx_findfile(folder='userslib'+path_sep()+'radio_flaring'+path_sep()+'windows')+path_sep()+$
      (keyword_set(array)?'mw_transfer_arr.pro':'gs_transfer_dp.pro')
  endif else  renderer=gx_findfile(folder='userslib'+path_sep()+'radio_flaring'+path_sep()+'unix')+$
      (keyword_set(array)?'mwtransferarr\mwtransferarr.pro':'mwtransfer\mwtransfer.pro')

  dirpath=file_dirname(renderer,/mark)
  cd,dirpath
  break_file, renderer, dsk_log, dir, filename, ext
  compile_test=execute('RESOLVE_ROUTINE, filename , /COMPILE_FULL_FILE ,/either')
  cd,cdir
  par=ROUTINE_INFO(filename,/par)
  if par.num_args lt 2 or par.num_kw_args lt 1 then goto,invalid_renderer
  template=filename+',parms,rowdata'
  for i=2,par.num_args-1 do template+=','+strlowcase(par.args[i])
  for i=0,par.num_kw_args-1 do begin
    if strupcase(par.kw_args[i]) ne 'INFO' then template+=','+strlowcase(par.kw_args[i])+'='+strlowcase(par.kw_args[i])
  end
  if execute(filename+',INFO=INFO') then begin
    if size(info,/tname) ne 'STRUCT' then goto,invalid_renderer
    return,CREATE_STRUCT('execute',template,info)
  end
  invalid_renderer:
  cd,cdir
  return,0
end