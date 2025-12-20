function gsfit_fastcode,renderer
  cdir=curdir()
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    goto,invalid_renderer
  end
  default, renderer,'mw_transfer_arr.pro'
  info=gx_rendererinfo(renderer)
  if size(info,/tname) ne 'STRUCT' then goto,invalid_renderer
  rowdata=make_array([1,info.pixdim],/float)
  Npix=1
  Nvox=2
  losparms=double(transpose(array_replicate(info.parms.value,Npix,Nvox),[1,2,0]))
  return,CREATE_STRUCT('rowdata',rowdata,'losparms',losparms,info)
  invalid_renderer:
  cd,cdir
  return,0
end