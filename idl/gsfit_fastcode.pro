;+
; NAME:
;   gsfit_fastcode
; PURPOSE:
;   Load the GX_SIMULATOR microwave renderer description used to build a GSFIT fit.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   info = gsfit_fastcode([renderer])
; INPUTS:
;   RENDERER: renderer source name passed to gx_rendererinfo. The default is mw_transfer_arr.pro.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   None.
; OUTPUTS:
;   INFO: structure of row data, line-of-sight parameters, and the renderer information. Returns 0 when the renderer cannot be loaded.
; ROUTINES CALLED:
;   gx_rendererinfo
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   None.
; RESTRICTIONS:
;   Requires GX_SIMULATOR.
; MODIFICATION HISTORY:
;   Written 16-Sep-2021 Gelu M. Nita (gnita@njit.edu)
;   10-Jun-2022 GN: Minor bug fixes.
;   23-Aug-2022 GN: Several bug fixes.
;   8-Dec-2025 GN: Fixed a fast-code call.
;   15-Dec-2025 GN: Updated GS_FIT_1D and the fast-code selection routines.
;   19-Dec-2025 GN: Updated fast-code and fitting-library selection.
;-

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