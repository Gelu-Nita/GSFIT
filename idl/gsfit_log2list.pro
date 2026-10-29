;+
; NAME:
;   gsfit_log2list
; PURPOSE:
;   Restore a GSFIT multi_save fit list or a GSFITCP sequential log into one fit-record array.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   fit = gsfit_log2list([file] [, header=header])
; INPUTS:
;   FILE: optional name of a .sav or .log file. A file dialog is opened when FILE is omitted or does not exist.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   HEADER: named variable that receives the header saved with the list.
; OUTPUTS:
;   FIT: structure array sorted by time index, or !NULL when the file cannot be read.
; ROUTINES CALLED:
;   MULTI_RESTORE
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   Opens a file dialog when no readable file is supplied.
; RESTRICTIONS:
;   A list written before 1-Mar-2019 is accepted only when the restored file still contains FIT, TIME, FREQ, and REFMAP. A whole-array GSFIT template is returned as that array; extra multi_save records are appended when the file pointer is not at the end.
; MODIFICATION HISTORY:
;   Written 5-May-2019 Gelu M. Nita (gnita@njit.edu)
;-

forward_function gsfit_libinfo
function gsfit_log2list,file,header=header
    if ~file_exist(file) then begin
      file=dialog_pickfile(Title='Please choose a gsfitcp log file to restore',filter=['*.log','*.sav'])
      if ~file_exist(file) then return,!null
    end
  rec=MULTI_RESTORE(lun,file=file, header=header,/new,/verb)
  
  ;for backward compatibility with fit list files produced before March 1st 2019. Not guaranteed to work with all of them!
  if size(rec,/tname) ne 'STRUCT' then begin
    restore,file
    if n_elements(header) eq 0 and n_elements(time) ne 0 and n_elements(freq) ne 0 and n_elements(refmap) ne 0 and  n_elements(fit) ne 0 then begin  
      parnames=tag_exist(fit[0],'parnames')?fit[0].parnames:['n_nth','B','theta','n_th','Delta','Emax','Temp','CHISQ']
      errparnames=tag_exist(fit[0],'errparnames')?fit[0].errparnames:['errn_nth','errB','errtheta','errn_th','errDelta','errEmax','errTemp','CHISQ']
      units=tag_exist(fit[0],'units')?fit[0].units:['cm^-3','G','deg','cm^-3','','MeV','MK','']
      errunits=tag_exist(fit[0],'errunits')?fit[0].errunits:['cm^-3','G','deg','cm^-3','','MeV','MK','']
      info=n_elements(info) gt 0  ? info : gsfit_libinfo()
      header={freq:freq,time:time, refmap:refmap,$
      info:info,units:units,errunits:errunits,parnames:parnames,errparnames:errparnames}
      fit=rem_tag(fit,['units','errunits','parnames','errparnames'])
      return,fit
    endif else return,!null  
  endif 
  ;end backward compatibility section
  
  stat=fstat(lun)
  nrec=(stat.size-stat.cur_ptr)/n_tags(rec,/data)
  if nrec gt 0 then begin
    message,strcompress(string(nrec,format="('Restoring ', g0,' additional records')")),/info
    fit=replicate(rec,nrec+1)
    for i=1,nrec do begin
      fit[i]=MULTI_RESTORE(lun,file=file)
    end
  endif else fit=temporary(rec)
  free_lun,lun
  fit=fit[sort(fit.t)]
  return,fit
end