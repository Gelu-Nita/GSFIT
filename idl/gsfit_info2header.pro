;+
; NAME:
;   gsfit_info2header
; PURPOSE:
;   Build the GSFIT fit header from a fast-code library description.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   header = gsfit_info2header(info [, freq=freq, pol=pol, time=time, refmap=refmap])
; INPUTS:
;   INFO: library-information structure returned by gsfit_libinfo, including parms_out.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   FREQ: frequency vector, in GHz, stored on the header.
;   POL: polarization labels.
;   TIME: time labels.
;   REFMAP: reference map copied onto the header.
; OUTPUTS:
;   HEADER: structure with INFO, UNITS, ERRUNITS, PARNAMES, and ERRPARNAMES, plus any of TIME, POL, FREQ, and REFMAP that were supplied. A chi-square output parameter is renamed CHISQR, and its error tag is Residual.
; ROUTINES CALLED:
;   rep_tag_value, valid_map
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   None.
; RESTRICTIONS:
;   INFO.PARMS_OUT must contain NAME and UNIT.
; MODIFICATION HISTORY:
;   Written 13-Nov-2025 Gelu M. Nita (gnita@njit.edu)
;-

function gsfit_info2header,info,freq=freq,pol=pol,time=time,refmap=refmap
  parnames=strcompress(info.parms_out.name,/rem)
  units=strcompress(info.parms_out.unit,/rem)
  errparnames='err'+parnames
  errunits=units
  chisq=where(strupcase(parnames) eq 'CHISQ' or strupcase(parnames) eq 'CHI-2' or strupcase(parnames) eq 'CHISQR',count)
  if count eq 1 then begin
    parnames[chisq]='CHISQR'
    errparnames[chisq]='Residual'
    errunits[chisq]='sfu'
  endif
  header={INFO:INFO,UNITS:UNITS, ERRUNITS:ERRUNITS, PARNAMES:PARNAMES, ERRPARNAMES:ERRPARNAMES}
  header=n_elements(time) gt 0? rep_tag_value(header,time,'time'):header
  header=n_elements(pol) gt 0? rep_tag_value(header,pol,'pol'):header
  header=n_elements(freq) gt 0? rep_tag_value(header,freq,'freq'):header
  header=valid_map(refmap) ? rep_tag_value(header,refmap,'refmap'):header
  return,header
end