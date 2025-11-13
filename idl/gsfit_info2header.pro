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