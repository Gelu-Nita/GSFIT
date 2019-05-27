function gsfit_log2map,fitxfile,full_size=full_size,header=header,_extra=_extra
  if size(fitxfile,/tname) eq 'STRUCT' then fit=fitxfile else fit=gsfit_log2list(fitxfile,header=header)
  
  fit=fit[sort(fit.t)]
  t=fit.t
  t=t[uniq(t)]
  Ntime=n_elements(t)
  Nfreq=n_elements(header.freq)

  if ~tag_exist(fit[0],'peakflux') then begin ; here I assume that if peakflux tag does not yet exist, peakfreq also does not
    fit=add_tag(fit,0.0,'peakflux')
    fit=add_tag(fit,0.0,'peakfreq')
    fit=add_tag(fit,0.0,'errpeakflux')
    fit=add_tag(fit,0.0,'errpeakfreq')
    npoints=10*n_elements(header.freq)
    freq=interpol(header.freq,npoints)
    errfreq=max(header.freq-shift(header.freq,1))
    for i=0,n_elements(fit)-1 do begin
      if max(fit[i].specfit) gt 0 then begin
        fit[i].peakflux=max(interpol(fit[i].specfit,header.freq,freq),imax)
        fit[i].errpeakflux=fit[i].residual/nfreq
        fit[i].peakfreq=freq[imax]
        fit[i].errpeakfreq=errfreq
      end
    end
    header=rep_tag_value(header,[header.parnames,'peakflux','peakfreq'],'parnames')
    header=rep_tag_value(header,[header.errparnames,'errpeakflux','errpeakfreq'],'errparnames')
    header=rep_tag_value(header,[header.units,'sfu','GHz'],'units')
    header=rep_tag_value(header,[header.errunits,'sfu','GHz'],'errunits')
  end
  

  if ~keyword_set(full_size) then begin
    xrange=minmax(fit.x)
    if xrange[1] eq xrange[0] then xrange=xrange+[-1,1]
    yrange=minmax(fit.y)
    if yrange[1] eq yrange[0] then yrange=yrange+[-1,1]
    fit.x=fit.x-xrange[0]
    fit.y=fit.y-yrange[0]
    sub_map,header.refmap,template_map,xrange=xrange,yrange=yrange,/pixel
  endif else template_map=header.refmap
  
  template_map=rep_tag_name(template_map,'dataunit','dataunits')
  
  template_map.data[*,*]=0
  errmap = (datamap =(fitmap = replicate(template_map,Nfreq,Ntime)))
  maps = {fitmaps: reform(temporary(fitmap),nfreq,ntime), datamaps: reform(temporary(datamap),nfreq,ntime), errmaps: reform(temporary(errmap),nfreq,ntime)}

  for k=0,n_elements(header.parnames)-1 do begin
    template_map.id=header.parnames[k]
    template_map.dataunits=header.units[k]
    parmap=replicate(template_map,Ntime)
    parmap.time=header.time[t]
    template_map.id=header.errparnames[k]
    template_map.dataunits=header.errunits[k]
    errparmap=replicate(template_map,Ntime)
    errparmap.time=header.time[t]
    maps=create_struct(maps,header.parnames[k],parmap,header.errparnames[k],errparmap)
  endfor
  

  for j=0,Ntime-1 do begin
    for i=0,nfreq-1 do begin
      maps.fitmaps[i,j].time=header.time[t[j]]
      maps.fitmaps[i,j].freq=header.freq[i]
      maps.fitmaps[i,j].id=string(header.freq[i],format="('Fit Flux @',f5.2,' GHz')")

      maps.datamaps[i,j].time=header.time[t[j]]
      maps.datamaps[i,j].freq=header.freq[i]
      maps.datamaps[i,j].id=string(header.freq[i],format="('Measured Flux @',f5.2,' GHz')")

      maps.errmaps[i,j].time=header.time[t[j]]
      maps.errmaps[i,j].freq=header.freq[i]
      maps.errmaps[i,j].id=string(header.freq[i],"('Applied Fit Error @',f5.2,' GHz')")
    endfor
    idx=where(fit.t eq t[j],count)
    if count gt 0 then begin
      afit=fit[idx]
      maps.datamaps[*,j].data[afit.x,afit.y]=reform(transpose(reform(afit.data)))
      maps.errmaps[*,j].data[afit.x,afit.y]=reform(transpose(reform(afit.errdata)))
      maps.fitmaps[*,j].data[afit.x,afit.y]=reform(transpose(reform(afit.specfit)))
      maptags=tag_names(maps)
      fittags=tag_names(afit)
      for k=0,n_elements(header.parnames)-1 do begin
        parmidx=where(maptags eq strupcase(header.parnames[k]))
        erridx=where(maptags eq strupcase(header.errparnames[k]))
        fitidx=where(fittags eq strupcase(header.parnames[k]))
        errfitidx=where(fittags eq strupcase(header.errparnames[k]))
        map=(maps.(parmidx))
        map[j].data[afit.x,afit.y]=reform(afit.(fitidx))
        maps.(parmidx)=map
        errmap=(maps.(erridx))
        errmap[j].data[afit.x,afit.y]=reform(afit.(errfitidx))
        (maps.(erridx))=errmap
      endfor
    endif
  endfor
  
  gsfit_energy_computation,maps,header,_extra=_extra
  
  maps=add_tag(maps,header,'header')
  
  return,maps
end