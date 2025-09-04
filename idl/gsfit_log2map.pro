function gsfit_log2map,fitxfile,full_size=full_size,header=header,_extra=_extra
  if size(fitxfile,/tname) eq 'STRUCT' then fit=fitxfile else fit=gsfit_log2list(fitxfile,header=header)
  
  fit=fit[sort(fit.t)]
  t=fit.t
  t=t[uniq(t)]
  Ntime=n_elements(t)
  Nfreq=n_elements(header.freq)
  if ~tag_exist(header,'pol') then header=add_tag(header,header.refmap.stokes,'pol')
  Npol=n_elements(header.pol)

  if ~tag_exist(fit[0],'peakflux') then begin ; here I assume that if peakflux tag does not yet exist, peakfreq also does not
    fit=add_tag(fit,0.0,'peakflux')
    fit=add_tag(fit,0.0,'peakfreq')
    fit=add_tag(fit,0.0,'errpeakflux')
    fit=add_tag(fit,0.0,'errpeakfreq')
    npoints=10*n_elements(header.freq)
    freq=interpol(header.freq,npoints)
    errfreq=max(header.freq-shift(header.freq,1))
    for i=0,n_elements(fit)-1 do begin
      specfit=total(reform(fit[i].specfit,Nfreq,Npol),2)
      if max(specfit) gt 0 then begin
        fit[i].peakflux=max(interpol(specfit,header.freq,freq),imax)
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
  errmap = (datamap =(fitmap = replicate(template_map,Nfreq,Npol,Ntime)))
  maps = {fitmaps: reform(temporary(fitmap),nfreq,Npol,ntime), datamaps: reform(temporary(datamap),nfreq,Npol,ntime), errmaps: reform(temporary(errmap),nfreq,Npol,ntime)}
  
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
       for k=0,npol-1 do begin
        maps.fitmaps[i,k,j].time=header.time[t[j]]
        maps.fitmaps[i,k,j].freq=header.freq[i]
        maps.fitmaps[i,k,j].stokes=header.pol[k]
        maps.fitmaps[i,k,j].id=string(header.freq[i],format="('Fit Flux @',f5.2,' GHz')")
  
        maps.datamaps[i,k,j].time=header.time[t[j]]
        maps.datamaps[i,k,j].freq=header.freq[i]
        maps.datamaps[i,k,j].stokes=header.pol[k]
        maps.datamaps[i,k,j].id=string(header.freq[i],format="('Measured Flux @',f5.2,' GHz')")
  
        maps.errmaps[i,k,j].time=header.time[t[j]]
        maps.errmaps[i,k,j].freq=header.freq[i]
        maps.errmaps[i,k,j].stokes=header.pol[k]
        maps.errmaps[i,k,j].id=string(header.freq[i],"('Applied Fit Error @',f5.2,' GHz')")
      end
    endfor
    idx=where(fit.t eq t[j],count)
    if count gt 0 then begin
      afit=fit[idx]
      npix=(size(afit))[1]
      for i=0, npol-1 do begin
;        maps.datamaps[*,i,j].data[afit.x,afit.y]=reform(transpose((reform(afit.data,nfreq,npix,npol))[*,*,i]))
;        maps.errmaps[*,i,j].data[afit.x,afit.y]=reform(transpose((reform(afit.errdata,nfreq,npix,npol))[*,*,i]))
;        maps.fitmaps[*,i,j].data[afit.x,afit.y]=reform(transpose((reform(afit.specfit,nfreq,npix,npol))[*,*,i]))
          maps.datamaps[*,i,j].data[afit.x,afit.y]=reform(transpose((reform(afit.data,nfreq,npol,npix))[*,i,*]))
          maps.errmaps[*,i,j].data[afit.x,afit.y]=reform(transpose((reform(afit.errdata,nfreq,npol,npix))[*,i,*]))
          maps.fitmaps[*,i,j].data[afit.x,afit.y]=reform(transpose((reform(afit.specfit,nfreq,npol,npix))[*,i,*]))
      end
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
  maps.fitmaps=reform(maps.fitmaps,nfreq,Npol,ntime)
  maps.datamaps=reform(maps.datamaps,nfreq,Npol,ntime)
  maps.errmaps=reform(maps.errmaps,nfreq,Npol,ntime)
  return,maps
end