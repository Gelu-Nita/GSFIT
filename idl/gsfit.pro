forward_function gsfit_log2map, gsfit_log2list

function gsfit_libinfo,libpath,getlib=getlib
  entry_point:
  if keyword_set(getlib) then begin
    case !version.os_family of
      'Windows':begin 
                 libpath=str_replace(file_dirname((ROUTINE_INFO('gsfit',/source)).path,/mark),'idl','win')+'gs_fit_1D.dll'
                 if ~file_test(libpath) then begin
                   libpath=dialog_pickfile(title='Please select a gsfit DLL or shared library')
                   if ~file_test(libpath) then message,'No valid DLL or shared library selected, not able to go on....'
                 endif
                end 
                
       else: begin 
              unix_dir=str_replace(file_dirname((ROUTINE_INFO('gsfit',/source)).path,/mark),'idl','unix')
              libpath=unix_dir+'/fit_Spectrum_Kl.so'
              if ~file_test(libpath) then begin
               makefile=unix_dir+'makefile'
               if makefile ne '' then begin
                 cdr=curdir()
                 cd,unix_dir
                 spawn, 'rm *.o',exit_status=exit_status
                 spawn, 'make',exit_status=exit_status
                 cd,cdr
                 libpath=unix_dir+'/fit_Spectrum_Kl.so'
                 if ~file_test(libpath) then message,'The attemp to make a sharable library failed, not able to go on....'
               endif
              endif
            end  
    endcase
  endif
  if size(libpath,/tname) eq 'STRING' then begin
    if file_test(libpath) then begin
      get_function=!version.os_family eq 'Windows'?['GET_TABLES','GET_MW_FIT']:['get_tables_','get_mw_fit_']
      CATCH, Error_status
      IF Error_status NE 0 THEN BEGIN
        ;try to change case
        get_function=!version.os_family eq 'Windows'?strlowcase(get_function):strupcase(get_function)
        message,'Fit library symbole case mismatch, trying to change it...',/cont
        CATCH, /CANCEL
      ENDIF

      res=call_external(libpath, get_function[0], /d_value,/unload)
    endif else  message,'Invalid library path provided, not able to go on....'
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
  save,info,file='libinfo.inp'
  exit_point:
  if ~tag_exist(info,'fastcode') $
    then info=add_tag(info,gsfit_fastcode(),'fastcode') $
    else info=rep_tag_value(info,gsfit_fastcode(),'fastcode')
  if size(info.fastcode,/tname) ne 'STRUCT' then begin
    info=rem_tag(info,'fastcode')
    info=add_tag(info,gsfit_fastcode(),'fastcode')
  endif
  return,info
end

function gsfit_info2header,info,freq=freq,time=time,refmap=refmap
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
  header=n_elements(time) gt 0? add_tag(header,time,'time'):header
  header=n_elements(freq) gt 0? add_tag(header,freq,'freq'):header
  header=valid_map(refmap) ? add_tag(header,refmap,'refmap'):header
  return,header
end

pro gsfit_fit2guess,state
  widget_control,state.wfitpix ,get_value=fit_idx
  if ~state.fit->IsEmpty() and fit_idx gt 0 then begin
    fit=state.fit(fit_idx-1)
    pnames=strupcase(strcompress((state.header.info.parms_in.name),/rem))
    pinput=state.header.info.parms_in.guess
    fnames=tag_names(fit)
    pinput[where(pnames eq 'N_NTH')]=fit.(where(fnames eq 'N_NTH'))*1d-7
    pinput[where(pnames eq 'B')]=fit.(where(fnames eq 'B'))*1d-2
    pinput[where(pnames eq 'THETA')]=fit.(where(fnames eq 'THETA'))
    pinput[where(pnames eq 'DELTA')]=fit.(where(fnames eq 'DELTA'))
    pinput[where(pnames eq 'N_TH')]=fit.(where(fnames eq 'N_TH'))*1d-9
    pinput[where(pnames eq 'E_MAX')]=fit.(where(fnames eq 'E_MAX'))
    pinput[where(pnames eq 'T_E')]=fit.(where(fnames eq 'T_E'))*1e-6
    state.header.info.parms_in.guess=pinput
    widget_control,state.lib.wGuessParms,set_value=pinput
  end
end


pro gsfit_fastcode_update,state,fastcode_update

if keyword_set(fastcode_update) and tag_exist(state.header.info,'fastcode') and ptr_valid(state.pmaps) then begin
  Npix=1l
  Nvox=1l
  Nfreq=state.header.info.fastcode.pixdim[0]
  freq=minmax(state.header.freq)*1d9
  logfreqrange=alog10(freq)
  df=(logfreqrange[1]-logfreqrange[0])/(nfreq-1)
  logfreq=logfreqrange[0]+findgen(nfreq)*df
  state.header.info.fastcode.spectrum.x.axis=10^logfreqrange
  parms=state.header.info.fastcode.parms.value
  names=strupcase(strcompress(state.header.info.fastcode.parms.name,/rem))
  nnames=strupcase(strcompress((state.header.info.nparms.name),/rem))
  ninput=state.header.info.nparms.value
  rnames=strupcase(strcompress((state.header.info.rparms.name),/rem))
  rinput=state.header.info.rparms.value
  pnames=strupcase(strcompress((state.header.info.parms_in.name),/rem))
  pinput=state.header.info.parms_in.guess
  arcsec2cm=7.27d7
  for i=0,n_elements(names)-1 do begin
    case names[i] of
      'DS':parms[i]=rinput[where(rnames eq 'PIXELAREA')]*(arcsec2cm^2)
      'DR':parms[i]=rinput[where(rnames eq 'LOSDEPTH')]*arcsec2cm
      'T_0':parms[i]=pinput[where(pnames eq 'T_E')]*1e6
      'EMIN':parms[i]=rinput[where(rnames eq 'E_MIN')]
      'EMAX':parms[i]=pinput[where(pnames eq 'E_MAX')]
      'DELTA1':parms[i]=pinput[where(pnames eq 'DELTA')]
      'N_0':parms[i]=pinput[where(pnames eq 'N_TH')]*1d9
      'N_B':parms[i]=pinput[where(pnames eq 'N_NTH')]*1d7
      'B':parms[i]=pinput[where(pnames eq 'B')]*100
      'THETA':parms[i]=pinput[where(pnames eq 'THETA')]
      'F_MIN':parms[i]=freq
      'DF':parms[i]=df
      'DIST_ANG':parms[i]=ninput[where(nnames eq 'ANGULARCODE')]
      else:
    endcase
  endfor
  state.header.info.fastcode.parms.value=parms
end  

end

pro gsfit_draw,state,draw
  if ~ptr_valid(state.pmaps) or ~keyword_set(draw) then return

  cmdot = STRING([183B]) ; Middle dot
  cbullet = STRING([149B]) ; Bullet
  ctab = STRING([9B]) ; tab
  ccr = STRING(13B) ; Carriage Return
  clf = STRING([10B]) ; line feed
  pm= STRING([177B])


  geometry=widget_info(state.wDataMap,/geometry)


  widget_control,state.wdatamap,get_value=wdatamap
  widget_control,state.wspectrum.plot,get_value=wspectrum
  widget_control,state.wtime,get_value=time_idx
  widget_control,state.wfreq,get_value=freq_idx
  pol_idx=widget_info(state.wpol,/droplist_select)
  widget_control,state.wx,get_value=i,get_uvalue=xpix
  widget_control,state.wy,get_value=j,get_uvalue=ypix

  freq= state.header.freq
  rgb_curr=bytarr(3,256)
  tvlct,rgb_curr,/get
  psave=!p
  widget_control,state.wspectrum.charsize, get_value=charsize
  !p.multi=0
  if (draw ne 0) then begin
    ;wset,state.wPixMap
    wset,wdatamap
    sz=size((*state.pmaps))
    npol=sz[2]
    if npol eq 2 then begin
      if pol_idx eq 0 then begin
        displaymap=(*state.pmaps)[freq_idx, 0,time_idx]
        splitid=strsplit(displaymap.id,/extract)
        id=splitid[0]+' ('+((*state.pmaps)[0, 0,time_idx]).stokes+'+'+((*state.pmaps)[0, 1,time_idx]).stokes+')'
        if n_elements(splitid) gt 2 then id=strjoin([id,' ',splitid[2:*]])
        displaymap.id=id
        displaymap.data=((*state.pmaps)[freq_idx, 0,time_idx].data+(*state.pmaps)[freq_idx, 1,time_idx].data)
        spectrum=((*state.pmaps)[*, 0,time_idx]).data>0+((*state.pmaps)[*, 1,time_idx]).data>0
        err=((*state.pmaps).rms)[*, 0,time_idx]>0+((*state.pmaps).rms)[*, 1,time_idx]>0
      endif else begin
        displaymap=(*state.pmaps)[freq_idx, pol_idx-1,time_idx]
        spectrum=((*state.pmaps)[*, pol_idx -1,time_idx]).data>0
        err=((*state.pmaps).rms)[*, pol_idx-1,time_idx]>0
      endelse
    endif else begin
      displaymap=(*state.pmaps)[freq_idx, pol_idx,time_idx]
      spectrum=((*state.pmaps)[*, pol_idx ,time_idx]).data>0
      err=((*state.pmaps).rms)[*, pol_idx ,time_idx]>0
    endelse
      
      
      
    widget_control,state.wspectrum.threshold,get_value=apply_threshold
    if apply_threshold[0] then begin
      if npol gt 1 then begin
        if pol_idx eq 0 then begin
          mask = total(total((*state.pmaps)[*, * ,time_idx].data,/nan,3),/nan,3) gt (state.header.info.rparms.value)[2]
        endif else mask = total((*state.pmaps)[*, pol_idx-1 ,time_idx].data,/nan,3) gt (state.header.info.rparms.value)[2]
      endif else mask = total((*state.pmaps)[*, pol_idx ,time_idx].data,/nan,3) gt (state.header.info.rparms.value)[2]
      displaymap.data*=mask
    end
    ;code added to bypass the situation of having a displaymap with all NaNs
     nan_idx=where(finite(displaymap.data) eq 0,count_nan)
     if count_nan gt 0 then displaymap.data[nan_idx]=0
    ;end of NaN fix
    plot_map,displaymap,title=displaymap.id,grid=10,/limb,/cbar,charsize=charsize,xrange=state.ppd_data_xrange,yrange=state.ppd_data_yrange
    if ~state.xroi.IsEmpty() then begin 
      xroi=state.xroi.ToArray()
      yroi=state.yroi.ToArray()
      plots,xroi,yroi,color=255
    endif
    g={x:!x,y:!y,z:!z,p:!p}
    widget_control,state.wdatamap,set_uvalue=g   
    oplot,xpix[[i,i]],!y.crange,linesty=2,color=250
    oplot,!x.crange,ypix[[j,j]],linesty=2,color=250
    
    widget_control,state.wxlabel,set_value=string(xpix[i],i,format="('CURSOR X:',f7.2,' arcsec [',i3,']')")
    widget_control,state.wylabel,set_value=string(ypix[j],j,format="('CURSOR Y:',f7.2,' arcsec [',i3,']')")

    wset,wspectrum
    erase,0

    widget_control,state.wx,get_value=i
    widget_control,state.wy,get_value=j
    spectrum=reform(spectrum[i,j,*])
    rmsweight=state.header.info.rms
    err_spectrum=(err+rmsweight*max(err)/freq)
    
    total_flux=total(spectrum,/nan)
    widget_control,state.wspectrum.totalflux,set_value=total_flux
    
    widget_control,state.wspectrum.fixed,get_value=fixed
    if fixed[0] then widget_control,state.wspectrum.range,get_value=yrange
    widget_control,state.wFastCode,get_value=fastcode_over
          
    if total_flux gt 0 then begin 
      plot,freq,spectrum,/xlog,/ylog,/xsty,/ysty,psym=2,xrange=xrange,yrange=yrange,$
        xtitle='Frequency (GHz)',ytitle='Flux [sfu]',title=string(i,j,format="('Flux [',i3,',',i3,']')"),ymargin=[6,6],charsize=charsize
      oploterr, freq, spectrum, err_spectrum, psym=3, hatlength=2, errcolor=255
      oplot,freq[[freq_idx,freq_idx]],10^!y.crange,linesty=2,color=250
      
      if fastcode_over[0] eq 1 then begin
        Npix=1l
        Nvox=1L
        rowdata=make_array([npix,state.header.info.fastcode.pixdim],/float)
        parms=double(reform(state.header.info.fastcode.parms.value, npix,nvox, n_elements(state.header.info.fastcode.parms.value)))
        res=execute(state.header.info.fastcode.execute)
        guess_freq=reform(datain[0,*])
        guess_flux=pol_idx eq 0?reform(rowdata[*,*,0,0]+rowdata[*,*,1,0]):reform(rowdata[*,*,pol_idx-1,0])
        oplot,guess_freq,guess_flux,color=100
        fmin=widget_info(state.wMinFreq,/droplist_select)
        fmax=widget_info(state.wMaxFreq,/droplist_select)
        guess_flux=spline(guess_freq,guess_flux,freq)
        CHISQR=total(((spectrum-guess_flux)/err_spectrum)^2,/double)/(2*n_elements(freq)-(state.header.info.nparms.value)[0])
        gx_plot_label,0.01,0.9,string(CHISQR,format="('Guess  CHISQR=',g0)"),/ylog,/xlog,color=100,charsize=1.2*charsize,charthick=2
      endif
    endif
     
    if n_elements(yrange) eq 0 then begin
      widget_control,state.wspectrum.range,set_value=10^!y.crange
    endif


    l=state.fit
    if ~l.IsEmpty() then begin
      filter=(l.Filter(Lambda(l,x,y,t:l.x eq x and l.y eq y and l.t eq t),i,j,time_idx))
      match=l.where(filter.toarray(),count=count)
      widget_control,state.wfitpix,set_value=(count gt 0)?match[0]+1:0
      color=[150,50,200]
      if count gt 0 then begin
        idx=match[0]
        fit=state.fit(idx)
        if npol eq 2 then begin
          if pol_idx ne 0 then begin
            specfit=fit.specfit[fit.fmin:fit.fmax,pol_idx-1]
          endif else specfit=total(fit.specfit[fit.fmin:fit.fmax,*],2)
        endif else specfit=fit.specfit[fit.fmin:fit.fmax]
        oplot,freq[fit.fmin:fit.fmax],specfit,color=color[pol_idx],thick=2
         
        if fastcode_over[0] eq 2 then begin 
            parms=double(state.header.info.fastcode.parms.value)
            names=strupcase(strcompress(state.header.info.fastcode.parms.name,/rem))
            pnames=tag_names(fit)
            for i=0,n_elements(names)-1 do begin
              case names[i] of
                'T_0':parms[i]=fit.(where(pnames eq 'T_E'))
                'EMAX':parms[i]=fit.(where(pnames eq 'E_MAX'))
                'DELTA1':parms[i]=fit.(where(pnames eq 'DELTA'))
                'N_0':parms[i]=fit.(where(pnames eq 'N_TH'))
                'N_B':parms[i]=fit.(where(pnames eq 'N_NTH'))
                'B':parms[i]=fit.(where(pnames eq 'B'))
                'THETA':parms[i]=fit.(where(pnames eq 'THETA'))
                else:
              endcase
            endfor
            npix=1l
            nvox=1l
            parms=reform(parms, npix,nvox, n_elements(parms))
            rowdata=make_array([npix,state.header.info.fastcode.pixdim],/float)
            res=execute(state.header.info.fastcode.execute)
            fast_freq=reform(datain[0,*])
            fast_flux=pol_idx eq 0?(reform(rowdata[*,*,0,0]+rowdata[*,*,1,0])):reform(rowdata[*,*,pol_idx-1,0])
            oplot,fast_freq,fast_flux,color=250,thick=2
            fit_flux=spline(fast_freq,fast_flux,freq)

            nfreq=n_elements(freq)
            specfit_flux=(pol_idx eq 0) ? total(reform(fit.specfit,nfreq,npol),2):fit.specfit[*,pol_idx-1]
            CHISQR=total(((spectrum-specfit_flux)/err_spectrum)^2,/double)/(2*n_elements(freq)-(state.header.info.nparms.value)[0])
            fcCHISQR=total(((spectrum-fit_flux)/err_spectrum)^2,/double)/(2*n_elements(freq)-(state.header.info.nparms.value)[0])
            gx_plot_label,0.01,0.9,string(fcCHISQR,format="('Fast Code  CHISQR=',g0)"),/ylog,/xlog,color=250,charsize=1.2*charsize,charthick=2
            gx_plot_label,0.01,0.8,string(CHISQR,format="('Fit  CHISQR=',g0)"),/ylog,/xlog,color=150,charsize=1.2*charsize,charthick=2
        end
         
         
         
         y=0.4
         tags=tag_names(fit)
         for k=0,n_elements(state.header.parnames)-1 do begin
          idx=where(tags eq strupcase(state.header.parnames[k]))
          erridx=where(tags eq strupcase(state.header.errparnames[k]))
          case state.header.parnames[k] of
            'CHISQR':begin
                      parmstr=str_replace(str_replace(str_replace(strcompress(string(state.header.parnames[k],fit.(idx),state.header.errparnames[k],fit.(erridx),state.header.errunits[k],format="(a10,'=',g16.3,' (Avg. ',a10,'=',g16.3,a10,')')")),'+0','+'),'+0','+'),'e+','x10^')
                     end
          else:parmstr=str_replace(str_replace(str_replace(str_replace(str_replace(strcompress(string(state.header.parnames[k],fit.(idx),fit.(erridx),state.header.units[k],format="(a10,'=(',g16.3,'!N!9+!3',g16.3,'!N)',a10)")),'+0','+'),'+0','+'),'^','!E'),'e+','x10!U'),'e-','x10!U-')
          endcase
          gx_plot_label,0.01,y,parmstr,/ylog,/xlog,color=255,charsize=1.2*charsize,charthick=2
          y-=0.05
         endfor
         
     end
    end
    tvlct,rgb_curr
    !p=psave
  end
end




pro gsfit_schedule,state,task,start=start
  if ~ptr_valid(state.pmaps) then return
  default,start,0
  bridges=state.bridges->Get(/all,count=count)
  sz=size((*state.pmaps).data)
  if ~tag_exist(task,'idx') then begin
    idx=reform(lindgen(sz[1]*sz[2]),sz[1],sz[2])
    idx=idx[task.x0:task.x0+task.nx-1,task.y0:task.y0+task.ny-1]
    idx=reform(idx,n_elements(idx))
  endif else idx=task.idx
  lidx=objarr(count)
  for i=0,count-1 do lidx[i]=list()
  k=0
  for i=0,n_elements(idx)-1 do begin
    lidx[k]->add,idx[i]
    k=k eq count-1?0:k+1
  endfor
  for i=0,task.nt-1,task.dt do begin
    for j=0,count-1 do begin
      if ~lidx[j].IsEmpty() then   state.fittasks->add,{id:state.fittasks.count(),idx:reform(lidx[j]->ToArray()),t:task.t0+i,fmin:task.fmin,fmax:task.fmax,status:'pending'}
    endfor
  endfor
  gsfit_update_queue,state
  obj_destroy,lidx
  if start eq 1 then widget_control,state.wStart,send_event={id:0l,top:0l,handler:0l,select:1},/set_button
end

pro gsfit_update_queue,state
  widget_control,state.wTab,set_tab_current=1
  for i=0,state.fittasks.Count()-1 do begin
    item=state.fittasks(i)
    line={tim_idx:long(item.t),npix:n_elements(item.idx),status:item.status}
    rows=i eq 0?line:[rows,line]
    ids=i eq 0 ? string(item.id,format='(g0)'):[ids, string(item.id,format='(g0)')]
  endfor
  widget_control,state.wQueue,table_ysize=n_elements(rows)
  if n_elements(ids) gt 0 then widget_control,state.wQueue,set_value=rows,row_labels=[ids]
end

function gsfit_where_pending,state,count
  l=state.fittasks
  pending=l.map(Lambda(l:strupcase(l.status) eq 'PENDING'))
  if pending.IsEmpty() then begin
    count=0
    return,-1
  endif
  pending=where(pending.toarray(),count)
  return,pending
end

pro gsfit_start,state
  if state.fittasks.IsEmpty() then begin
    message,'No tasks in queue, nothing to be done!',/info
    return
  endif
  pending=gsfit_where_pending(state,task_count)
  if task_count eq 0 then begin
    message,'No pending tasks in queue, nothing to be done!',/info
    return
  endif
  allbridges=state.bridges->Get(/all,count=count)
  for i=0, count-1 do begin
    code=allbridges[i]->Status(error=error)
    if code ne 1 then bridges=n_elements(bridges) eq 0?allbridges[i]:[bridges,allbridges[i]]
  end
  bridge_count=n_elements(bridges)
  if bridge_count eq 0 then begin
    message,'All bridges are bussy, tasks will be waiting in queue!',/info
    return
  endif
  for i=0,min([bridge_count,task_count])-1 do begin
    task=state.fittasks(pending[i])
    gsfit_sendfittask,bridges[i],state,task
  endfor
end


pro gsfit_readframe, bridge,state,task
  spec_in=bridge->GetVar('spec_in')
  spec_out=bridge->GetVar('spec_out')
  aparms=bridge->GetVar('aparms')
  eparms=bridge->GetVar('eparms')
  time_idx=task.t
  data_size=size((*state.pmaps).data)
  xy=array_indices(data_size[1:2],task.idx,/dim)
  x=xy[0,*]
  y=xy[1,*]
  data_spectrum=(errdata_spectrum=(fit_spectrum=fltarr(n_elements(state.header.freq),n_elements(state.header.pol)<2)))
  sz=size(aparms)
  npix=sz[1]
  nparms=sz[2]
  
    for j=0,npix-1 do begin
      l=state.fit
      if ~l.IsEmpty() then begin
        filter=(l.Filter(Lambda(l,x,y,t:l.x eq x and l.y eq y and l.t eq t),x[j],y[j],time_idx))
        match=l.where(filter.toarray(),count=count)
        if count gt 0 then (state.fit).remove,match
      end
      data_spectrum[*]=0
      errdata_spectrum[*]=0
      fit_spectrum[*]=0
      if n_elements(state.header.pol) ne 1 then begin
        data_spectrum[task.fmin:task.fmax,*]=spec_in[j,*,0:1]
        errdata_spectrum[task.fmin:task.fmax,*]=spec_in[j,*,2:3]
        fit_spectrum[task.fmin:task.fmax,*]=spec_out[j,*,0:1]
      endif else begin
        data_spectrum[task.fmin:task.fmax]=spec_in[j,*,0]+spec_in[j,*,1]
        errdata_spectrum[task.fmin:task.fmax]=spec_in[j,*,2]+spec_in[j,*,3]
        fit_spectrum[task.fmin:task.fmax]=spec_out[j,*,0]+spec_out[j,*,1]
      endelse
      afit={x:x[j],y:y[j],t:time_idx,fmin:fix(task.fmin),fmax:fix(task.fmax),data:data_spectrum,errdata:errdata_spectrum, specfit:fit_spectrum}
      for k=0,nparms-1 do begin
        afit=create_struct(afit,state.header.parnames[k],aparms[j,k],state.header.errparnames[k],eparms[j,k])
      endfor
      state.fit.Add,afit
      widget_control,state.wsavefitlist,set_uvalue=1
    endfor
end

pro gsfit_sendfittask,bridge,state,task
  if ~ptr_valid(state.pmaps) then return
  time_idx=task.t
  data_size=size((*state.pmaps).data)
  ij=array_indices(data_size[1:2],task.idx,/dim)
  npix=n_elements(task.idx)
  freq=double(state.header.freq[task.fmin:task.fmax])
  nfreq=n_elements(freq)
  npol=n_elements(state.header.pol)
  dx=((*state.pmaps).dx)[0]
  dy=((*state.pmaps).dy)[0]


  ;===========Define arrays for fitting. First - model, second -EOVSA

  spec_in=dblarr(npix,nfreq,4)
  spec_out=dblarr(npix,nfreq,2)
  
  widget_control,state.lib.wNinput,get_value=ninput
  ninput[2:3]=[npix,nfreq]
  ninput[5]=(npol eq 1)?1:2
  widget_control,state.lib.wNinput,set_value=ninput
  widget_control,state.lib.wRinput,get_value=rinput
  rinput[3]=dx*dy
  widget_control,state.lib.wRinput,set_value=rinput
  widget_control,state.lib.wRMS,get_value=rmsweight
  widget_control,state.lib.wGuessParms,get_value=GuessParms
  widget_control,state.lib.wMinParms,get_value=MinParms
  widget_control,state.lib.wMaxParms,get_value=MaxParms
  ParGuess=[[temporary(GuessParms)],[temporary(MinParms)],[temporary(MaxParms)]]
  aparms=(eparms=dblarr(npix, n_elements(state.header.info.parms_out)))


  flux_roi=reform((*state.pmaps)[task.fmin:task.fmax,*,task.t].data[ij[0,*],ij[1,*]],npix,nfreq,npol)
  err=reform(((*state.pmaps).rms)[task.fmin:task.fmax,*,task.t],nfreq,npol)
  if npol eq 1 then begin
    FOR i = 0, npix-1 DO BEGIN
      spec_in[i,*,0]=flux_roi[i,*,0]>0; only I STOKES fluxes
      spec_in[i,*,2]=err[*,0]+rmsweight*max(err)/freq; only I STOKES errors
      ;the second term attempts to account for the frequency-dependent spatial resolution
    ENDFOR
  endif else begin
    FOR i = 0, npix-1 DO BEGIN
      spec_in[i,*,0]=flux_roi[i,*,0]>0
      spec_in[i,*,2]=err[*,0]+rmsweight*max(err[*,0])/freq
      spec_in[i,*,1]=flux_roi[i,*,1]>0
      spec_in[i,*,3]=err[*,1]+rmsweight*max(err[*,1])/freq
    ENDFOR  
  endelse

  

  ;===========End array definition============================================================

  ;==========Transfer to the bridge the arrays required for fitting==========

  
  bridge->SetVar,'start_time',systime(/seconds)
  bridge->SetVar,'task_id',task.id
  bridge->SetVar,'ninput',ninput
  bridge->SetVar,'rinput',rinput
  bridge->SetVar,'parguess',parguess
  bridge->SetVar,'freq',double(freq)
  bridge->SetVar,'spec_in',spec_in
  bridge->SetVar,'spec_out',spec_out
  bridge->SetVar,'aparms',aparms
  bridge->SetVar,'eparms',eparms
  bridge->SetVar,'libname',state.header.info.path
  id=bridge->GetVar('id')
  
  bridge->Execute,"res=call_external(libname, "+"'"+state.header.info.get_function[1]+"'"+",ninput,rinput,parguess,freq,spec_in,aparms,eparms,spec_out, /d_value,/unload)",/nowait
  
  ;;==========update the queue;==========
  task.status=string(id,format="('Active on bridge #',g0)")
  state.fittasks(task.id)=task
  gsfit_update_queue,state
end

pro gsfit_callback,status,error,bridge,userdata
  task_id=bridge->GetVar('task_id')
  start_time=bridge->GetVar('start_time')
  duration=systime(/seconds)-start_time
  widget_control,widget_info(userdata,Find_By_Uname='STATEBASE'),get_uvalue=state
  if state.fittasks->IsEmpty() then return

  task=state.fittasks(task_id)
  if task_id ne task.id then message,'WARNING! Task ID mismatch- Data may be corrupted!!!', /info
  if status eq 4 then begin
    task.status=string(duration,format="('Aborted after ', g0, ' seconds')")
    state.fittasks(task.id)=task
    gsfit_update_queue,state
    abort=1
  endif else  begin
    gsfit_readframe, bridge, state, task
    task.status=string(duration,format="('Completed in ', g0, ' seconds')")
    state.fittasks(task.id)=task
    gsfit_update_queue,state
  endelse
  widget_control,state.wfitpix,sensitive=1,set_slider_max=n_elements(state.fit)
  widget_control,state.wfitpix,set_value=n_elements(state.fit)
  widget_control,state.wfitpix,send_event={id:0l,top:0l,handler:0l,value:n_elements(state.fit),drag:0}
  pending=gsfit_where_pending(state,task_count)
  if task_count gt 0 and ~keyword_set(abort) then begin
   task=state.fittasks(pending[0])
   gsfit_sendfittask,bridge,state,task
  endif else begin
    bridges=state.bridges->Get(/all,count=count)
    active=0
    for i=0,count-1 do begin
      code=bridges[i]->Status(error=error)
      case code of
        0:status='Idle'
        1:begin
          status='Active'
          active=1
        end
        2:status='Completed'
        3:status='Error'
        4:status='Aborted'
        else:status='Unknown'
      endcase
      message, 'Bridge #'+string(i)+' status: '+ status,/info
    end
  endelse
end

pro gsfit_abort,state
  bridges=state.bridges->Get(/all,count=count)
  for i=0,count-1 do begin
    code=bridges[i]->Status(error=error)
    if code eq 1 then bridges[i]->Abort
  end
end



pro gsfit_event,event
  widget_control,widget_info(event.top,Find_By_Uname='STATEBASE'),get_uvalue=state
  subdirectory=['resource', 'bitmaps']
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    answer=dialog_message(/question,'Do you want to quit GSFIT?')
    if strupcase(answer) eq 'YES' then begin
      widget_control,state.wsavefitlist,get_uvalue=fits2save
      if fits2save then begin
        answ=dialog_message('The current fit list has unsaved changes, do you want to save the list before deleting it from memory?',/question)
        case strupcase(answ) of
          'YES': begin
            file=dialog_pickfile(filter='*.sav',title='Select a filename to save the current fit list',/write,/overwrite)
            if file ne '' then begin
              fit=state.fit.toarray()
              multi_save,lun,fit,file=file,header=state.header,/new,/close
              widget_control,state.wsavefitlist,set_uvalue=0
            endif
          end
          'NO': widget_control,state.wsavefitlist,set_uvalue=0
          else:
        endcase
      endif
      widget_control,state.wsavefitlist,get_uvalue=fits2save
      if ~fits2save then begin
        ptr_free,state.pmaps
        gsfit_abort,state
        bridges=state.bridges->Get(/all,count=count)
        obj_destroy,bridges
        info=state.header.info
        save,info,file='libinfo.inp'
        widget_control,event.top,/destroy
      end
      return
    end
  END  
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW' THEN BEGIN
   if ptr_valid(state.pmaps)  then begin
        widget_control,event.id,get_value=win_ppd,get_uvalue=g
        wset,win_ppd
        !x=g.x
        !y=g.y
        !z=g.z
        !p=g.p
        widget_control,state.wx,get_uvalue=xpix
        widget_control,state.wy,get_uvalue=ypix
       case event.type of
        0: begin
        ;MouseDown
        cursor,x,y,/nowait,/data
        mindx=min(abs(xpix-x),imin)
        x=xpix[imin]
        mindy=min(abs(ypix-y),jmin)
        y=ypix[jmin]
        case event.press of
          1:begin
            device,set_graphics_function=6
            state.ppd_dev_xrange=[event.x,event.x]
            state.ppd_dev_yrange=[event.y,event.y]
            case 1 of
              state.cursor_on: begin
                state.ppd_data_xrange=[x,x]
                state.ppd_data_yrange=[y,y]
              end
              else: begin
                if (state.proi_on eq 0) or  (state.left_button_down eq 0) then begin
                  state.xroi->Remove,/all
                  state.yroi->Remove,/all
                end
                state.xroi->Add,x
                state.yroi->Add,y
              end
            endcase
            if (state.proi_on eq 1)  then begin
              plots, [state.xroi(state.xroi->Count()-2),x],[state.yroi(state.yroi->Count()-2),y],color=255
            endif
            state.left_button_down=1
          end
          4:begin
            if (state.proi_on eq 0) then begin
              widget_control,state.wx,get_uvalue=xpix
              widget_control,state.wy,get_uvalue=ypix
              mindeltax=min(abs(xpix-x),cx)
              widget_control,state.wx,set_value=cx
              mindeltay=min(abs(ypix-y),cy)
              widget_control,state.wy,set_value=cy
              draw=1
            endif else begin
              state.left_button_down=0
              if state.xroi->Count() ge 2 then begin
                state.xroi->Add,x
                state.yroi->Add,y
                state.xroi->Add,(state.xroi)(0)
                state.yroi->Add,(state.yroi)(0)
              endif else begin
                state.xroi->Remove,/all
                state.yroi->Remove,/all
              endelse
              draw=1
            endelse
          end
          else:
        endcase
      end
      
      1:begin
        ;Mouse Up
        if event.release eq 1 then begin
          cursor,x,y,/nowait,/data
          mindx=min(abs(xpix-x),imin)
          x=xpix[imin]
          mindy=min(abs(ypix-y),jmin)
          y=ypix[jmin]
          state.left_button_down=state.proi_on
          state.ppd_dev_xrange[1]=event.x
          state.ppd_dev_yrange[1]=event.y
          state.ppd_dev_xrange=state.ppd_dev_xrange[sort(state.ppd_dev_xrange)]
          x0= state.ppd_dev_xrange[0]
          x1= state.ppd_dev_xrange[1]
          y0= state.ppd_dev_yrange[0]
          y1= state.ppd_dev_yrange[1]
          if ~state.proi_on then plots,/dev,[x0,x0,x1,x1,x0],[y0,y1,y1,y0,y0]   ; Erase current box
          case 1 of
            state.cursor_on: begin
              state.ppd_data_xrange[1]=x
              state.ppd_data_yrange[1]=y
              state.ppd_data_yrange=state.ppd_data_yrange[sort(state.ppd_data_yrange)]
              draw=1
            end
            state.rroi_on: begin
              x0=(state.xroi->Remove(/all))[0]
              y0=(state.yroi->Remove(/all))[0]
              x1=x
              y1=y
              if (x1 ne x0) AND (y1 NE y0) then begin
                x_list=[x0,x0,x1,x1,x0]
                y_list=[y0,y1,y1,y0,y0]
                state.xroi->Add,x_list,/extract
                state.yroi->Add,y_list,/extract
              endif else begin
                state.xroi->Remove,/all
                state.yroi->Remove,/all
              endelse
              draw=1
            end
            state.froi_on : begin
              if state.xroi->Count() ge 2 then begin
                state.xroi->Add,x
                state.yroi->Add,y
                state.xroi->Add,(state.xroi)(0)
                state.yroi->Add,(state.yroi)(0)
              endif else begin
                state.xroi->Remove,/all
                state.yroi->Remove,/all
              endelse
              draw=1
            end
            else:     begin
              state.ppd_dev_xrange[0]=event.x
              state.ppd_dev_yrange[0]=event.y
            end
          endcase
          if state.froi_on eq 0 then device,set_graphics_function=3
        end
      end
      
      2: begin
        ;MouseMove
        cursor,data_x,data_y,/nowait,/data
        mindx=min(abs(xpix-data_x),imin)
        data_x=xpix[imin]
        mindy=min(abs(ypix-data_y),jmin)
        data_y=ypix[jmin]
        if state.left_button_down eq 1 then begin
          x0= state.ppd_dev_xrange[0]
          x1= state.ppd_dev_xrange[1]
          y0= state.ppd_dev_yrange[0]
          y1= state.ppd_dev_yrange[1]
          state.ppd_dev_xrange[1]=(x=event.x)
          state.ppd_dev_yrange[1]=(y=event.y)
          case 1 of
            state.froi_on: begin
              plots,/dev,[x1,x],[y1,y],thick=2
              state.xroi->Add,data_x
              state.yroi->Add,data_y
            end
            state.proi_on: begin
              ;                                      plots,/dev,[x0,x1],[y0,y1]   ; Erase current box
              ;                                      plots,/dev,[x0,x],[y0,y]       ; Draw new box
            end
            else: begin
              plots,/dev,[x0,x0,x1,x1,x0],[y0,y1,y1,y0,y0]   ; Erase current box
              plots,/dev,[x0,x0,x,x,x0],[y0,y,y,y0,y0]       ; Draw new box
            end
          endcase
        end
      end
      else:
     endcase
   end
  ENDIF 
  case event.id of
   state.wMouse.Cursor: begin
                          state.cursor_on=event.select
                          widget_control,event.id,set_value=gx_bitmap(filepath(event.select?'scale_active.bmp':'scale.bmp', subdirectory=subdirectory))
                        end
   state.wMouse.RROI: begin
                          state.rroi_on=event.select
                          widget_control,event.id,set_value=gx_bitmap(filepath(event.select?'rectangl_active.bmp':'rectangl.bmp', subdirectory=subdirectory))
                       end
   state.wMouse.FROI: begin
                          state.froi_on=event.select
                          widget_control,event.id,set_value=gx_bitmap(filepath(event.select?'freepoly_active.bmp':'freepoly.bmp', subdirectory=subdirectory))
                      end  
   state.wMouse.PROI: begin
                          state.proi_on=event.select
                          widget_control,event.id,set_value=gx_bitmap(filepath(event.select?'segpoly_active.bmp':'segpoly.bmp', subdirectory=subdirectory))
                       end           
   state.wtime:begin 
                time=state.header.time
                if n_elements(time) gt event.value then time=time[event.value]
                widget_control,state.wtimelabel,set_value=time+strcompress(string(event.value,format="('[',i3,']')"),/rem)
                draw=1
               end 
   state.wfreq:begin 
                freq=state.header.freq
                if n_elements(freq) gt event.value then freq=string(freq[event.value],format="(f5.2,' GHz')")
                widget_control,state.wfreqlabel,set_value=freq+strcompress(string(event.value,format="('[',i3,']')"),/rem)
                draw=1
              end
   state.wpol:draw=1
   state.wx:begin 
             draw=1
            end 
   state.wy:begin
             draw=1
            end 
   state.wpalette: begin
                     xloadct,/silent,/block
                     draw=1
                   end  
   state.wimport: begin
                   ;added capability to directly import a sequence of EOVSA fits files
                   maps=gsfit_readfits(files,flux_threshold=flux_threshold,rms_mask=rms_mask,/sfu)
                   if valid_map(maps) then begin
                    break_file, files[0], disk_log, file
                    goto, validmaps
                   endif
                  end                 
   state.wopen: begin
                 widget_control,state.wsavefitlist,get_uvalue=fits2save
                 if fits2save then begin
                   answ=dialog_message('The current fit list has unsaved changes, do you want to save the list before deleting it from memory?',/question)
                   case strupcase(answ) of
                     'YES': begin
                       file=dialog_pickfile(filter='*.sav',title='Select a filename to save the current fit list',/write,/overwrite)
                       if file ne '' then begin
                         fit=state.fit.toarray()
                         multi_save,lun,fit,file=file,header=state.header,/new,/close
                         widget_control,state.wsavefitlist,set_uvalue=0
                       endif
                     end
                     'NO': widget_control,state.wsavefitlist,set_uvalue=0
                     else:
                   endcase
                 endif
                 widget_control,state.wsavefitlist,get_uvalue=fits2save
                 state.fittasks->Remove,/all
                 state.fit->Remove,/all
                 gsfit_update_queue,state
                 widget_control,state.wfitpix,sensitive=1,set_slider_max=n_elements(state.fit)
                 widget_control,state.wfitpix,set_value=n_elements(state.fit)
                 widget_control,state.wfitpix,send_event={id:0l,top:0l,handler:0l,value:n_elements(state.fit),drag:0}
                 file=dialog_pickfile(filter='*.sav',title='Select an IDL sav file containg an EOVSA map cube structure',/read)
                 if file ne '' then begin
                  maps=eovsa_maps2gsfit(file,/sfu)
                  if valid_map(maps) then begin
                      validmaps:
                      sz=size(maps.data)
                      nx=sz[1]
                      ny=sz[2]
                      sz=size(maps)
                      nfreq=sz[1]
                      maxfreq=nfreq-1
                      npol=sz[2]
                      maxpol=npol-1
                      ntimes=sz[3]
                      maxtime=ntimes-1
                      dx=(maps.dx)[0]
                      dy=(maps.dy)[0]
                      freq=reform(maps[*,0,0].freq)
                      pol=reform(maps[0,*,0].stokes)
                      time=reform(maps[0,0,*].time)
                      ptr_free,state.pmaps
                      state.pmaps=ptr_new(temporary(maps))     
                      header=state.header
                      header=rem_tag(header,['freq','pol','time','refmap'])
                      header=add_tag(header,freq,'freq')
                      header=add_tag(header,pol,'pol')
                      header=add_tag(header,time,'time')
                      header=add_tag(header,(*state.pmaps)[0,0],'refmap')
                      state=rem_tag(state,'header')
                      state=add_tag(state,header,'header')
                    
                      xrange=get_map_xrange((*state.pmaps)[0])
                      xpix=xrange[0]+findgen(nx)*dx
                      yrange=get_map_yrange((*state.pmaps)[0])
                      ypix=yrange[0]+findgen(ny)*dy
                      yrange=get_map_xrange((*state.pmaps)[0])
                      freq=state.header.freq
                      flabels=string(freq[0],format="(f5.2,' GHz')")
                      for i=1,n_elements(freq)-1 do begin
                        flabels=[flabels,string(freq[i],format="(f5.2,' GHz')")]
                      endfor
                      widget_control,state.wMinFreq,set_value=flabels
                      widget_control,state.wMaxFreq,set_value=flabels,SET_DROPLIST_SELECT=maxfreq
                      
                      stokes=reform((*state.pmaps)[0,*,0].stokes)
                      widget_control,state.wpol,set_value=n_elements(stokes) eq 1 ?stokes:['I',stokes],sensitive=(n_elements(stokes) gt 1)
                      widget_control,state.lib.wNinput,get_value=ninput
                      ninput[3]=n_elements(freq)
                      ninput[4]=n_elements(stokes)
                      ninput[5]=n_elements(stokes)
                      widget_control,state.lib.wNinput,set_value=ninput
                      
                      widget_control,state.wfreq, set_slider_max=maxfreq,set_value=0,sensitive=1
                      widget_control,state.wtime, set_slider_max=maxtime,set_value=0,sensitive=1
                      widget_control,state.wx,set_value=0,set_slider_max=nx,set_uvalue=xpix,sensitive=1
                      widget_control,state.wy,set_value=0,set_slider_max=ny,set_uvalue=ypix,sensitive=1
                      widget_control,state.wspectrum.fixed,sensitive=1
                      state.lock=1
                      state.datafile=file
                      widget_control,state.main_base,base_set_title='GSFIT ['+file+']'
                      fastcode_update=1
                      draw=1
                     endif
                 endif             
                end
                
  state.wMinFreq: begin
                    minfreq=widget_info(state.wMinFreq,/droplist_select)
                    maxfreq=widget_info(state.wMaxFreq,/droplist_select)
                    if minfreq gt maxfreq then begin
                      widget_control,state.wMaxFreq,set_droplist_select=minfreq
                      widget_control,state.wMinFreq,set_droplist_select=maxfreq
                    endif
                    widget_control,state.lib.wNinput,get_value=ninput
                    ninput[3]=maxfreq-minfreq+1
                    widget_control,state.lib.wNinput,set_value=ninput
                 end   
  state.wMaxFreq: begin
                    minfreq=widget_info(state.wMinFreq,/droplist_select)
                    maxfreq=widget_info(state.wMaxFreq,/droplist_select)
                    if minfreq gt maxfreq then begin
                     widget_control,state.wMaxFreq,set_droplist_select=minfreq
                     widget_control,state.wMinFreq,set_droplist_select=maxfreq
                    endif
                    widget_control,state.lib.wNinput,get_value=ninput
                    ninput[3]=maxfreq-minfreq+1
                    widget_control,state.lib.wNinput,set_value=ninput
                 end                          
  
  state.wFitOne:begin
              widget_control,state.wx,get_value=x0
              widget_control,state.wy,get_value=y0
              widget_control,state.wtime,get_value=t0
              minfreq=widget_info(state.wMinFreq,/droplist_select)
              maxfreq=widget_info(state.wMaxFreq,/droplist_select)
              task={x0:x0,y0:y0,t0:t0,nx:1,ny:1,nt:1,dt:1,fmin:minfreq,fmax:maxfreq}
              gsfit_schedule,state,task,/start
             end    
              
  state.wFitRange:begin
                if ptr_valid(state.pmaps) then begin
                sz=size((*state.pmaps).data)
                nx=sz[1]
                ny=sz[2]
                nf=sz[3]
                np=sz[4]
                nt=sz[5]
                widget_control,state.wtime,get_value=t0
                freq=state.header.freq
                flabels=string(freq[0],format="(f5.2,' GHz')")
                for i=1,n_elements(freq)-1 do begin
                  flabels=flabels+'|'+string(freq[i],format="(f5.2,' GHz')")
                endfor
                minfreq=widget_info(state.wMinFreq,/droplist_select)
                maxfreq=widget_info(state.wMaxFreq,/droplist_select)
                
                if state.xroi.Count() gt 1 then begin
                  xroi=state.xroi.ToArray()
                  yroi=state.yroi.ToArray()
                  widget_control,state.wx,get_uvalue=xpix
                  widget_control,state.wy,get_uvalue=ypix
                  idx=polyfillv(floor((xroi-xpix[0])/(xpix[1]-xpix[0])),floor((yroi-ypix[0])/(ypix[1]-ypix[0])),nx,ny)
                  ij=array_indices([nx,ny],idx,/dim)
                  xrange=minmax(ij[0,*])
                  yrange=minmax(ij[1,*])
                  roi_info=strcompress(string([n_elements(idx),xrange,yrange],format="(i10,' ROI pixels inscribed in x_pix=[',g0,'; ',g0,'] y_pix=[',g0,'; ',g0,']')"))
                  desc=[$
                    '0, BASE,, Column, FRAME', $
                    '1, BASE,, Row, FRAME', $
                    '2, LABEL,'+roi_info+', CENTER', $
                    '1, BASE,, Row, FRAME', $
                    '0, INTEGER, 1, LABEL_LEFT= Fit Starts @ T= '+state.header.time[t0]+' for   , WIDTH=6, TAG=nt',$
                    '2, LABEL,time frames, left', $
                    '1, BASE,, Column, FRAME', $
                    '2, INTEGER, 1, LABEL_LEFT= Time Step:, WIDTH=6, TAG=dt',$
                    '1, BASE,, Row, FRAME', $
                    '2, BUTTON, Fit Selected ROI and Time Range|Fit Full Datacube( '+$
                     strcompress(string(nx,ny,nt,format="(g0,'x',g0,'x',g0,'/dt pixels')"))+'), Exclusive, Column,SET_VALUE=0,LABEL_LEFT='', TAG=full', $
                    '1, BASE,, Row, FRAME', $
                    '0, Droplist,'+flabels +',LABEL_LEFT= Min freq. range:,set_value='+string(minfreq)+', TAG=fmin',$
                    '2, Droplist,'+flabels +', LABEL_LEFT= Max freq. range:, set_value='+string(maxfreq)+',TAG=fmax',$
                    '1, BASE,, ROW,Frame', $
                    '2, BUTTON, Enqueue and Wait|Enqueue and Process|Save Task List, Exclusive, Row,SET_VALUE=1,LABEL_LEFT='', TAG=start', $
                    '1, BASE,, ROW,Frame', $
                    '0, BUTTON, OK, QUIT,' $
                    + 'TAG=OK', $
                    '2, BUTTON, Cancel, QUIT']
                    a = CW_FORM(desc, /COLUMN,title='Fit ROI Selection')
                    if a.full then begin
                      idx=findgen(nx*ny)
                      t0=0
                    endif else begin
                      nt=a.nt>1<(nt-t0)
                    end
                    dt=a.dt>1<nt
                    frange=[a.fmin,a.fmax]
                    frange=frange[sort(frange)]
                    task={idx:idx,t0:t0,nt:nt,dt:dt,fmin:frange[0],fmax:frange[1]}
                endif else begin
                  widget_control,state.wxlabel,get_value=x0
                  widget_control,state.wx,get_value=x
                  x0=strmid(x0,9)
                  widget_control,state.wylabel,get_value=y0
                  widget_control,state.wy,get_value=y
                  y0=strmid(y0,9)
                  desc=[$
                   '0, BASE,, Column, FRAME', $
                   '1, BASE,, Row, FRAME', $
                   '0, INTEGER, 1, LABEL_LEFT= Fit Starts @ Cursor X= '+x0+' for , WIDTH=6, TAG=nx',$
                   '2, LABEL, pixels, left', $
                   '1, BASE,, Row, FRAME', $
                   '0, INTEGER, 1, LABEL_LEFT= Fit Starts @ Cursor Y= '+y0+' for , WIDTH=6, TAG=ny',$
                   '2, LABEL, pixels, left', $
                   '1, BASE,, Row, FRAME', $
                   '0, INTEGER, 1, LABEL_LEFT= Fit Starts @ T= '+state.header.time[t0]+' for   , WIDTH=6, TAG=nt',$
                   '2, LABEL,time frames, left', $
                   '1, BASE,, Column, FRAME', $
                   '2, INTEGER, 1, LABEL_LEFT= Time Step:, WIDTH=6, TAG=dt',$
                   '1, BASE,, Column, FRAME', $
                   '2, BUTTON, Fit Selected Rectangle and Time Range|Fit Full Datacube( '+$
                       strcompress(string(nx,ny,nt,format="(g0,'x',g0,'x',g0,'/dt pixels')"))+'), Exclusive, Column,SET_VALUE=0,LABEL_LEFT='', TAG=full', $
                   '1, BASE,, Row, FRAME', $
                   '0, Droplist,'+flabels +',LABEL_LEFT= Min freq. range:,set_value='+string(minfreq)+', TAG=fmin',$
                   '2, Droplist,'+flabels +', LABEL_LEFT= Max freq. range:, set_value='+string(maxfreq)+',TAG=fmax',$
                   '1, BASE,, ROW,Frame', $
                   '2, BUTTON, Enqueue and Wait|Enqueue and Process|Save Task List, Exclusive, Row,SET_VALUE=1,LABEL_LEFT='', TAG=start', $
                   '1, BASE,, ROW,Frame', $
                   '0, BUTTON, OK, QUIT,' $
                   + 'TAG=OK', $
                   '2, BUTTON, Cancel, QUIT']
                   a = CW_FORM(desc, /COLUMN,title='Fit Range Selection')
                   if a.full then begin
                     x0=0
                     y0=0
                     t0=0
                   endif else begin
                     x0=x
                     y0=y
                     ny=a.ny>1<(ny-y)
                     nx=a.nx>1<(nx-x)
                     nt=a.nt>1<(nt-t0)
                     t0=t0
                   end
                   dt=a.dt>1<nt
                   frange=[a.fmin,a.fmax]
                   frange=frange[sort(frange)]
                   task={x0:x0,y0:y0,t0:t0,nx:nx,ny:ny,nt:nt,dt:dt,fmin:frange[0],fmax:frange[1]}
               endelse
               if a.ok then begin
                 widget_control,state.wMinFreq,set_droplist_select=frange[0]
                 widget_control,state.wMaxFreq,set_droplist_select=frange[1]
                 widget_control,state.lib.wNinput,get_value=ninput
                 ninput[3]=frange[1]-frange[0]+1
                 widget_control,state.lib.wNinput,set_value=ninput
                 fastcode_update=1
                 if a.start eq 2 then begin
                   info=state.header.info
                   cpinput=rep_tag_value(rep_tag_value(info,state.datafile,'datapath'),task,'task')
                   cpfile=dialog_pickfile(title='Select a filename to save a gsfit command prompt input',filter='*.sav',default='*.sav',/write,/overwrite)
                   if cpfile ne '' then save,cpinput,file=cpfile
                 endif else gsfit_schedule,state,task,start=a.start
               end
               end
             end           
  state.wfitpix: begin
                   widget_control, event.id ,get_value=idx
                   if idx gt 0 then begin
                    fit=state.fit(idx-1)
                    widget_control,state.wx,set_value=fit.x
                    widget_control,state.wy,set_value=fit.y
                    widget_control,state.wtime,set_value=fit.t
                    widget_control,state.wtimelabel,set_value=state.header.time[fit.t]+strcompress(string(fit.t,format="('[',i3,']')"),/rem)
                    draw=1
                   end
                  end    
                  
   state.wQueue: begin
                  help,event
                 end                
   state.wSaveFitList:begin
                       if ptr_valid(state.pmaps) and ~state.fit.IsEmpty() then begin
                        file=dialog_pickfile(filter='*.sav',title='Select a filename to save the current fit list',/write,/overwrite)
                        if file ne '' then begin
                         fit=state.fit.toarray()
                         multi_save,lun,fit,file=file,header=state.header,/new,/close
                         widget_control,state.wsavefitlist,set_uvalue=0
                        end
                       end 
                       end   
    state.wList2View:begin
                          if ~state.fit.IsEmpty() then begin
                           fit=state.fit.toarray()
                           maps=gsfit_log2map(fit,header=state.header)
                           gsfitview,maps
                          end 
                       end 
   state.wExportFitList:begin
                         if  ~state.fit.IsEmpty() then begin
                           file=dialog_pickfile(filter='*.sav',title='Select a filename to conveet and save the current fit list as a structure',/write,/overwrite)
                           if file ne '' then begin
                             fit=state.fit.toarray()
                             maps=gsfit_log2map(fit,header=state.header)
                             save,maps,file=file
                           end  
                         end
                       end                                       
   state.wRestoreFitList:begin
                         if ~ptr_valid(state.pmaps) then begin
                          answ=dialog_message('There is no EOVSA map cube in memory, upload one first!',/error)
                          goto,skip_import
                         endif
                         if ~state.fit.IsEmpty() then begin
                          answ=dialog_message('A not empty fit list already exists in memory, do you want to overwrite it?',/question)
                          if strupcase(answ) ne 'YES' then goto,skip_import
                         endif
                         file=dialog_pickfile(filter=['*.log','*.sav'],title='Select a filename to restore a fit list',/read)
                         if file ne '' then begin
                           fit=gsfit_log2list(file,header=header)
                           if n_elements(fit) eq 0 then begin
                            answ=dialog_message('This file does not have the expected content. Operation aborted!',/error)
                            goto,skip_import
                           endif
                           curr_time=state.header.time
                           curr_freq=state.header.freq     
                           if ~(array_equal(header.time,curr_time) and array_equal(header.freq,curr_freq) ) then begin ;and ~array_equal(header.refmap.data,(*state.pmaps)[0,0].data)
                             answ=dialog_message('This file contains a fit structure not compatible with the EOVSA map cube in memory. Operation aborted!',/error)
                             goto,skip_import
                           endif
                           
                           localinfo=gsfit_libinfo()
                           localinfo.nparms=header.info.nparms
                           localinfo.rparms=header.info.rparms
                           localinfo.parms_in=header.info.parms_in
                           header=rem_tag(header,'info')
                           header=add_tag(header,localinfo,'info')
                           state=rem_tag(state,'header')
                           state=add_tag(state,header,'header')
                           state.fit.remove,/all
                           state.fit.add,fit,/extract
                           widget_control,state.wfitpix,sensitive=1,set_slider_max=n_elements(state.fit) 
                           widget_control,state.wfitpix,set_value=n_elements(state.fit) 
                           info=header.info
                           goto,newinfo
                         end
                         skip_import:
                       end  
   state.wSelectTarget: begin
                          libpath=dialog_pickfile(filter=(!version.os_family eq 'Windows')?'*.dll':'*.so',title='Please select a gxfit executable routine',path=file_dirname((ROUTINE_INFO('gsfit',/source)).path,/mark),/read)
                          if file_exist(libpath) then begin
                            info=gsfit_libinfo(libpath)
                            if n_elements(info) eq 0 then begin
                              answ=dialog_message('Restoring the GSFIT settings from the GSFIT library failed!')
                              return
                            endif
                            newinfo:
                            fastcode_update=1
                            if MATCH_STRUCT(state.header.info,info,/TYPE_ONLY) then begin
                              state.header.info=info
                              widget_control,state.lib.wNinput, set_value=info.nparms.value
                              widget_control,state.lib.wRinput, set_value=info.rparms.value
                              widget_control,state.lib.wGuessParms, set_value=info.parms_in.guess
                              widget_control,state.lib.wMinParms, set_value=info.parms_in.min
                              widget_control,state.lib.wMaxParms, set_value=info.parms_in.max
                              widget_control,state.lib.wLibPath, set_value=info.path
                              widget_control,state.lib.wrms, set_value=info.rms
                            endif else begin
                              header=state.header
                              header=rep_tag_value(header,info,'info')
                              state=rep_tag_value(state,header,'header')
                              wInputBase=widget_info(event.top,find_by_uname='InputBase')
                              gsfit_create_input_widgets,wInputBase,info,input_widgets=input_widgets
                              widget_control,state.lib.wLibPath, set_value=info.path
                              state=rep_tag_value(state,create_struct(input_widgets,'wLibPath',state.lib.wLibPath),'lib')
                            end
                          endif
                        end   
   state.wAbort:begin
                   desc = [ $
                           '1, BASE,, ROW, FRAME', $
                           '2, BUTTON, Abort all active tasks|Delete all pending tasks|Abort all active tasks and flush the queue|Delete all fits and flush the queue!!! , EXCLUSIVE,LABEL_TOP=Select with caution!,' $
                           + 'COLUMN,Set_Value=0,TAG=select', $
                           '1, BASE,, ROW', $
                           '0, BUTTON, OK, QUIT,' $
                           + 'TAG=OK', $
                           '2, BUTTON, Cancel, QUIT']
                           a = CW_FORM(desc, /COLUMN,title='Delete Options')
                           if a.ok then begin
                            case a.select of
                              0: gsfit_abort,state 
                              1: begin
                                  l=state.fittasks
                                  pending=l.map(Lambda(l:strupcase(l.status) eq 'PENDING'))
                                  if ~pending.IsEmpty() then begin
                                    pending=where(pending.toarray(),count)
                                    if count gt 0 then state.fittasks.remove, pending
                                  end
                                  gsfit_update_queue,state
                                 end
                              2:begin 
                                 gsfit_abort,state 
                                 state.fittasks->Remove,/all
                                 gsfit_update_queue,state
                                end 
                              3:begin
                                   gsfit_abort,state 
                                   state.fittasks->Remove,/all
                                   gsfit_update_queue,state
                                   widget_control,state.wsavefitlist,get_uvalue=fits2save
                                   if fits2save then begin
                                    answ=dialog_message('The current fit list has unsaved changes, do you want to save the list before deleting it from memory?',/question)
                                    case strupcase(answ) of
                                    'YES': begin
                                            file=dialog_pickfile(filter='*.sav',title='Select a filename to save the current fit list',/write,/overwrite)
                                            if file ne '' then begin
                                              fit=state.fit.toarray()
                                              multi_save,lun,fit,file=file,header=state.header,/new,/close
                                              widget_control,state.wsavefitlist,set_uvalue=0
                                            endif
                                           end
                                      'NO': widget_control,state.wsavefitlist,set_uvalue=0    
                                      else:   
                                      endcase  
                                   endif    
                                   widget_control,state.wsavefitlist,get_uvalue=fits2save  
                                   if ~fits2save then begin
                                    state.fit->Remove,/all
                                    widget_control,state.wfitpix,sensitive=1,set_slider_max=n_elements(state.fit)
                                    widget_control,state.wfitpix,set_value=n_elements(state.fit)
                                    widget_control,state.wfitpix,send_event={id:0l,top:0l,handler:0l,value:n_elements(state.fit),drag:0}
                                    draw=1
                                   end 
                                 end   
                              else: print,a.select
                            endcase  
                           endif

                 end 
                 
   state.lib.wGuessParms:begin
                          widget_control,event.id,get_value=value
                          state.header.info.parms_in.guess=value
                          fastcode_update=1
                          draw=1
                         end 
   state.lib.wMinParms:begin
                         widget_control,event.id,get_value=value
                         state.header.info.parms_in.min=value
                       end      
   state.lib.wMaxParms:begin
                         widget_control,event.id,get_value=value
                         state.header.info.parms_in.max=value
                       end         
   state.lib.wrinput:begin
                         widget_control,event.id,get_value=value
                         state.header.info.rparms.value=value
                         fastcode_update=1
                         draw=1
                       end  
   state.lib.wninput:begin
                         widget_control,event.id,get_value=value
                         if value[5] eq 1 then value[4]=1
                         value[4]=value[4]>1<4
                         widget_control,event.id,set_value=value
                         state.header.info.nparms.value=value
                         fastcode_update=1
                         draw=1
                       end 
   state.lib.wrms:begin
                         widget_control,event.id,get_value=value
                         state.header.info.rms=value
                         draw=1
                       end                       
   state.wOpenSettings:begin
                        file=dialog_pickfile(filter='*.inp',title='Please select a GSFIT settings file',/read)
                        if file ne '' then begin
                          restore,file
                          if ~(tag_exist(info,'nparms') and tag_exist(info,'rparms')  and tag_exist(info,'parms_out') and tag_exist(info,'parms_in')and tag_exist(info,'path')) then begin
                            answ=dialog_message('Invalid GSFIT settings file!')
                            return
                          endif
                            if MATCH_STRUCT(state.header.info,info,/TYPE_ONLY) then begin
                              state.header.info=info
                              widget_control,state.lib.wNinput, set_value=info.nparms.value
                              widget_control,state.lib.wRinput, set_value=info.rparms.value
                              widget_control,state.lib.wGuessParms, set_value=info.parms_in.guess
                              widget_control,state.lib.wMinParms, set_value=info.parms_in.min
                              widget_control,state.lib.wMaxParms, set_value=info.parms_in.max
                              widget_control,state.lib.wLibPath, set_value=info.path
                              widget_control,state.lib.wrms, set_value=info.rms
                            endif else begin
                              header=state.header
                              header=rep_tag_value(header,info,'info')
                              state=rep_tag_value(state,header,'header')
                              wInputBase=widget_info(event.top,find_by_uname='InputBase')
                              gsfit_create_input_widgets,wInputBase,info,input_widgets=input_widgets
                              widget_control,state.lib.wLibPath, set_value=info.path
                              state=rep_tag_value(state,create_struct(input_widgets,'wLibPath',state.lib.wLibPath),'lib')
                            end
                            fastcode_update=1
                            draw=1
                        endif
                       end   
   state.wSaveSettings: begin
                           file=dialog_pickfile(default='inp',filter='*.inp',title='Please select a filename to save the current GSFIT settings',/overwrite,/write)
                           if file ne '' then begin
                             info=state.header.info
                             save,info,file=file
                           endif
                        end                    
   state.wResetSettings:begin
                           answ=dialog_message(['This action will reset the GSFIT settings to their default value!',$
                            'Do you want to save the current settings to a file for later use before proceeding further?'],/question,/cancel )
                           case strupcase(answ) of
                            'YES': begin
                                     file=dialog_pickfile(default='inp',filter='*.inp',title='Please select a filename to save the current GSFIT settings',/overwrite,/write)
                                     if file ne '' then begin
                                       info=state.header.info
                                       save,info,file=file
                                     endif
                                   end
                            'CANCEL':
                            else: begin
                                   info=gsfit_libinfo(state.header.info.path)
                                   if n_elements(info) eq 0 then begin
                                    answ=dialog_message('Restoring the GSFIT settings from the GSFIT library failed!')
                                    return
                                   endif
                                   state.header.info=info
                                   widget_control,state.lib.wNinput, set_value=info.nparms.value
                                   widget_control,state.lib.wRinput, set_value=info.rparms.value
                                   widget_control,state.lib.wGuessParms, set_value=info.parms_in.guess
                                   widget_control,state.lib.wMinParms, set_value=info.parms_in.min
                                   widget_control,state.lib.wMaxParms, set_value=info.parms_in.max
                                   widget_control,state.lib.wLibPath, set_value=info.path
                                   widget_control,state.lib.wrms, set_value=info.rms
                                   fastcode_update=1
                                   draw=1
                                 end
                           endcase
                       end   
   state.wCopySettings: begin
                         gsfit_fit2guess,state
                         fastcode_update=1
                         draw=1
                        end                    
   state.wspectrum.fixed:begin
                             widget_control,state.wspectrum.range,sensitive=event.select
                             draw=~event.select
                         end    
   state.wspectrum.range: draw=1  
   state.wspectrum.charsize: draw=1  
   state.wspectrum.threshold: draw=1   
   state.wFastCode:draw=event.select                                                                                                                                             
   state.wStart:if event.select then gsfit_start,state   
   state.wBridges: begin
                    widget_control,event.id,get_value=nbridges
                    if (state.bridges->Count() eq nbridges) then begin
                      message,'Requested number of bridges matches the number already existing, nothing to be done!',/info
                      goto, exit_bridges
                    endif
                    if (state.bridges->Count() gt nbridges) then begin
                       n=state.bridges->Count()-nbridges
                       message,string(n,format=(n gt 1)?"('Removing ',g0,' bridges')":"('Removing ',g0,' bridge')"),/info
                       for i= nbridges, state.bridges->Count()-1  do begin
                         bridge=state.bridges->Get(position=state.bridges->Count()-1 )
                         state.bridges->Remove,bridge
                         obj_destroy,bridge
                       endfor
                       goto,exit_bridges
                     endif
                     if (state.bridges->Count() lt nbridges) then begin
                       n=nbridges-state.bridges->Count()
                       message,string(n,format=(n gt 1)?"('Adding ',g0,' bridges')":"('Adding ',g0,' bridge')"),/info
                       start_index=state.bridges->Count()
                     endif
                     for i=start_index,nbridges-1 do begin
                      message,strcompress(string(i+1,format="('Initializing bridge #',i3)")),/info
                      bridge=obj_new('IDL_Bridge',userdata=state.main_base,callback='gsfit_callback',out=GETENV('IDL_TMPDIR')+GETENV('USER')+strcompress('gsfit_bridge'+string(i)+'.log',/rem))  
                      if obj_valid(bridge) then begin
                        bridge->SetVar,'id',i
                        code=bridge->Status(error=error)
                        state.bridges->Add,bridge
                        widget_control,state.wBridges,set_value=state.bridges->Count()
                      end
                     end
                     exit_bridges:
                   end 
   state.wMouse.wROISave: begin
                     file=dialog_pickfile(title='Select a filename to save ROI coordinates',filter='*.sav',/overwrite,/write)
                     if file ne '' then begin
                       xroi=state.xroi->ToArray()
                       yroi=state.yroi->ToArray()
                       save,xroi,yroi,file=file
                     endif
                   end  
   
   state.wMouse.wROIopen:begin
                     widget_control,state.wMouse.Cursor,send_event={ID:0L,Top:0l,Handler:0L,SELECT:1L}
                     if widget_info(state.wMouse.RROI,/button) then widget_control,state.wMouse.RROI,send_event={ID:0L,Top:0l,Handler:0L,SELECT:0L}
                     if widget_info(state.wMouse.PROI,/button) then widget_control,state.wMouse.PROI,send_event={ID:0L,Top:0l,Handler:0L,SELECT:0L}
                     if widget_info(state.wMouse.FROI,/button) then widget_control,state.wMouse.FROI,send_event={ID:0L,Top:0l,Handler:0L,SELECT:0L}
                     file=dialog_pickfile(title='Select a filename to restore a set of ROI coordinates',filter='*.sav',/read)
                     if file ne '' then begin
                       restore,file
                       if n_elements(xroi) eq 0 or n_elements(yroi) eq 0 then begin
                         answ=dialog_message('Unexpected file content!')
                       endif else begin
                         state.xroi->Remove,/all
                         state.yroi->Remove,/all
                         state.xroi->Add,xroi,/extract
                         state.yroi->Add,yroi,/extract
                         draw=1
                       endelse
                     endif
                   end                
                                                                           
   state.wHelp: begin
                 help='http://www.ovsa.njit.edu/wiki/index.php/GSFIT_Help'
                 if !version.os_family eq 'Windows' then spawn,'start /max '+help else answ=dialog_message(' For help, please open ' +help+' in your preffred browser.')    
                end                                      
   else:
  endcase
  gsfit_fastcode_update,state,fastcode_update
  widget_control,widget_info(event.top,Find_By_Uname='STATEBASE'),set_uvalue=state
  gsfit_draw,state,draw
end

pro gsfit_create_input_widgets,wInputBase,info,input_widgets=input_widgets
  wInfoBase=widget_info(wInputBase,find_by_uname='InfoBase')
  if widget_valid(wInfoBase) then widget_control,wInfoBase,/destroy
  wInfoBase=widget_base(wInputBase,/column,uname='InfoBase')
  wNRInputBase=widget_base(wInfoBase,/row)
  wNinputBase=widget_base(wNRInputBase,/column)
  wNinput=cw_objarray(wNinputBase,value=info.nparms.value,names=strcompress(info.nparms.name+info.nparms.unit),/frame,inc=1,/static,sensitive=info.nparms.user,/vert,/right,xtextsize=10,font=!defaults.font,type=1l)
  wRMS=cw_objarray(wNInputBase,value=info.rms,names=' RMS weighting',/right,xtextsize=10,/frame,font=!defaults.font,/static)
  wRInputBase=widget_base(wNRInputBase,/column)
  wRinput=cw_objarray(wRInputBase,value=info.rparms.value,names=strcompress(info.rparms.name+info.rparms.unit),/frame,inc=0.1,/static,sensitive=info.rparms.user,/vert,/right,xtextsize=6,font=!defaults.font)


  wPInputBase=widget_base(wInfoBase,/row)
  wGuessBase=widget_base(wPInputBase,/column)
  wGuessLabel=widget_label(wGuessBase,value='Initial Guess Parameters',/align_center,font=!defaults.font)
  wGuessParms=cw_objarray(wGuessBase,value=info.parms_in.guess,names=strcompress(info.parms_in.name+info.parms_in.unit),/frame,inc=0.1,/static,sensitive=info.parms_in.user,display=info.parms_in.user,/vert,/right,xtextsize=10,font=!defaults.font)

  wMinBase=widget_base(wPInputBase,/column)
  wMinLabel=widget_label(wMinBase,value='Minimum bound',/align_center,font=!defaults.font)
  wMinParms=cw_objarray(wMinBase,value=info.parms_in.min,names='',/frame,inc=0.1,/static,sensitive=info.parms_in.user,display=info.parms_in.user,/vert,xtextsize=12,font=!defaults.font)

  wMaxBase=widget_base(wPInputBase,/column)
  wMaxLabel=widget_label(wMaxBase,value='Maximum bound',/align_center,font=!defaults.font)
  wMaxParms=cw_objarray(wMaxBase,value=info.parms_in.max,names='',/frame,inc=0.1,/static,sensitive=info.parms_in.user,display=info.parms_in.user,/vert,xtextsize=13,font=!defaults.font)
  input_widgets={wNInput:wNinput,wRInput:wRInput,wGuessParms:wGuessParms,wMinParms:wMinParms,wMaxParms:wMaxParms,wRMS:wRMS}
end

pro gsfit_list_slider_set_value,id,value
 max_value=max(widget_info(id,/slider_min_max))
 widget_control,widget_info(widget_info(id,/parent),find_by_uname='fitpixlabel'),set_value=(value eq 0)?'No fit solution computed for the selected pixel!':'Selected Fit Solution: '+strcompress(string(value,max_value,format="(i8,'/',i8)"),/rem)
 widget_control,id,pro_set_value=''
 widget_control,id,set_value=value
 widget_control,id,pro_set_value='gsfit_list_slider_set_value'
end

pro gsfit,nthreads
  gsfitdir=file_dirname((ROUTINE_INFO('gsfit',/source)).path,/mark)
  cd,gsfitdir,curr=cdir
    resolve_routine,'oploterr'
    resolve_routine,'gsfit_log2map',/is_function,/no_recompile
    resolve_routine,'gsfit_log2list',/is_function,/no_recompile
    resolve_routine,'gsfitview',/no_recompile
    resolve_routine,'gsfitcp',/no_recompile
    resolve_routine,'gsfit_energy_computation',/no_recompile
    cd,get_logenv('SSW')+'/packages/gx_simulator/util'
    resolve_routine,'gx_plot_label',/no_recompile
    cd,get_logenv('SSW')+'/packages/gx_simulator/support'
    resolve_routine,'tvplot'
  cd,cdir
  if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'
  defsysv,'!DEFAULTS',EXISTS=exists
  if not exists then gx_defparms
  device, get_screen_size=scr
  if !version.os_family eq 'Windows' then begin
    if scr[0] lt 3200 then !defaults.font='lucida console*12' else !defaults.font='lucida console*24'
  endif else begin
    if scr[0] lt 3200 then !defaults.font='' else !defaults.font=''
  endelse
  if scr[0] lt 3200 then nb=16 else nb=32
  subdirectory=['resource', 'bitmaps']
  xsize=scr[1]/2
  ysize=0.8*scr[1]
  main_base= WIDGET_BASE(Title ='GSFIT',/column,UNAME='MAINBASE',/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=0,$
  x_scroll_size=3*xsize*1.06,y_scroll_size=ysize,/scroll)
  state_base=widget_base(main_base, /column,UNAME='STATEBASE')
  wToolbarBase=widget_base(state_base, /row,/frame,UNAME='TOOLBAR',/toolbar)
  wImport= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('importf.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Import EOVSA Fits Dataset')
  wOpen= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Upload EOVSA Map Cube')
  wPalette = widget_button( wToolbarBase, $
    value=gx_bitmap(filepath('palette.bmp', subdirectory=subdirectory)), $
    /bitmap,tooltip='Change Color Table')   
    
  wCursorBase=widget_base(wToolbarBase,/exclusive,/row)
  wCursor= WIDGET_BUTTON(wCursorBase, VALUE=gx_bitmap(filepath('scale_active.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Cursor',uname='cursor')
  widget_control,wCursor,set_button=1
  wRROI= WIDGET_BUTTON(wCursorBase, VALUE=gx_bitmap(filepath('rectangl.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Rectangular ROI Selection',uname='rroi')
  wPROI= WIDGET_BUTTON(wCursorBase, VALUE=gx_bitmap(filepath('segpoly.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Polygonal ROI Selection',uname='froi')
  wFROI= WIDGET_BUTTON(wCursorBase, VALUE=gx_bitmap(filepath('roi.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Free-hand ROI Selection',uname='froi')
  wROISave= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Save ROI')
  wROIOpen= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('importf.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Import ROI')
  
  wlabel=widget_label(wToolbarBase,value='Fit Frequency Range:',font=!defaults.font)  
  wMinFreq=widget_droplist(wToolbarBase,value=[' 1.00 GHz '])
  wMaxFreq=widget_droplist(wToolbarBase,value=['18.00 GHz'])
  
  wFitOne= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('plot.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Fit Selected Pixel')  
  wFitRange= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('data_range.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Fit Range')   
  wStart=widget_button(wToolbarBase,value=gx_bitmap(gx_findfile('play.bmp')),tooltip='Process the task queue',/bitmap,uname='Queue_START')
  wAbort=widget_button(wToolbarBase,value=gx_bitmap(filepath('delete.bmp', subdirectory=subdirectory)),tooltip='Abort or Delete Tasks',/bitmap,uname='Queue_ABORT')
  wSaveFitList= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('save.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Save Fit List',uvalue=0)
  wRestoreFitList= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('importf.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Restore Fit List')
  wList2View= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('view.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Send Fit List to GSFITVIEW')
  wExportFitList= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('export.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Export Fit List as a Map Structure',uvalue=0)
  wHelp= WIDGET_BUTTON(wToolbarBase, VALUE=gx_bitmap(filepath('help.bmp', subdirectory=subdirectory)),$
    /bitmap,tooltip='Help')  
  
  display_base=WIDGET_BASE(state_base,/row,UNAME='DISPLAYBASE')
  left_panel=WIDGET_BASE(display_base,/column,UNAME='LEFTPANEL')
  right_panel=WIDGET_BASE(display_base,/column,UNAME='RIGHTPANEL')
  display_panel=WIDGET_BASE(display_base,/column,UNAME='DisplayPANEL')
  wfitpixlabel=widget_label(display_panel,xsize=xsize,font=!defaults.font,uname='fitpixlabel',value='Fit Solution Selector')
  wfitpix=WIDGET_SLIDER(display_panel,xsize=xsize,uvalue='Go To Fit',max=1,font=!defaults.font,uname='fitpix',sensitive=0,/suppress,pro_set_value='gsfit_list_slider_set_value')
    
    wTab=WIDGET_TAB(display_panel,/align_left,font=!defaults.font,uname='Tab')
    
    wInputBase=widget_base(wTab,title='GSFIT SETTINGS',/column,uname='InputBase')  
    wSettingsToolbarBase=widget_base(wInputBase, /row,/frame,UNAME='TOOLBAR',/toolbar)
    wOpenSettings= WIDGET_BUTTON(wSettingsToolbarBase, VALUE=gx_bitmap(filepath('open.bmp', subdirectory=subdirectory)),$
      /bitmap,tooltip='Upload GSFIT Settings')
    wSaveSettings= WIDGET_BUTTON(wSettingsToolbarBase, VALUE=gx_bitmap(filepath('save.bmp', subdirectory=subdirectory)),$
      /bitmap,tooltip='Save Current GSFIT Settings')
    wResetSettings= WIDGET_BUTTON(wSettingsToolbarBase, VALUE=gx_bitmap(filepath('reset.bmp', subdirectory=subdirectory)),$
      /bitmap,tooltip='Restore Default GSFIT Settings')
    wCopySettings= WIDGET_BUTTON(wSettingsToolbarBase, VALUE=gx_bitmap(filepath('copy.bmp', subdirectory=subdirectory)),$
      /bitmap,tooltip='Copy guess parameters from fit')  
    wSelectTarget= WIDGET_BUTTON( wSettingsToolbarBase, VALUE=gx_bitmap(filepath('select.bmp', subdirectory=subdirectory)),$
      /bitmap,tooltip='Select Compiled Fitting Routine')
    info=gsfit_libinfo()
    if size(info,/tname) eq 'UNDEFINED' then info=gsfit_libinfo(/get)
    wLibPath=widget_label( wSettingsToolbarBase,/dynamic,value=info.path)    
       
  
    gsfit_create_input_widgets,wInputBase,info,input_widgets=input_widgets
    
    wTaskBase=widget_base(wTab,title='GSFIT Task Queue')
    
    wQueue=widget_table(wTaskBase,font=!defaults.font,ysize=0,xsize=3,$
      y_scroll_size=0,x_scroll_size=0,COLUMN_WIDTHS =[150,150,600],$
      edit=0,format=format,$
      column_labels=['time_idx','#pixels','status'],uname='Queue',$
      scr_ysize=xsize,scr_xsize=xsize,/resizeable_columns)
    widget_control,wQueue,table_ysize=0 
  lleft_panel=widget_base(left_panel,/row)  
 
  freq_base=widget_base(lleft_panel,/column)
  wfreqlabel=widget_label(freq_base,xsize=0.9*xsize,value='Frequency',font=!defaults.font,/align_center,uname='freqlabel')
  wfreq=WIDGET_SLIDER(freq_base,xsize=0.9*xsize,max=maxfreq,uvalue='FREQ',font=!defaults.font,/suppress,uname='frequency',sensitive=0)
  label=WIDGET_LABEL(left_panel,xsize=xsize,font=!defaults.font,value='')
  stokes_base=widget_base(lleft_panel,/column)
  wstokeslabel=widget_label(stokes_base,xsize=0.1*xsize,value='Stokes',font=!defaults.font,/align_center,uname='stokeslabel')
  wpol=widget_droplist(stokes_base,xsize=0.1*xsize,value=['   '],sensitive=0)
  
  rright_panel=widget_base(right_panel,/column)
  wtimelabel=widget_label(rright_panel,xsize=xsize,value='Time',font=!defaults.font,/align_center,uname='timelabel')
  wtime=WIDGET_SLIDER(rright_panel,xsize=xsize,uvalue='TIME',max=maxtime,font=!defaults.font,/suppress,uname='time',sensitive=0)
  label=WIDGET_LABEL(rright_panel,xsize=xsize,font=!defaults.font,value='')
  

   
  wdatamap=WIDGET_DRAW(left_panel,xsize=xsize,ysize=xsize,UNAME='DATAMAP',$
                          /button_events, $
                          /motion_events, $
                          retain=1,$
                          /expose_events, $
                          graphics_level=1 $
                          )                      
  wspecplot=WIDGET_DRAW(right_panel,xsize=xsize,ysize=xsize,UNAME='SPECTRUM',$
                          /expose_events, $
                          retain=1,$
                          graphics_level=1 $
                          )
  wslider_base=widget_base(left_panel,/row)
  wslider_left_base=widget_base(  wslider_base,/column)
  wslider_right_base=widget_base( wslider_base,/column)
  wxlabel=widget_label(wslider_left_base,xsize=xsize/2,value='Cursor X',font=!defaults.font,/align_center,uname='xlabel')
  wx=WIDGET_SLIDER(wslider_left_base,xsize=xsize/2,uvalue='x',max=xsize-1,font=!defaults.font,/suppress,uname='cursor_x',sensitive=0)
  wylabel=widget_label(wslider_right_base,xsize=xsize/2,value='Cursor Y',font=!defaults.font,/align_center,uname='ylabel')
  wy=WIDGET_SLIDER(wslider_right_base,xsize=xsize/2,max=xsize-1,uvalue='y',font=!defaults.font,uname='cursor_y',/suppres,sensitive=0)
  
  woptions_base=widget_base(left_panel,/row,/frame)
  wcharsize=cw_objfield(widget_base(woptions_base,/frame),value=!version.os_family eq 'Windows'?2:1,inc=0.1,min=0.1,max=5,label='Plot Charsize: ',font=!defaults.font,xtextsize=6)
  wapplythreshold=cw_bgroup(widget_base(woptions_base,/frame),['Apply Integrated Flux Threshold'],/nonexclusive,font=!defaults.font)
  
  wyrangelabel=widget_label(right_panel,xsize=xsize,value=' ',font=!defaults.font,/align_center,uname='xlabel')
  wyrangebase=widget_base(right_panel,/row,/frame)
  wyrange=cw_objarray(wyrangebase,value=[0.0,0.0],inc=100,names=['Min Flux: ','Max Flux: '],font=!defaults.font,/static,xtextsize=10)
  widget_control,wyrange,sensitive=0
  wyrangefix=cw_bgroup(widget_base(wyrangebase,/frame),['Fix Y Range'],/nonexclusive,font=!defaults.font)
  widget_control,wyrangefix,sensitive=0 
  wtotalflux=cw_objfield(widget_base(right_panel,/frame),value=0.0,inc=100,label='Bandwith Integrated Flux: ',font=!defaults.font,/indicator,xtextsize=24, units='sfu*GHz')
  wfastcode=cw_bgroup(widget_base(right_panel,/frame,sensitive=(size(info.fastcode,/tname) eq 'STRUCT')),['No Fast Code Overplot','Guess Solution','Fit Solution'],$
    set_value=0,/exclusive,/row) 
  
  if !version.os_family eq 'Windows' then set_plot,'win' else set_plot,'x'                    
  window,/free,/pixmap,xsiz=xsize,ysiz=xsize
  erase,0
  wpixmap=!d.window                        
  WIDGET_CONTROL, /REALIZE, main_base
  default,nx,100l
  default,ny,100l
  default,nfreq,100l
  default,ntimes,100l
  default,pmaps,ptr_new()
  
  if n_elements(nthreads) eq 0 then nthreads=1 else nthreads=nthreads>1
  bridges=obj_new('IDL_Container')
  wBridges= cw_objfield(wToolbarBase,min=1,max=!CPU.HW_NCPU,value=1,label='Number of parallel threads:',xtextsize=3,inc=1,type=1l)
  for i=0,nthreads-1 do begin
    message,strcompress(string(i+1,format="('Initializing bridge #',i3)")),/info
    bridge=obj_new('IDL_Bridge',userdata=main_base,callback='gsfit_callback',out=GETENV('IDL_TMPDIR')+GETENV('USER')+strcompress('gsfit_bridge'+string(i)+'.log',/rem))  
    if obj_valid(bridge) then begin
      bridge->SetVar,'id',i
      bridges->Add,bridge
      widget_control,wBridges,set_value=bridges->Count()
    end
  end
  
  state=$
    {main_base:main_base,$
    wBridges:wBridges,$
    datafile:'',$
    nfreq:nfreq,$
    ntimes:ntimes,$
    lock:0b,$
    pmaps:pmaps,$
    wbase:main_base,$
    wdatamap:wdatamap,$
    wspectrum:{plot:wspecplot,range:wyrange,fixed:wyrangefix,totalflux:wtotalflux,charsize:wcharsize,threshold:wapplythreshold},$
    wfreq:wfreq,$
    wpol:wpol,$
    wx:wx,$
    wy:wy,$
    wtime:wtime,$
    wpixmap:wpixmap,$
    wOpen:wOpen,$
    wImport:wImport,$
    wFitOne:wFitOne,$
    wFitRange:wFitRange,$
    wRestoreFitList:wRestoreFitList,$
    wSaveFitList:wSaveFitList,$
    wSelectTarget:wSelectTarget,$
    wExportFitList:wExportFitList,$
    wfreqlabel:wfreqlabel,$
    wtimelabel:wtimelabel,$
    wxlabel:wxlabel,$
    wylabel:wylabel,$
    wPalette:wPalette,$
    fit:list(),$
    wfitpix:wfitpix,$
    whelp:whelp,$
    fittasks:list(),$
    bridges:bridges,$
    wTab:wTab,$
    wList2View: wList2View,$
    wQueue:wQueue,wStart:wStart,wAbort:wAbort,$
    wOpenSettings:wOpenSettings,$
    wSaveSettings:wSaveSettings,$
    wResetSettings:wResetSettings,$
    wCopySettings:wCopySettings,$
    header:gsfit_info2header(info),$
    wMouse:{Cursor:wCursor,RROI:wRROI,FROI:wFROI,PROI:wPROI,wROISave:wROISave,wROIOpen:wROIOpen},$
    ppd_dev_xrange:[0,0],ppd_dev_yrange:[0,0],$
    ppd_data_xrange:[0.0,0.0],ppd_data_yrange:[0.0,0.0],$
    left_button_down:0, $
    cursor_on:1, $
    froi_on:0, $
    rroi_on:0, $
    proi_on:0, $
    xroi:list(), $
    yroi:list(), $
    wMinFreq:wMinFreq,$
    wMaxFreq:wMaxFreq,$
    wFastCode:wFastCode,$
    lib:create_struct(input_widgets,'wLibPath',wLibPath)}
  widget_control,state_base,set_uvalue=state  
  XMANAGER, 'gsfit', main_base ,/no_block
  loadct,39
  GSFIT_draw,state
end