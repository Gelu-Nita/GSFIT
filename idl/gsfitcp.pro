
function gsfitcp_get_bridges
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  return,bridges
end

function gsfitcp_get_tasklist
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  return,tasklist
end

function gsfitcp_get_maps
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  return,maps
end

function gsfitcp_get_cpinput
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  return,cpinput
end

function gsfitcp_where,pending=pending,completed=completed,aborted=aborted,active=active,count
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  if ~obj_valid(tasklist) then begin
    count=0
    return,-1
  endif
  l=tasklist
  if keyword_set(pending) then p=l.map(Lambda(l:strupcase(l.status) eq 'PENDING')) else p=l.map(Lambda(l:strupcase(l.status) eq ''))
  if keyword_set(completed) then c=l.map(Lambda(l:strupcase(l.status) eq 'COMPLETED')) else c=l.map(Lambda(l:strupcase(l.status) eq ''))
  if keyword_set(aborted) then ab=l.map(Lambda(l:strupcase(l.status) eq 'ABORTED')) else ab=l.map(Lambda(l:strupcase(l.status) eq ''))
  if keyword_set(active) then ac=l.map(Lambda(l:strupcase(l.status) eq 'ACTIVE')) else ac=l.map(Lambda(l:strupcase(l.status) eq ''))
  if p.IsEmpty() then begin
    pcount=0
    p=[-1]
  endif else p=where(p.toarray(),pcount)
  if c.IsEmpty() then begin
    ccount=0
    c=[-1]
  endif  else c=where(c.toarray(),ccount)
  if ab.IsEmpty() then begin
    abcount=0
    ab=[-1]
  endif else ab=where(ab.toarray(),abcount)
  if ac.IsEmpty() then begin
    account=0
    ac=[-1]
  endif else ac=where(ac.toarray(),account)
  count=pcount+ccount+abcount+account
  selected=[-1]
  selected=[p,c,ab,ac]
  sidx=where(selected ne -1,scount)
  selected=(scount ne 0)?selected[sidx]:-1
  return,selected
end


pro gsfitcp_set_bridges, nbridges,new=new,force=force
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
    if n_elements(nbridges) eq 0 then nbridges=1 else nbridges=nbridges>1
    if (nbridges gt !CPU.HW_NCPU ) and (~keyword_set(force)) then begin
      message, string(!CPU.HW_NCPU, nbridges,format="('The number of bridges is limited to the ',g0, ' available CPUs! Use IDL> gsfitcp,nbridges,/force if you are sure you really want to create ',g0,' bridges!')"),/info
      nbridges=!CPU.HW_NCPU 
    endif
    if obj_valid(bridges) then begin
      if keyword_set(new) then begin
        message,'Replacing all bridges',/info
        obj_destroy,bridges
        bridges=obj_new('IDL_Container')
        start_index=0
      endif else begin
        
        if (bridges->Count() eq nbridges) then begin
            message,'Requested number of bridges matches the number already existing, nothing to be done!',/info
            goto,exit_point
        endif
      
       if (bridges->Count() gt nbridges) then begin
        n=bridges->Count()-nbridges
        message,string(n,format=(n gt 1)?"('Removing ',g0,' bridges')":"('Removing ',g0,' bridge')"),/info
        for i= nbridges, bridges->Count()-1  do begin
          bridge=bridges->Get(position=bridges->Count()-1 )
          bridges->Remove,bridge
          obj_destroy,bridge
        endfor
        goto,exit_point 
       endif
       
       if (bridges->Count() lt nbridges) then begin
        n=nbridges-bridges->Count()
        message,string(n,format=(n gt 1)?"('Adding ',g0,' bridges')":"('Adding ',g0,' bridge')"),/info
        start_index=bridges->Count()
       endif
     endelse
    endif else begin
      bridges=obj_new('IDL_Container')
      start_index=0
    endelse
      
    
    bridge_state=replicate({status:'',task:'',time:'',calls:'',error:''},nbridges)
    for i=start_index,nbridges-1 do begin
      message,string(i+1,format="('Initializing bridge #',i3)"),/info
      bridge=obj_new('IDL_Bridge',userdata=main_base,callback='gsfitcp_callback',out=GETENV('IDL_TMPDIR')+GETENV('USER')+strcompress('gsfitcp_bridge'+string(i)+'.log',/rem))
      if obj_valid(bridge) then begin
        bridge->SetVar,'id',i+1
        code=bridge->Status(error=error)
        case code of
          0:bridge_state[i].status='Idle'
          1:bridge_state[i].status='Active'
          2:bridge_state[i].status='Completed'
          3:bridge_state[i].status='Error'
          4:bridge_state[i].status='Aborted'
          else:bridge_state[i].status='Unknown'
        endcase
        bridge_state[i].error=error
        bridges->Add,bridge
      end
    end
    exit_point:
    gsfitcp_status
  end
  
pro gsfitcp_set_maps, datafile
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  maps=eovsa_maps2gsfit(datafile,/sfu)
end

pro gsfitcp_add_task, task, remove_existent=remove_existent
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
     if n_elements(tasklist) eq 0 then tasklist=list()
     if keyword_set(remove_existent) then tasklist->Remove,/all
     count=bridges->Count()
     sz=size(maps.data)
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
         if ~lidx[j].IsEmpty() then   tasklist->add,{id:tasklist.count(),idx:reform(lidx[j]->ToArray()),t:task.t0+i,fmin:task.fmin,fmax:task.fmax,status:'pending',bridge:0l,duration:0d}
       endfor
     endfor
     obj_destroy,lidx
     pending=gsfitcp_where(/pending,count)
     message,string (count,format="('Tasks list updated: ',g0, ' pending tasks in the proceesing queue')"),/info
end

pro gsfitcp_flush_queue
 common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
 if n_elements(tasklist) eq 0 then tasklist=list()
 tasklist->Remove,/all
 pending=gsfitcp_where(/pending,count)
 message,string (count,format="('Tasks list updated: ',g0, ' pending tasks in the proceesing queue')"),/info
end

pro gsfitcp_close_bridges
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
   if obj_valid(bridges) then obj_destroy,bridges
end


pro gsfitcp_sendfittask,bridge,task
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  if size(maps,/tname) ne 'STRUCT' then return
  freqs=reform(maps[*,0,0].freq)
  times=reform(maps[0,0,*].time)
  time_idx=task.t
  data_size=size(maps.data)
  ij=array_indices(data_size[1:2],task.idx,/dim)
  npix=n_elements(task.idx)
  freq=double(freqs[task.fmin:task.fmax])
  nfreq=n_elements(freq)
  npol=(size(maps))[2]
  dx=(maps.dx)[0]
  dy=(maps.dy)[0]
  ;===========Define arrays for fitting. First - model, second -EOVSA

  spec_in=dblarr(npix,nfreq,4)
  spec_out=dblarr(npix,nfreq,2)
  ninput=cpinput.nparms.value
  ninput[2:3]=[npix,nfreq]

  rinput=cpinput.rparms.value
  rinput[3]=dx*dy

  rmsweight=cpinput.rms

  ParGuess=[[cpinput.parms_in.guess],[cpinput.parms_in.min],[cpinput.parms_in.max]]
  aparms=(eparms=dblarr(npix, n_elements(cpinput.parms_out)))
  
  flux_roi=reform(maps[task.fmin:task.fmax,*,task.t].data[ij[0,*],ij[1,*]],npix,nfreq,npol)
  err=reform((maps.rms)[task.fmin:task.fmax,*,task.t],nfreq,npol)

  if npol eq 1 then begin
    FOR i = 0, npix-1 DO BEGIN
      spec_in[i,*,0]=flux_roi[i,*,0]>0; only I STOKES fluxes
      spec_in[i,*,2]=err[*,0]+rmsweight*median(err)/freq; only I STOKES errors
      ;the second term attempts to account for the frequency-dependent spatial resolution
    ENDFOR
  endif else begin
    FOR i = 0, npix-1 DO BEGIN
      spec_in[i,*,0]=flux_roi[i,*,0]>0; only I STOKES fluxes
      spec_in[i,*,2]=err[*,0]+rmsweight*median(err[*,0])/freq; only I STOKES errors
      spec_in[i,*,1]=flux_roi[i,*,1]>0; only I STOKES fluxes
      spec_in[i,*,3]=err[*,1]+rmsweight*median(err[*,1])/freq; only I STOKES errors
    ENDFOR
  endelse
  ;===========End array definition============================================================

  ;==========Transfer to the bridge the arrays required for fitting==========


  bridge->SetVar,'start_time',systime(/seconds)
  bridge->SetVar,'task_id',task.id
  bridge->SetVar,'ninput',long(ninput)
  bridge->SetVar,'rinput',double(rinput)
  bridge->SetVar,'parguess',double(parguess)
  bridge->SetVar,'freq',double(freq)
  bridge->SetVar,'spec_in',double(spec_in)
  bridge->SetVar,'spec_out',double(spec_out)
  bridge->SetVar,'aparms',double(aparms)
  bridge->SetVar,'eparms',double(eparms)
  bridge->SetVar,'libname',cpinput.path
  id=bridge->GetVar('id')

  bridge->Execute,"res=call_external(libname, "+"'"+cpinput.get_function[1]+"'"+",ninput,rinput,parguess,freq,spec_in,aparms,eparms,spec_out, /d_value,/unload)",/nowait
  ;;==========update the queue;==========
  task.status='Active'
  task.bridge=id
  tasklist(task.id)=task
end

pro gsfitcp_start
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  if tasklist.IsEmpty() then begin
    message,'No tasks in queue, nothing to be done!',/info
    return
  endif
  pending=gsfitcp_where(/pending,task_count)
  if task_count eq 0 then begin
    message,'No pending tasks in queue, nothing to be done!',/info
    return
  endif
  
  if n_elements(lun) gt 0 then begin
   if lun gt 0 then begin
    free_lun, lun
   end
   lun=0 
  endif else lun=0
  
  allbridges=bridges->Get(/all,count=count)
  for i=0, count-1 do begin
    code=allbridges[i]->Status(error=error)
    if code ne 1 then freebridges=n_elements(freebridges) eq 0?allbridges[i]:[freebridges,allbridges[i]]
  end
  bridge_count=n_elements(freebridges)
  if bridge_count eq 0 then begin
    message,'All bridges are busy, tasks will be waiting in the queue!',/info
    return
  endif
  cp_start_time=systime(/seconds)
  for i=0,min([bridge_count,task_count])-1 do begin
    task=tasklist(pending[i])
    gsfitcp_sendfittask,freebridges[i],task
  endfor
  gsfitcp_status,/nobridges
end

pro gsfitcp_readframe, bridge,task
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  spec_in=bridge->GetVar('spec_in')
  spec_out=bridge->GetVar('spec_out')
  aparms=bridge->GetVar('aparms')
  eparms=bridge->GetVar('eparms')
  time_idx=task.t
  data_size=size(maps.data)
  xy=array_indices(data_size[1:2],task.idx,/dim)
  x=xy[0,*]
  y=xy[1,*]
  nfreq=data_size[3]
  npol=data_size[4]
  data_spectrum=(errdata_spectrum=(fit_spectrum=fltarr(nfreq,npol)))
  sz=size(aparms)
  npix=sz[1]
  nparms=sz[2]
  tags=strcompress(cpinput.parms_out.name,/rem)
  units=strcompress(cpinput.parms_out.unit,/rem)
  errtags='err'+tags
  errunits=units
  chisq=where(strupcase(tags) eq 'CHISQ' or strupcase(tags) eq 'CHI-2' or strupcase(tags) eq 'CHISQR',count)
  if count eq 1 then begin
    tags[chisq]='CHISQR'
    errtags[chisq]='Residual'
    errunits[chisq]='sfu'
  endif
  for j=0,npix-1 do begin
    data_spectrum[*]=0
    errdata_spectrum[*]=0
    fit_spectrum[*]=0
    if n_elements(npol) ne 1 then begin
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
      afit=create_struct(afit,tags[k],aparms[j,k],errtags[k],eparms[j,k])
    endfor
    new=(lun eq 0)
    if keyword_set(new) then begin
      header={freq:reform(maps[*,0,0].freq),time:reform(maps[0,0,*].time), refmap:maps[0,0,0],info:cpinput,units:units,errunits:errunits,parnames:tags,errparnames:errtags}
      default,cplog,'gsfitcp.log'
    endif
    MULTI_SAVE,lun,afit,file=cplog, header=header,new=new,xdr=xdr
  endfor
end


pro gsfitcp_status,quiet=quiet,nobridges=nobridges
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
    active=0
    if ~obj_valid(bridges) then begin
      if ~keyword_set(quiet) then message, 'No bridges alive, use IDL>gsfitcp,nbridges to create at least one.',/info
      return
    endif
    bridge_array=bridges->Get(/all,count=count)
    if count eq 0 then begin
      if ~keyword_set(quiet) then  message, 'No bridges alive, use IDL>gsfitcp,nbridges to create at least one.',/info
      return
    endif
    for i=0,count-1 do begin
      code=bridge_array[i]->Status(error=error)
      case code of
        0:status='Idle'
        1:status='Active'
        2:status='Completed'
        3:status='Error'
        4:status='Aborted'
        else:status='Unknown'
      endcase
      if ~(keyword_set(quiet) or keyword_set(nobridges)) then  message, strcompress('Bridge ID='+string(i+1)+' status: '+ status),/info
    end
     pending=gsfitcp_where(/pending,pending_count)
     completed=gsfitcp_where(/completed,completed_count)
     aborted=gsfitcp_where(/aborted,aborted_count)
     active=gsfitcp_where(/active,active_count)
     if ~keyword_set(quiet) then begin
      if obj_valid(tasklist) then begin
        count=tasklist->Count() 
      endif else begin
        count=0
      endelse
      if count eq 0 then message,'WARNING: No task list exists in memory! Use "IDL>gsfitcp, cpinput" to provide one.',/info
      if aborted_count gt 0 then message, strcompress(string(aborted_count,count,format="('ABORTED TASKS: ', g0, '/',  g0)")),/info
      if pending_count gt 0 then message, strcompress(string(pending_count,count,format="('PENDING TASKS: ', g0, '/',  g0)")),/info
      if active_count gt 0 then message, strcompress(string(active_count,count,format="('ACTIVE TASKS: ', g0, '/',  g0)")),/info
      if completed_count gt 0 then begin
        duration=0d
        npix=0l
        for i=0,completed_count-1 do begin
          npix+=n_elements((tasklist(completed[i])).idx)
        endfor
        duration=systime(/s)-cp_start_time
        message, strcompress(string(completed_count,count,npix,duration,format="('TOTAL: ', g0, '/' , g0, ' tasks (',g0,' pixels) completed in ', g0,' seconds')")),/info
      endif
     endif
end

pro gsfitcp_callback,status,error,bridge,userdata
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  bridge_id=bridge->GetVar('id')
  task_id=bridge->GetVar('task_id')
  start_time=bridge->GetVar('start_time')
  duration=systime(/seconds)-start_time
  if tasklist->IsEmpty() then return
  task=tasklist(task_id)
  if task_id ne task.id then message,'WARNING! Task ID mismatch- Data may be corrupted!!!', /info
  if status eq 4 then begin
    task.status='Aborted'
    task.duration=duration
    tasklist(task.id)=task
    abort=1
  endif else  begin
    gsfitcp_readframe, bridge, task
    task.status='Completed'
    task.duration=duration
    tasklist(task.id)=task
  endelse
  
  if ~keyword_set(cpquiet) then begin
    print,'________________________'
    message,strcompress(string(n_elements(task.idx),task.duration,format="(g0,' pixels fitted in ',g0,' seconds')")),/info
    gsfitcp_status,/nobridges
    print,'________________________'
  end
  
  pending=gsfitcp_where(/pending,task_count)
  if task_count gt 0 and ~keyword_set(abort) then begin
    task=tasklist(pending[0])
    gsfitcp_sendfittask,bridge,task
  endif else  begin
    completed=gsfitcp_where(/completed,/aborted,ccount)
    if ccount eq tasklist->Count() then begin
      if lun gt 0 then free_lun,lun
      message, 'All tasks have been processed and results looged to ' +cplog+'. Use IDL>map=gsfit_log2map("'+cplog+'") to retrieve fit results.',/info
      gsfitcp_flush_queue
    endif
  endelse
end

pro gsfitcp_abort
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  allbridges=bridges->Get(/all,count=count)
  for i=0,count-1 do begin
    code=allbridges[i]->Status(error=error)
    active=0
    if code eq 1 then begin
      active+=1
      message,strcompress(string(id,format="('Working on aborting the active task on Bridge # ', g0,', please wait....')")),/info
      allbridges[i]->Abort
      id=allbridges[i]->GetVar('id')
      message,strcompress(string(id,format="('Bridge # ', g0,' execution aborted on user request!')")),/info
    endif
  end
  if active eq 0 then message, 'No active tasks, nothing to be aborted!',/info else $
  message,'All active tasks aborted on user request!',/info
  gsfitcp_flush_queue
  message,'All pending tasks have been deleted from memory on user request!',/info
  gsfitcp_status
end


pro gsfitcp,cp_input,nbridges,start=start,status=status,out=out,quiet=quiet,abort=abort,flush=flush,exit=exit,_extra=_extra
  common cpblock, lun, bridges, tasklist, maps, cpinput, cp_start_time, cplog, cpquiet
  if n_elements(quiet) gt 0 then begin
    cpquiet=keyword_set(quiet)
    case cpquiet of
      0: message,'Run-time progress report messages turned on! Use "IDL> gsfitcp,/quiet" to turn them off.',/info
      else:  message,'Run-time progress report messages turned off! Use "IDL> gsfitcp, quiet=0" to turn them on.',/info
    endcase
  endif
  if keyword_set(status) then begin
    gsfitcp_status
    return
  endif
  if keyword_set(flush) then begin
    gsfitcp_flush_queue
    return
  endif
  if keyword_set(abort) then begin
    gsfitcp_abort
    return
  endif
  if keyword_set(exit) then begin
    gsfitcp_flush_queue
    obj_destroy,bridges
    if n_elements(lun) gt 0 then if lun gt 0 then free_lun,lun
    dummy=temporary(cplog)
    dummy=temporary(tasklist)
    dummy=temporary(maps)
    dummy=temporary(cpinput)
    dummy=temporary(cpquiet)
    return
  endif
  if N_PARAMS() eq 0 and n_elements(quiet) eq 0 then begin
    message,'For a detailed description visit http://www.ovsa.njit.edu/wiki/index.php/GSFITCP_Help, or use one of the folowing calling sequences:',/info
    print,'% IDL-> gsfitcp, taskfilename; to provide a path to a stored GSFIT task structure
    print,'% IDL-> gsfitcp, taskstructure; to provide an already restored GSFIT task structure
    print,'% IDL-> gsfitcp, nthreads; to set,increase, or decrease the number of ashyncronious threads to be used
    print,'% IDL-> gsfitcp, out=out; to change the default "gsfitcp.log" path for run-time logging of the results.
    print,'% IDL-> gsfitcp, /status; to report the status of the application
    print,'% IDL-> gsfitcp, /quiet; to inhibit automatic printing of progress report messages at run-time
    print,'% IDL-> gsfitcp, /start; to start processing the task queue
    print,'% IDL-> gsfitcp, /flush; to flush the pending task queue
    print,'% IDL-> gsfitcp, /abort; to abort all active tasks and flush the pending task queue
    print,'% IDL-> gsfitcp, /exit; to abort all active tasks, flush the pending task queue, and exit the application
    message,'% Any logical combination of the arguments and keywords listed above should result in a valid single-line calling sequence',/info
  endif

  if is_number(cp_input) then nbridges=temporary(cp_input)
  if is_number(nbridges) or n_elements(bridges) eq 0 then gsfitcp_set_bridges, nbridges,_extra=_extra
  if size(out,/tname) eq 'STRING' then begin
     if n_elements(lun) gt 0 then begin
      if lun gt 0 then begin
       free_lun, lun
       message,"Active "+cplog+" has been closed at user's request and replaced by "+ cplog ,/info
      end
      lun=0 
      cplog=out
  endif else lun=0
  endif

;CP_INPUT ARGUMENT PROVIDED  SECTION
  if n_elements(cp_input) gt 0 then begin
        case size(cp_input,/tname) of
            'STRING': begin
                       if file_exist(cp_input) then begin
                        osav=obj_new('idl_savefile',cp_input)
                         names=osav->names()
                         valid=0
                         for i=0,n_elements(names)-1 do begin
                           osav->restore,names[i]
                           e=execute('result=size('+names[i]+',/tname)')
                           if (result eq 'STRUCT') then begin
                             e=execute('cp_input=temporary('+names[i]+')')
                           endif
                         endfor
                         obj_destroy,osav
                        if size(cp_input,/tname) ne 'STRUCT' then begin
                          message,'unexpected file content',/info
                          return
                        endif else goto,cpstruct
                       endif else begin
                        message,cp_input+': the task fileme argument provided is not avalid path on thismachine',/info
                        return
                       endelse
                      end
            'STRUCT': begin
                        cpstruct:
                        tnames=tag_names(cp_input)
                        enames=['NPARMS','RPARMS','PARMS_IN','PARMS_OUT','PATH','RMS','DATAPATH','TASK']
                        tnames=tnames[sort(tnames)]
                        enames=enames[sort(enames)]
                        cpvalid=tag_exist(cp_input, 'NPARMS') and $
                                tag_exist(cp_input, 'RPARMS') and $
                                tag_exist(cp_input, 'PARMS_IN') and $
                                tag_exist(cp_input, 'PARMS_OUT') and $
                                tag_exist(cp_input, 'DATAPATH') and $
                                tag_exist(cp_input, 'GET_FUNCTION')
                        if ~cpvalid then begin
                          message,'provided cp_input argument is not a valid gsfit command prompt input structure',/info
                          return  
                        endif else cpinput=cp_input   
                      end     
            else: begin 
                    message,'No valid cp_input argument provided',/info
                    return
                  end  
          endcase
          if ~file_exist(cpinput.datapath) then begin
            message,'No valid data file path provided! Plese manually select a valid data file to cntinue',/info
            cpinput.datapath=dialog_pickfile(filter='*.sav',title='Please select a filename containing valid GSFIT data input structure')
            if ~file_exist(cpinput.datapath) then begin
              message,['The attempt to replace the data path failed. Sorry, I cannot continue from here!',' Please manually fix the datapath tag in the cpinput structure and try again.'],/info
              return
            endif
          endif
          gsfitcp_set_maps, cpinput.datapath 
          if size(cpinput.task,/tname) eq 'STRUCT' then gsfitcp_add_task, cpinput.task,/remove
  end
;END OF CP_INPUT ARGUMENT PROVIDED SECTION
    
  if keyword_set(start) then begin
    if size(cpinput,/tname) eq 'STRUCT' then gsfitcp_start else message,'No cpinput string or structure argument provided. Please use "gsfitcp, cpinput" to provide one.',/info
  endif
end