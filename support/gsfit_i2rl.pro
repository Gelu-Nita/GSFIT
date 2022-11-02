function gsfit_i2rl,maps,_extra=_extra
if size(maps,/tname) ne 'STRUCT' then begin
  message,'Expected maps structure argument missing!',/info
  return,!null
endif
if ~tag_exist(maps,'datamaps') then begin
  message,'Expected DATAMAPS tag missing!',/info
  return,!null
endif

dummy_map=maps.datamaps[0]
if ~valid_map(dummy_map) then begin
  message,'The input argument does not contain any valid map structure!',/info
  return,!null
endif
dimensions=['Freq','Pol','Time']
if ~tag_exist(dummy_map,'stokes')then begin
  message,'Expected STOKES tag missing!',/info
  return,!null
endif
if ~tag_exist(dummy_map,'freq')then begin
  message,'Expected FREQ tag missing!',/info
  return,!null
endif
if ~tag_exist(dummy_map,'time')then begin
  message,'Expected TIME tag missing!',/info
  return,!null
endif
dummy_map.id='GSFIT Flux'


add_prop,dummy_map,dimensions=dimensions,/replace
add_prop,dummy_map,datatype='Flux',/replace
add_prop,dummy_map,dataunits='sfu',/replace
has_rms=tag_exist(dummy_map,'rms')
if ~has_rms then add_prop,dummy_map,rms=0.0
dummy_map.rmsunits='sfu'
sz=size(maps.datamaps)
case sz[0] of 
  1: begin
      nfreq=sz[1]
      npol=2
      ntime=1
     end
  2: begin
       nfreq=sz[1]
       npol=2
       ntime=sz[2]
     end
  3: begin
   nfreq=sz[1]
   npol=2
   ntime=sz[3]
  end  
  else: begin
         message,'Unxpected number of dimensions for the DATAMAPS tag!',/info
         return,!null
        end 
endcase
rlmaps=reform(replicate(dummy_map,nfreq,npol,ntime),nfreq,npol,ntime)
rlmaps[*,0,*].stokes='LL'
rlmaps[*,1,*].stokes='RR'
rlmaps[*,0,*].time=reform(maps.datamaps.time,nfreq,1,ntime)
rlmaps[*,1,*].time=reform(maps.datamaps.time,nfreq,1,ntime)
rlmaps.freq=array_replicate(maps.header.freq,2,ntime)
fastcode=gsfit_fastcode(/arr)
parms=double(fastcode.parms.value)
names=strupcase(strcompress(fastcode.parms.name,/rem))
pnames=tag_names(maps)
hnames=strcompress(maps.header.info.parms_in.name,/rem)
pmatch={T_0:{parmsidx:0l,fitidx:0l,guess:1d},EMAX:{parmsidx:0l,fitidx:0l,guess:1d},DELTA1:{parmsidx:0l,fitidx:0l,guess:1d},$
  N_0:{parmsidx:0l,fitidx:0l,guess:1d},N_B:{parmsidx:0l,fitidx:0l,guess:1d},B:{parmsidx:0l,fitidx:0l,guess:1d},THETA:{parmsidx:0l,fitidx:0l,guess:1d}}
for i=0,n_elements(names)-1 do begin
  case names[i] of
    'T_0':begin
           pmatch.T_0.parmsidx=i
           pmatch.T_0.fitidx=where(pnames eq 'T_E')
           pmatch.T_0.guess=(maps.header.info.parms_in.guess)[where(hnames eq 'T_e')]*1e6
          end 
    'EMAX':begin
           pmatch.EMAX.parmsidx=i
           pmatch.EMAX.fitidx=where(pnames eq 'EMAX')
           pmatch.EMAX.guess=(maps.header.info.parms_in.guess)[where(hnames eq 'E_max')]
          end 
    'DELTA1':begin
           pmatch.DELTA1.parmsidx=i
           pmatch.DELTA1.fitidx=where(pnames eq 'DELTA')
           pmatch.DELTA1.guess=(maps.header.info.parms_in.guess)[where(hnames eq 'Delta')]
          end 
    'N_0':begin
           pmatch.N_0.parmsidx=i
           pmatch.N_0.fitidx=where(pnames eq 'NTH')
           pmatch.N_0.guess=(maps.header.info.parms_in.guess)[where(hnames eq 'n_th')]*1d9
          end 
    'N_B':begin
           pmatch.N_B.parmsidx=i
           pmatch.N_B.fitidx=where(pnames eq 'N_NTH')
           pmatch.N_B.guess=(maps.header.info.parms_in.guess)[where(hnames eq 'n_nth')]*1d6
          end
    'B':begin
           pmatch.B.parmsidx=i
           pmatch.B.fitidx=where(pnames eq 'B')
           pmatch.B.guess=(maps.header.info.parms_in.guess)[where(hnames eq 'B')]*100
          end
    'THETA':begin
             pmatch.THETA.parmsidx=i
             pmatch.THETA.fitidx=where(pnames eq 'THETA')
             pmatch.THETA.guess=(maps.header.info.parms_in.guess)[where(hnames eq 'theta')]
          end
    else:
  endcase
endfor
freqlist=maps.header.freq
dx=maps[0].datamaps.dx
dy=maps[0].datamaps.dy
dim=size(rlmaps,/dim)
;compute sfu2Tb conversion factor
coeff= 1.4568525d026/((dx*dy)*(7.27d7)^2)/freqlist^2
coeff/=2; Division by two is needed to comply with the
; EOVSA map convention that assumes I=(XX+YY)/2=(RR_LL)/2
coeff_arr = reform(replicate(1,sz[1]*sz[2])#coeff,sz[1],sz[2],dim[0])
fastcode.pixdim[0]=Nfreq
rparms=fastcode.rparms.value
rparms[where(strcompress(fastcode.rparms.name,/rem) eq 'f_min')]=0
nparms=fastcode.nparms.value
sz=size(maps.b[0].data)
Nx=sz[1]
Ny=sz[2]
Npix=Nx*Ny
Nvox=1L
parms=transpose(reform(double(array_replicate(fastcode.parms.value, npix,nvox)),n_elements(fastcode.parms.value),npix,nvox),[1,2,0])
rnames=strupcase(strcompress((maps.header.info.rparms.name),/rem))
names=strupcase(strcompress(fastcode.parms.name,/rem))
rinput=maps.header.info.rparms.value
arcsec2cm=7.27d7
rparms[where(strcompress(fastcode.rparms.name,/rem) eq 'dS')]=rinput[where(rnames eq 'PIXELAREA')]*(arcsec2cm^2)
parms[*,0,where(names eq 'DR')]=rinput[where(rnames eq 'LOSDEPTH')]*arcsec2cm
parms[*,0,where(names eq 'EMIN')]=rinput[where(rnames eq 'E_MIN')]
rowdata=make_array([npix,fastcode.pixdim],/float)
for t=0, ntime-1 do begin
 t0=systime(/s)
 message,string(t+1,ntime,format="('Computing time frame #',i0,' out of ',i0)"),/info
 for k=0,n_tags(pmatch)-1 do begin
  if pmatch.(k).fitidx ne -1 then begin
   parms[*,0,pmatch.(k).parmsidx]=(maps.(pmatch.(k).fitidx).data)[*,*,t]
  endif else begin
    parms[*,0,pmatch.(k).parmsidx]=pmatch.(k).guess
  endelse
 endfor
 res=execute(fastcode.execute)
 rlmaps[*,0,t].data=reform(rowdata[*,*,0,0],Nx,Ny,Nfreq)
 rlmaps[*,1,t].data=reform(rowdata[*,*,1,0],Nx,Ny,NFreq)
 message,string(systime(/s)-t0,format="('Time frame computation performed in ',f5.2,' seconds')"),/info
end
res=maps.datamaps.data-total(rlmaps.data,4)
rlmaps[*,0,*].data+=reform(res/2,nx,ny,nfreq,1,ntime)
rlmaps[*,1,*].data+=reform(res/2,nx,ny,nfreq,1,ntime)
rms=sqrt(total(total(res^2,1),1)/Nx/Ny)
rlmaps[*,0,*].rms=reform(rms,dim[0],1,dim[2])
rlmaps[*,1,*].rms=reform(rms,dim[0],1,dim[2])
rlmaps.data*=2
return,eovsa_maps2gsfit(rlmaps,_extra=_extra)
end