pro gsfit_spectralpeak_computation,maps
if ~tag_exist(maps,'header') then begin
  message,'Header structure expected to contain the frequency list is missing from the map structure!',/info
  return
endif

if ~tag_exist(maps,'peakfreq') then begin
  npoints=10*n_elements(maps.header.freq)
  freq=interpol(maps.header.freq,npoints)
  errfreq=max(maps.header.freq-shift(maps.header.freq,1))
  sz=size(maps.b.data)
  nx=sz[1]
  ny=sz[2]
  nt=sz[3]
  dummy_map=maps.b
  dummy_map.data[*]=0
  maps=rep_tag_value(maps,dummy_map,'peakflux')
  maps.peakflux.id[*]='peakflux'
  maps.peakflux.dataunits[*]='sfu'
  maps=rep_tag_value(maps,dummy_map,'peakfreq')
  maps.peakfreq.id[*]='peakfreq'
  maps.peakflux.dataunits[*]='GHz'
  maps=rep_tag_value(maps,dummy_map,'errpeakflux')
  maps.errpeakflux.id[*]='errpeakflux'
  maps.errpeakflux.dataunits[*]='sfu'
  maps=rep_tag_value(maps,dummy_map,'errpeakfreq')
  maps.errpeakfreq.id[*]='errpeakfreq'
  maps.errpeakfreq.dataunits[*]='sfu'
  if ~tag_exist(maps.header,'peakflux') then begin
    header=maps.header
    header=rep_tag_value(header,[header.parnames,'peakflux','peakfreq'],'parnames')
    header=rep_tag_value(header,[header.errparnames,'errpeakflux','errpeakfreq'],'errparnames')
    maps=rep_tag_value(maps,header,'header')
  end
  for i=0,nt-1 do begin
    for j=0, nx-1 do begin
      for k=0, ny-1 do begin
        if max(maps.fitmaps[*,i].data[j,k]) gt 0 then begin
          maps.peakflux[i].data[j,k]=max(interpol(maps.fitmaps[*,i].data[j,k],header.freq,freq),imax)
          residual=(total((maps.fitmaps[*,i].data[j,k]-maps.datamaps[*,i].data[j,k])^2,/double,/nan))
          maps.errpeakflux[i].data[j,k]=sqrt(residual/n_elements(header.freq))
          maps.peakfreq[i].data[j,k]=freq[imax]
          maps.errpeakfreq[i].data[j,k]=errfreq
        end
      endfor
    endfor
  endfor
endif
end
