;+
; NAME:
;   gsfit_spectralpeak_computation
; PURPOSE:
;   Add peak-flux and peak-frequency maps by interpolating the fitted spectra.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   gsfit_spectralpeak_computation, maps
; INPUTS:
;   MAPS: fit-map structure with HEADER.FREQ and the fitted flux in FITMAPS. When PEAKFREQ already exists, the routine returns without changing MAPS.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   None.
; OUTPUTS:
;   MAPS is updated in place with PEAKFLUX and ERRPEAKFLUX, in sfu, and PEAKFREQ and ERRPEAKFREQ, in GHz.
; ROUTINES CALLED:
;   None.
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   None.
; RESTRICTIONS:
;   MAPS.HEADER must exist and must contain the frequency list. The frequency step stored as the peak-frequency error is the maximum separation of the header frequencies.
; MODIFICATION HISTORY:
;   Written 27-May-2019 Gelu M. Nita (gnita@njit.edu)
;   8-Oct-2019 GN: Incremental upgrade.
;   2-Nov-2022 GN: Updated run-time error handling.
;-

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
