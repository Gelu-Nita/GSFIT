pro gsfit_energy_computation,maps,header,emin=emin
ds=(header.info.rparms.value)[3]
dl=(header.info.rparms.value)[4]
default,emin,(header.info.rparms.value)[5]*1000
vol=ds*dl*(7.27d7)^3
WB=vol*maps.b.data^2/8./!pi ; B energy density from a pixel
errWB=vol*2*maps.b.data*maps.errb.data/8./!pi
del=maps.delta.data
Wnth=vol*1.6d-9*emin*(del-1)/(del-2)*maps.n_nth.data
errWnth=vol*1.6d-9*emin*sqrt(maps.errdelta.data^2*maps.n_nth.data^2/(del-2)^4+((del-1)/(del-2)*maps.errn_nth.data)^2)
;Wnth=vol*1.6d-9*emin*(del-1)/(del-2)*maps.nrl.data
;errWnth=vol*1.6d-9*emin*sqrt(maps.errdelta.data^2*maps.nrl.data^2/(del-2)^4+((del-1)/(del-2)*maps.errnrl.data)^2)
if ~tag_exist(maps,'WB') or ~tag_exist(maps,'Wnth') then begin
  WBmap=maps.b
  WBmap.id='WB'
  WBmap.dataunits='erg'
  WBmap.data=WB
  maps=create_struct(maps,'WB',temporary(WBmap))
  errWBmap=maps.b
  errWBmap.id='errWB'
  errWBmap.dataunits='erg'
  errwBmap.data=errWB
  maps=create_struct(maps,'errWB',temporary(errWBmap))
 
  Wnthmap=maps.b
  Wnthmap.id='Wnth'
  Wnthmap.dataunits='erg'
  Wnthmap.data=Wnth
  maps=create_struct(maps,'Wnth',temporary(Wnthmap))
  errWnthmap=maps.b
  errWnthmap.id='errWnth'
  errWnthmap.dataunits='erg'
  errwnthmap.data=errWnth
  maps=create_struct(maps,'errWnth',temporary(errWnthmap))
  
endif else begin
  maps.WB.data=WB
  maps.errWB.data=errWB
  maps.Wnth.data=Wnth
  maps.errWnth.data=errWnth
endelse

end
