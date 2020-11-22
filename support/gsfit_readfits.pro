function gsfit_readfits,files,_extra=_extra
 if n_elements(files) eq 0 then files=dialog_pickfile(filter='*.fit*',title='Select a sequence of EOVSA fits files to import into GSFIT',/read,/multiple)
 if files[0] eq '' then return,!null
 return,eovsa_fits2cube(files,_extra=_extra)
end