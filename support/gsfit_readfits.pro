;+
; NAME:
;   gsfit_readfits
; PURPOSE:
;   Import a sequence of EOVSA FITS files as a GSFIT map cube.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   maps = gsfit_readfits([files] [, _extra=extra])
; INPUTS:
;   FILES: one file name or an array of FITS file names. A multiple-file dialog is opened when FILES is omitted.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   _EXTRA: passed to eovsa_fits2map. See that routine for SFU, NO_IV2RL, FLUX_THRESHOLD, RMS_MASK, and NO_RMS.
; OUTPUTS:
;   MAPS: the map cube returned by eovsa_fits2map, or !NULL when the dialog is cancelled.
; ROUTINES CALLED:
;   eovsa_fits2map, dialog_pickfile
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   Opens a file dialog when FILES is omitted.
; RESTRICTIONS:
;   None beyond those of eovsa_fits2map.
; MODIFICATION HISTORY:
;   Written 13-Nov-2019 Gelu M. Nita (gnita@njit.edu)
;   22-Nov-2020 GN: Updated while adding EOVSA FITS import.
;   12-Feb-2021 GN: Fixed selection of FITS files that share a timestamp.
;-

function gsfit_readfits,files,_extra=_extra
 if n_elements(files) eq 0 then files=dialog_pickfile(filter='*.fit*',title='Select a sequence of EOVSA fits files to import into GSFIT',/read,/multiple)
 if files[0] eq '' then return,!null
 return,eovsa_fits2map(files,_extra=_extra)
end