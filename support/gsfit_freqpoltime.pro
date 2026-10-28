;+
; NAME:
;   gsfit_freqpoltime
; PURPOSE:
;   Force a map-structure array to dimensions [frequency, polarization, time] and record that order.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   maps = gsfit_freqpoltime(maps [, nfreq=nfreq, npol=npol, ntime=ntime])
; INPUTS:
;   MAPS: a valid map structure or a map-structure array.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   NFREQ, NPOL, NTIME: axis lengths to use when their product equals the number of maps. gsfit_log2map passes the header lengths. When they are omitted, the dimensions tag is used. A tag longer than the array means trailing axes of length 1 were dropped and are inserted again. With no tag, or with a tag that is not Freq, Pol, and Time in that order, a 1-D array is frequency and a 2-D array is frequency by time.
;   OK: 1 when the array was arranged as [Freq, Pol, Time], 0 when the input is returned unchanged.
; OUTPUTS:
;   MAPS: the same maps reformed to [NFREQ, NPOL, NTIME], with DIMENSIONS=['Freq','Pol','Time'] stored on every element. The input is returned unchanged, and OK is 0, when it is not a valid map or the element count does not match.
; ROUTINES CALLED:
;   valid_map, add_tag, rem_tag
; COMMENTS:
;   Called by gsfit_readtb, at the end of gsfit_log2map, and at the start of the gsfitview cube open path.
; SIDE EFFECTS:
;   None. The result is a new array. Trailing axes of length 1 are kept on the returned array; storing that array in a structure field may drop them again, while the dimensions tag still names them.
; RESTRICTIONS:
;   Parameter maps, which are one plane per time, must not be passed to this routine. It is for FITMAPS, DATAMAPS, ERRMAPS, and input image cubes.
; MODIFICATION HISTORY:
;   Written 22-Oct-2026 Gelu M. Nita (gnita@njit.edu)
;   28-Oct-2026 GN: An unreadable dimensions tag uses the same axis order as a missing tag. OK reports whether the array was arranged.
;-

function gsfit_freqpoltime, maps, nfreq=nfreq_in, npol=npol_in, ntime=ntime_in, ok=ok
  ok = 0b
  if size(maps, /tname) ne 'STRUCT' then return, maps
  if ~valid_map(maps) then return, maps

  canon = ['Freq', 'Pol', 'Time']
  rank = size(maps, /n_dimensions)
  if rank eq 0 then dim = [1L] else dim = long(size(maps, /dimensions))
  nmaps = n_elements(maps)

  nfreq = 1L
  npol = 1L
  ntime = 1L
  use_explicit = n_elements(nfreq_in) eq 1 && n_elements(npol_in) eq 1 && n_elements(ntime_in) eq 1
  if use_explicit then use_explicit = long(nfreq_in) * long(npol_in) * long(ntime_in) eq nmaps
  arranged = 0b
  if use_explicit then begin
    nfreq = long(nfreq_in)
    npol = long(npol_in)
    ntime = long(ntime_in)
    arranged = 1b
  endif else if tag_exist(maps, 'dimensions') && rank lt 3 then begin
    names = strupcase(strtrim(maps[0].dimensions, 2))
    ; < and > are minimum and maximum.
    nuse = n_elements(names) < rank
    slots = intarr(nuse > 1)
    known = nuse ge 1
    for i = 0, nuse-1 do begin
      hit = where(names[i] eq ['FREQ', 'POL', 'TIME'], nfound)
      if nfound ne 1 then known = 0b else slots[i] = hit[0]
    endfor
    if known && nuse gt 1 then begin
      for i = 1, nuse-1 do if slots[i] le slots[i-1] then known = 0b
    endif
    if known then begin
      for i = 0, nuse-1 do begin
        case slots[i] of
          0: nfreq = dim[i]
          1: npol = dim[i]
          2: ntime = dim[i]
        endcase
      endfor
      arranged = 1b
    endif else message, 'Dimensions tag is not Freq, Pol, Time in that order. Using the default axis order.', /info
  endif
  if ~arranged then begin
    case rank of
      0:
      1: nfreq = dim[0]
      2: begin
        nfreq = dim[0]
        ntime = dim[1]
      end
      else: begin
        nfreq = dim[0]
        npol = dim[1]
        ntime = dim[2]
      end
    endcase
  endif

  if (nfreq * npol * ntime) ne nmaps then begin
    message, 'Map array cannot be arranged as [Freq, Pol, Time], operation aborted!', /info
    return, maps
  endif

  ; Reform into a new variable. Assigning the reform back to the
  ; argument and returning that argument hands back the original
  ; dimensions, and a trailing axis of length 1 is lost.
  out = reform(maps, nfreq, npol, ntime)

  if tag_exist(out, 'dimensions') then begin
    current = strupcase(strtrim(out[0].dimensions, 2))
    if n_elements(current) eq 3 then begin
      if array_equal(current, strupcase(canon)) then begin
        if size(out, /n_dimensions) eq 3 then begin
          ok = 1b
          return, out
        endif
      endif
    endif
    out = rem_tag(out, 'dimensions')
  endif
  ; add_tag rebuilds the array and drops trailing axes of length 1.
  out = add_tag(out, canon, 'dimensions', /no_pair)
  if size(out, /n_dimensions) ne 3 then out = reform(out, nfreq, npol, ntime)
  ok = 1b
  return, out
end
