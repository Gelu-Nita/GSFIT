;+
; NAME:
;   gsfit_roi2idx
; PURPOSE:
;   Convert a polygon drawn in map coordinates to pixel indexes inside that map.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   ij = gsfit_roi2idx(map, xroi, yroi [, idx=idx])
; INPUTS:
;   MAP: map structure whose data array defines the pixel grid.
;   XROI, YROI: vertices of the polygon in the map's coordinate units.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   IDX: named variable that receives the one-dimensional indexes returned by POLYFILLV.
; OUTPUTS:
;   IJ: 2 by N array of column and row indexes for the pixels inside the polygon.
; ROUTINES CALLED:
;   get_map_coord, polyfillv
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   None.
; RESTRICTIONS:
;   XROI and YROI must be in the same coordinate system as MAP.XC, MAP.YC, MAP.DX, and MAP.DY.
; MODIFICATION HISTORY:
;   Written 17-Sep-2020 Gelu M. Nita (gnita@njit.edu)
;   22-Nov-2020 GN: Updated while adding EOVSA FITS import.
;-

function gsfit_roi2idx,map,xroi,yroi,idx=idx
 get_map_coord,map,xpix,ypix
 xpix=xpix[*,0]
 ypix=ypix[0,*]
 sz=size(map.data)
 nx=sz[1]
 ny=sz[2]
 idx=polyfillv(floor((xroi-xpix[0])/(xpix[1]-xpix[0])),floor((yroi-ypix[0])/(ypix[1]-ypix[0])),nx,ny)
 ij=array_indices([nx,ny],idx,/dim)
 return,ij
end