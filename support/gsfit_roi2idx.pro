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