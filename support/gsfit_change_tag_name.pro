;+
; NAME:
;   gsfit_change_tag_name
; PURPOSE:
;   Rename one tag of a structure, keeping the value that was stored under the old name.
; CATEGORY:
;   GSFIT
; CALLING SEQUENCE:
;   gsfit_change_tag_name, struct, old_tag_name, new_tag_name [, found=found]
; INPUTS:
;   STRUCT: structure or structure array, updated in place.
;   OLD_TAG_NAME: existing tag name.
;   NEW_TAG_NAME: name to give that tag.
; OPTIONAL (KEYWORD) INPUT PARAMETERS:
;   FOUND: named variable that receives 1 when the old tag existed and 0 otherwise.
; OUTPUTS:
;   STRUCT is returned through its argument. When the old name is absent, STRUCT is unchanged and a message is printed.
; ROUTINES CALLED:
;   tag_exist, rem_tag, add_tag
; COMMENTS:
;   None.
; SIDE EFFECTS:
;   None.
; RESTRICTIONS:
;   Renames one top-level tag. The new name must not already belong to the structure.
; MODIFICATION HISTORY:
;   Written 8-Oct-2019 Gelu M. Nita (gnita@njit.edu)
;   2-Nov-2022 GN: Updated run-time error handling.
;-

pro gsfit_change_tag_name,struct,old_tag_name,new_tag_name,found=found
found=tag_exist(struct,old_tag_name,index=i)
if found then begin
  tmp=struct.(i)
  struct=rem_tag(struct,old_tag_name)
  struct=add_tag(struct,tmp,new_tag_name)
endif else message,strupcase(old_tag_name)+': no such tag found, nothing to replace!',/info
end