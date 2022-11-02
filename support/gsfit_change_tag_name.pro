pro gsfit_change_tag_name,struct,old_tag_name,new_tag_name,found=found
found=tag_exist(struct,old_tag_name,index=i)
if found then begin
  tmp=struct.(i)
  struct=rem_tag(struct,old_tag_name)
  struct=add_tag(struct,tmp,new_tag_name)
endif else message,strupcase(old_tag_name)+': no such tag found, nothing to replace!',/info
end