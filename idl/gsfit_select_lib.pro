FUNCTION gsfit_select_lib, UPDATE=update, SELECT=select
  COMPILE_OPT IDL2
  ;Check the OS
  isWin=strupcase(!version.os_family) eq strupcase('Windows')
  
  ;--------------------------------------
  ; Resolve external root from GSFITPATH
  ;--------------------------------------
  base = GETENV('gsfitpath')
  IF base EQ '' THEN BEGIN
    MESSAGE, 'GSFIT_SELECT_LIB: GSFITPATH is not defined.', /INFO
    RETURN, !NULL
  ENDIF

  ; If GSFITPATH points to .../gsfit, use .../gsfit/external.
  ; If it already points to .../gsfit/external, external_root will be that.
  external_root = FILEPATH('external', ROOT = base)
  IF ~FILE_TEST(external_root, /DIRECTORY) THEN external_root = base

  sep = PATH_SEP()

  ;--------------------------------------
  ; Find all entries directly under external_root,
  ; then filter to directories only (first level only).
  ;--------------------------------------
  all = FILE_SEARCH(external_root + sep + '*')

  IF (N_ELEMENTS(all) EQ 1) AND (all[0] EQ '') THEN BEGIN
    MESSAGE, 'GSFIT_SELECT_LIB: no entries found under ' + external_root, /INFO
    RETURN, !NULL
  ENDIF

  ; Build mask of which entries are directories
  mask = BYTARR(N_ELEMENTS(all))
  FOR i = 0, N_ELEMENTS(all)-1 DO BEGIN
    IF FILE_TEST(all[i], /DIRECTORY) THEN mask[i] = 1B ELSE mask[i] = 0B
  ENDFOR

  idx = WHERE(mask EQ 1B, n_dirs)
  IF n_dirs LE 0 THEN BEGIN
    MESSAGE, 'GSFIT_SELECT_LIB: no subdirectories found under ' + external_root, /INFO
    RETURN, !NULL
  ENDIF

  dirs = all[idx]

  ;--------------------------------------
  ; Folder names to display (sorted)
  ;--------------------------------------
  labels = STRARR(n_dirs)
  FOR i = 0, n_dirs-1 DO labels[i] = FILE_BASENAME(dirs[i])

  sort_idx = SORT(labels)
  labels   = labels[sort_idx]
  dirs     = dirs[sort_idx]

  ;--------------------------------------
  ; Preselected list index from SELECT keyword
  ;--------------------------------------
  sel = 0L
  IF N_ELEMENTS(select) GT 0 THEN sel = LONG(select)
  IF sel LT 0 THEN sel = 0L
  IF sel GE n_dirs THEN sel = n_dirs - 1L

  ; Build the item list string for the LIST field: "a|b|c|..."
  items = labels[0]
  FOR i = 1, n_dirs-1 DO items = items + '|' + labels[i]
  if isWin then items=str_rep(items,'transfer','')
  ;--------------------------------------
  ; Initial states for UPDATE 
  ;--------------------------------------
  upd_set  = KEYWORD_SET(update) ? 'SET_VALUE=1' : 'SET_VALUE=0'

  ;--------------------------------------
  ; Describe the CW_FORM layout
  ;--------------------------------------
  desc = []

  desc = [desc,'0, LABEL, Select one of GSFIT external libraries listed below, LEFT']
  desc = [desc,'0, LIST, ' + items + ', TAG=CHOICE, SET_VALUE=' + STRTRIM(sel, 2)]

  ; Row of two checkboxes
  if ~isWin then begin
    desc = [desc, '1, BASE,, ROW']
    desc=[desc, '2, BUTTON, Use Existent|Update library, EXCLUSIVE, ROW, TAG=update, '+upd_set]
  end

  ; Row with OK / Cancel
  desc = [desc, '1, BASE,, ROW']
  desc = [desc, '0, BUTTON, Use Selected, QUIT, TAG=OK']
  desc = [desc, '0, BUTTON, Select local repository, QUIT, TAG=LOCAL']
  desc = [desc, '2, BUTTON, Cancel, QUIT, TAG=CANCEL']

  ;--------------------------------------
  ; Create a modal top-level form.
  ; CW_FORM returns a structure with tags:
  ; CHOICE (int), UPDATE (int), UNIX (int), OK, CANCEL, ...
  ;--------------------------------------
  form = CW_FORM(desc, /COLUMN, TITLE='Select GSFIT External Library')


  
  CASE 1 OF
    form.OK: BEGIN
              ; CHOICE is the zero-based index of the selected list item
              choice = form.CHOICE
              root = labels[choice]
             END
    form.LOCAL: BEGIN
                root=dialog_pickfile(Title='SELECT A LOCAL REPOSITORY ROOT CONTAININGG A GSFIT LIBRARY OR SOURCE CODE',/dir)
                IF root EQ ''  THEN RETURN, !NULL 
                full_path=1
               END         
    ELSE:RETURN, !NULL
  ENDCASE

  ; Final checkbox states (override initial keyword suggestions)
  do_update = tag_exist(form,'update')?form.update[0]:0
  
 
  lib = gsfit_libpath(root,update=do_update, full=full_path)

  ;--------------------------------------
  ; Validate result
  ;--------------------------------------
  IF N_ELEMENTS(lib) EQ 0 THEN BEGIN
    answ = DIALOG_MESSAGE('"' + root + '" is not a valid selection for this operating system!')
    RETURN, !NULL
  ENDIF ELSE RETURN, lib[0]
END
