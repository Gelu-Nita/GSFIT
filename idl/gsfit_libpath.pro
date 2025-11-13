FUNCTION gsfit_libpath, unix_dir, update=update
  COMPILE_OPT IDL2
  default, unix_dir, file_search(getenv('gsfitpath'), 'unix', /mark)
  ; Define user-writable target directory
  home_dir = getenv('HOME')
  IF home_dir EQ '' THEN home_dir = curdir()  ; Fallback if HOME is unset
  user_root = home_dir
  binary_path = user_root + path_sep() + 'gsfit_binaries'
  IF ~file_test(binary_path) THEN file_mkdir, binary_path

  ; Define expected output library name
  libname = 'fit_Spectrum_Kl.so'
  lib_path = binary_path + path_sep() + libname

  ; Return early if library already exists and no update is requested
  IF file_test(lib_path) AND ~keyword_set(update) THEN RETURN, lib_path

  ; Determine name of copied source folder
  copied_folder = file_basename(unix_dir)
  tmp_build = user_root + path_sep() + copied_folder

  ; OS-specific deletion command
  delete_cmd = (!version.os_family EQ 'Windows') ? 'rmdir /S /Q ' : 'rm -r '

  ; Clean up any previous copy
  IF file_test(tmp_build) THEN spawn, delete_cmd + tmp_build

  ; Copy source files to user directory
  file_copy, unix_dir, user_root, /overwrite, /force, /recursive

  ; Check for makefile
  makefile = tmp_build + path_sep() + 'makefile'
  IF ~file_test(makefile) THEN BEGIN
    message, 'No makefile found in source directory. Cannot build library.', /info
    IF file_test(tmp_build) THEN spawn, delete_cmd + tmp_build
    contents = file_search(binary_path + path_sep() + '*')
    IF contents EQ '' THEN spawn, delete_cmd + binary_path
    RETURN, !NULL
  ENDIF

  ; Attempt to build the library
  cdr = curdir()
  cd, tmp_build
  spawn, 'rm *.o'
  spawn, 'make'
  cd, cdr

  ; Copy built library to user binary path
  built_lib = tmp_build + path_sep() + libname
  IF file_test(built_lib) THEN BEGIN
    file_copy, built_lib, binary_path, /overwrite, /force
    message, libname + ' successfully built and copied to ' + binary_path, /info
  ENDIF ELSE BEGIN
    message, 'Build failed. No valid library produced.', /info
    IF file_test(tmp_build) THEN spawn, delete_cmd + tmp_build
    contents = file_search(binary_path + path_sep() + '*')
    IF contents EQ '' THEN spawn, delete_cmd + binary_path
    RETURN, !NULL
  ENDELSE

  ; Clean up temporary build directory
  IF file_test(tmp_build) THEN spawn, delete_cmd + tmp_build

  RETURN, lib_path
END