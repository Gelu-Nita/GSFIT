# GSFIT
This is an IDL-widget(GUI)-based spectral fitting package that provides a user-friendly display of EOVSA image cubes and an interface to fast fitting codes (via platform-dependent shared-object libraries).  Fits to individual spectra can be done quickly for manual investigation, while parallel/multi-core batch processing of selected blocks of data can also be performed. A helper routine called ''gsfitview'' allows further display and investigation of the fitting results.

# Here are the steps needed to download the latest version of the package

- [ ]  if not already installed on your system, download and install git for command line

https://git-scm.com/

- [ ]  launch the Git Bash terminal

- [ ]  cd to  your SSW/packages/ installation folder and issue the following sequence of commands

```bash
rm -rf gsfit
git clone https://github.com/Gelu-Nita/GSFIT gsfit
```

- [ ]  add GSFIT to your SSW_INSTR list by editig the appropriate SSWIDL script

NOTE: GSFIT relies on some functionality that is part of the GX_SIMULATOR package. 
To ensure proper functionality, you must also install it following the instructions from 

https://github.com/Gelu-Nita/GX_SIMULATOR

## Using GSFIT on Mac with Apple silicon chip

When launched for the first time on Mac OS, GSFIT attempts to invoke gfortran to compile a shared library named fit_Spectrum_Kl.so, which provides its backbone functionality. Therefore, the folowing steps should be performed to succeed launching GSFIT.

### Install x86 homebrew:

- [ ]  /usr/sbin/softwareupdate --install-rosett (if have not done so)
- [ ]  [installing and verifying x86 homebrew](https://medium.com/mkdir-awesome/how-to-install-x86-64-homebrew-packages-on-apple-m1-macbook-54ba295230f)

### Install gcc with x86 homebrew

- [ ]  arch -x86_64 /usr/local/homebrew/bin/brew gcc (or uss the alias you just set up in the *rc file)
- [ ]  make sure you are using the x86 gfortran: ‘which gfortran’, the output should be something like  ***‘/usr/local/homebrew/Cellar/gcc/12.2.0/bin/gfortran’ ,*** if nothing is coming out or the path starts with ‘/opt/homebrew’ (the default arm 64 homebrew path), you can add ***setenv PATH "/usr/local/homebrew_x86/Cellar/gcc/12.2.0/bin:$PATH”***  (or the **equivalent** in the other shells) to the end of your *shrc file of the current shell, from which you start the sswidl.
- [ ]  delete gsfit package, install it again by cloning it from Github, then it should be working.
- [ ]  afterward, resuming the arm64 gfortran as the default compiler is strongly recommended, to do so, just command out ***setenv PATH "/usr/local/homebrew_x86/Cellar/gcc/12.2.0/bin:$PATH” or the* equivalent.  ****

