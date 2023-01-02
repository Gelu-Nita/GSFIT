# GSFIT
This is an IDL-widget(GUI)-based spectral fitting package that provides a user-friendly display of EOVSA image cubes and an interface to fast fitting codes (via platform-dependent shared-object libraries).  Fits to individual spectra can be done quickly for manual investigation, while parallel/multi-core batch processing of selected blocks of data can also be performed. A helper routine called ''gsfitview'' allows further display and investigation of the fitting results.

# Here are the steps needed to download the latest version of the package

#### If not already installed on your system, download and install git for command line

https://git-scm.com/

#### Launch the Git Bash terminal

#### CD to  your SSW/packages/ installation folder and issue the following sequence of commands

```bash
rm -rf gsfit
git clone https://github.com/Gelu-Nita/GSFIT gsfit
```

#### Add GSFIT to your SSW_INSTR list by editig the appropriate SSWIDL script

NOTE: GSFIT relies on some functionality that is part of the GX_SIMULATOR package. 
To ensure proper functionality, you must also install it following the instructions from 

https://github.com/Gelu-Nita/GX_SIMULATOR

