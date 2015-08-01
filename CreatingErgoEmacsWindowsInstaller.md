# ErgoEmacs Maintainer Package #

If you want to create a customized ErgoEmacs installer for Windows, you can download the [ErgoEmacs Maintainer](http://ergoemacs.googlecode.com/files/ErgoEmacs-Maintainer-Emacs-23-3.7z) package, which is supposed to be used by ErgoEmacs's maintainers. Right there are three steps to follow (three .bat files) which must be executed in order double-clicking them.

# The Hard Way #

In this section is a detailed instruction on creating the ErgoEmacs installer “ErgoEmacs Setup.exe” manually. It also explains what the `ErgoEmacs.exe` runner do. You need to have some sys admin or software compiling experience to understand this guide.

We recommend you to use the ErgoEmacs Maintainer package, because this guide can be outdated.

## Components And Tools You Need ##

You need the following components of ErgoEmacs:

  * GNU/Emacs for Windows
  * ErgoEmacs source code.
  * msys (unix shell utils)
  * [hunspell](http://en.wikipedia.org/wiki/Hunspell)
  * Libraries ([DLL](http://en.wikipedia.org/wiki/Dynamic-link_library)) for dealing with images, zip/unzip, pdf, etc.

The above are binary files that are bundled or part of the ErgoEmacs. The Emacs binary of ErgoEmacs is from GNU Emacs. The other binaries are used in ErgoEmacs for reading image files, pdf files, spell checking, providing bash and unix shells tools, etc.

You need the following tools to create the installer:

  * [MinGW](http://en.wikipedia.org/wiki/Mingw)
  * [7-zip](http://en.wikipedia.org/wiki/7-zip)
  * [InnoSetup](http://en.wikipedia.org/wiki/Inno_Setup)

The MingGW is a gcc compiler tool bundle for building unix software on Windows. 7-zip is a new archiving utility used to un-pack some of the binaries we'll need. The InnoSetup is a program that creates software installers for Windows.

### Step By Step Instruction ###

  * Creat a folder `~\packages`. This folder will be used as the base to build the ErgoEmacs installer.

  * GNU/Emacs base system:
    * Download GNU/Emacs binary for Windows: [emacs-23.3-bin-i386.zip](http://ftp.gnu.org/pub/gnu/emacs/windows/emacs-23.3-bin-i386.zip). Home page [ftp.gnu.org](http://ftp.gnu.org/pub/gnu/emacs/windows/)
    * Uncompress the zip, and placed the folder in `~\packages` (you will have `~\packages\emacs-23.3`.)
  * MSYS system:
    * Download [msysCORE-1.0.11](https://sourceforge.net/projects/mingw/files/MSYS%20Base%20System/Current%20Release_%20MSYS-1.0.11/msysCORE-1.0.11-bin.tar.gz/download), [coreutils-5.97](https://sourceforge.net/projects/mingw/files/MSYS%20Base%20System/Current%20Release_%20MSYS-1.0.11/coreutils-5.97-MSYS-1.0.11-snapshot.tar.bz2/download), [make-3.81](https://sourceforge.net/projects/mingw/files/MSYS%20Base%20System/Current%20Release_%20MSYS-1.0.11/make-3.81-MSYS-1.0.11-2.tar.bz2/download) (Home page: [mingw.org](http://www.mingw.org/))
    * Uncompress everything in `~\packages\msys`
  * Spell checking:
    * Download [hunspell-1.2.8](https://sourceforge.net/projects/hunspell/files/Hunspell/1.2.8/hunspell-1.2.8-win32.zip/download) (Home page http://hunspell.sourceforge.net/)
    * Uncompress it in `~\packages\hunspell`
  * Image support:
    * Download libraries: [jpeg](http://ftp.gnome.org/pub/GNOME/binaries/win32/dependencies/jpeg-6b-4.zip), [libpng](http://ftp.gnome.org/pub/GNOME/binaries/win32/dependencies/libpng_1.2.39-1_win32.zip), [zlib](http://ftp.gnome.org/pub/GNOME/binaries/win32/dependencies/zlib-1.2.3.zip), [tiff](http://ftp.gnome.org/pub/GNOME/binaries/win32/dependencies/libtiff-3.8.2.zip)
    * unzip all of them, then copy the 4 resulting “.dll” files to `~\packages\emacs-23.3\bin`. Also, rename libpng12-0.dll to libpng12.dll.
  * PDF support:
    * Download [gs864w32.exe](http://mirror.cs.wisc.edu/pub/mirrors/ghost/GPL/gs864/gs864w32.exe). Home page [wisc.edu](http://pages.cs.wisc.edu/~ghost/doc/GPL/gpl864.htm), link at bottom.
    * Download 7-zip from http://www.7-zip.org/.
    * Use 7-zip to extract `gs864w32.exe` file, so you should get a file named `gswin32c.exe`.
    * Rename `gswin32c.exe` to `gs.exe` and move to `~\packages\emacs-23.3\bin`
  * Support to zip/unzip files in shell:
    * Download zip/unzip source code from https://sourceforge.net/projects/infozip/files/.
    * Compile them with MinGW.
    * You could use the GnuWin32 distribution of these packages, but they depend on MSVC c-runtime libraries.
    * Put the zip.exe and unzip.exe in the `~\packages\msys\bin` directory.
  * Check-out ErgoEmacs trunk from SVN:
```
~\packages>svn checkout http://ergoemacs.googlecode.com/svn/trunk/ ErgoEmacs-trunk
```
  * Elisp byte compile all `.el` files in `ErgoEmacs-trunk` with the same `emacs.exe` that will be distributed:
```
~\packages\ErgoEmacs-trunk>make compile EMACS=../emacs-23.3/bin/emacs.exe
```
  * Compile the setup (you need [MinGW](http://www.mingw.org/)):
```
~\packages\ErgoEmacs-trunk\win32-setup>make
```
  * Then open `~\packages\ErgoEmacs-trunk\win32-setup\ErgoEmacs.iss` with InnoSetup and compile the installer.

### Default ErgoEmacs installation folder ###

By default the ErgoEmacs is installed in
```
C:\Program Files\ErgoEmacs\
```
When ErgoEmacs is running, you can use the `$emacs_dir` enviroment variable to know this folder (remember that user can select any folder where to install ErgoEmacs, so you should not use absolute paths, everything should be relative to `$emacs_dir`).


### ErgoEmacs.exe runner ###

The `ErgoEmacs.exe` runner is a little program used to start GNU/Emacs executing all ErgoEmacs initialization scripts to recreate a comfortable look-and-feel for common Windows users. It is installed in
```
C:\Program Files\ErgoEmacs\ErgoEmacs.exe
```
When `ErgoEmacs.exe` is executed, it will look the `HOME` environment variable. If it is not set (most Windows users does not have it set), it will take the following value:
```
C:\Documents and Settings\UserName
```
The `HOME` location is where `.emacs` will be loaded/saved. For example, if you customize some options in ErgoEmacs, the configuration will be saved in the `C:\Documents and Settings\UserName\.emacs` file.