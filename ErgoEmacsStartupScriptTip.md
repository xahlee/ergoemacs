One common problem in ErgoEmacs development is that sometimes you want to launch GNU Emacs without any customization for testing, sometimes you need a emacs with full personal customization for your day job, sometimes you want a emacs that loads latest dev elisp code... basically, managing multiple versions and instances of emacs can be tedious to start them with -Q and manually load elisp files you want.

This page contain shell script tips that sets up shell command aliases or clickable icons that lets you easily launch several different versions of emacs.

In this page, we show you how to create shell scripts in Windows or Mac OS X or Linux, that loads the latest ErgoEmacs's elisp files in repository and loads your own emacs customizations, so that, (1) you have your own emacs customization for your daily use of emacs. (2) you are also using the latest ErgoEmacs so any problems you'll know.

Based on info in this page, you can easily create scripts that launches other variations.

# For Windows #

## ErgoEmacs Setup + SVN ##

Download ErgoEmacs setup and install it. Do a checkout of the SVN trunk and then move all the content to `C:\Program Files\ErgoEmacs` folder, including hidden .svn folders. You should override the existent files.

In this way you can use/test your installed copy using the latest SVN ErgoEmacs code (`svn update` to download latest changes).

## cmd.exe ##

A .bat file example from David Capello.

```
@echo off
set PATH=C:\Program Files\ErgoEmacs\bin;%PATH%
set PATH=C:\Program Files\ErgoEmacs\msys\bin;%PATH%
set PATH=C:\Program Files\ErgoEmacs\hunspell;%PATH%
set HOME=C:\Documents and Settings\YourUserNameHere
set ERGOEMACS_KEYBOARD_LAYOUT=us
runemacs.exe -Q -title ErgoEmacs-trunk -load "C:/ErgoEmacs-trunk/ergoemacs/init.el" -eval "(if (file-exists-p \"~/.emacs\") (load \"~/.emacs\"))"
```

## PowerShell ##

Here's a tip on startup script for [PowerShell](http://xahlee.org/powershell/index.html) v2.

```
# launch ErgoEmacs with latest source code and your own emacs customization
& "C:\Program Files (x86)\ErgoEmacs\bin\runemacs.exe" -Q --load=~\ErgoEmacs_Source\ergoemacs\ergoemacs\init.el --load=~\.emacs
```

```
# launch ErgoEmacs with latest source code and your own emacs customization
start-process "C:\Program Files (x86)\ErgoEmacs\bin\runemacs.exe" -ArgumentList "-Q", "--load=~\ErgoEmacs_Source\ergoemacs\ergoemacs\init.el", "--load=~\.emacs"
```

You can create a alias for it. First, create a PowerShell profile file if you don't have already:

```
New-Item -type file -force $profile
```

The “$profile” shows the path to your PowerShell init script. Then, in your profile, put:

```
# launch ErgoEmacs with latest source code and your own emacs customization
function ErgoEmacsLaunch 
{
$env:path = $env:path + ";C:\Program Files (x86)\ErgoEmacs\hunspell"
# and other env var you want for your ErgoEmacs.

start-process "C:\Program Files (x86)\ErgoEmacs\bin\runemacs.exe" -ArgumentList "-Q", "--load=~\ErgoEmacs_Source\ergoemacs\ergoemacs\init.el", "--load=~\.emacs"
}

set-alias ee ErgoEmacsLaunch
```

Note: the emacs binary can be any built for your Windows. The difference between emacs.exe and runemacs.exe is that the latter does not leave behind a Windows Console.

# OS X or Unix #

Here's example for OS X with Bash, similar for other Unixes and Linux.

```
nohup /Applications/Emacs.app/Contents/MacOS/Emacs -Q --load=~/ErgoEmacs_Source/ErgoEmacs/ergoemacs/init.el --load=~/.emacs &
```

Make it a alias:

```
alias ee='nohup /Applications/Emacs.app/Contents/MacOS/Emacs -Q --load=~/ErgoEmacs_Source/ErgoEmacs/ergoemacs/init.el --load=~/.emacs &'
```

Note: if you close Mac's Terminal window by clicking on the close widget or Cmd+w, it'll also kill your emacs instance. There's no easy way around that. However, you can close the terminal window by typing “exit” or “Ctrl+d”.

Note: the emacs binary can be any of Carbon Emacs, Aquamac Emacs, or your own compilation for OS X.

## Byte Compile Elisp Code ##

The source repository's elisp code are not byte compiled. (build process will byte compile them.) Byte compiled code increase your load speed and executing speed some 6 times, so you might want to do that.

To byte compile all elisp files, you can do like this in bash:

```
emacs -Q --batch --load=~/ErgoEmacs_Source/ergoemacs/win32-setup/byte-compile_lisp_files.el
```

You can also use “make” in bash shell prompt on unix or Windows.

```
cd ~/ErgoEmacs_Source/ergoemacs
make EMACS=../emacs-23.1/bin/emacs.exe
```

## Tips for AutoHotKey ##

The following lets you press one key shortcuts to switch or launch your emacs, or, launch GNU Emacs without any init.

```
; Ctrl+F6 to launch a GNU Emacs 23 without loading customization
^F6::Run "C:\Program Files (x86)\emacs-23.1-bin-i386\emacs-23.1\bin\runemacs.exe" "-Q"

; F6 to launch ErgoEmacs, or switch to it if already running

F6::
IfWinExist ahk_class Emacs
{
  WinActivate
}
Else
{
  Run "C:\Program Files (x86)\ErgoEmacs3\ErgoEmacs.exe"
  WinWait ahk_class Emacs
  WinActivate
}
Return
```

## Call for Improvement ##

Best solution is to make them double-clickable icon that can be placed in QuickLaunch TaskBar or Mac's Dock. Both can be easily done but i haven't had time to figure it out... (on Windows, probably a .bat will do, and on Mac, bash and or Applescript )

If anyone has tips, please add in.