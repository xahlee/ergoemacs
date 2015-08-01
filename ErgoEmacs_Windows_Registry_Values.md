# Introduction #

Windows store many configuration and setting values in its Registry, a binary file.

The ErgoEmacs Setup creates some entries in the Windows registry if you choose the "Associate .el files with ErgoEmacs" checkbox in the installation. These entries are removed automatically when you uninstall ErgoEmacs.

## Registered Entries ##

The list of entries that will be registered are in the InnoSetup .iss file:

http://code.google.com/p/ergoemacs/source/browse/trunk/win32-setup/ErgoEmacs.iss

See the [[Registry](Registry.md)] section.

## File Association ##

If you choose to associate .el files with ErgoEmacs, the following entries are created:

Key: Computer > HKEY\_CLASSES\_ROOT > .el > ErgoEmacsFile > (Default)

Value: ErgoEmacsFile

Key: Computer > HKEY\_CLASSES\_ROOT > ErgoEmacsFile > DefaultIcon > (Default)

Value: C:\Program Files (x86)\ErgoEmacs\ErgoEmacs.exe,0

Key: Computer > HKEY\_CLASSES\_ROOT > ErgoEmacsFile > shell > open > command > (Default)

Value: "C:\Program Files (x86)\ErgoEmacs\ErgoEmacs.exe" "%1"