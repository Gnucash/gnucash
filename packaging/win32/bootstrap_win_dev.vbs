' bootstap_win_dev.vbs
'
' The goal of this script is to simplify setting up a development
' environment to develop for GnuCash on Windows.
' It will set up an absolute minimal environment from where
' the regular GnuCash Windows build system can take over.
' This minimal environment consists of
' - mingw-get: the mingw package installer tool
' - msys-base: a basic MSYS shell environment
' - git for windows, required for:-
' - the GnuCash source code repository, cloned from the github GnuCash repository
'
' The bootstrap script can also be run on top of an existing set up
' in which case the script will only do what is necessary to get
' the above items in place. For example, if git is already installed
' in the location pointed to by GIT_DIR below, it won't be installed
' again.
'
' IN CASE OF UNEXPECTED CLOSING OF THE CONSOLE
' Please open a console (cmd.exe) and run the script under cscript.exe as follows:
' cscript.exe <path-to-this-script>
' This will keep your console open, so you can read if there were errors

' Script start
' ------------
' Ensure we have a visible console to display output
CheckStartMode

' Parameters
' ----------
' All of the above will be installed in the base directory specified below.
' If this path doesn't suit you, please feel free to modify it before
' running this bootstrap script.
' Note: avoid paths with spaces or other special characters (like &).
'       these can confuse msys/mingw or some of the tools depending on them.
GLOBAL_DIR = "c:\soft"
MINGW_DIR  = GLOBAL_DIR & "\mingw"
TMP_DIR= GLOBAL_DIR & "\tmp"
DOWNLOAD_DIR= GLOBAL_DIR & "\downloads"
GIT_PKG = "Git-1.7.10-preview20120409.exe"
strGitBaseUrl = "http://msysgit.googlecode.com/files/"
GIT_URL = strGitBaseUrl & GIT_PKG
GIT_DIR = GLOBAL_DIR & "\git-1.7.10"
REPOS_URL = "git://github.com/Gnucash/gnucash.git"
REPOS_DIR = GLOBAL_DIR & "\gnucash.git"

' Global parameters for visual basic
Set objFso = CreateObject("Scripting.FileSystemObject")
Set stdout = objFso.GetStandardStream(1)
Set stdin  = objFso.GetStandardStream(0)
Set objWsh = WScript.CreateObject ("WScript.Shell")
Const ForReading = 1, ForWriting = 2, ForAppending = 8

Welcome


' Create base directories if necessary
' ------------------------------------
If Not objFso.FolderExists(GLOBAL_DIR) Then
    stdout.Write "Creating " & GLOBAL_DIR & "... "
    objFso.CreateFolder(GLOBAL_DIR)
    stdout.WriteLine "Ok"
End If
If Not objFso.FolderExists(MINGW_DIR) Then
    stdout.Write "Creating " & MINGW_DIR & "... "
    objFso.CreateFolder(MINGW_DIR)
    stdout.WriteLine "Ok"
End If
If Not objFso.FolderExists(TMP_DIR) Then
    stdout.Write "Creating " & TMP_DIR & "... "
    objFso.CreateFolder(TMP_DIR)
    stdout.WriteLine "Ok"
End If
If Not objFso.FolderExists(DOWNLOAD_DIR) Then
    stdout.Write "Creating " & DOWNLOAD_DIR & "... "
    objFso.CreateFolder(DOWNLOAD_DIR)
    stdout.WriteLine "Ok"
End If


' Install mingw-get
' -----------------

strMingwGet = MINGW_DIR & "\bin\mingw-get.exe"
stdout.Write "Checking " & strMingwGet & "... "
If objFso.FileExists(strMingwGet) Then
    stdout.WriteLine "Found, no need to install"
Else
    stdout.WriteLine "Not found, will be installed"

    strMingwGetZip = DOWNLOAD_DIR & "\mingw-get.zip"
    If Not objFso.FileExists(strMingwGetZip) Then
        stdout.Write "Downloading mingw-get.zip (slow!)... "
        strMingwGetZipUrl = "https://github.com/gjanssens/gnucash-on-windows/raw/master/mingw-get.zip"
        HTTPDownload strMingwGetZipUrl, strMingwGetZip
        stdout.WriteLine "Success"
    End If

    ' Extract mingw-get.zip into our MINGW_DIR
    ' using a detour via a temporary directory to deal with the
    ' cludgy way to detect when extracting is finished.
    ' I couldn't find a better way so far.
    stdout.Write "Installing mingw-get... "
    strMingwTmpDir = TMP_DIR & "\mingwtmp"
    If objFso.FolderExists(strMingwTmpDir) Then
        objFso.DeleteFolder strMingwTmpDir , True
    End If
    ExtractAll objFso.GetAbsolutePathName(".")& "\mingw-get.zip", strMingwTmpDir
    objFso.CopyFolder strMingwTmpDir & "\*", MINGW_DIR, True
    objFso.DeleteFolder strMingwTmpDir , True
    stdout.WriteLine "Success"

End If


' Instal Basic Msys (we need msys-wget to install git)
' ----------------------------------------------------
' Note: we don't check if these are installed already.
'       mingw-get will do this for us automatically.
stdout.Write "Installing msys and wget... "
strMingwGet = MINGW_DIR & "\bin\mingw-get.exe"

objWsh.Run strMingwGet & " install msys-base msys-wget", 1, True
'Set objExec = objWsh.Exec (strMingwGet & " install msys-base msys-wget")

strWget = MINGW_DIR & "\msys\1.0\bin\wget.exe"
If Not objFso.FileExists(strWget) Then
    stdout.WriteLine "Failed"
    stdout.WriteBlankLines (1)
    stdout.WriteLine "*** ERROR ***"
    stdout.WriteLine "Msys/Wget installation failed."
    stdout.WriteBlankLines (1)
    stdout.WriteLine "Cannot continue until this has been resolved."
    AbortScript
End If
stdout.WriteLine "Success"


' Install Git
' -----------
strGit = GIT_DIR & "\bin\git.exe"
stdout.Write "Checking " & strGit & "... "
If objFso.FileExists(strGit) Then
    stdout.WriteLine "Found, no need to install"
Else
    stdout.WriteLine "Not found, will be installed"

    strGitPkg = DOWNLOAD_DIR & "\" & GIT_PKG
    If Not objFso.FileExists(strGitPkg) Then
        stdout.Write "Downloading git installer... "
        objWsh.Run strWget & " -P" & DOWNLOAD_DIR & " " & GIT_URL, 1, true

        If Not objFso.FileExists(strGitPkg) Then
            stdout.WriteLine "Failed"
            stdout.WriteBlankLines (1)
            stdout.WriteLine "*** ERROR ***"
            stdout.WriteLine "Download git installer failed."
            stdout.WriteBlankLines (1)
            stdout.WriteLine "Cannot continue until this has been resolved."
            AbortScript
        End If
        stdout.WriteLine "Success"
    End If

    stdout.Write "Installing git... "
    objWsh.Run strGitPkg & " /SP- /SILENT /DIR=" & GIT_DIR, 1, true

    If Not objFso.FileExists(strGit) Then
        stdout.WriteLine "Failed"
        stdout.WriteBlankLines (1)
        stdout.WriteLine "*** ERROR ***"
        stdout.WriteLine "Git installation failed."
        stdout.WriteBlankLines (1)
        stdout.WriteLine "Cannot continue until this has been resolved."
        AbortScript
    End If
    stdout.WriteLine "Sucess"
End If


' Set up git repository
' ---------------------
strInstall = REPOS_DIR & "\packaging\win32\install.sh"
stdout.WriteLine "Checking if " & REPOS_DIR
stdout.Write "         is a GnuCash git repository... "
If objFso.FolderExists(REPOS_DIR & "\.git") And objFso.FileExists(strInstall) Then
    stdout.WriteLine "Most likely ok, won't clone"
Else
    stdout.WriteLine "Not found"
    stdout.WriteLine "Set up GnuCash git repository... "
    objWsh.Run strGit & " clone " & REPOS_URL & " " & REPOS_DIR, 1, true

    If Not objFso.FileExists(strInstall) Then
        stdout.WriteLine "Failed"
        stdout.WriteBlankLines (1)
        stdout.WriteLine "*** ERROR ***"
        stdout.WriteLine "Failed to set up GnuCash git repository."
        stdout.WriteBlankLines (1)
        stdout.WriteLine "Cannot continue until this has been resolved."
        AbortScript
    End If
    stdout.WriteLine "Ok"
End If

' Create custom.sh
' ----------------
strCustomSh = REPOS_DIR & "\packaging\win32\custom.sh"
bExistingCustomSh = False
If objFso.FileExists(strCustomSh) Then
    stdout.WriteLine "Found existing custom.sh file"
    bExistingCustomSh = True
Else
    ' Create a custom.sh file that matches the parameters set at the beginning of this script
    ' This ensures install.sh will find the development environment we set up
    ' Note: we're deliberately not storing versions of used components in the autogenerated custom.sh
    '       This allows install.sh to update to newer versions if deemed useful
    stdout.Write "Autogenerating custom.sh file... "
    Set myRegExp = New RegExp
    myRegExp.Global = True
    myRegExp.Pattern = "\\"

    strGlobalDir   = myRegExp.Replace (GLOBAL_DIR, "\\")
    strMingwDir    = myRegExp.Replace (MINGW_DIR, "\\")
    strMsysDir    = myRegExp.Replace (MINGW_DIR & "\msys\1.0", "\\")
    strTmpDir      = myRegExp.Replace (TMP_DIR, "\\")
    strDownloadDir = myRegExp.Replace (DOWNLOAD_DIR, "\\")
    strGitDir      = myRegExp.Replace (GIT_DIR, "\\")
    strReposDir    = myRegExp.Replace (REPOS_DIR, "\\")

    Set objCustomSh = objFso.OpenTextFile( strCustomSh, ForWriting, True )
    objCustomSh.WriteLine "# custom.sh, automatically created by bootstrap_win_dev.vbs"
    objCustomSh.WriteLine "#"
    objCustomSh.WriteLine "# The parameters set here match the parameters used by"
    objCustomSh.WriteLine "# bootstrap_win_dev.vbs to set up the GnuCash development"
    objCustomSh.WriteLine "# environment and should ensure the install.sh works out"
    objCustomSh.WriteLine "# of the box."
    objCustomSh.WriteLine "#"
    objCustomSh.WriteLine "# You are free to modify these parameters to suit you,"
    objCustomSh.WriteLine "# but keep in mind that if you ever want to run"
    objCustomSh.WriteLine "# bootstrap_win_dev.vbs again you should make sure"
    objCustomSh.WriteLine "# the parameters it uses match the ones you set here."
    objCustomSh.WriteBlankLines 1
    objCustomSh.WriteLine "GLOBAL_DIR=" & strGlobalDir
    objCustomSh.WriteLine "MINGW_DIR=" & strMingwDir
    objCustomSh.WriteLine "MSYS_DIR=" & strMsysDir
    objCustomSh.WriteLine "TMP_DIR=" & strTmpDir
    objCustomSh.WriteLine "DOWNLOAD_DIR=" & strDownloadDir
    objCustomSh.WriteLine "GIT_DIR=" & strGitDir
    objCustomSh.WriteLine "REPOS_TYPE=git" ' Bootstrap only works with a git repo
    objCustomSh.WriteLine "REPOS_URL=" & REPOS_URL
    objCustomSh.WriteLine "REPOS_DIR=" & strReposDir
    objCustomSh.Close
    stdout.WriteLine "Success"
End If


' End message
' -----------
stdout.WriteBlankLines 1
stdout.WriteLine "Bootstrap completed successfully !"
stdout.WriteBlankLines 1
stdout.WriteLine "You can now continue as follows"
stdout.WriteLine "- Open the msys shell"
stdout.WriteLine "- cd " & REPOS_DIR & "\packaging\win32"
stdout.WriteLine "- Properly configure a custom.sh"
stdout.WriteLine "  (if you changed any default path in the bootstrap script)"
stdout.WriteLine "- Run install.sh"
stdout.WriteBlankLines 1
stdout.WriteLine "Happy hacking !"

AbortScript


' Functions used in the script
' ----------------------------
' Initial message to user
Sub Welcome
    stdout.WriteLine "Boostrap GnuCash Development on Windows"
    stdout.WriteLine "---------------------------------------"
    stdout.WriteLine "This script is intended for people that wish to develop GnuCash on Windows"
    stdout.WriteLine "It will download and install the minimal set of tools"
    stdout.WriteLine "to run a first build of the GnuCash sources."
    stdout.WriteLine "It will install"
    stdout.WriteLine "- mingw-get, an msys shell and wget in " & MINGW_DIR
    stdout.WriteLine "- git in " & GIT_DIR
    stdout.WriteLine "- a GnuCash git repository cloned from"
    stdout.WriteLine "  " & REPOS_URL
    stdout.WriteLine "  into " & REPOS_DIR
    stdout.WriteBlankLines 1
    stdout.WriteLine "Notes:"
    stdout.WriteLine "* Components already found in the given locations"
    stdout.WriteLine "  won't be touched. Instead the available versions"
    stdout.WriteLine "  will be used in that case."
    stdout.WriteLine "* If the proposed locations don't suit you, you can"
    stdout.WriteLine "  customize them before running this script."
    stdout.WriteLine "  All of them are located at the beginning of this file."
    stdout.WriteBlankLines 1
    stdout.Write "Continue with the set up (Y/N) ? "
    chRead = stdin.ReadLine
    If Not (UCase(Left(chRead,1)) = "Y") Then
        stdout.WriteLine "Installation interrupted."
        AbortScript
    End If
End Sub
    

' Download a file over http
Sub HTTPDownload( myURL, myPath )
' This Sub downloads the FILE specified in myURL to the path specified in myPath.
'
' myURL must always end with a file name
' myPath may be a directory or a file name; in either case the directory must exist
'
' Based on a script written by Rob van der Woude
' http://www.robvanderwoude.com

    ' Standard housekeeping
    Dim i, objFile, objHTTP, strFile, strMsg

    ' Check if the specified target file or folder exists,
    ' and build the fully qualified path of the target file
    If objFso.FolderExists( myPath ) Then
        strFile = objFso.BuildPath( myPath, Mid( myURL, InStrRev( myURL, "/" ) + 1 ) )
    ElseIf objFso.FolderExists( Left( myPath, InStrRev( myPath, "\" ) - 1 ) ) Then
        strFile = myPath
    Else
        stdout.WriteLine "ERROR: Target folder not found."
        AbortScript
    End If

    ' Create or open the target file
    Set objFile = objFso.OpenTextFile( strFile, ForWriting, True )

    ' Create an HTTP object
    Set objHTTP = CreateObject( "MSXML2.ServerXMLHTTP" )

    ' Download the specified URL
    objHTTP.Open "GET", myURL, False
    objHTTP.Send

    ' Write the downloaded byte stream to the target file
    For i = 1 To LenB( objHTTP.ResponseBody )
        objFile.Write Chr( AscB( MidB( objHTTP.ResponseBody, i, 1 ) ) )
    Next

    ' Close the target file
    objFile.Close( )
End Sub


' Extract a zip file strZipFile into strFolder
Function ExtractAll(strZipFile, strFolder)
   Set objShell = CreateObject("Shell.Application")
   If Not objFso.FolderExists(strFolder) Then
       objFso.CreateFolder(strFolder)
   End If

   intCount = objShell.NameSpace(strFolder).Items.Count
   Set colItems = objShell.NameSpace(strZipFile).Items
   objShell.NameSpace(strFolder).CopyHere colItems, 256
   Do Until objShell.NameSpace(strFolder).Items.Count = intCount + colItems.Count
       WScript.Sleep 200
   Loop
End Function


' Make sure we run in a console (so output is visible)
Sub CheckStartMode
    ' Returns the running executable as upper case from the last \ symbol
    strStartExe = UCase( Mid( wscript.fullname, instrRev(wscript.fullname, "\") + 1 ) )

    If Not strStartExe = "CSCRIPT.EXE" Then
        ' This wasn't launched with cscript.exe, so relaunch using cscript.exe explicitly!
        ' wscript.scriptfullname is the full path to the actual script

        set oSh = CreateObject("wscript.shell")
        oSh.Run "cscript.exe """ & wscript.scriptfullname & """"
        wscript.quit

    End If
End Sub


' Abort the script
Sub AbortScript
    stdout.WriteBlankLines 1
    stdout.Write "Pres enter to continue... "
    chRead = stdin.Read (1)
    WScript.Quit
End Sub