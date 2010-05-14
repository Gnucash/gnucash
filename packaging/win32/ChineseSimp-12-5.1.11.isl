; *** Inno Setup version 5.1.11+ Simplified Chinese messages ***
;
; Based on previous version by Peng Bai
; Update by Mack Zhang (hua_wuxin@21cn.com) on Apr. 10, 2008
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=<4E2D><6587> (<7B80><4F53>)
LanguageID=$0804
LanguageCodePage=936
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
DialogFontName=����
DialogFontSize=9
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=��װ��
SetupWindowTitle=��װ�� - %1
UninstallAppTitle=ж����
UninstallAppFullTitle=%1 ж����

; *** Misc. common
InformationTitle=��Ϣ
ConfirmTitle=ȷ��
ErrorTitle=����

; *** SetupLdr messages
SetupLdrStartupMessage=��װ�򵼽�����ĵ����ϰ�װ %1����ȷ��Ҫ������
LdrCannotCreateTemp=�޷�������ʱ�ļ�����װ��ֹ
LdrCannotExecTemp=�޷�������ʱ�ļ����е��ļ�����װ��ֹ

; *** Startup error messages
LastErrorMessage=%1.%n%n���� %2��%3
SetupFileMissing=��װ�ļ���ȱ���ļ� %1�����������������ȡ������°汾��
SetupFileCorrupt=��װ�ļ����𻵡�����ȡ������°汾��
SetupFileCorruptOrWrongVer=��װ�ļ����𻵣�������˰�װ�򵼵İ汾�����ݡ����������������ȡ������°汾��
NotOnThisPlatform=�˳������� %1 �����С�
OnlyOnThisPlatform=�˳�������� %1 �����С�
OnlyOnTheseArchitectures=�˳���ֻ�ܰ�װ��Ϊ���д������ܹ���Ƶ� Windows �汾�У�%n%n%1
MissingWOW64APIs=��ǰ�� Windows �汾û�а�ִ�� 64 λ��װ������ĺ�����Ҫ��������⣬�밲װ Service Pack %1��
WinVersionTooLowError=�˳�����Ҫ %1 v%2 ���߰汾��
WinVersionTooHighError=�˳����ܰ�װ�� %1 v%2 ���߰汾�ϡ�
AdminPrivilegesRequired=��װ�˳���ʱ������Թ���Ա��ݵ�¼��
PowerUserPrivilegesRequired=��װ�˳���ʱ������Թ���Ա�� Power Users ���Ա����ݵ�¼��
SetupAppRunningError=��װ�򵼷��� %1 �������С�%n%n�������ر�������ʵ��Ȼ�󵥻���ȷ��������򵥻���ȡ���˳���
UninstallAppRunningError=ж�س����� %1 �������С�%n%n�������ر�������ʵ��Ȼ�󵥻���ȷ��������򵥻���ȡ���˳���

; *** Misc. errors
ErrorCreatingDir=��װ���޷������ļ��С�%1��
ErrorTooManyFilesInDir=�޷����ļ��С�%1���д����ļ�����Ϊ�����̫���ļ�

; *** Setup common messages
ExitSetupTitle=�˳���װ
ExitSetupMessage=��װ��δ��ɡ�����������˳�����������ᰲװ��%n%n�����������ʱ���������а�װ������ɰ�װ��%n%n�����˳���װ��
AboutSetupMenuItem=���ڰ�װ��(&A)...
AboutSetupTitle=���ڰ�װ��
AboutSetupMessage=%1 �汾 %2%n%3%n%n%1 ��ҳ��%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< ��һ��(&B)
ButtonNext=��һ��(&N) >
ButtonInstall=��װ(&I)
ButtonOK=ȷ��
ButtonCancel=ȡ��
ButtonYes=��(&Y)
ButtonYesToAll=ȫ��(&A)
ButtonNo=��(&N)
ButtonNoToAll=ȫ��(&O)
ButtonFinish=���(&F)
ButtonBrowse=���(&B)...
ButtonWizardBrowse=���(&R)...
ButtonNewFolder=�����ļ���(&M)

; *** "Select Language" dialog messages
SelectLanguageTitle=ѡ��װ����
SelectLanguageLabel=ѡ��װ�ڼ�Ҫʹ�õ����ԣ�

; *** Common wizard text
ClickNext=��������һ��������򵥻���ȡ���˳���װ��
BeveledLabel=
BrowseDialogTitle=����ļ���
BrowseDialogLabel=ѡ��һ���ļ��У�Ȼ�󵥻���ȷ������
NewFolderName=�½��ļ���

; *** "Welcome" wizard page
WelcomeLabel1=��ӭʹ�� [name] ��װ��
WelcomeLabel2=��װ�򵼽�����ĵ����ϰ�װ [name/ver]��%n%n�������ڼ���֮ǰ�ر���������Ӧ�ó���

; *** "Password" wizard page
WizardPassword=����
PasswordLabel1=�˰�װ�������뱣����
PasswordLabel3=���������룬Ȼ�󵥻�����һ����������һ����������ִ�Сд��
PasswordEditLabel=����(&P)��
IncorrectPassword=����������벻��ȷ�������ԡ�

; *** "License Agreement" wizard page
WizardLicense=���Э��
LicenseLabel=���ڼ���֮ǰ�Ķ�������Ҫ��Ϣ��
LicenseLabel3=���Ķ��������Э�顣�ڼ���װ֮ǰ���������ܴ�Э������
LicenseAccepted=�ҽ���Э��(&A)
LicenseNotAccepted=�Ҳ�����Э��(&D)

; *** "Information" wizard pages
WizardInfoBefore=��Ϣ
InfoBeforeLabel=���ڼ���֮ǰ�Ķ�������Ҫ��Ϣ��
InfoBeforeClickLabel=����׼���ü���װ���뵥������һ������
WizardInfoAfter=��Ϣ
InfoAfterLabel=���ڼ���֮ǰ�Ķ�������Ҫ��Ϣ��
InfoAfterClickLabel=����׼���ü���װ���뵥������һ������

; *** "User Information" wizard page
WizardUserInfo=�û���Ϣ
UserInfoDesc=�����������Ϣ��
UserInfoName=�û���(&U)��
UserInfoOrg=��֯(&O)��
UserInfoSerial=���к�(&S)��
UserInfoNameRequired=���������û���

; *** "Select Destination Location" wizard page
WizardSelectDir=ѡ��Ŀ��λ��
SelectDirDesc=�� [name] ��װ�����
SelectDirLabel3=��װ�򵼽��� [name] ��װ�������ļ����С�
SelectDirBrowseLabel=��Ҫ����������һ�����������Ҫѡ��ͬ���ļ��У��뵥�����������
DiskSpaceMBLabel=������Ҫ [mb] MB �Ŀ��д��̿ռ䡣
ToUNCPathname=��װ�򵼲��ܰ�װ�� UNC ·�����������Ҫͨ�����簲װ����ӳ������������
InvalidPath=�������������̷������·�������磺%n%nC:\APP%n%n���� UNC ·����ʽ��%n%n\\server\share
InvalidDrive=��ѡ��������� UNC ���?���ڻ򲻿ɷ��ʡ�������ѡ��
DiskSpaceWarningTitle=û���㹻�Ĵ��̿ռ�
DiskSpaceWarning=��װ��������Ҫ %1 KB ��ʣ��ռ䣬������ѡ����ֻ�� %2 KB ���á�%n%n���������ҲҪ������
DirNameTooLong=�ļ�����ƻ�·��̫����
InvalidDirName=�ļ��������Ч��
BadDirName32=�ļ�����Ʋ��ܰ������ַ�%n%n%1
DirExistsTitle=�ļ����Ѵ���
DirExists=�ļ��У�%n%n%1%n%n�Ѵ��ڡ���ȷ��Ҫ��װ�����ļ�����
DirDoesntExistTitle=�ļ��в�����
DirDoesntExist=�ļ��У�%n%n%1%n%n�����ڡ��������ļ�����

; *** "Select Components" wizard page
WizardSelectComponents=ѡ�����
SelectComponentsDesc=Ҫ��װ��Щ�����
SelectComponentsLabel2=��ѡ����Ҫ��װ�����������㲻�밲װ�������׼���ú�������һ������
FullInstallation=����װ
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=��లװ
CustomInstallation=���ư�װ
NoUninstallWarningTitle=����Ѵ���
NoUninstallWarning=��װ�򵼷�����������Ѿ���װ��%n%n%1%n%nȡ��ѡ������ж����Щ�����%n%n����װ��
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=��ǰ��ѡ��������Ҫ [mb] MB ���̿ռ䡣

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=ѡ�񸽼�����
SelectTasksDesc=Ҫִ����Щ��������
SelectTasksLabel2=��ѡ���ڰ�װ [name] �ڼ䰲װ��Ҫִ�еĸ�������Ȼ��������һ������

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=ѡ��ʼ�˵��ļ���
SelectStartMenuFolderDesc=�ѳ����ݷ�ʽ�ŵ����
SelectStartMenuFolderLabel3=��װ�򵼽������¿�ʼ�˵��ļ����д��������ݷ�ʽ��
SelectStartMenuFolderBrowseLabel=�������һ����������һ���������Ҫѡ��ͬ���ļ��У��������������
MustEnterGroupName=����������ļ������
GroupNameTooLong=�ļ�����ƻ�·��̫��
InvalidGroupName=�ļ��������Ч
BadGroupName=�ļ�����Ʋ��ܰ������ַ�%n%n%1
NoProgramGroupCheck2=��ֹ������ʼ�˵��ļ���(&D)

; *** "Ready to Install" wizard page
WizardReady=׼����װ
ReadyLabel1=��װ������׼����ʼ��װ [name]��
ReadyLabel2a=�������װ������װ���������Ҫ�鿴���߸��������������һ������
ReadyLabel2b=�������װ������װ��
ReadyMemoUserInfo=�û���Ϣ��
ReadyMemoDir=Ŀ��λ�ã�
ReadyMemoType=��װ���ͣ�
ReadyMemoComponents=��ѡ�����
ReadyMemoGroup=��ʼ�˵��ļ��У�
ReadyMemoTasks=��������

; *** "Preparing to Install" wizard page
WizardPreparing=����׼����װ
PreparingDesc=��װ������׼����װ [name]��
PreviousInstallNotCompleted=��ǰ����İ�װ/ж����δ��ɡ�����Ҫ������������ɰ�װ��%n%n��������֮�����������а�װ������� [name] �İ�װ��
CannotContinue=��װ�򵼲��ܼ���������ȡ���˳���

; *** "Installing" wizard page
WizardInstalling=���ڰ�װ
InstallingLabel=������ļ�����а�װ [name]�����Ե�...

; *** "Setup Completed" wizard page
FinishedHeadingLabel=��� [name] ��װ
FinishedLabelNoIcons=��װ������� [name] �İ�װ��
FinishedLabel=��װ������� [name] �İ�װ������ͨ��ѡ���Ѱ�װ��ͼ��������Ӧ�ó���
ClickFinish=�������ɡ��˳���װ��
FinishedRestartLabel=Ϊ����� [name] �İ�װ����װ�򵼱����������ԡ���Ҫ����������
FinishedRestartMessage=Ϊ����� [name] �İ�װ����װ�򵼱����������ԡ�%n%n��Ҫ����������
ShowReadmeCheck=�ǣ���Ҫ�鿴�����ļ�
YesRadio=�ǣ�������������(&Y)
NoRadio=���Ժ���������(&N)
; used for example as 'Run MyProg.exe'
RunEntryExec=���� %1
; used for example as 'View Readme.txt'
RunEntryShellExec=�鿴 %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=��װ����Ҫ��һ������
SelectDiskLabel2=�������� %1 �������ȷ������%n%n����ڳ���������ʾ���ļ���������ļ������Ҳ����ô����ϵ��ļ�������������ȷ��·���������������
PathLabel=·��(&P)��
FileNotInDir2=�ļ���%1�����ڡ�%2���С��������ȷ�Ĵ��̻�ѡ�������ļ��С�
SelectDirectoryLabel=��ָ����һ�����̵�λ�á�

; *** Installation phase messages
SetupAborted=��װ��δ��ɡ�%n%n��������Ⲣ�������а�װ�򵼡�
EntryAbortRetryIgnore=��������ԡ����³��ԣ���������ԡ�����װ����������ֹ��ȡ��װ��

; *** Installation status messages
StatusCreateDirs=���ڴ����ļ���...
StatusExtractFiles=������ȡ�ļ�...
StatusCreateIcons=���ڴ�����ݷ�ʽ...
StatusCreateIniEntries=���ڴ��� INI ��Ŀ...
StatusCreateRegistryEntries=���ڴ���ע�����Ŀ...
StatusRegisterFiles=����ע���ļ�...
StatusSavingUninstall=���ڱ���ж����Ϣ...
StatusRunProgram=������ɰ�װ...
StatusRollback=���ڻع����...

; *** Misc. errors
ErrorInternal2=�ڲ�����%1
ErrorFunctionFailedNoCode=%1 ʧ��
ErrorFunctionFailed=%1 ʧ�ܡ����� %2
ErrorFunctionFailedWithMessage=%1 ʧ�ܡ����� %2��%n%3
ErrorExecutingProgram=�޷�ִ���ļ���%n%1

; *** Registry errors
ErrorRegOpenKey=��ע����ʱ���?%n%1\%2
ErrorRegCreateKey=����ע����ʱ���?%n%1\%2
ErrorRegWriteKey=д��ע����ʱ���?%n%1\%2

; *** INI errors
ErrorIniEntry=���ļ���%1���д��� INI ��Ŀʱ���?

; *** File copying errors
FileAbortRetryIgnore=��������ԡ����³��ԣ���������ԡ������ļ� (���Ƽ�)����������ֹ��ȡ��װ��
FileAbortRetryIgnore2=��������ԡ����³��ԣ���������ԡ�����װ (���Ƽ�)����������ֹ��ȡ��װ��
SourceIsCorrupted=Դ�ļ�����
SourceDoesntExist=Դ�ļ���%1��������
ExistingFileReadOnly=�����ļ�Ϊֻ����%n%n��������ԡ��Ƴ�ֻ�����Բ����ԣ���������ԡ������ļ�����������ֹ��ȡ��װ��
ErrorReadingExistingDest=��ȡ�����ļ�ʱ�������
FileExists=�ļ��Ѵ��ڡ�%n%n��Ҫ��������
ExistingFileNewer=�����ļ��Ȱ�װ��Ҫ��װ�Ļ��¡������㱣�������ļ���%n%n���������ļ���
ErrorChangingAttr=��������ļ�������ʱ�������
ErrorCreatingTemp=��Ŀ���ļ����д����ļ�ʱ�������
ErrorReadingSource=��ȡԴ�ļ�ʱ�������
ErrorCopying=�����ļ�ʱ�������
ErrorReplacingExistingFile=�滻�����ļ�ʱ�������
ErrorRestartReplace=�������滻ʧ�ܣ�
ErrorRenamingTemp=������Ŀ���ļ����е��ļ�ʱ�������
ErrorRegisterServer=�޷�ע�� DLL/OCX��%1
ErrorRegSvr32Failed=RegSvr32 ʧ�ܡ�����ֵ��%1
ErrorRegisterTypeLib=�޷�ע�����Ϳ⣺%1

; *** Post-installation errors
ErrorOpeningReadme=�������ļ�ʱ�������
ErrorRestartingComputer=��װ���޷��������ԡ����ֶ�������

; *** Uninstaller messages
UninstallNotFound=�ļ���%1�������ڡ�����ж�ء�
UninstallOpenError=�ļ���%1�����ܴ򿪡�����ж��
UninstallUnsupportedVer=ж����־�ļ���%1���ĸ�ʽ���ܱ��˰汾��ж�س���ʶ�𡣲���ж��
UninstallUnknownEntry=ж����־������һ��δ֪����Ŀ (%1)
ConfirmUninstall=���Ƿ�ȷ��Ҫ��ȫɾ�� %1 �������������
UninstallOnlyOnWin64=�˰�װֻ���� 64 λ Windows ��ж�ء�
OnlyAdminCanUninstall=�˰�װֻ���ɾ߱�����ԱȨ�޵��û�ж�ء�
UninstallStatusLabel=����ɾ�� %1�����Ե�...
UninstalledAll=%1 �ѳɹ�ɾ��
UninstalledMost=%1 ж����ɡ�%n%nĳЩ��Ŀ����ɾ������ֶ�ɾ��
UninstalledAndNeedsRestart=��Ҫ��� %1 ��ж�أ������������ԡ�%n%n��Ҫ����������
UninstallDataCorrupted=�ļ���%1�����𻵡�����ж��

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=ɾ�����ļ���
ConfirmDeleteSharedFile2=���й����ļ����ٱ��κγ���ʹ�á���Ҫɾ��ù����ļ���%n%n����г���ʹ�ø��ļ������ѱ�ɾ����Щ��������޷������С�����㲻ȷ��������ѡ�񡰷񡱡����¸��ļ������ϵͳ����κ�Σ����
SharedFileNameLabel=�ļ���
SharedFileLocationLabel=λ�ã�
WizardUninstalling=ж��״̬
StatusUninstalling=����ж�� %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 �汾 %2
AdditionalIcons=����ͼ�꣺
CreateDesktopIcon=��������ͼ��(&D)
CreateQuickLaunchIcon=��������������ͼ��(&Q)
ProgramOnTheWeb=%1 ��վ
UninstallProgram=ж�� %1
LaunchProgram=���� %1
AssocFileExtension=�� %1 �� %2 �ļ���չ�����(&A)
AssocingFileExtension=���ڽ� %1 �� %2 �ļ���չ�����...
