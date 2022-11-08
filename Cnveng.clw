!Clarion Template Chain Application Converter - Engine Implementation Module
!Copyright TopSpeed Corporation - All rights reserved
!Modified by RADFusion International, LLC for use in Clarion 7.x and beyond
!Open sourced by kind permission of Robert Zaunere
!In Memory of Russell Eggen
!-------------------------------------------------------------------------------------------------
! History 
!-------------------------------------------------------------------------------------------------
! 2022-11-07  C. Barnes     Change Help HLP to CHM. All HLP('~xxx') added '.htm' to open CHM topic.
! 2022-11-08  C. Barnes     Window Cosmetics - Change font to Microsoft Sans Serif - adjust controls
!-------------------------------------------------------------------------------------------------

                    PROGRAM

    INCLUDE('CNVENG.INC'),ONCE
    INCLUDE('ABPOPUP.INC'),ONCE                              !PopupManager from TopSpeed's ABC's
    INCLUDE('ABRESIZE.INC'),ONCE                             !Window Resizer from TopSpeed's ABC's
    INCLUDE('ABUTIL.INC'),ONCE                               !Utility module from TopSpeed's ABC's
    INCLUDE('ABWINDOW.INC'),ONCE                             !Window manager module from TopSpeed's ABC's
    INCLUDE('ABTOOLBA.INC'),ONCE                             !Window manager module from TopSpeed's ABC's
    INCLUDE('ABBROWSE.INC'),ONCE                             !Window manager module from TopSpeed's ABC's
    INCLUDE('ERRORS.CLW'),ONCE                               !Error code constants
    INCLUDE('KEYCODES.CLW'),ONCE                             !Keycode constants
    INCLUDE('CNVENG.TRN')                                    !Translation Constants

HANDLE              EQUATE(SIGNED)
NUMCOLORS           EQUATE(24)
RASTERCAPS          EQUATE(38)
RC_PALETTE          EQUATE(0100H)

                    MAP
    !Exported procedures
EngineStartup           PROCEDURE,NAME('EngineStartup')

                        MODULE('Runtime Library')
FreeLibrary                 PROCEDURE(UNSIGNED hInstance),NAME('Clw$FreeLibrary')
LoadLibrary                 PROCEDURE(*CSTRING LibraryName),UNSIGNED,RAW,NAME('Clw$LoadLibrary')
FnMerge                     PROCEDURE(*CSTRING Path,*CSTRING Drive,*CSTRING Directory,*CSTRING Name,*CSTRING Extension),RAW,NAME('_fnmerge')
FnSplit                     PROCEDURE(*CSTRING Path,*CSTRING Drive,*CSTRING Directory,*CSTRING Name,*CSTRING Extension),SHORT,RAW,PROC,NAME('_fnsplit')
DESTROY                     PROCEDURE(FILE,SIGNED=0),NAME('Cla$FILE_DESTROY')
                        END

                        MODULE('-- Windows API PROCEDUREs ---')                  !16-bit API protos removed
API32_GetTempPath           PROCEDURE(LONG dwBuffer,*CSTRING lpszTempPath),LONG,RAW,PASCAL,NAME('GetTempPathA')
GetDC                       PROCEDURE(HANDLE),HANDLE,PASCAL
ReleaseDC                   PROCEDURE(HANDLE, HANDLE),SIGNED,PASCAL,PROC
GetDeviceCaps               PROCEDURE(HANDLE, SIGNED),SIGNED,PASCAL
                        END

EngineShutDown          PROCEDURE()
ConfigAppNames          PROCEDURE(*CSTRING AppNameIn,*CSTRING AppNameOut,BYTE lType=0),BYTE,PRIVATE
ConfirmChanges          PROCEDURE(SectionClass SecQMgr,InfoTextClass Info,*CSTRING SectionHeader,USHORT XPos,USHORT YPos,USHORT Width,USHORT Height,BYTE ReselAllMode=False),BYTE,PRIVATE
CopyFile                PROCEDURE(STRING Src,STRING Dest),BYTE,PRIVATE
FileExists              PROCEDURE(*CSTRING Filename),BYTE,PRIVATE
FileExtension           PROCEDURE(STRING Filename),STRING,PRIVATE
GetBMPName              PROCEDURE(BYTE Number),STRING
GetTempPath             PROCEDURE(),STRING,PRIVATE
KeyboardBreak           PROCEDURE(),BYTE,PRIVATE
PerformConversion       PROCEDURE(ApplicationClass TXAQMgr,SectionClass SecQMgr,*STRING[] ComboUse,ProgressManagerClass ProgMgr,SHORT ProgressPromptFeq,SHORT PassStringFeq,SHORT SheetPageFeq),BYTE,PRIVATE
RegisterAddin           PROCEDURE(RuleClass AClass),PRIVATE
RemoveAmpersand         PROCEDURE(*CSTRING Txt),STRING,PRIVATE
SecQSetColor            PROCEDURE(SectionClass SELF,LONG FGNorm,LONG FGSelect,LONG BGNorm,LONG BGSelect),PRIVATE
SecQSetInLine           PROCEDURE(SectionClass SELF,LONG Pos,BYTE Force=False),PRIVATE
SecQSetOutLine          PROCEDURE(SectionClass SELF,LONG Pos,BYTE Force=False),PRIVATE
SecQSyncColors          PROCEDURE(SectionClass SELF,LONG StartPos,LONG EndPos),PRIVATE
SetProgressValue        PROCEDURE(ProgressManagerClass SELF,BYTE Value),PRIVATE
TXAQSetLine             PROCEDURE(ApplicationClass SELF,LONG Idx),PRIVATE
UserInterface           PROCEDURE(),BYTE,PRIVATE
YieldCheck              PROCEDURE(),BYTE,PRIVATE

                        INCLUDE('DDE.CLW'),ONCE                       !This won't work for C7 and later apps, must use TXA files
                    END

InAsciiFileName     STRING(File:MaxFilePath),AUTO
OutAsciiFileName    STRING(File:MaxFilePath),AUTO

LogFile             FILE,DRIVER('ASCII'),CREATE,NAME('DDELOG.TXT')  !DDE Log file
                        RECORD
Line                        STRING(128)
                        END
                    END

InAsciiFile         FILE,DRIVER('ASCII','/FILEBUFFERS=32 /QUICKSCAN=ON'),NAME(InAsciiFileName)          !Incoming TXA File
Record                  RECORD
Line                        STRING(MaxLineLen-1)
                        END
                    END

OutAsciiFile        FILE,DRIVER('ASCII','/FILEBUFFERS=8 /QUICKSCAN=ON'),NAME(OutAsciiFileName),CREATE  !Outgoing TXA File
Record                  RECORD
Line                        STRING(MaxLineLen-1)
                        END
                    END


Addins              AddinQueueType
AppNameIn           CSTRING(File:MaxFilePath),AUTO          !in comming application
AppNameOut          CSTRING(File:MaxFilePath),AUTO          !out going application
CurAddition         CSTRING(256)                            !last addition template section encountered
CurEmbed            CSTRING(256)                            !last embed section ecounters
CurProcedure        CSTRING(256)                            !last procedure section encountered
DDEMgr              DDEMgrClass
LocalErrorStatus    ErrorStatusClass,THREAD
LocalError          ErrorClass
GlobalErrors        &ErrorClass
HelpSystem          BYTE(False)                             !Set to true when Help Sytem enabled
INI                 ConvertorINIClass
InTXA               InTXAClass
OutTXA              OutTXAClass
Translator          TranslatorClass
Version             STRING('7.00')

    CODE

EngineStartup       PROCEDURE()
    CODE
        SETCURSOR(CURSOR:Wait)
       ! DB.Init('Convertor')
        
        LocalError.Init(LocalErrorStatus)
        IF GlobalErrors &= NULL
            GlobalErrors &= LocalError
        END
        INI.Init(GlobalErrors)
        Translator.Init
        IF UPPER(INI.TryFetch('Translation','ExtractText'))='YES' THEN Translator.ExtractText='CONVC55.TRX'.
        Translator.AddTranslation(LocalTRN)
        InTXA.Init(GlobalErrors)
        OutTXA.Init(GlobalErrors)
        DDEMgr.Init(GlobalErrors,0)
        INI.LoadRuleDlls()
        SETCURSOR()
                                             !DDE connection failure assumes C7 or better 
        IF UserInterface() = Level:Benign
            IF FileExtension(AppNameOut) = 'TXA'
                IF CopyFile(NAME(OutAsciiFile),AppNameOut) = Level:Notify 
                    ASSERT(False) 
                END 
            END 
        END
 
        EngineShutDown()
        RETURN

EngineShutDown      PROCEDURE()
    CODE
        INI.Kill()
        DDEMgr.Kill()
        OutTXA.Kill()
        InTXA.Kill()
        Translator.Kill()
        GlobalErrors.Kill()
        !DB.Kill()
        RETURN

!--- Local Procedures ------------------------------------------------------------------

UserInterface       PROCEDURE()
Progress                LONG
SecQMgr                 SectionClass
TXAQMgr                 ApplicationClass
SheetPage               USHORT(1)
InfoText                QUEUE
                            STRING(64)
                        END
Window                  WINDOW('Clarion Legacy to ABC Application Conversion Wizard'),MAX,AT(,,424,216),CENTER,ICON('CWCONV.ICO'),FONT('Microsoft Sans Serif', 8,, FONT:regular),GRAY,ALRT(MouseRight),PALETTE(256),IMM,RESIZE,HLP('~AppConv.htm')
                            PROGRESS, AT(76,199,151,13), USE(?ProgressBar), HIDE, RANGE(1,100)
                            STRING('x{23}'), AT(4,203,68,10), USE(?ProgressPrompt), HIDE, TRN, LEFT
                            IMAGE, AT(4,6,112,181), USE(?Image1), HIDE
                            PANEL, AT(124,25,289,2), USE(?Line1), BEVEL(0,0,0600H)
                            SHEET, AT(120,4,300,183), USE(SheetPage), WIZARD
                                TAB(''), USE(?Tab1), HLP('~WizTab1.htm')
                                    PROMPT('Welcome to the Clarion to ABC Application Conversion Wizard'), AT(128,13), USE(?String1), |
                                        FONT(, 8, COLOR:Navy, FONT:bold)
                                    PROMPT('The wizard will guide you through the process of converting an existing C6 or C7 application to use the ABC templates and base classes.'), AT(128,34,284,22), |
                                        USE(?String2), TRN
                                    PROMPT('The wizard will NOT destroy or invalidate the existing App, so you may repeat this process any number of times, or continue to use the existing App file with the Clarion template chain supplied with Clarion 6.x or 7.x.'), AT(128,56,284,30), |
                                        USE(?String6), TRN
                                    PROMPT('You may choose to convert from an C6 application (APP) or text application (TXA) file from C6 or C7. And may output directly to another application file or just generate a new TXA file for later import. <13,10><13,10>NOTE: If using C7, TXA is your only option.'), AT(128,86,284,46), |
                                        USE(?Prompt6), TRN
                                END
                                TAB(''), USE(?Tab2), HLP('~WizTab2.htm')
                                    STRING('Application Source and Destination Files'), AT(128,13), USE(?String9), FONT(, 8, COLOR:Navy, FONT:bold)
                                    PROMPT('&Source Application:'), AT(128,43), USE(?SourceAppPrompt)
                                    ENTRY(@S255), AT(208,40,188,12), USE(AppNameIn), UPR
                                    BUTTON('...'), AT(400,39,14,14), USE(?LookupSourceAppFile), TIP('Lookup source APP or TXA file')
                                    PROMPT('&Destination Application:'), AT(128,63), USE(?DestAppPrompt)
                                    ENTRY(@s255), AT(208,60,188,12), USE(AppNameOut), UPR
                                    BUTTON('...'), AT(400,59,14,13), USE(?LookupDestAppFile), TIP('Lookup destination APP or TXA file')
                                    PROMPT('You may choose to convert from an application (APP) or text application (TXA) file. And may output directly to another application file or just generate a new TXA file for later import. <13,10><13,10>NOTE: APP files require ClarionCL.exe from the EE edition.'), AT(128,86,284,46), |
                                        USE(?Files:Prompt1), TRN                                    
                                END
                                TAB(''), USE(?Tab3), HLP('~WizTab3.htm')
                                    STRING('Application Conversion Options'), AT(128,13,284,10), USE(?String11), FONT(, 8, COLOR:Navy, FONT:bold)
                                END
                                TAB(''), USE(?StartConversionTab), HLP('~WizStartConversionTab.htm')
                                    STRING('Start Conversion Process'), AT(128,13,284,10), USE(?String12), FONT(, 8, COLOR:Navy, FONT:bold)
                                    PROMPT('Pressing Proceed below starts the conversion process by applying the conversion rules. If you selected Manual for any rule you will be prompted to confirm the rules suggested changes before they are applied.'), AT(128,34,287,26), |
                                        USE(?Prompt7), TRN
                                    PROMPT('Any rules set to Automatic will apply their changes without further input.'), AT(128,65,223,13), |
                                        USE(?Prompt8), TRN
                                    PROMPT('Rules set to None will not be applied to this conversion process.'), AT(128,78,231,14), |
                                        USE(?Prompt9), TRN
                                END
                                TAB(''), USE(?ProcessingTab)
                                    PANEL, AT(164,82,212,26), USE(?Panel1), BEVEL(-2)
                                    STRING('Please wait, processing...'), AT(184,90,172,10), USE(?String15), FONT(,, COLOR:Navy, FONT:bold), |
                                        CENTER
                                END
                            END
                            STRING('xxxx'), AT(4,188,416,10), USE(?PassString), HIDE, TRN
                            STRING('xxx'), AT(326,188,92,10), USE(?VersionString), TRN, RIGHT
                            BUTTON('<< &Back'), AT(230,198,46,15), USE(?Previous), DISABLE, TIP('Previous Page')
                            BUTTON('&Next  >'), AT(276,198,46,15), USE(?Next), DEFAULT, TIP('Next Page')
                            BUTTON('&Cancel'), AT(326,198,46,15), USE(?Cancel), TIP('Exit conversion wizard')
                            BUTTON('&Help'), AT(374,198,46,15), USE(?Help), STD(STD:Help), TIP('Get Help')
                        END

ChangedPage             BYTE(False)
Completed               BYTE(False)
ComboUse                STRING(9),DIM(32),AUTO                  !Use variables for selection combos - NOTE LIMIT of 32 concurrent addins
ConfirmClose            BYTE(True)
i                       USHORT,AUTO
OwnerMap                OwnerMapType                            !Maps Owners <==> TabFeq's
ProgressMgr             ProgressManagerClass
PopupMgr                PopupClass
Resizer                 WindowResizeClass

ThisWindow              CLASS(WindowManager)
ControlsCreated             BYTE,PRIVATE
Init                        PROCEDURE(),BYTE,VIRTUAL
Kill                        PROCEDURE(),BYTE,VIRTUAL,PROC
TakeAccepted                PROCEDURE(),VIRTUAL,BYTE,PROC
TakeCloseEvent              PROCEDURE(),VIRTUAL,BYTE,PROC
TakeWindowEvent             PROCEDURE(),VIRTUAL,BYTE,PROC
                        END

    CODE
        SYSTEM{PROP:VScrollPos}=TRUE    !Proportional Thumb on scrollbar
        ThisWindow.Run()
        FREE(OwnerMap)
        RETURN CHOOSE(Completed=True,LEVEL:Benign,LEVEL:Notify)

AssignWindowSize    ROUTINE
    Addins.Mgr.WindowSize.XPos = 0{PROP:XPos} + ?SheetPage{PROP:XPos}
    Addins.Mgr.WindowSize.YPos = 0{PROP:YPos} + ?SheetPage{PROP:YPos} + 14
    Addins.Mgr.WindowSize.Width = ?SheetPage{PROP:Width} - 2
    Addins.Mgr.WindowSize.Height = ?SheetPage{PROP:Height} - 16

ThisWindow.Kill     PROCEDURE()
    CODE
        TXAQMgr.Kill()
        PopupMgr.Kill()
        SecQMgr.Kill()
        SETCURSOR()
        RETURN PARENT.Kill()

ThisWindow.Init     PROCEDURE()
XPos                    USHORT,AUTO
YPos                    USHORT,AUTO
BeforeINI               GROUP,AUTO
Width                       SHORT
Height                      SHORT
                        END
AfterINI                LIKE(BeforeINI),AUTO
ButtonXPos              USHORT,AUTO
ButtonYPos              USHORT,AUTO

    CODE
        IF PARENT.Init() THEN RETURN LEVEL:Notify.
        SELF.ControlsCreated = False
        OPEN(Window)
        SYSTEM{PROP:Icon} = '~C55CONV.ICO'
        SORT(Addins,Addins.Info.Owner,-Addins.Info.Priority)        !Sort highest priority first per owner
        ?VersionString{PROP:Text} = Translator.TranslateString('Version ') & Version
        ?Image1{PROP:Text} = GetBMPName(1)
        ?Image1{PROP:Centered} = True
        LOOP i = 1 TO RECORDS(Addins)
            GET(Addins,i)
            ASSERT(~ERRORCODE())
            OwnerMap.Owner = Addins.Info.Owner
            GET(OwnerMap,OwnerMap.Owner)
            IF ERRORCODE()
                OwnerMap.Owner = Addins.Info.Owner
                OwnerMap.TabID = OwnerTabFeq+RECORDS(OwnerMap)
                OwnerMap.ButtonId = OwnerButtonFeq+RECORDS(OwnerMap)
                OwnerMap.Combos = 0
                ADD(OwnerMap,OwnerMap.Owner)
                ASSERT(~ERRORCODE())
                CREATE(OwnerMap.TabId,CREATE:Tab,?SheetPage)
                OwnerMap.TabId{PROP:Hlp} = '~WizRulesTab.htm'
                CREATE(OwnerMap.ButtonId,CREATE:Button,?Tab3)
                OwnerMap.ButtonId{PROP:Text} = OwnerMap.Owner
                OwnerMap.ButtonId{PROP:Tip} = Translator.TranslateString('Configure the') & ' ' & OwnerMap.Owner & ' ' & Translator.TranslateString('rule family')
                IF RECORDS(OwnerMap) <= 8
                    ButtonXPos = 20
                    ButtonYPos = 30 + ((RECORDS(OwnerMap) - 1) * 18)
                ELSE
                    ButtonXPos = 160
                    ButtonYPos = 30 + ((RECORDS(OwnerMap) - 9) * 18)
                END
                SETPOSITION(OwnerMap.ButtonId,?SheetPage{PROP:XPos} + ButtonXPos,?SheetPage{PROP:YPos} + ButtonYPos,120)
                UNHIDE(OwnerMap.ButtonId)
                CREATE(OwnerTabTitle + RECORDS(OwnerMap),CREATE:String,OwnerMap.TabId)
                (OwnerTabTitle + RECORDS(OwnerMap)){PROP:Text} = Translator.TranslateString('Configure') & ' ' & OwnerMap.Owner & ' ' & Translator.TranslateString('Rule Family')
                SETPOSITION(OwnerTabTitle+RECORDS(OwnerMap),128,13)
                (OwnerTabTitle + RECORDS(OwnerMap)){PROP:Font,4} = FONT:Bold
                (OwnerTabTitle + RECORDS(OwnerMap)){PROP:Font,3} = 0800000H
                UNHIDE(OwnerTabTitle + RECORDS(OwnerMap))
            END
            Addins.TabFeq = OwnerMap.TabID
            DO AssignWindowSize
            OwnerMap.Combos += 1
            ASSERT(OwnerMap.Combos <= 18)
            CREATE(PromptFeq + i - 1,CREATE:Prompt,OwnerMap.TabID)
            (PromptFeq + i - 1){PROP:Text} = Addins.Info.PromptText
            CREATE(ComboFeq + i - 1,CREATE:DropCombo,OwnerMap.TabID)
            (ComboFeq + i - 1){PROP:ReadOnly} = True
            (ComboFeq + i - 1){PROP:Drop} = 3
            (ComboFeq + i - 1){PROP:From} = CHOOSE(Addins.Info.AllowAuto,'None|Manual|Automatic','None|Manual')
            (ComboFeq + i - 1){PROP:Use} = ComboUse[i]
            ComboUse[i] =  'Automatic'
            
            !ComboUse[i]=INI.TryFetch('Selections',Addins.Info.Name)
            IF ~ComboUse[i] THEN ComboUse[i]='Manual'.
            
            Addins.ComboUseIdx = i
            PUT(Addins)
            ASSERT(~ERRORCODE())
            PUT(OwnerMap)
            ASSERT(~ERRORCODE())
            IF OwnerMap.Combos <= 9
                XPos = 128
                YPos = 38 + ((OwnerMap.Combos - 1) * 16)
            ELSE
                XPos = 276
                YPos = 38 + ((OwnerMap.Combos - 10) * 16)
            END
            
            SETPOSITION(PromptFeq + i - 1,XPos,YPos)
            UNHIDE(PromptFeq + i - 1)
           ! UPDATE(ComboFeq + i - 1)
            
            SETPOSITION(ComboFeq + i - 1,XPos + 70,YPos - 5,55,12)
            UNHIDE(ComboFeq + i - 1)
            
        END
        Resizer.Init(AppStrategy:Surface,Resize:SetMinSize)
        Resizer.SetStrategy(?Line1,Resize:LockXPos+Resize:LockYPos,Resize:LockHeight+Resize:ConstantRight)
        Resizer.SetStrategy(?Line1,?AppNameIn)
        Resizer.SetStrategy(?Line1,?AppNameOut)
        Resizer.SetStrategy(?Image1,Resize:LockXpos+Resize:LockYPos,Resize:LockWidth+Resize:ConstantBottom)
        Resizer.SetStrategy(?String9,Resize:FixTop+Resize:FixLeft,Resize:LockWidth+Resize:LockHeight)
        Resizer.SetStrategy(?String9,?String11)
        Resizer.SetStrategy(?String9,?String12)
        Resizer.SetStrategy(?ProgressPrompt,Resize:FixLeft+Resize:FixBottom,Resize:LockWidth+Resize:LockHeight)
        Resizer.SetStrategy(?ProgressPrompt,?PassString)
        Resizer.SetStrategy(?Panel1,Resize:FixXCenter+Resize:FixYCenter,Resize:LockWidth+Resize:LockHeight)
        Resizer.SetStrategy(?Panel1,?String15)
        Resizer.SetStrategy(?ProgressBar,Resize:FixLeft+Resize:FixBottom,Resize:LockHeight+Resize:ConstantRight)
        Resizer.SetStrategy(?VersionString,Resize:FixRight+Resize:FixBottom,Resize:LockWidth+Resize:LockHeight)
  !Resize startegies for dynamically created controls
        LOOP i = 1 TO RECORDS(Addins)
            Resizer.SetStrategy(PromptFeq+i-1,Resize:LockXPos+Resize:LockYPos,Resize:LockWidth+Resize:LockHeight)
            Resizer.SetStrategy(PromptFeq+i-1,ComboFeq+i-1)
        END
        LOOP i = 1 TO RECORDS(OwnerMap)
            GET(OwnerMap,i)
            ASSERT(~ERRORCODE())
            Resizer.SetStrategy(OwnerMap.ButtonID,Resize:LockXPos+Resize:LockYPos,Resize:LockWidth+Resize:LockHeight)
            Resizer.SetStrategy(OwnerMap.ButtonID,OwnerTabTitle+i)
        END
        PopupMgr.Init(INI)
        PopupMgr.SetTranslator(Translator)
        INI.Fetch('MainWindow',WINDOW)
        UNHIDE(?Image1)
        TXAQMgr.Init(GlobalErrors)
        SecQMgr.Init(GlobalErrors)
        ProgressMgr.Init(?ProgressBar)
        Translator.TranslateWindow
        INI.TryFetchAppNames(AppNameIn,AppNameOut)
        DISPLAY(?AppNameIn)
        DISPLAY(?AppNameOut)
        SELF.ControlsCreated = True
        DISABLE(?Previous)
        IF ~HelpSystem THEN DISABLE(?Help).
        Resizer.SetParentDefaults()
        SELF.AddItem(Resizer)
        RETURN LEVEL:Benign

ThisWindow.TakeCloseEvent   PROCEDURE()
    CODE
        IF PARENT.TakeCloseEvent() THEN RETURN LEVEL:Notify.
        IF ConfirmClose AND MESSAGE(Translator.TranslateString('Are you sure that you wish to quit?'),Translator.TranslateString('Confirm Exit'),ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:Yes) = BUTTON:No
            RETURN LEVEL:Notify
        END
        INI.Update('MainWindow',Window)
        RETURN LEVEL:Benign

ThisWindow.TakeWindowEvent  PROCEDURE()
ModeStr                         CSTRING(10),AUTO

    CODE
        CASE EVENT()
        OF EVENT:Sized
            IF PARENT.TakeWindowEvent() THEN RETURN LEVEL:Notify.
            LOOP i = 1 TO RECORDS(Addins)
                GET(Addins,i)
                ASSERT(~ERRORCODE())
                DO AssignWindowSize
                PUT(Addins)
                ASSERT(~ERRORCODE())
            END
        OF EVENT:AlertKey
            IF PARENT.TakeWindowEvent() THEN RETURN LEVEL:Notify.
            IF KEYCODE() = MouseRight AND INRANGE(MOUSEX() - ?SheetPage{PROP:XPos},0,?SheetPage{PROP:Width}) AND INRANGE(MOUSEY() - ?SheetPage{PROP:YPos},0,?SheetPage{PROP:Height}) AND (SheetPage > 5 OR SheetPage = 3)
                PopupMgr.AddMenu('All Manual|All Automatic|All None')
                IF PopupMgr.Ask()
                    CASE PopupMgr.GetLastSelection()
                    OF 'AllManual'
                        ModeStr = 'Manual'
                    OF 'AllAutomatic'
                        ModeStr = 'Automatic'
                    OF 'AllNone'
                        ModeStr = 'None'
                    ELSE
                        ASSERT(False,'Bad value returned by Popup.Ask()')             !Bad value retuned by popup.ask()
                    END
                    LOOP i = 1 TO RECORDS(Addins)
                        GET(Addins,i)
                        ASSERT(~ERRORCODE())
                        IF SheetPage > 5
                            IF ~Addins.Info.AllowAuto AND ModeStr = 'Automatic'
                                IF Addins.TabFeq = OwnerMap.TabID THEN ComboUse[i] = 'Manual'.
                            ELSE
                                IF Addins.TabFeq=OwnerMap.TabID THEN ComboUse[i] = ModeStr.
                            END
                        ELSIF SheetPage = 3
                            IF ~Addins.Info.AllowAuto AND ModeStr = 'Automatic'
                                ComboUse[i] = 'Manual'
                            ELSE
                                ComboUse[i] = ModeStr
                            END
                        END
                    END
                    DISPLAY()
                END
            END
        OF EVENT:GainFocus
            IF SELF.ControlsCreated                                     !only process GainFocus Events after dynamic controls have been created
                IF PARENT.TakeWindowEvent() THEN RETURN Level:Notify.
            END
        ELSE
            IF PARENT.TakeWindowEvent() THEN RETURN Level:Notify.
        END
        RETURN LEVEL:Benign

ThisWindow.TakeAccepted     PROCEDURE()
Fnd                             BYTE(False)

    CODE
        CASE ACCEPTED()
        OF ?AppNameIn
            AppNameIn = LONGPATH(SHORTPATH(AppNameIn))
            DISPLAY(?)
        OF ?AppNameOut
            AppNameOut = LONGPATH(SHORTPATH(AppNameOut))
            DISPLAY(?)
        OF OwnerButtonFeq TO OwnerButtonFeq + RECORDS(OwnerMap)
            OwnerMap.ButtonID = ACCEPTED()
            GET(OwnerMap,OwnerMap.ButtonId)
            ASSERT(~ERRORCODE())
            UNHIDE(OwnerMap.TabId)
            HIDE(?Tab1)
            HIDE(?Tab2)
            HIDE(?Tab3)
            HIDE(?StartConversionTab)
            HIDE(?Previous)
            HIDE(?Next)
            SELECT(OwnerMap.TabID)
            ?Cancel{PROP:Text} = Translator.TranslateString('&Ok')
            ?Cancel{PROP:Tip} = Translator.TranslateString('Confirm rule setting Ok')
        OF ?Next
            DO ValidateSheet
            ChangedPage = True
            SheetPage += 1
            DISPLAY(?SheetPage)
            IF SheetPage = 5
                HIDE(?Line1)
                DO ConfirmAppNameIn
                DO SetPage5
                DO Conversion
                HIDE(?PassString)
                ?ProgressPrompt{PROP:Text} = Translator.TranslateString(CHOOSE(FileExtension(AppNameOut)='APP','Creating New APP:','Creating New TXA:'))
                Completed = CHOOSE(OutTXA.WriteFile(TXAQMgr,ProgressMgr) = LEVEL:Benign,True,False)
                ConfirmClose = False
                POST(EVENT:CloseWindow)
            END
            DO UpdateOnNewPage
        OF ?Previous
            ChangedPage = True
            SheetPage -= 1
            DISPLAY(?SheetPage)
            DO UpdateOnNewPage
        OF ?Cancel
            IF SheetPage > 5
                HIDE(OwnerMap.TabID)
                UNHIDE(?Tab1)
                UNHIDE(?Tab2)
                UNHIDE(?Tab3)
                UNHIDE(?StartConversionTab)
                UNHIDE(?Previous)
                UNHIDE(?Next)
                ?Cancel{PROP:Text} = Translator.TranslateString('&Cancel')
                ?Cancel{PROP:Tip} = Translator.TranslateString('Exit conversion wizard')
                SELECT(?Tab3)
            ELSE
                ConfirmClose = ChangedPage
                POST(EVENT:CloseWindow)
            END
        OF ?LookupSourceAppFile
            IF ConfigAppNames(AppNameIn,AppNameOut,0) = LEVEL:Notify
                DISPLAY(?AppNameIn)
                DISPLAY(?AppNameOut)
            END
        OF ?LookupDestAppFile
            IF ConfigAppNames(AppNameIn,AppNameOut,1) = LEVEL:Notify THEN DISPLAY(?AppNameOut).
        END
        RETURN PARENT.TakeAccepted()

UpdateOnNewPage     ROUTINE
    IF INRANGE(SheetPage,1,5) THEN ?Image1{PROP:Text} = GetBMPName(SheetPage).
    ?Image1{PROP:Centered} = True
    ?Previous{PROP:Disable} = CHOOSE(SheetPage = 1,True,False)
    ?Next{PROP:Text} = CHOOSE(SheetPage = 4,Translator.TranslateString('&Proceed'),Translator.TranslateString('&Next  >'))
    ?Next{PROP:ToolTip} = CHOOSE(SheetPage = 4,Translator.TranslateString('Proceed with application conversion'),Translator.TranslateString('Next Page'))
    DISPLAY(?Previous,?Next)

ValidateSheet       ROUTINE
    IF INLIST(EVENT(),EVENT:NewSelection,EVENT:Accepted)
        IF SheetPage = 2
            IF ~AppNameIn
                GlobalErrors.TakeError(Msg:SourceNameReq)
                SELECT(?AppNameIn)
                RETURN LEVEL:Notify
            END
            IF ~FileExists(AppNameIn)
                GlobalErrors.SetFile(AppNameIn)
                GlobalErrors.TakeError(Msg:FileNotFound)
                SELECT(?AppNameIn)
                RETURN LEVEL:Notify
            END
            IF ~AppNameOut
                GlobalErrors.TakeError(Msg:DestNameReq)
                SELECT(?AppNameOut)
                RETURN LEVEL:Notify
            END
            IF ~(FileExtension(AppNameOut) = 'APP' OR FileExtension(AppNameOut) = 'TXA')
                GlobalErrors.TakeError(Msg:OutputAPPorTXA)
                SELECT(?AppNameOut)
                RETURN LEVEL:Notify
            END
            IF UPPER(AppNameOut) = UPPER(AppNameIn)
                GlobalErrors.TakeError(Msg:SrcAndDestSame)
                SELECT(?AppNameOut)
                RETURN LEVEL:Notify
            END
            IF FileExists(AppNameOut) AND MESSAGE(Translator.TranslateString('Destination file already exists.|Do you want to replace it?'),Translator.TranslateString('File Exists'),ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No)=BUTTON:No
                RETURN LEVEL:Notify
            END
            INI.UpdateAppNames(AppNameIn,AppNameOut)
        END
    END

Conversion          ROUTINE
    SORT(Addins,-Addins.Info.Priority)
    CASE PerformConversion(TXAQMgr,SecQMgr,ComboUse,ProgressMgr,?ProgressPrompt,?PassString,?SheetPage)
    OF LEVEL:Notify
    OROF LEVEL:Cancel
        SheetPage = 3
        ChangedPage = True
        DISPLAY(?SheetPage)
        HIDE(?ProgressPrompt)
        HIDE(?ProgressBar)
        HIDE(?PassString)
        UNHIDE(?Previous,?Help)
        UNHIDE(?Line1)
        SORT(Addins,Addins.Info.Owner,-Addins.Info.Priority)
        RETURN LEVEL:Notify
    END
    SORT(Addins,Addins.Info.Owner,-Addins.Info.Priority)

ConfirmAppNameIn    ROUTINE
    HIDE(?Previous,?Help)
    IF FileExtension(AppNameIn) = 'APP'
        IF DDEMgr.ExportTXA(AppNameIn) = LEVEL:Notify
            SELECT(?AppNameIn)
            DO ReselectPage2
            RETURN LEVEL:Notify
        END
    ELSIF CopyFile(AppNameIn,GetTempPath() & 'INTXA.$$$') = Level:Notify
        GlobalErrors.TakeError(Msg:BadTXAOpen)
        SELECT(?AppNameIn)
        DO ReselectPage2
        RETURN LEVEL:Notify
    END

ReselectPage2       ROUTINE
    SheetPage = 2
    ChangedPage = True
    DISPLAY(?SheetPage)
    HIDE(?ProgressPrompt)
    HIDE(?ProgressBar)
    UNHIDE(?Previous,?Help)
    DO UpdateOnNewPage

SetPage5            ROUTINE
    SheetPage = 5
    DISPLAY(?SheetPage)
    ?ProgressPrompt{PROP:Text} = Translator.TranslateString('Reading Source File:')
    ?ProgressBar{PROP:Width} = 0{PROP:Width} - (?ProgressBar{PROP:XPos} + 2)
    UNHIDE(?ProgressPrompt)
    UNHIDE(?ProgressBar)
    CASE InTXA.ReadFile(TXAQMgr,ProgressMgr)
    OF LEVEL:Cancel
        InTxa.Close()
        SheetPage = 3
        HIDE(?ProgressPrompt)
        HIDE(?ProgressBar)
        UNHIDE(?Previous,?Help)
        DO UpdateOnNewPage
        DISPLAY()
        SETCURSOR()
    OF LEVEL:Notify
        InTXA.Close()
        SETCURSOR()
        DO ReselectPage2
        RETURN LEVEL:Notify
    END

PerformConversion   PROCEDURE(ApplicationClass TXAQMgr,SectionClass SecQMgr,*STRING[] ComboUse,ProgressManagerClass ProgressMgr,SHORT ProgressPromptFeq,SHORT PassStringFeq,SHORT SheetPageFeq)
b                       BYTE,AUTO
cStr                    CSTRING(MaxLineLen),AUTO
EndBlock                LONG,AUTO
i                       LONG,AUTO
PassCnt                 BYTE
PassInc                 BYTE
Progress                LONG,AUTO
pText                   LIKE(AddinQueueType.Info.PromptText),AUTO
ResetAllMode            BYTE,AUTO
RVal                    BYTE(Level:Benign)
SectionResponse         BYTE(Level:Benign)
StartBlock              LONG,AUTO
TerminateAll            BYTE(False)
tLimit                  LONG,AUTO
InfoTextMgr             InfoTextClass
ThisSection             CSTRING(MaxLineLen),AUTO
FileList                FileQueue
ProcedureQueue          ProcedureList                           !queue of procedures and template additions with instance numbers

    CODE
        PassStringFeq{PROP:Text} = ''
        UNHIDE(PassStringFeq)
        InfoTextMgr.Init(GlobalErrors)
        IF TXAQMgr.PreProcess(ProcedureQueue,FileList,ProgressMgr,ProgressPromptFeq)
            DO TidyUp
            RETURN LEVEL:Cancel
        ELSE
            SETCURSOR(CURSOR:Wait)
            LOOP b = 1 TO RECORDS(Addins)
                GET(Addins,b)
                ASSERT(~ERRORCODE(),'Error: ' & Error() & ' reading Addins Queue')
                IF ComboUse[Addins.ComboUseIdx] NOT = 'None' THEN PassCnt += 1.
                INI.Update('Selections',Addins.Info.Name,ComboUse[Addins.ComboUseIdx])
            END
            LOOP b = 1 TO RECORDS(Addins)
                GET(Addins,b)
                ASSERT(~ERRORCODE(),'Error: ' & Error() & ' reading Addins Queue')
                IF YieldCheck() THEN YIELD.
                Addins.Mgr.ProcedureQueue &= ProcedureQueue
                Addins.Mgr.FileList &= FileList
                ResetAllMode = True
                ProgressMgr.Reset()
                Progress = 1
                IF CLIP(ComboUse[Addins.ComboUseIdx]) NOT = 'None'
                    PassInc += 1
                    PassStringFeq{PROP:Text} = Translator.TranslateString('Pass') & ' ' & PassInc & ' ' & Translator.TranslateString('of') & ' ' & PassCnt & '  -  ' & Addins.Info.Name & ' (' & Addins.Info.Owner & ')'
                    Addins.Mgr.BeforeConversion()
                    ProgressPromptFeq{PROP:Text} = RemoveAmpersand(Addins.Info.PromptText)
                    DISPLAY(ProgressPromptFeq)
                    DO ThisRule
                    IF SectionResponse=Level:Fatal              !Terminate processing on Level:Fatal return
                        RVal = LEVEL:Cancel
                        BREAK
                    END
                    IF TXAQMgr.BuildPositionals(ProgressMgr)
                        DO TidyUp
                        RETURN LEVEL:Cancel
                    END
                    ProgressMgr.Complete()
                END
                Addins.Mgr.AfterConversion()
            WHILE TerminateAll = False                            !end addins LOOP
        END
        DO TidyUp
        RETURN RVal

TidyUp              ROUTINE
    LOOP i=1 TO RECORDS(ProcedureQueue)                 !Kill procedure queue @ end of conversion
        GET(ProcedureQueue,i)
        ASSERT(~ERRORCODE())
        DISPOSE(ProcedureQueue.Calls)
        DISPOSE(ProcedureQueue.Additions)
    END
    FREE(ProcedureQueue)
    LOOP i=1 TO RECORDS(FileList)
        GET(FileList,i)
        ASSERT(~ERRORCODE())
        DISPOSE(FileList.Keys)
    END
    FREE(FileList)
    InfoTextMgr.Kill
    SecQMgr.ClearAll
    SETCURSOR

ThisRule            ROUTINE
    DATA
cDefinition     CSTRING('[DEFINITION]')
InsideDefinition        BYTE(False)
InsideSection   BYTE(False)
    CODE
        tLimit=TXAQMgr.GetMaxLineNo()
        ProgressMgr.SetTriggers(0,tLimit,50,1)          !set triggers for 1st half of process
        StartBlock=0
        EndBlock=0
        CLEAR(ThisSection)
        Progress=1
        LOOP
            IF YieldCheck() THEN YIELD.
            IF KeyboardBreak()
                SectionResponse=Level:Fatal
                EXIT
            END
            ProgressMgr.Update(Progress)
            DO UpdateSymbols
            IF TXAQMgr.IsBraced(Progress)
                IF InsideDefinition AND TXAQMgr.IsEND(Progress) THEN InsideDefinition=False.
                IF TXAQMgr.IsHeader(Progress,cDefinition) THEN InsideDefinition=True.
                IF InsideSection
                    ASSERT(LEN(ThisSection)>0)
                    IF TXAQMgr.IsTerminator(ThisSection,Progress)
                        IF (ThisSection='[PROCEDURE]' AND ~InsideDefinition) OR ThisSection NOT='[PROCEDURE]'
                            ASSERT(StartBlock>0)
                            EndBlock=Progress-1
                            IF EndBlock<StartBlock THEN EndBlock=StartBlock.
                            DO ProcessSection
                            IF INLIST(SectionResponse,Level:Notify,Level:Fatal) THEN BREAK.
                            InsideSection=False
                            Progress=StartBlock-1
                            StartBlock=0
                        ELSE
                            TXAQMgr.AppendToSectionQ(Progress,SecQMgr)
                        END
                    ELSE
                        TXAQMgr.AppendToSectionQ(Progress,SecQMgr)
                    END
                ELSIF TXAQMgr.IsHeader(Progress,Addins.Info.Sections,ThisSection)
                    IF (ThisSection='[PROCEDURE]' AND ~InsideDefinition) OR ThisSection NOT='[PROCEDURE]'
                        SecQMgr.ClearAll
                        ASSERT(ThisSection)
                        InsideSection=True
                        StartBlock=Progress+1
                        EndBlock=0
                    END
                END
            ELSIF InsideSection
                TXAQMgr.AppendToSectionQ(Progress,SecQMgr)
            END
            Progress+=1
            IF Progress>tLimit
                IF InsideSection
                    ASSERT(StartBlock>0)
                    EndBlock=Progress-1
                    DO ProcessSection
                END
                BREAK
            END
        END
        ProgressMgr.SetTriggers(0,tLimit,49,51)         !Set triggers for 2nd half of process
        SecQMgr.ClearAll

ProcessSection      ROUTINE
    IF SecQMgr.GetLineCount() OR Addins.Info.EmptySections
        SectionResponse=Addins.Mgr.TakeSection(SecQMgr,InfoTextMgr,ThisSection)
!TakeSection() Return values  ->  Level:Benign, commit changes in OutQ
!                                 Level:Notify, commit changes in OutQ do not make any further passes
!                                 Level:Fatal,  terminate conversion completely
!                                 Level:Cancel, keep original section ignor OutQ completely
        IF SectionResponse=Level:Fatal THEN EXIT.
        IF SecQMgr.Touched() AND (SectionResponse=Level:Benign OR SectionResponse=Level:Notify)
            DO ProcessActions
        END
    ELSE
        SectionResponse=Level:Cancel
    END

ProcessActions      ROUTINE
    DATA
r       BYTE,AUTO
    CODE
        IF CLIP(ComboUse[Addins.ComboUseIdx])='Manual' OR (CLIP(ComboUse[Addins.ComboUseIdx])='Automatic' AND ~Addins.Info.AllowAuto)
            SETCURSOR
            r=ConfirmChanges(SecQMgr,InfoTextMgr,ThisSection,0{PROP:XPos}+SheetPageFeq{PROP:XPos},0{PROP:YPos}+SheetPageFeq{PROP:YPos},SheetPageFeq{PROP:Width},SheetPageFeq{PROP:Height},ResetAllMode)
            SETCURSOR(CURSOR:Wait)
            ResetAllMode=False
            CASE r
            OF Action:Cancel
                EXIT
            OF Action:Apply

            OF Action:OmitCode
                SecQMgr.PrependLine('OMIT(''_ConverterOmittedCode_'')')
                SecQMgr.AppendLine('_ConverterOmittedCode_')
            OF Action:UnCompile
                SecQMgr.PrependLine('*** '&Translator.TranslateString('Code made UnCompilable by Convertor')&' ***')
            OF Action:AssertFail
                SecQMgr.PrependLine('ASSERT(False)            !'&Translator.TranslateString('Assertion Failure added by Convertor'))
            OF Action:Abort
                SectionResponse=Level:Fatal
                EXIT
            END
        END
        tLimit=TXAQMgr.ReplaceBlock(StartBlock,EndBlock,SecQMgr)

UpdateSymbols       ROUTINE
    DATA
cAddition       CSTRING('[ADDITION]')
cProcedure      CSTRING('[PROCEDURE]')
    CODE
        TXAQMgr.GetLine(Progress,cStr)
        IF SUB(cStr,1,6)='EMBED '
            CurEmbed=cStr[7 : LEN(cStr)]
        ELSIF TXAQMgr.IsHeader(Progress,cAddition)
            TXAQMgr.GetLine(Progress+1,cStr)
            IF SUB(cStr,1,5)='NAME '
                CurAddition=cStr[6 : LEN(cStr)]
            ELSE
                ASSERT(False)
            END
        ELSIF TXAQMgr.IsHeader(Progress,cProcedure)
            TXAQMgr.GetLine(Progress+1,cStr)
            IF SUB(cStr,1,5)='NAME ' THEN CurProcedure=cStr[6 : LEN(cStr)].
        END

ConfirmChanges      PROCEDURE(SectionClass SecQMgr,InfoTextClass Info,*CSTRING SectionHeader,USHORT XPos,USHORT YPos,USHORT Width,USHORT Height,BYTE ResetAllMode=False)
AllAction               BYTE(255),STATIC
AllMode                 BYTE(False),STATIC
AllCheck                BYTE(False)
EditStr                 CSTRING(MaxLineLen),AUTO
EditStrBck              LIKE(EditStr),AUTO

window                  WINDOW('Confirm Conversion'),AT(,,300,183),FONT('Microsoft Sans Serif',8,,FONT:regular),IMM,HLP('~ConfirmWindow.htm'), |
                            GRAY,MAX,RESIZE
                            LIST,AT(2,2,146,103),USE(?InList),VSCROLL,COLOR(COLOR:White),FORMAT('23L(2)|~Line~L(1)@s5@E(00H,,00H,0FFFFH)255L(2)|*~Original Code:~L(1)S(255)@S255@'), |
                                FROM(SecQMgr.InQ),FONT('Consolas')
                            LIST,AT(154,2,146,103),USE(?OutList),VSCROLL,COLOR(COLOR:White),ALRT(MouseLeft2),ALRT(MouseRight), |
                                ALRT(InsertKey),ALRT(DeleteKey),ALRT(F2Key),ALRT(Alt2),FORMAT('23L(2)|~Line~L(1)@s5@E(00H,,00H,0FFFFH)255L(2)|*~Suggested New Code:~L(1)S(255)@' &|
                                'S255@'),FROM(SecQMgr.OutQ),FONT('Consolas')
                            LIST,AT(2,121,297,43),USE(?Notes),VSCROLL,FONT(,,COLOR:Navy,),COLOR(COLOR:White,COLOR:Navy,COLOR:Yellow), |
                                FROM(Info.Q.TextLine),GRID(COLOR:White)
                            ENTRY(@s255),AT(264,0,33,12),USE(EditStr),HIDE,ALRT(EnterKey),ALRT(CtrlA),ALRT(MouseRight)
                            STRING(''),AT(2,108,293,10),USE(?InfoLine),TRN,LEFT
                            CHECK('A&ll'),AT(2,172,32,10),USE(AllCheck),RIGHT,TIP('Apply next button selection automatically to ALL other sections')
                            BUTTON('A&ssertion'),AT(36,168,36,14),USE(?Assertion),HIDE,TIP('Insert an assertion failure into the embde code')
                            BUTTON('&UnComp'),AT(74,168,36,14),USE(?UnCompile),HIDE,TIP('Make embed code uncompilable')
                            BUTTON('&Omit'),AT(112,168,36,14),USE(?Omit),HIDE,TIP('Omit the embed code using OMIT() statement')
                            BUTTON('&Apply'),AT(150,168,36,14),USE(?Apply),HIDE,TIP('Apply the suggested changes'),DEFAULT
                            BUTTON('I&gnore'),AT(188,168,36,14),USE(?Ignore),TIP('Ignore the suggested changes')
                            BUTTON('A&bort'),AT(226,168,36,14),USE(?Abort),TIP('Abort the conversion process')
                            BUTTON('&Help'),AT(264,168,36,14),USE(?Help),STD(STD:Help)
                        END

Popup                   PopupClass
Resizer                 WindowResizeClass
RVal                    BYTE(Action:Cancel)
ThisWindow              CLASS(WindowManager)
Ask                         PROCEDURE,VIRTUAL
Init                        PROCEDURE,BYTE,VIRTUAL
Kill                        PROCEDURE,BYTE,VIRTUAL,PROC
TakeEvent                   PROCEDURE,VIRTUAL,BYTE
TakeFieldEvent              PROCEDURE,VIRTUAL,BYTE
TakeAccepted                PROCEDURE,VIRTUAL,BYTE,PROC
                        END

    CODE
        IF ResetAllMode
            AllMode = False
            AllAction = 255
        ELSIF AllMode
            ASSERT(AllAction NOT = 255)                         !Check for bad AllAction value
            RETURN AllAction
        END
        ThisWindow.Run()
        RETURN RVal

ThisWindow.Init     PROCEDURE()
i                       LONG,AUTO

    CODE
        IF PARENT.Init() THEN RETURN LEVEL:Notify.
        OPEN(Window)
        Resizer.Init(AppStrategy:Surface)
        Resizer.SetParentDefaults
        Resizer.SetStrategy(?InList,Resize:LockXPos+Resize:LockYPos,Resize:ConstantBottom)
        Resizer.SetStrategy(?OutList,Resize:LockYPos,Resize:ConstantBottom)
        Resizer.SetStrategy(?Notes,Resize:FixBottom+Resize:LockXPos,Resize:LockHeight+Resize:ConstantRight)
        Resizer.SetStrategy(?Notes,?InfoLine)
        Resizer.SetStrategy(?AllCheck,Resize:FixLeft+Resize:FixBottom,Resize:LockHeight+Resize:LockWidth)
        Resizer.SetStrategy(?Ignore,Resize:FixRight+Resize:FixBottom,Resize:LockHeight+Resize:LockWidth)
        Resizer.SetStrategy(?Ignore,?Apply)
        Resizer.SetStrategy(?Ignore,?Omit)
        Resizer.SetStrategy(?Ignore,?UnCompile)
        Resizer.SetStrategy(?Ignore,?Assertion)
        Resizer.SetStrategy(?Ignore,?Help)
        SETPOSITION(0,XPos,YPos + 14,Width - 2,Height - 16)
        0{PROP:MinWidth} = 0{PROP:Width}
        0{PROP:MinHeight} = 0{PROP:Height}
        INI.Fetch('ConfirmWindow',Window)
        ThisWindow.AddItem(Resizer)
        IF ~HelpSystem THEN DISABLE(?Help).
        Window{PROP:Text} = Window{PROP:Text} & ' - ' & Addins.Info.Name
        IF Addins.Mgr.CurrentProcedure()
            ?InfoLine{PROP:Text} = Translator.TranslateString('Current Procedure:') & ' ' & Addins.Mgr.CurrentProcedure()
        END
        Info.ListControl = ?Notes
        ?InList{PROPLIST:Header,2} = Translator.TranslateString(CHOOSE(SectionHeader='[SOURCE]','Original Clarion Source Code','Original TXA Source Code'))
        ?OutList{PROPLIST:Header,2}=Translator.TranslateString(CHOOSE(SectionHeader='[SOURCE]','Suggested Clarion Source Code','Suggested TXA Source Code'))
        Popup.Init()
        Popup.SetTranslator(Translator)
        Popup.AddMenu('Insert Line|Delete Line|-|Edit Line|-|~Restore Line|-|Top|Page Up|Page Down|Bottom')
        ?Assertion{PROP:Hide} = CHOOSE(BAND(Addins.Mgr.Buttons,Action:AssertFail) > 0,False,True)
        ?UnCompile{PROP:Hide} = CHOOSE(BAND(Addins.Mgr.Buttons,Action:UnCompile) > 0,False,True)
        ?Omit{PROP:Hide} = CHOOSE(BAND(Addins.Mgr.Buttons,Action:OmitCode) > 0,False,True)
        ?Apply{PROP:Hide} = CHOOSE(BAND(Addins.Mgr.Buttons,Action:Apply) > 0,False,True)
        ?OutList{Prop:Selected} = 1
        ?InList{PROP:Selected} = 1
        SELECT(?OutList)
        Translator.TranslateWindow()
        LOOP i = 1 TO RECORDS(SecQMgr.OutQ)
            GET(SecQMgr.InQ,i)
            ASSERT(~ERRORCODE(),'Error reading InQ')
            GET(SecQMgr.OutQ,i)
            ASSERT(~ERRORCODE(),'Error reading OutQ')
            IF SecQMgr.OutQ.Line <> SECQMgr.InQ.Line
                ?InList{PROP:Selected} = i
                ?OutList{PROP:Selected} = i
                BREAK
            END
        END
        RETURN LEVEL:Benign

ThisWindow.Kill     PROCEDURE()
    CODE
        Popup.Kill()
        RETURN PARENT.Kill()

ThisWindow.Ask      PROCEDURE()
    CODE
        PARENT.Ask()
        INI.Update('ConfirmWindow',Window)
        AllAction = RVal
        IF AllCheck THEN AllMode = True.

ThisWindow.TakeAccepted     PROCEDURE()
    CODE
        CASE ACCEPTED()
        OF ?Abort
            RVal = ACTION:Abort
            RETURN LEVEL:Fatal
        OF ?Apply
            RVal = ACTION:Apply
            RETURN LEVEL:Fatal
        OF ?Ignore
            RVal = ACTION:Cancel
            RETURN LEVEL:Fatal
        OF ?Omit
            RVal = ACTION:OmitCode
            RETURN LEVEL:Fatal
        OF ?UnCompile
            RVal = ACTION:UnCompile
            RETURN LEVEL:Fatal
        OF ?Assertion
            RVal = ACTION:AssertFail
            RETURN LEVEL:Fatal
        OF ?EditStr
            ?OutList{PROP:Edit,2} = False
            SecQMgr.SetLine(CHOICE(?OutList),EditStr)
            IF EditStr NOT = EditStrBck
                SecQMgr.OutQ.UserChanged = True
                SecQMgr.OutQ.FG_Normal = COLOR:Fuschia
                SecQMgr.OutQ.FG_Selected = COLOR:Fuschia
                PUT(SecQMgr.OutQ)
                ASSERT(~ERRORCODE(),'Error writing to OutQ')
            END
            ENABLE(?OutList)
            SELECT(?OutList)
        END
        RETURN PARENT.TakeAccepted()

ThisWindow.TakeFieldEvent   PROCEDURE()
i                               LONG,AUTO

    CODE
        CASE EVENT()
        OF EVENT:AlertKey
            CASE FIELD()
            OF ?EditStr
                CASE KEYCODE()
                OF MouseRight
                OROF EnterKey
                    POST(EVENT:Accepted,?EditStr)
                OF CtrlA
                    EditStr = EditStrBck
                    DISPLAY(?EditStr)
                    SELECT(?EditStr)
                END
            OF ?OutList
                CASE KEYCODE()
                OF DeleteKey
                    SecQMgr.RemoveLine(CHOICE(?OutList))
                OF InsertKey
                    SecQMgr.InsertLine('',CHOICE(?OutList),0)
                OF F2Key
                OROF MouseLeft2
                    IF INRANGE(CHOICE(?OutList),1,RECORDS(SecQMgr.OutQ))
                        DO InitiateEditInPlace
                    END
                OF Alt2
                    SecQMgr.RestoreLine(CHOICE(?OutList))
                OF MouseRight
                    IF CHOICE(?OutList)
                        Popup.SetItemEnable('RestoreLine',SecQMgr.CanRestore(CHOICE(?OutList)))
                        CASE Popup.Ask()
                        OF 'Top'
                            SELECT(?OutList,1)
                        OF 'PageUp'
                            SELECT(?OutList,CHOOSE(CHOICE(?OutList)- ?OutList{PROP:Items} < 1,1,CHOICE(?OutList) - ?OutList{PROP:Items}))
                        OF 'PageDown'
                            SELECT(?OutList,CHOOSE(CHOICE(?OutList) + ?OutList{PROP:Items} > RECORDS(SecQMgr.OutQ),RECORDS(SecQMgr.OutQ),CHOICE(?OutList) + ?OutList{PROP:Items}))
                        OF 'Bottom'
                            SELECT(?OutList,RECORDS(SecQMgr.OutQ))
                        OF 'InsertLine'
                            SecQMgr.InsertLine('',CHOICE(?OutList),0)
                        OF 'DeleteLine'
                            SecQMgr.RemoveLine(CHOICE(?OutList))
                        OF 'EditLine'
                            DO InitiateEditInPlace
                        OF 'RestoreLine'
                            SecQMgr.RestoreLine(CHOICE(?OutList))
                        END
                        DISPLAY(?OutList)
                    END
                END
            END
        OF EVENT:NewSelection
            CASE FIELD()
            OF ?Notes
                IF Info.GetLineNumber()
                    i=SecQMgr.GetQueueLine(Info.GetLineNumber())
                    IF i THEN
                        ?OutList{PROP:Selected} = i
                        DISPLAY(?OutList)
                        ?InList{PROP:Selected} = CHOICE(?OutList)
                        DISPLAY(?inList)
                    END
                END
            OF ?OutList
                ?InList{PROP:Selected} = CHOICE(?OutList)
                DISPLAY(?InList)
                IF SecQMgr.GetLineNumber(CHOICE(?OutList))
                    Info.SetNoteLineNumber(SecQMgr.GetLineNumber(CHOICE(?OutList)))
                END
            OF ?InList
                ?OutList{PROP:Selected} = CHOICE(?InList)
                DISPLAY(?OutList)
                IF SecQMgr.GetLineNumber(CHOICE(?OutList))
                    Info.SetNoteLineNumber(SecQMgr.GetLineNumber(CHOICE(?OutList)))
                END
            END
        END
        RETURN PARENT.TakeFieldEvent()

InitiateEditInPlace ROUTINE
    SecQMgr.GetLine(CHOICE(?OutList),EditStr)
    EditStrBck=EditStr
    ?EditStr{PROP:Text}=?OutList{PROPLIST:Picture,2}
    ?OutList{PROP:Edit,2}=?EditStr
    SELECT(?EditStr)
    DISABLE(?OutList)

ThisWindow.TakeEvent        PROCEDURE()
    CODE
        CASE EVENT()
        OF EVENT:Selected
            CASE SELECTED()
            OF ?EditStr
                ?{PROP:Touched} = True
            END
        END
        RETURN PARENT.TakeEvent()

ConfigAppNames      PROCEDURE(*CSTRING InAppName,*CSTRING OutAppName,BYTE lType=0)
DOSFileLookup           SelectFileClass
FN                      LIKE(FNInfoType),AUTO
Save:InAppName          CSTRING(File:MaxFilePath),AUTO
Save:OutAppName         CSTRING(File:MaxFilePath),AUTO

    CODE
        Save:InAppName = InAppName
        Save:OutAppName = OutAppname
        DOSFileLookup.Init()
        CASE lType
        OF 0        !LookupSourceAppFile
            DOSFileLookup.SetMask(Translator.TranslateString('Text Application Files (*.TXA)'),'*.TXA')
            DOSFileLookup.AddMask(Translator.TranslateString('Application Files (*.APP)'),'*.APP')
            DOSFileLookup.AddMask(Translator.TranslateString('All Files'),'*.*')
            DOSFileLookup.WindowTitle=Translator.TranslateString('Select Application')
            InAppName=DOSFileLookup.Ask(1)
            IF InAppName
                InAppName = SHORTPATH(InAppName)
                FN.Path = CLIP(InAppName)
                FNSplit(FN.Path,FN.Drive,FN.Directory,FN.Name,FN.Extension)
                FN.Name = CHOOSE(FN.Name[LEN(FN.Name)] = '5',CLIP(SUB(FN.Name,1,7)) & 'X',CLIP(SUB(FN.Name,1,7)) & '5')
                FNMerge(FN.Path,FN.Drive,FN.Directory,FN.Name,FN.Extension)
                OutAppName = FN.Path
                ASSERT(InAppName NOT = OutAppName)
            END
        OF 1        !LookupDestAppFile
            DOSFileLookup.SetMask(Translator.TranslateString('Text Application Files (*.TXA)'),'*.TXA')
            DOSFileLookup.AddMask(Translator.TranslateString('Application Files (*.APP)'),'*.APP')
            DOSFileLookup.WindowTitle=Translator.TranslateString('Select Destination Application')
            DOSFileLookup.Flags = FILE:Save
            OutAppName = DOSFileLookup.Ask(1)
            IF OutAppName THEN OutAppName = SHORTPATH(OutAppName).
        ELSE
            ASSERT(False)
        END
        RETURN CHOOSE(Save:InAppName = InAppName AND Save:OutAppName = OutAppName, LEVEL:Benign,LEVEL:Notify)

CopyFile            PROCEDURE(STRING Src,STRING Dest)
    CODE
        REMOVE(Dest)
        COPY(CLIP(Src),CLIP(Dest))
        RETURN CHOOSE(ERRORCODE() = NoError,LEVEL:Benign,LEVEL:Notify)

KeyboardBreak       PROCEDURE()
    CODE
        LOOP WHILE KEYBOARD();ASK.
        RETURN CHOOSE(KEYCODE() = EscKey,LEVEL:Notify,LEVEL:Benign)

RemoveAmpersand     PROCEDURE(*CSTRING Txt)
i                       BYTE,AUTO
t                       BYTE,AUTO
RVal                    STRING(MaxLineLen-1)

    CODE
        i = 0
        LOOP t = 1 TO LEN(Txt)
            IF Txt[t] NOT = '&'
                RVal=RVal[1 : i]&Txt[t]
                i += 1
            ELSIF Txt[t] = '&' AND t < LEN(Txt) AND Txt[t + 1] ='&'
                RVal = RVal[1 : i] & Txt[t]
                i += 1
            END
        END
        RETURN CLIP(RVal)

RegisterAddin       PROCEDURE(RuleClass AddinClass)
    CODE
        CLEAR(Addins)
        Addins.Mgr &= AddinClass
        ADD(Addins)
        ASSERT(~ERRORCODE())

SetProgressValue    PROCEDURE(ProgressManagerClass SELF,BYTE Value)
    CODE
        IF SELF.ProgressControl{PROP:Progress} NOT= Value AND (Value = 0 OR Value > SELF.ProgressControl{PROP:Progress})
            SELF.ProgressControl{PROP:Progress} = Value
            DISPLAY(SELF.ProgressControl)
        END

TXAQSetLine         PROCEDURE(ApplicationClass SELF,LONG Idx)
    CODE
        IF SELF.TXAQ.Positional NOT = Idx
            SELF.TXAQ.Positional = Idx
            GET(SELF.TXAQ,SELF.TXAQ.Positional)
            ASSERT(~ERRORCODE(),'Error reading TXA Queue')
        END
        IF ~SELF.TXAQ.OutLine &= NULL     !AND SELF.TXAQ.OutLine
            SELF.CurrentLine &= SELF.TXAQ.OutLine
        ELSE
            ASSERT(~SELF.TXAQ.InLine &= NULL)
            SELF.CurrentLine &= SELF.TXAQ.InLine
        END

SecQSetOutLine      PROCEDURE(SectionClass SELF,LONG Pos,BYTE Force=False)
    CODE
        IF Force
            DO GetRecord
        ELSIF POINTER(SELF.OutQ) NOT = Pos
            DO GetRecord
        END

GetRecord           ROUTINE
    GET(SELF.OutQ,Pos)
    COMPILE('***',_DEBUG_)
    IF ERRORCODE()
        STOP('OutQ get failed: ' & Pos & ',' & RECORDS(SELF.OutQ) & ',' & ERROR())
    END
    ***
    OMIT('***',_DEBUG_)
    ASSERT(~ERRORCODE(),'Error reading OutQ')
    ***
SecQSetInLine       PROCEDURE(SectionClass SELF,LONG Pos,BYTE Force=False)
    CODE
        IF Force
            DO GetRecord
        ELSIF POINTER(SELF.InQ) NOT=Pos
            DO GetRecord
        END

GetRecord           ROUTINE
    GET(SELF.InQ,Pos)
    ASSERT(~ERRORCODE(),'Error reading InQ')

SecQSetColor        PROCEDURE(SectionClass SELF,LONG FGNorm,LONG FGSelect,LONG BGNorm,LONG BGSelect)
    CODE
        SELF.OutQ.FG_Normal = FGNorm
        SELF.OutQ.FG_Selected = FGSelect
        SELF.OutQ.BG_Normal = BGNorm
        SELF.OutQ.BG_Selected = BGSelect

SecQSyncColors      PROCEDURE(SectionClass SELF,LONG StartPos,LONG EndPos)
i                       LONG,AUTO

    CODE
        LOOP i = StartPos TO EndPos
            SecQSetOutLine(SELF,i)
            IF SELF.OutQ.UserChanged
                SecQSetColor(SELF,COLOR:Fuschia,COLOR:Fuschia,COLOR:Black,COLOR:Yellow)
            ELSE
                IF INRANGE(i,1,RECORDS(SELF.InQ))
                    SecQSetInLine(SELF,i)
                    IF SELF.InQ.Line=SELF.OutQ.Line
                        SecQSetColor(SELF,COLOR:Black,COLOR:Black,COLOR:White,COLOR:Yellow)
                    ELSE
                        SecQSetColor(SELF,COLOR:Red,COLOR:Red,COLOR:White,COLOR:Yellow)
                    END
                END
            END
            PUT(SELF.OutQ)
            ASSERT(~ERRORCODE(),'Error updating colors to OutQ')
        END

FileExists          PROCEDURE(*CSTRING Filename)
FN                      STRING(File:MaxFilePath),STATIC,AUTO
TstFile                 FILE,DRIVER('DOS'),NAME(FN)
                            RECORD
                                BYTE
                            END
                        END
RVal                    BYTE(False)

    CODE
        FN = Filename
        OPEN(TstFile)
        CASE ERRORCODE()
        OF NoError
            CLOSE(TstFile)
            RVal = True
        OF IsOpenErr
        OROF NoAccessErr
            RVal = True
        END
        RETURN RVal

GetBMPName          PROCEDURE(BYTE No)
hdcNULL                 HANDLE
RVal                    CSTRING(13),AUTO

    CODE
        hdcNULL = GetDC(0)
        RVal = CHOOSE(GetDeviceCaps(hdcNULL, NUMCOLORS) = 16,'~CONV_' & No & 'L.BMP','~CONV_' & No & 'M.BMP')
        ReleaseDC(0, hdcNULL);
        RETURN RVal

FileExtension       PROCEDURE(STRING FullName)
FN                      LIKE(FNInfoType),AUTO

    CODE
        FN.Path = FullName
        FNSplit(FN.Path,FN.Drive,FN.Directory,FN.Name,FN.Extension)
        RETURN UPPER(FN.Extension[2 : LEN(FN.Extension)])        

GetTempPath         PROCEDURE()
cTempPath               CSTRING(File:MaxFilePath),AUTO
RVal                    CSTRING(File:MaxFilePath),AUTO

    CODE
        IF API32_GetTempPath(File:MaxFilePath,cTempPath)
            RVal = CLIP(cTempPath)
        END
        IF RVal
            IF Rval[LEN(RVal)] NOT='\' THEN RVal=RVal&'\'.
        ELSE
            RVal='\'
        END
        RETURN CLIP(RVal)

YieldCheck          PROCEDURE()
YieldCnt                BYTE(1),STATIC

    CODE
        YieldCnt += 1
        RETURN CHOOSE(YieldCnt%YieldGranularity = 0,True,False)

!--- Class Methods ----------------------------------------------------------------------

ConvertorINIClass.Init      PROCEDURE(ErrorClass E)
i                               USHORT,AUTO
INIPath                         CSTRING(File:MaxFilePath),AUTO
INIStr                          CSTRING(File:MaxFilePath)

    CODE
        SELF.ErrMgr &= E
        ASSERT(~SELF.ErrMgr &= NULL,'Bad ErrorClass object')
        INIPath = COMMAND(0)
        LOOP i = LEN(INIPath) TO 1 BY -1
            IF INIPath[i] = '\'
                INIPath = INIPath[1:i]
                BREAK
            END
        END
        PARENT.Init(INIPath&'C55CONV.INI')
        SELF.Update('ConfirmWindow','Maximize','')      !clear confirmation window reset sizes
        SELF.Update('ConfirmWindow','XPos','')
        SELF.Update('ConfirmWindow','YPos','')
        SELF.Update('ConfirmWindow','Width','')
        SELF.Update('ConfirmWindow','Height','')
        SELF.hInstances &= NEW hInstancesQueue
        LOOP i = 1 TO 255                                 !Add default rules to RuleDLLs section if no rules found
            INIStr = SELF.TryFetch('RuleDLLs',i)
        WHILE ~INIStr
        IF ~INIStr THEN SELF.Update('RuleDLLs','1','CNVRULES.DLL').
        INIStr = SELF.TryFetch('Files','HelpFile')        !Configure help system
        IF UPPER(INIStr) <> 'OFF'
            IF ~INIStr OR UPPER(RIGHT(INIStr,4)) <> '.CHM' THEN     !2022-11-07 Carl Barnes: change HLP to CHM
                INIStr = 'Legacy2ABC.chm'                           !2022-11-07 Carl Barnes: was 'C55CONV.HLP'
                SELF.Update('Files','HelpFile',INIStr)
            END
            HELP(INIStr)
            HelpSystem = True                               !Flag help system enabled
        END
        INIStr = SELF.TryFetch('Files','CleanupTempFiles')
        IF ~INIStr
            INIStr = 'Yes'
            SELF.Update('Files','CleanupTempFiles',INIStr)
        END

ConvertorINIClass.TryFetchAppNames  PROCEDURE(*CSTRING NameIn,*CSTRING NameOut)
    CODE
        NameIn=SELF.TryFetch('Files','AppNameIn')
        NameOut=SELF.TryFetch('Files','AppNameOut')

ConvertorINIClass.UpdateAppNames    PROCEDURE(*CSTRING NameIn,*CSTRING NameOut)
    CODE
        SELF.Update('Files','AppNameIn',NameIn)
        SELF.Update('Files','AppNameOut',NameOut)

ConvertorINIClass.LoadRuleDlls      PROCEDURE()
DllErr                                  LONG,AUTO
DllName                                 CSTRING(File:MaxFilePath),AUTO
i                                       BYTE,AUTO
j                                       BYTE,AUTO

    CODE
        LOOP i = 1 TO 255                     !Load and initialize all dll's listed in C55CONV.INI RuleDLLs section
            LOOP j = 1 TO 255
                DllName = INI.TryFetchField('RuleDLLs',i,j)
                IF ~DllName THEN BREAK.
                SELF.hInstances.Item = LoadLibrary(DllName)
                ASSERT(SELF.hInstances.Item)
                ADD(SELF.hInstances)
                ASSERT(~ERRORCODE())
                DllErr = CALL(DllName,'InitializeDLL')
                IF DllErr THEN SELF.ErrMgr.ThrowMessage(MSG:DLLNotLoaded,DllName & ' Error ' & DllErr).
            END
        END

ConvertorINIClass.Kill      PROCEDURE()
i                               USHORT,AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.hInstances)                 !Unload rule dll's
            GET(SELF.hInstances,i)
            ASSERT(~ERRORCODE())
            FreeLibrary(SELF.hInstances.Item)
        END
        DISPOSE(SELF.hInstances)
        PARENT.Kill

RuleClass.Construct PROCEDURE()
    CODE
        SELF.Lexer &= NEW LexerClass
        SELF.Lexer.Init
        RegisterAddin(SELF)

RuleClass.Destruct  PROCEDURE()
    CODE
        SELF.Lexer.Kill
        DISPOSE(SELF.Lexer)
        SELF.Lexer &= NULL

RuleClass.Register  PROCEDURE(BYTE Priority,STRING Owner,STRING Descrip,STRING Prmpt,STRING Sections,BYTE EmptySections,BYTE AllowAuto)
    CODE
        ASSERT(~Addins.Mgr &= NULL)                         !Mgr must already exists
        ASSERT(INRANGE(Priority,1,255))
        ASSERT(Sections)                                  !Rules cannot register without registering 1 or more sections
        Addins.Info.Priority = Priority
        Addins.Info.Owner = Owner
        Addins.Info.Name = Descrip
        Addins.Info.PromptText = Prmpt
        Addins.Info.Sections = Sections
        Addins.Info.EmptySections = EmptySections
        Addins.Info.AllowAuto = AllowAuto
        PUT(Addins)
  !DB.DumpQue('Addins',Addins)
        ASSERT(~ERRORCODE())

RuleClass.TakeSection       PROCEDURE(SectionClass SecQMgr,InfoTextClass Info,STRING SectionHeader)
    CODE
        RETURN Level:Benign

RuleClass.AfterConversion   PROCEDURE()
    CODE

RuleClass.BeforeConversion  PROCEDURE()
    CODE

RuleClass.PreFilterLine     PROCEDURE(*CSTRING TextLine,STRING Contain)
cLine                           CSTRING(MaxLineLen),AUTO
Contains                        CSTRING(MaxLineLen),AUTO
i                               USHORT,AUTO

    CODE
        ASSERT(Contain)
        Contains = Contain
        cLine = CHOOSE(INSTRING('!',cline,1,1) > 0,SUB(TextLine,1,i-1),TextLine)
        IF cLine
            cLine = UPPER(cLine)
            Contains = UPPER(Contains)
            LOOP
                i=INSTRING('|',Contains,1,1)
                IF ~i
                    RETURN CHOOSE(INSTRING(Contains,cLine,1,1) > 0,True,False)
                ELSE
                    IF INSTRING(Contains[1 : i - 1],cLine,1,1) THEN RETURN True.
                    Contains=Contains[i + 1 : LEN(Contains)]
                END
            END
        END
        RETURN False

RuleClass.Replace   PROCEDURE(*CSTRING Src,STRING Fnd,STRING Rpl,BYTE ReplaceAll=True)
p1                      USHORT,AUTO
p2                      USHORT,AUTO
RVal                    BYTE(False)
sp                      USHORT(1)

    CODE
        LOOP
            p1 = INSTRING(UPPER(Fnd),UPPER(Src),1,sp)
            IF p1
                p2 = P1 + (LEN(Fnd) - 1)
                Src = Src[1 : P1 - 1] & Rpl & Src[P2 + 1 : LEN(Src)]
                sp = p1 + LEN(Rpl)                              !This prevents recursive finds, ie. continues looking from after replacement inserted
                RVal = True
                IF ~ReplaceAll THEN BREAK.                      !break after first replacement
            ELSE
                BREAK
            END
        END
        RETURN RVal

RuleClass.WildInstring      PROCEDURE(STRING LookFor,STRING Txt,*? StartPos,*? Length)
Cmp                             CSTRING(255),AUTO
i                               USHORT,AUTO
is                              USHORT,AUTO
Lf                              CSTRING(255),AUTO
p                               USHORT(1)
RVal                            USHORT(0)

    CODE
        StartPos = 0
        Length = 0
        Lf = UPPER(CLIP(LookFor))
        IF Lf
            LOOP
                is = INSTRING('!',Lf,1,1)
                i = CHOOSE(is > 0,is,LEN(Lf) + 1)
                Cmp = Lf[1 : i - 1]
                IF Cmp
                    p = INSTRING(Cmp,UPPER(Txt),1,p)
                    IF ~p THEN RETURN False.
                    IF ~StartPos THEN StartPos=p.
                    p += (LEN(Cmp) - 1)
                END
                IF i > LEN(Lf)
                    CLEAR(Lf)
                ELSE
                    Lf = Lf[i + 1 : LEN(Lf)]
                END
            WHILE LEN(Lf)
            Length = p - StartPos + 1
            RETURN True
        END
        RETURN False

RuleClass.WildReplace       PROCEDURE(*CSTRING TargetStr,STRING FindStr,STRING ReplaceStr,BYTE ReplaceAll=True)
EndPos                          USHORT,AUTO
Fnd                             CSTRING(MaxLineLen)
i                               USHORT,AUTO
Length                          USHORT,AUTO
RVal                            BYTE(False)
StartPos                        USHORT,AUTO

    CODE
        IF SELF.WildInstring(FindStr,TargetStr,StartPos,Length)
            EndPos = StartPos + Length - 1
            Fnd = TargetStr[StartPos : EndPos]
            StartPos = INSTRING('!',FindStr,1,1)
            EndPos = INSTRING(UPPER(Fnd[StartPos + 1 : LEN(Fnd)]),UPPER(Fnd),1,1) - 1
            IF LEN(ReplaceStr)
                i = INSTRING('!',ReplaceStr,1,1)
                IF i THEN ReplaceStr = ReplaceStr[1 : i - 1] & Fnd[StartPos : EndPos] & ReplaceStr[i + 1 : LEN(ReplaceStr)].
            END
            SELF.Replace(TargetStr,Fnd,CLIP(ReplaceStr),ReplaceAll)
            RVal = True
        END
        RETURN RVal

RuleClass.WildReplace       PROCEDURE(*CSTRING TargetStr,ReplaceBlockItems Items,SectionClass SectionMgr,InfoTextClass Info,LONG LineNo)
ItemStr                         &STRING
FindStr                         CSTRING(MaxLineLen),AUTO
i                               USHORT(3)
Msg                             CSTRING(MaxLineLen),AUTO
sLen                            BYTE,AUTO
ReplStr                         CSTRING(MaxLineLen),AUTO

    CODE
        ItemStr &= Items
        LOOP Items.Number TIMES
            sLen = VAL(ItemStr[i])
            i += 1
            FindStr = SUB(ItemStr,i,slen)
            i += sLen
            sLen = VAL(ItemStr[i])
            i += 1
            ReplStr = SUB(ItemStr,i,sLen)
            i += sLen
            sLen = VAL(ItemStr[i])
            i += 1
            Msg = SUB(ItemStr,i,sLen)
            i += sLen
            IF SELF.WildReplace(TargetStr,FindStr,ReplStr) THEN Info.AddLine(Msg,LineNo).
        END

RuleClass.SaveComment       PROCEDURE(*CSTRING Txt)
i                               USHORT,AUTO
j                               LIKE(i),AUTO
qCnt                            BYTE(0)

    CODE
        i = INSTRING('!',Txt,1,1)
        IF i
            LOOP j = i - 1 TO 1 BY -1
                IF Txt[j]='''' THEN qCnt += 1.
            END
            IF ~(qCnt / 2 = INT(qCnt / 2))      !exclamation mark must be inside string literal
                DO ClearLastItems
            ELSE
                SELF.LastCommentPos = i
                SELF.LastComment = Txt[i : LEN(Txt)]
                Txt = SUB(Txt,1,i-1)
            END
        ELSE
            DO ClearLastItems
        END
        RETURN LEN(Txt)

ClearLastitems      ROUTINE
    CLEAR(SELF.LastCommentPos)
    CLEAR(SELF.LastComment)

RuleClass.RestoreComment    PROCEDURE(*CSTRING Txt)
    CODE
        IF SELF.LastCommentPos
            IF SELF.LastCommentPos <= LEN(Txt) + 1
                Txt = CLIP(Txt) & '  ' & SELF.LastComment
            ELSE
                Txt = CLIP(Txt) & ALL(' ',SELF.LastCommentPos - LEN(Txt) - 1) & SELF.LastComment
            END
        END

RuleClass.CurrentAddition   PROCEDURE()
    CODE
        RETURN CurAddition

RuleClass.CurrentEMBED      PROCEDURE()
    CODE
        RETURN CurEmbed

RuleClass.CurrentProcedure  PROCEDURE()
    CODE
        RETURN CurProcedure

RuleClass.IsKnownFileLabel  PROCEDURE(STRING TestLabel)
i                               USHORT,AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.FileList)
            GET(SELF.FileList,i)
            ASSERT(~ERRORCODE())
            IF UPPER(SELF.FileList.Name)=UPPER(TestLabel) THEN RETURN True.
        END
        RETURN False

RuleClass.IsKnownKeyLabel   PROCEDURE(STRING TestLabel)
i                               USHORT,AUTO
j                               LIKE(i),AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.FileList)
            GET(SELF.FileList,i)
            ASSERT(~ERRORCODE())
            IF ~SELF.FileList.Keys &= NULL
                LOOP j = 1 TO RECORDS(SELF.FileList.Keys)
                    GET(SELF.FileList.Keys,j)
                    ASSERT(~ERRORCODE())
                    IF UPPER(SELF.FileList.Keys.Name) = UPPER(TestLabel) THEN RETURN True.
                END
            END
        END
        RETURN False

DDEMgrClass.Init    PROCEDURE(ErrorClass E,BYTE LogConversation=False)

    CODE
        SELF.ErrorManager &= E
        SELF.LogFile &= LogFile
        SELF.LogFileLine &= LogFile.Line
        SELF.Logging = LogConversation
        IF SELF.Logging
            CREATE(SELF.LogFile)
            ASSERT(~ERRORCODE())
            OPEN(SELF.LogFile)
            ASSERT(~ERRORCODE())
        END

DDEMgrClass.Kill    PROCEDURE()
    CODE
        IF SELF.Logging
            DESTROY(SELF.LogFile)
            ASSERT(~ERRORCODE())
        END

DDEMgrClass.Connect PROCEDURE()
    CODE
        SYSTEM{PROP:DDETimeOut} = 6000
        SELF.CWServer = DDEClient('ClarionWin')
        SELF.AppendLogLine('DDEClient Connection Result: ' & SELF.CWServer)
        IF ~SELF.CWServer
!    SELF.ErrorManager.TakeError(MSG:DDEConnectErr)
            RETURN LEVEL:Notify
        END
        RETURN LEVEL:Benign

DDEMgrClass.Disconnect      PROCEDURE()
    CODE
        IF SELF.CWServer
            DDEClose(SELF.CWServer)
            SELF.AppendLogLine('DDEClose Errorcode: '&ERRORCODE())
        END

DDEMgrClass.ExportTXA       PROCEDURE(*CSTRING AppName)
DDECmd                          STRING(255),AUTO
Err                             USHORT,AUTO

    CODE
        ASSERT(SELF.CWServer)
        SETCURSOR(CURSOR:Wait)
        DDECmd = '[ExportApp(' & CLIP(AppName) & ',' & GetTempPath() & 'INTXA.$$$,0)]'
        DDEExecute(SELF.CWServer,DDECmd)
        Err = ERRORCODE()
        SELF.AppendLogLine('Command: '& DDECmd)
        SETCURSOR()
        IF Err
            SELF.ShowErrorInfo(Err, MSG:BadTXAExport)
            RETURN LEVEL:Notify
        END
        RETURN LEVEL:Benign

DDEMgrClass.ImportTXA       PROCEDURE(*CSTRING AppName)
DDECmd                          STRING(255),AUTO
Err                             USHORT,AUTO

    CODE
        ASSERT(SELF.CWServer)
        SETCURSOR(CURSOR:Wait)
        DDECmd = '[ImportApp(' & GetTempPath() & 'OUTTXA.$$$,' & CLIP(AppName) & ',1,Replace)]'
        Err = ERRORCODE()
        DDEExecute(SELF.CWServer,DDECmd)
        SELF.AppendLogLine('Command: ' & DDECmd)
        SETCURSOR()
        IF Err THEN SELF.ShowErrorInfo(Err, MSG:BadTXAImport).

DDEMgrClass.ShowErrorInfo   PROCEDURE(USHORT Err, USHORT ErrID)
ErrTxt                          STRING(255),AUTO
ErrNo                           USHORT,AUTO

    CODE
        IF Err
            IF Err>600
                DDEREAD(SELF.CWServer,DDE:Manual,'GetErrorMsg',ErrTxt)
                DDEREAD(SELF.CWServer,DDE:Manual,'GetErrorNum',ErrNo)
                SELF.AppendLogLine('Retrieved Error Status: ' & CLIP(ErrTxt) & '(' & ErrNo & ')')
                SELF.ErrorManager.ThrowMessage(ErrId,CLIP(ErrTxt) & '(' & ErrNo & ')')
            ELSE
                SELF.ErrorManager.ThrowMessage(ErrId,CLIP(Err))
            END
        END

DDEMgrClass.AppendLogLine   PROCEDURE(STRING Txt)
ErrNo                           USHORT

    CODE
        IF SELF.Logging
            IF ERRORCODE() > 600
                DDEREAD(SELF.CWServer,DDE:Manual,'GetErrorNum',ErrNo)
                SELF.LogFileLine = FORMAT(TODAY(),@D6) & ' ' & FORMAT(CLOCK(),@T3) & ' -> ' & CLIP(Txt) & '  DDEErrorNum: ' & ErrNo
            ELSE
                SELF.LogFileLine = FORMAT(TODAY(),@D6) & ' ' & FORMAT(CLOCK(),@T3) & ' -> ' & CLIP(Txt)
            END
            ADD(SELF.LogFile)
            ASSERT(~ERRORCODE())
        END

InTXAClass.Init     PROCEDURE(ErrorClass E)
    CODE
        PARENT.Init(InAsciiFile,E)
        SELF.LazyOpen = False
        CLEAR(SELF.FilenameValue)
        SELF.Filename &= InAsciiFileName
        SELF.SetName(GetTempPath() & 'INTXA.$$$')
        SELF.LockRecover = 0
        SELF.OpenMode = ReadOnly + DenyAll
        SELF.Create = False
        SELF.Buffer &= InAsciiFile.Record
        SELF.Line &= InAsciiFile.Line

InTXAClass.Kill     PROCEDURE()

    CODE
        IF UPPER(INI.TryFetch('Files','CleanupTempFiles')) = 'YES'
            REMOVE(SELF.File)
        END
        DESTROY(SELF.File)
        PARENT.Kill()

InTXAClass.Next     PROCEDURE()
    CODE
        NEXT(SELF.File)
        RETURN CHOOSE(ERRORCODE() = NoError,LEVEL:Benign,LEVEL:Notify)

InTXAClass.ReadFile PROCEDURE(ApplicationClass TXAQMgr,ProgressManagerClass Progress)
CallsReqd               BYTE(False)
InsideProcedure         BYTE(False)
EndNest                 SHORT(0)
FirstLine               BYTE(True)
Pos                     LONG(0)
Prg                     LONG(0)
cLine                   CSTRING(MaxLineLen),AUTO

    CODE
        SETCURSOR(CURSOR:Wait)
        SELF.Close()                              !ROYMORE - driver bug
        IF SELF.Open() = LEVEL:Notify
            SETCURSOR()
            RETURN LEVEL:Notify
        END
        SELF.FileSize = BYTES(SELF.File)
        Progress.Reset()
        Progress.SetTriggers(0,SELF.FileSize,100,1,200)
        SET(SELF.File)
        LOOP WHILE ~SELF.Next()
            IF YieldCheck() THEN YIELD.
            IF KeyboardBreak() THEN RETURN LEVEL:Cancel.
            IF FirstLine AND SELF.Line
                IF UPPER(SUB(SELF.Line,1,13)) <> '[APPLICATION]'
                    SELF.Close()
                    GlobalErrors.TakeError(Msg:BadTXA)
                    RETURN LEVEL:Notify
                END
                FirstLine = False
            END
            Prg += BYTES(SELF.File)
            Progress.Update(Prg)
    !This code enforces that a [CALLS] section must exist in each procedure, it appends an empty one if
    !one does not already exist
            IF SELF.Line = '[END]'
                EndNest -= 1
                ASSERT(EndNest >= 0)                  !Nested [END]s should never go negative
                IF EndNest = 0                        !End of [MODULE] section
                    IF CallsReqd
                        DO AddCallsSection
                    END
                    InsideProcedure = False
                END
            ELSIF SELF.Line = '[PROCEDURE]' AND EndNest = 1   !EndNest=1 condition ensures that [PROCEDURE] section inside [DEFINITION] is ignored
                IF InsideProcedure
                    IF CallsReqd
                        DO AddCallsSection
                    END
                END
                InsideProcedure = True
                CallsReqd = True                      !Assume will need to add a [CALLS] section
            ELSIF SELF.Line='[EMBED]' OR SELF.Line = '[INSTANCES]' OR SELF.Line = '[DEFINITION]'
                EndNest += 1
            ELSIF SELF.Line = '[MODULE]' OR SELF.Line = '[PROGRAM]'
                EndNest += 1
                InsideProcedure = False
            ELSIF SELF.Line = '[CALLS]'
                IF InsideProcedure
                    CallsReqd = False                   ![CALLS] section found in procedure, so no need to add one
                END
            END
            cLine = CLIP(SELF.Line)
            IF cLine
                IF ~TXAQMgr.AppendLine(cLine) = LEVEL:Benign THEN BREAK.
            END
        END
        Progress.Complete()
        SELF.Close()
        SETCURSOR()
        RETURN LEVEL:Benign

AddCallsSection     ROUTINE
    cLine = '[CALLS]'
    IF ~TXAQMgr.AppendLine(cLine) = LEVEL:Benign
        SELF.Close()
        SETCURSOR()
        RETURN LEVEL:Notify
    END
    CallsReqd = False

OutTXAClass.Init    PROCEDURE(ErrorClass E)
    CODE
        PARENT.Init(OutAsciiFile,E)
        SELF.LazyOpen = False
        CLEAR(SELF.FilenameValue)
        SELF.Filename &= OutAsciiFileName
        SELF.SetName(GetTempPath() & 'OUTTXA.$$$')
        SELF.LockRecover = 0
        SELF.OpenMode = ReadWrite + DenyAll
        SELF.Create = True
        SELF.Buffer &= OutAsciiFile.Record
        SELF.Line &= OutAsciiFile.Line

OutTXAClass.Kill    PROCEDURE()
    CODE
        IF UPPER(INI.TryFetch('Files','CleanupTempFiles')) = 'YES'
            REMOVE(SELF.File)
        END
        DESTROY(SELF.File)
        PARENT.Kill()

OutTXAClass.WriteFile       PROCEDURE(ApplicationClass TXAQMgr,ProgressManagerClass Progress)
i                               LONG,AUTO
cStr                            CSTRING(MaxLineLen+1),AUTO

    CODE
        SETCURSOR(CURSOR:Wait)
        Progress.Reset()
        Progress.SetTriggers(0,TXAQMgr.GetMaxLineNo())
        SELF.Close()                            !ROYMORE - here to fix driver bug on {PROP:Name}
        CREATE(SELF.File)
        IF ERRORCODE() 
            SETCURSOR()
            RETURN LEVEL:Notify
        END
        IF SELF.Open() = LEVEL:Notify
            DO CloseRemove
            SETCURSOR()
            RETURN LEVEL:Notify
        END
        LOOP i = 1 TO TXAQMgr.GetMaxLineNo()
            IF YieldCheck() THEN YIELD.
            Progress.Update(i)
            TXAQMgr.GetLine(i,cStr)
            IF cStr
                SELF.Line = cStr
                IF SELF.Update() = LEVEL:Notify
                    DO CloseRemove
                    SETCURSOR()
                    RETURN LEVEL:Notify
                END
            END
        END
        Progress.Complete()
        SELF.Close()
        SETCURSOR()
        RETURN LEVEL:Benign

CloseRemove         ROUTINE
    SELF.Close()
    IF UPPER(INI.TryFetch('Files','CleanupTempFiles'))='YES'
        REMOVE(SELF.File)
    END
    DESTROY(SELF.File)

OutTXAClass.Update  PROCEDURE()
    CODE
        ADD(SELF.File)
        RETURN CHOOSE(ERRORCODE() = NoError,Level:Benign,Level:Notify)

ProgressManagerClass.Init   PROCEDURE(SHORT Ctrl)
    CODE
        SELF.ProgressControl = Ctrl
        SELF.ProgressControl{PROP:RangeLow} = 0
        SELF.ProgressControl{PROP:RangeHigh} = 100
        SELF.Reset(False)

ProgressManagerClass.Reset  PROCEDURE(BYTE UpdateDisplay=True)
    CODE
        SELF.BaseTrigger = 1
        ClearTriggers(SELF)
        SELF.Granularity = 1
        SELF.Calls = 0
        IF UpdateDisplay THEN SetProgressValue(SELF,0).

ProgressManagerClass.SetPercentile  PROCEDURE(Byte Percent)
    CODE
        ASSERT(INRANGE(Percent,0,100))
        SetProgressValue(SELF,Percent)

ProgressManagerClass.Complete       PROCEDURE()
    CODE
        SetProgressValue(SELF,100)

ProgressManagerClass.SetTriggers    PROCEDURE()
    CODE
        SELF.SetTriggers(1,100)

ProgressManagerClass.SetTriggers    PROCEDURE(*LONG[] TriggerValues,BYTE BaseTrigger=1,LONG Granularity)
i                                       BYTE,AUTO

    CODE
        ASSERT(INRANGE(BaseTrigger,1,100))
        ASSERT(MAXIMUM(TriggerValues,1)=100-BaseTrigger+1)
        LOOP i=BaseTrigger TO 100
            SELF.Trigger[i]=TriggerValues[i]
        END
        SELF.BaseTrigger=BaseTrigger
        SELF.Granularity=Granularity

ProgressManagerClass.ClearTriggers  PROCEDURE()
i                                       BYTE,AUTO

    CODE
        LOOP i = 1 TO 100
            SELF.Trigger[i] = 0
        END

ProgressManagerClass.SetTriggers    PROCEDURE(LONG LoRange,LONG HiRange,BYTE NoTriggers=100,BYTE BaseTrigger=1,<LONG Granularity>)
i                                       BYTE,AUTO
TriggerIncrement                        LONG,AUTO
TriggerValue                            LONG,AUTO

    CODE
        ASSERT(LoRange <= HiRange)
        ASSERT(INRANGE(BaseTrigger,1,100))
        ASSERT(INRANGE(BaseTrigger + NoTriggers - 1,1,100))
        TriggerIncrement = (HiRange - LoRange) / (NoTriggers - 1)
        TriggerValue = LoRange
        LOOP i = BaseTrigger TO BaseTrigger + NoTriggers-1
            SELF.Trigger[i] = TriggerValue
            TriggerValue += TriggerIncrement
        END
        SELF.BaseTrigger = BaseTrigger
        SELF.Granularity = CHOOSE(OMITTED(Granularity) = True,TriggerIncrement,Granularity)

ProgressManagerClass.Update PROCEDURE(LONG Value)
i                               BYTE,AUTO

    CODE
        SELF.Calls += 1
        IF SELF.Calls = SELF.Granularity
            LOOP i = SELF.BaseTrigger TO 100
                IF SELF.Trigger[i] >= Value
                    SetProgressValue(SELF,i)
                    BREAK
                END
            END
            SELF.Calls = 0
        END

BlockClass.Init     PROCEDURE(ErrorClass E)
    CODE
        SELF.ErrMgr &= E

BlockClass.ErrorManager     PROCEDURE()
    CODE
        SELF.ErrMgr.ThrowMessage(Msg:QueueAccessError,ERROR())

ApplicationClass.Init       PROCEDURE(ErrorClass E)
                                INCLUDE('TERMS.CLW'),ONCE
b                               BYTE,AUTO
t                               USHORT,AUTO
TerminatorStr                   &STRING

    CODE
        ASSERT(Terminators.Number > 0)
        PARENT.Init(E)
        SELF.TXAQ &= NEW TextLines
        SELF.TermQ &= NEW TerminatorList
        TerminatorStr &= Terminators
        t = 3
        LOOP Terminators.Number TIMES
            b = VAL(TerminatorStr[t])
            t += 1
            SELF.TermQ.Sections = UPPER(TerminatorStr[t : t + b - 1])
            t += b
            b = VAL(TerminatorStr[t])
            t += 1
            SELF.TermQ.Terminators = UPPER(TerminatorStr[t : t + b - 1])
            t += b
            ASSERT(SELF.TermQ.Sections)
            ASSERT(SELF.TermQ.Terminators)
            ADD(SELF.TermQ)
            ASSERT(~ERRORCODE())
        END
        SORT(SELF.TermQ,SELF.TermQ.Sections)
        SELF.NextID=1

ApplicationClass.Kill       PROCEDURE()
i                               LONG,AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.TXAQ)
            GET(SELF.TXAQ,i)
            ASSERT(~ERRORCODE())
            DISPOSE(SELF.TXAQ.InLine)
            DISPOSE(SELF.TXAQ.OutLine)
        END
        DISPOSE(SELF.TXAQ)
        DISPOSE(SELF.TermQ)

ApplicationClass.AppendLine PROCEDURE(*CSTRING TXALine)
Lp                              LONG(0)

    CODE
        ASSERT(LEN(TXALine) + 1 < MaxLineLen)
        IF RECORDS(SELF.TXAQ)
            GET(SELF.TXAQ,RECORDS(SELF.TXAQ))
            ASSERT(~ERRORCODE())
            Lp = SELF.TXAQ.Positional
        END
        CLEAR(SELF.TXAQ)
        SELF.TXAQ.Positional = Lp + 1
        SELF.TXAQ.InLine &= NEW CSTRING(LEN(TXALine) + 1)
        SELF.TXAQ.InLine = TXALine
        ADD(SELF.TXAQ,SELF.TXAQ.Positional)
        ASSERT(~ERRORCODE())
        RETURN LEVEL:Benign

ApplicationClass.BuildPositionals   PROCEDURE(<ProgressManagerClass ProgressMgr>)
i                                       LIKE(TextLines.Positional),AUTO
tLimit                                  LONG,AUTO

    CODE
        tLimit = RECORDS(SELF.TXAQ)
        LOOP i = tlimit TO 1 BY -1
            GET(SELF.TXAQ,i)
            ASSERT(~ERRORCODE())
            IF YieldCheck() THEN YIELD.
            ProgressMgr.Update(tLimit - i)
            IF KeyboardBreak() THEN RETURN LEVEL:Notify.
            IF SELF.TXAQ.Positional <> i
                SELF.TXAQ.Positional = i
                PUT(SELF.TXAQ,SELF.TXAQ.Positional)
                ASSERT(~ERRORCODE())
            END
        END
        RETURN LEVEL:Benign

ApplicationClass.GetLine    PROCEDURE(LONG Idx,*CSTRING Dest)
    CODE
        TXAQSetline(SELF,Idx)
        Dest = SELF.CurrentLine

ApplicationClass.GetLine    PROCEDURE(LONG Idx)
cStr                            CSTRING(MaxLineLen),AUTO

    CODE
        SELF.GetLine(Idx,cStr)
        RETURN cStr

ApplicationClass.SetLine    PROCEDURE(Long Idx,*CSTRING Txt)
    CODE
        TXAQSetLine(SELF,Idx)
        IF ~SELF.TXAQ.OutLine &= NULL THEN DISPOSE(SELF.TXAQ.OutLine).
        SELF.TXAQ.OutLine &= NEW CSTRING(LEN(Txt)+1)
        SELF.TXAQ.OutLine = Txt

ApplicationClass.GetMaxLineNo       PROCEDURE()
i                                       LONG,AUTO

    CODE
        i = RECORDS(SELF.TXAQ)
        IF i
            IF POINTER(SELF.TXAQ) <> i
                GET(SELF.TXAQ,i)
                ASSERT(~ERRORCODE())
            END
            RETURN SELF.TXAQ.Positional
        END
        RETURN 0

ApplicationClass.IsBraced   PROCEDURE(LONG Idx)
    CODE
        TXAQSetLine(SELF,Idx)
        RETURN CHOOSE(SELF.CurrentLine[1] = '[',True,False)

ApplicationClass.IsHeader   PROCEDURE(LONG Idx,*CSTRING SectionHeader,<*CSTRING MatchedHeader>)
RVal                            BYTE,AUTO

    CODE
        TXAQSetLine(SELF,Idx)
        IF SELF.IsBraced(Idx)
            IF INSTRING(SELF.CurrentLine,SectionHeader,1,1)
                RVal = True
                IF ~OMITTED(MatchedHeader) THEN MatchedHeader = SELF.CurrentLine.
            ELSE
                Rval = False
                IF ~OMITTED(MatchedHeader) THEN CLEAR(MatchedHeader).
            END
        ELSE
            RVal = False
            IF ~OMITTED(MatchedHeader) THEN CLEAR(MatchedHeader).
        END
        RETURN RVal

ApplicationClass.IsEND      PROCEDURE(LONG Idx)
    CODE
        TXAQSetline(SELF,Idx)
        RETURN CHOOSE(SELF.CurrentLine = '[END]',True,False)

ApplicationClass.IsPrompt   PROCEDURE(LONG Idx)
    CODE
        TXAQSetLine(SELF,Idx)
        RETURN CHOOSE(SELF.CurrentLine AND SELF.CurrentLine[1] = '%',True,False)

ApplicationClass.IsTerminator       PROCEDURE(*CSTRING SectionHeader,LONG Idx)
    CODE
        ASSERT(SectionHeader)
        TXAQSetLine(SELF,Idx)
        SELF.TermQ.Sections=SectionHeader
        GET(SELF.TermQ,SELF.TermQ.Sections)
        ASSERT(~ERRORCODE())                      !Requested section unavailable - not in TERMS.CLW
        IF SELF.TermQ.Terminators = '<<HEADER>'
            RETURN SELF.IsBraced(Idx)
        ELSIF SELF.TermQ.Terminators = '<<ALL>'
            RETURN False
        ELSE
            RETURN CHOOSE(INSTRING(UPPER(SELF.CurrentLine),SELF.TermQ.Terminators,1,1) > 0,True,False)
        END

ApplicationClass.AppendToSectionQ   PROCEDURE(LONG Idx,SectionClass SecQMgr)
cQ                                      &SectionLines

    CODE
        TXAQSetLine(SELF,Idx)
        cQ &= SecQMgr.InQ
        DO AddQueue
        cQ &= SecQMgr.OutQ
        DO AddQueue
        SecQSyncColors(SecQMgr,RECORDS(cQ),RECORDS(cQ))

AddQueue            ROUTINE
    cQ.LineNumber=RECORDS(cQ)+1
    cQ.Line=SELF.CurrentLine
    cQ.FG_Normal=Color:Black
    cQ.FG_Selected=Color:Black
    cQ.BG_Normal=Color:White
    cQ.BG_Selected=Color:Yellow
    cQ.UserChanged=False
    ADD(cQ)
    ASSERT(~ERRORCODE())

ApplicationClass.ReplaceBlock       PROCEDURE(LONG StartIdx,LONG EndIdx,SectionClass SecQMgr)
i                                       LONG,AUTO
j                                       LIKE(i),AUTO
OutQRecs                                LONG,AUTO
Psv                                     CSTRING(MaxLineLen+1),AUTO
RecsInBlock                             LONG,AUTO

    CODE
        IF EndIdx = 0 THEN EndIdx = RECORDS(SELF.TXAQ).
        ASSERT(StartIdx <= EndIdx)
        OutQRecs = RECORDS(SecQMgr.OutQ)
        RecsInBlock = EndIdx - StartIdx + 1
        IF RecsInBlock < OutQRecs                             !section increased in size
            LOOP j = 1 TO OutQRecs
                IF StartIdx + j - 1 > EndIdx
                    SecQSetOutLine(SecQMgr,j,1)
                    CLEAR(SELF.TXAQ)
                    SELF.TXAQ.Positional = EndIdx
                    DISPOSE(SELF.TXAQ.OutLine)
                    SELF.TXAQ.OutLine &= NEW CSTRING(LEN(SecQMgr.OutQ.Line) + 1)
                    SELF.TXAQ.OutLine = SecQMgr.OutQ.Line
                    ADD(SELF.TXAQ,SELF.TXAQ.Positional)
                    ASSERT(~ERRORCODE())
                ELSE
                    i = StartIdx + j - 1
                    DO MoveOver
                END
            END
        ELSIF RecsInBlock > OutQRecs                          !Section decreased in size
            LOOP i = StartIdx TO EndIdx
                IF i - StartIdx + 1 > OutQRecs
                    TXAQSetLine(SELF,i)
                    DELETE(SELF.TXAQ)
                    ASSERT(~ERRORCODE())
                ELSE
                    DO MoveOver
                END
            END
        ELSE
            IF StartIdx = EndIdx
                GET(SecQMgr.InQ,1)
                ASSERT(~ERRORCODE())
                IF ~SecQMgr.InQ.Line
                    TXAQSetLine(SELF,StartIdx)
                    Psv = SELF.CurrentLine
                ELSE
                    CLEAR(Psv)
                END
            END
            LOOP i = StartIdx TO EndIdx
                DO MoveOver
            END
            IF StartIdx = EndIdx AND Psv
                CLEAR(SELF.TXAQ)
                SELF.TXAQ.Positional = StartIdx
                SELF.TXAQ.OutLine &= NEW CSTRING(LEN(Psv)+1)
                SELF.TXAQ.OutLine = Psv
                ADD(SELF.TXAQ,SELF.TXAQ.Positional)
                ASSERT(~ERRORCODE())
            END
        END
        RETURN SELF.GetMaxLineNo()

MoveOver            ROUTINE
    SecQSetOutLine(SecQMgr,i - StartIdx + 1,1)
    TXAQSetLine(SELF,i)
    IF SELF.TXAQ.OutLine &= NULL OR SecQMgr.OutQ.Line <> SELF.TXAQ.OutLine
        DISPOSE(SELF.TXAQ.OutLine)
        SELF.TXAQ.OutLine &= NEW CSTRING(LEN(SecQMgr.OutQ.Line)+1)
        SELF.TXAQ.OutLine=SecQMgr.OutQ.Line
        PUT(SELF.TXAQ,SELF.TXAQ.Positional)
        ASSERT(~ERRORCODE())
    END

ApplicationClass.PreProcess PROCEDURE(ProcedureList ProcedureQueue,FileQueue FileList,ProgressManagerClass ProgressMgr,SHORT ProgressPromptFeq)
AdditionName                    CSTRING(MaxLineLen),AUTO
cAddition                       CSTRING('[ADDITION]')
cCalls                          CSTRING('[CALLS]')
cFiles                          CSTRING('[FILES]')
cInstance                       CSTRING('[INSTANCE]')
cKeyl                           CSTRING('[KEY]')
cOthers                         CSTRING('[OTHERS]')
cPrimary                        CSTRING('[PRIMARY]')
cProcedure                      CSTRING('[PROCEDURE]')
cProject                        CSTRING('[PROJECT]')
cSecondary                      CSTRING('[SECONDARY]')
cStr                            CSTRING(MaxLineLen),AUTO
CurrentProc                     CSTRING(MaxLineLen)                     !Must be initialized to blank
InsideAddition                  BYTE(False)
InsideCalls                     BYTE(False)
InsideFiles                     BYTE(False)
InsideProject                   BYTE(False)
MaxLine                         LONG,AUTO
P1                              USHORT,AUTO
P2                              USHORT,AUTO
Progress                        LONG(1)

    CODE
        ProgressPromptFeq{PROP:Text} = 'Pre Processing:'
        DISPLAY(ProgressPromptFeq)
        MaxLine=SELF.GetMaxLineNo()
        ProgressMgr.Reset()
        ProgressMgr.SetTriggers(0,MaxLine)
        LOOP
            IF YieldCheck() THEN YIELD.
            ProgressMgr.Update(Progress)
            IF KeyboardBreak() THEN RETURN LEVEL:Notify.
            IF SELF.IsBraced(Progress)
                IF InsideProject
                    InsideProject = False
                ELSIF SELF.IsHeader(Progress,cProject)
                    InsideProject = True
                END
                IF InsideFiles AND SELF.IsTerminator(cFiles,Progress)
                    InsideFiles = False
                ELSIF SELF.IsHeader(Progress,cFiles)
                    InsideFiles = True
                END
                IF InsideCalls
                    InsideCalls = False
                ELSIF SELF.IsHeader(Progress,cCalls)
                    InsideCalls = True
                END
                DO ProcessHeaders
            ELSIF SELF.IsPrompt(Progress)
                SELF.GetLine(Progress,cStr)
                IF INSTRING(' PROCEDURE',cStr,1,1)
                    P1 = INSTRING('(',cStr,1,1)
                    P2 = INSTRING(')',cStr,1,1)
                    IF P1 > 0 AND P2 > 0 AND P1 < P2
                        cStr = cStr[P1 + 1 : P2 - 1]
                        IF cStr THEN DO AddToCalls.
                    END
                END
            END
            IF InsideFiles
                IF SELF.IsHeader(Progress,cSecondary) OR SELF.IsHeader(Progress,cPrimary)
                    SELF.GetLine(Progress + 1,cStr)
                    p1 = INSTRING(' ',cStr,1,1)
                    IF ~p1 THEN p1=LEN(cStr)+1.
                    cStr = cStr[1 : p1]
                    DO AddToFilesList
                ELSIF SELF.IsHeader(Progress,cOthers)
                    p1 = 1
                    LOOP WHILE ~SELF.IsBraced(Progress + p1)
                        SELF.GetLine(Progress + p1,cStr)
                        DO AddToFilesList
                        p1 += 1
                    END
                ELSIF SELF.IsHeader(Progress,cKeyl)
                    SELF.GetLine(Progress + 1,cStr)
                    IF cStr
                        FileList.Keys.Name = cStr
                        GET(FileList.Keys,FileList.Keys.Name)
                        IF ERRORCODE()
                            FileList.Keys.Name = cStr
                            ADD(FileList.Keys,FileList.Keys.Name)
                            ASSERT(~ERRORCODE())
                        END
                    END
                END
            END
            IF InsideCalls
                SELF.Getline(Progress,cStr)
                DO AddToCalls
            END
            Progress += 1
        WHILE Progress <= MaxLine
        ProgressMgr.Complete()
        RETURN LEVEL:Benign

ProcessHeaders      ROUTINE
    IF SELF.IsHeader(Progress,cProcedure)
        InsideAddition = False
        CLEAR(ProcedureQueue)
        Progress += 1
        SELF.GetLine(Progress,cStr)
        IF cStr[1 : 4] = 'NAME'                                   !prevents inclusion of [PROCEDURE] section of [EMBED][END] sections
            CurrentProc = cStr[6 : LEN(cStr)]
            ProcedureQueue.Name = CurrentProc
            Progress += 1
            SELF.GetLine(Progress,cStr)
            IF cStr[1 : 9] = 'PROTOTYPE'
                ProcedureQueue.Prototype = cStr[11 : LEN(cStr)]
            END
            ProcedureQueue.Additions &= NEW AdditionList
            ProcedureQueue.Calls &= NEW CallsList
            ADD(ProcedureQueue,ProcedureQueue.Name)
            ASSERT(~ERRORCODE(),'Error inserting ProcedureQueue')
        ELSE                                                  !must be [PROCEDURE] sub-section of [EMBED]
            IF CurrentProc                                      !prevents processing of procedures called directly from main program
                ProcedureQueue.Name = CurrentProc
                GET(ProcedureQueue,ProcedureQueue.Name)
                ASSERT(~ERRORCODE(),'Error reading procedure queue. ' & Error())
                DO AddToCalls
            END
        END
    ELSIF SELF.IsHeader(Progress,cAddition)
        InsideAddition = True
        Progress += 1
        SELF.GetLine(Progress,cStr)
        ASSERT(cStr[1 : 4] = 'NAME','cStr value is [' & cStr & ']')
        AdditionName = cStr[6 : LEN(cStr)]
    ELSIF InsideAddition AND SELF.IsHeader(Progress,cInstance)
        IF CurrentProc                                              !Not interested in global [ADDITION]'s
            ProcedureQueue.Name = CurrentProc
            GET(ProcedureQueue,ProcedureQueue.Name)
            ASSERT(~ERRORCODE(),'Error reading procedure queue. ' & Error())
            Progress += 1
            SELF.GetLine(Progress,cStr)
            ASSERT(cStr[1 : 8] = 'INSTANCE','cStr value is [' & cStr & ']')
            ProcedureQueue.Additions.Name = AdditionName
            ProcedureQueue.Additions.Instance = cStr[10 : LEN(cStr)]
            Progress += 1
            SELF.GetLine(Progress,cStr)
            IF cStr[1 : 6] = 'PARENT'
                ProcedureQueue.Additions.ParentInstance = cStr[8 : LEN(cStr)]
            END
            ADD(ProcedureQueue.Additions,ProcedureQueue.Additions.Instance)
            ASSERT(~ERRORCODE(),'Error inserting procedure queue. ' & Error())
        END
    END

AddToCalls          ROUTINE
    ASSERT(cStr,'cStr [' & cStr & ']')
    ASSERT(~ProcedureQueue &= NULL,'Procedure queue')
    IF ProcedureQueue.Calls &= NULL
        ProcedureQueue.Calls &= NEW CallsList
    END
    ASSERT(~ProcedureQueue.Calls &= NULL,'ProcedureQueue.Calls is NULL')
    ProcedureQueue.Calls.Name = cStr
    GET(ProcedureQueue.Calls,ProcedureQueue.Calls.Name)
    IF ERRORCODE()
        ProcedureQueue.Calls.Name = cStr
        ADD(ProcedureQueue.Calls,ProcedureQueue.Calls.Name)
        ASSERT(~ERRORCODE(),'Error adding to ProcedureQueue.Calls in AddToCalls')
    END

AddToFilesList      ROUTINE
    FileList.Name = cStr
    GET(FileList,FileList.Name)
    IF ERRORCODE()
        FileList.Name = cStr[1 : p1]
        FileList.Keys &= NEW KeyQueue
        ADD(FileList,FileList.Name)
        ASSERT(~ERRORCODE(),'Error adding to FileList')
    END

SectionClass.Init   PROCEDURE(ErrorClass E)
    CODE
        PARENT.Init(E)
        SELF.InQ &= NEW SectionLines
        SELF.OutQ &= NEW SectionLines

SectionClass.Kill   PROCEDURE()
    CODE
        DISPOSE(SELF.InQ)
        DISPOSE(SELF.OutQ)

SectionClass.ClearAll       PROCEDURE()
    CODE
        FREE(SELF.InQ)
        FREE(SELF.OutQ)

SectionClass.Touched        PROCEDURE()
i                               LONG,AUTO

    CODE
        IF ~SELF.EqualLineCount() THEN RETURN True.
        LOOP i = 1 TO RECORDS(SELF.InQ)
            SecQSetInLine(SELF,i)
            SecQSetOutLine(SELF,i)
            IF SELF.InQ.Line <> SELF.OutQ.Line THEN RETURN True.
        END
        RETURN False

SectionClass.EqualLineCount PROCEDURE
    CODE
        RETURN CHOOSE(RECORDS(SELF.InQ)=RECORDS(SELF.OutQ),True,False)

SectionClass.AppendLine     PROCEDURE(STRING Txt,BYTE Indent=2)
cQ                              &SectionLines
cStr                            CSTRING(MaxLineLen),AUTO

    CODE
        cQ &= SELF.OutQ
        DO AppendLineCode
        cQ &= SELF.InQ
        DO AppendLineCode
        CLEAR(cQ.Line)
        cQ.FG_Normal=COLOR:Black
        cQ.FG_Selected=COLOR:Black
        cQ.BG_Normal=COLOR:White
        cQ.BG_Selected=COLOR:Yellow
        PUT(cQ)
        ASSERT(~ERRORCODE())
        SecQSyncColors(SELF,RECORDS(cQ),RECORDS(cQ))

AppendLineCode      ROUTINE
    CLEAR(cQ)
    ADD(cQ)
    ASSERT(~ERRORCODE())
    IF LEN(CLIP(Txt))
        cStr=CHOOSE(Indent=0,Txt,ALL(' ',Indent)&Txt)
        cQ.Line=cStr
        PUT(cQ)
        ASSERT(~ERRORCODE())
    END

SectionClass.CanRestore     PROCEDURE(LONG Pos)
    CODE
        IF ~INRANGE(Pos,1,RECORDS(SELF.InQ)) THEN RETURN False.
        SecQSetInLine(SELF,Pos,True)
        SecQSetOutLine(SELF,Pos,True)
        IF SELF.InQ.Line = SELF.OutQ.Line THEN RETURN False.
        IF ~SELF.InQ.LineNumber THEN RETURN False.
        RETURN True

SectionClass.RestoreLine    PROCEDURE(LONG Pos)
    CODE
        IF INRANGE(Pos,1,RECORDS(SELF.InQ))
            SecQSetInLine(SELF,Pos,True)
            SecQSetOutLine(SELF,Pos,True)
            SELF.OutQ.Line=SELF.InQ.Line
            PUT(SELF.OutQ)
            ASSERT(~ERRORCODE())
            SecQSyncColors(SELF,Pos,Pos)
        END

SectionClass.PrependLine    PROCEDURE(STRING Txt,BYTE Indent=2)
i                               USHORT,AUTO
Fnd                             USHORT(0)

    CODE
        LOOP i = 1 TO RECORDS(SELF.OutQ)
            GET(SELF.OutQ,i)
            ASSERT(~ERRORCODE())
            IF UPPER(SELF.OutQ.Line) = 'PROPERTY:END'
                Fnd = i
                BREAK
            END
        END
        SELF.InsertLine(Txt,Fnd + 1,Indent)

SectionClass.InsertLine     PROCEDURE(STRING Txt,LONG Pos,BYTE Indent=2)
cQ                              &Sectionlines
i                               LONG,AUTO
Ln                              LIKE(SectionLines.LineNumber)
t                               CSTRING(MaxLineLen),AUTO
uC                              LIKE(SectionLines.UserChanged),AUTO

    CODE
        cQ &= SELF.OutQ
        DO InsertLineCode
        IF LEN(CLIP(txt))
            GET(cQ,Pos + 1)
            ASSERT(~ERRORCODE())
            t=CHOOSE(Indent > 0,ALL(' ',Indent) & Txt,Txt)
            cQ.Line = t
            DO AssignDefaultColors
            PUT(cQ)
            ASSERT(~ERRORCODE())
        END
        cQ &= SELF.InQ
        DO InsertLineCode
        Do AssignDefaultColors
        PUT(cQ)
        ASSERT(~ERRORCODE())
        GET(cQ,RECORDS(cQ))
        ASSERT(~ERRORCODE())
        DO AssignDefaultColors
        PUT(cQ)
        ASSERT(~ERRORCODE())
        SecQSyncColors(SELF,Pos,RECORDS(cQ))

AssignDefaultColors ROUTINE
    cQ.FG_Normal=COLOR:Black
    cQ.FG_Selected=COLOR:Black
    cQ.BG_Normal=COLOR:White
    cQ.BG_Selected=COLOR:Yellow
    cQ.UserChanged=False

InsertLineCode      ROUTINE
    IF INRANGE(Pos,1,RECORDS(cQ))
        CLEAR(cQ)
        ADD(cQ)
        ASSERT(~ERRORCODE())
        LOOP i=RECORDS(cQ) - 1 TO Pos BY -1
            GET(cQ,i)
            Ln = cQ.LineNumber
            t = cQ.Line
            uC = cQ.UserChanged
            ASSERT(~ERRORCODE())
            GET(cQ,i + 1)
            ASSERT(~ERRORCODE())
            cQ.LineNumber = Ln
            cQ.Line = t
            cQ.UserChanged = uC
            PUT(cQ)
            ASSERT(~ERRORCODE())
        END
        CLEAR(cQ.LineNumber)
        CLEAR(cQ.Line)
        PUT(cQ)
        ASSERT(~ERRORCODE())
    ELSIF Pos = 0
        SELF.InsertLine(Txt,1,Indent)
    ELSE
        SELF.AppendLine(Txt,Indent)
    END

SectionClass.RemoveLine     PROCEDURE(LONG Pos)
    CODE
        SecQSetInLine(SELF,Pos)
        DELETE(SELF.InQ)
        SecQSetOutLine(SELF,Pos)
        DELETE(SELF.OutQ)
        IF Pos<=RECORDS(SELF.OutQ)
            SecQSetOutLine(SELF,Pos,1)                        !Force new record retrieval
            SecQSetInLine(SELF,Pos,1)
            SecQSyncColors(SELF,Pos,RECORDS(SELF.OutQ))
        END

SectionClass.ClearLine      PROCEDURE(LONG Pos)
    CODE
        SecQSetOutLine(SELF,Pos)
        CLEAR(SELF.OutQ.Line)
        SELF.OutQ.FG_Normal = COLOR:Black
        SELF.OutQ.FG_Selected = COLOR:Black
        SELF.OutQ.BG_Normal = COLOR:White
        SELF.OutQ.BG_Selected = COLOR:Yellow
        SELF.OutQ.UserChanged = False
        PUT(SELF.OutQ)
        ASSERT(~ERRORCODE())

SectionClass.GetLine        PROCEDURE(LONG Pos,*CSTRING Dest)
    CODE
        SecQSetOutLine(SELF,Pos)
        Dest = SELF.OutQ.Line

SectionClass.GetLineNumber  PROCEDURE(LONG Pos)
    CODE
        SecQSetOutLine(SELF,Pos)
        RETURN SELF.OutQ.LineNumber

SectionClass.GetLineCount   PROCEDURE()
    CODE
        RETURN RECORDS(SELF.OutQ)

SectionClass.SetLine        PROCEDURE(LONG Pos,*CSTRING Src)
    CODE
        SecQSetOutLine(SELF,Pos)
        SELF.OutQ.Line = Src
        PUT(SELF.OutQ)
        ASSERT(~ERRORCODE())
        SecQSyncColors(SELF,Pos,Pos)

SectionClass.LineChanged    PROCEDURE(LONG Pos,*CSTRING Txt)
oLine                           CSTRING(MaxLineLen),AUTO
i                               USHORT,AUTO

    CODE
        SecQSetInLine(SELF,Pos)
        i = INSTRING('!',SELF.InQ.Line,1,1)
        oLine = CHOOSE(i = 0,SELF.InQ.Line,SELF.InQ.Line[1: i-1])
        SecQSetOutLine(SELF,Pos)
        RETURN CHOOSE(CLIP(oLine) = CLIP(Txt),False,True)

SectionClass.GetQueueLine   PROCEDURE(LONG LineNo)
i                               LONG,AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.OutQ)
            GET(SELF.OutQ,i)
            ASSERT(~ERRORCODE())
            IF SELF.OutQ.LineNumber = LineNo THEN RETURN i.
        END
        RETURN 0

 ! COMPILE('_END_',_DEBUG_)
SectionClass.ViewOutStream  PROCEDURE()

window                          WINDOW('Section Output Stream'),AT(,,260,215),FONT('Microsoft Sans Serif',8,,FONT:regular),SYSTEM,GRAY, |
                                    DOUBLE
                                    LIST,AT(4,4,252,207),USE(?List1),VSCROLL,FROM(SELF.OutQ.Line)
                                END

    CODE
        OPEN(Window)
        ACCEPT
        END
        CLOSE(Window)

 ! _END_

!  OMIT('_END_',_DEBUG_)
!
!SectionClass.ViewOutStream  PROCEDURE()
!
!  CODE
!
!  _END_

InfoTextClass.Init  PROCEDURE(ErrorClass E)
    CODE
        PARENT.Init(E)
        SELF.Q &= NEW InfoTextQueueType

InfoTextClass.Kill  PROCEDURE()
    CODE
        DISPOSE(SELF.Q)

InfoTextClass.ClearAll      PROCEDURE()
    CODE
        FREE(SELF.Q)

InfoTextClass.AddLine       PROCEDURE(STRING Txt)
    CODE
        SELF.AddLine(Txt,0)

InfoTextClass.AddLine       PROCEDURE(STRING Txt,LONG LineNo)
FirstItem                       BYTE(True)
i                               USHORT,AUTO

    CODE
        LOOP
            i = INSTRING('|',Txt,1,1)
            IF ~i THEN i = LEN(Txt) + 1.
            SELF.Q.TextLine = Translator.TranslateString(Txt[1 : i - 1])
            SELF.Q.LineNo = CHOOSE(LineNo AND FirstItem,LineNo,0)
            IF LineNo >= 0 THEN SELF.Q.TextLine = CHOOSE(SELF.Q.LineNo > 0,Translator.TranslateString('Line ') & SELF.Q.LineNo & ': ',ALL(' ',12)) & SELF.Q.TextLine.
            ADD(SELF.Q)
            ASSERT(~ERRORCODE())
            FirstItem = False
            IF i < LEN(Txt) THEN Txt = Txt[i + 1 : LEN(Txt)].
        WHILE i < LEN(Txt)

InfoTextClass.AppendLastLine        PROCEDURE(STRING Txt)
    CODE
        SELF.Q.TextLine = CLIP(SELF.Q.TextLine) & ' ' & Translator.TranslateString(Txt)
        PUT(SELF.Q)
        ASSERT(~ERRORCODE())

InfoTextClass.AddTitle      PROCEDURE(STRING Txt)
    CODE
        SELF.ClearAll
        SELF.AddLine(Txt & '| ',-1)

InfoTextClass.GetLineNumber PROCEDURE()
    CODE
        IF SELF.ListControl AND CHOICE(SELF.ListControl)
            GET(SELF.Q,CHOICE(SELF.ListControl))
            IF ~ERRORCODE() THEN RETURN SELF.Q.LineNo.
        END
        RETURN 0

InfoTextClass.SetNoteLineNumber     PROCEDURE(LONG LineNo)
i                                       LONG,AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.Q)
            GET(SELF.Q,i)
            ASSERT(~ERRORCODE())
            IF SELF.Q.LineNo = LineNo 
                SELF.ListControl{PROP:Selected} = i
                BREAK
            END
        END

LexerClass.Init     PROCEDURE()
    CODE
        SELF.TokenList &= NEW TokenQueue

LexerClass.Kill     PROCEDURE()
i                       USHORT,AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.TokenList)
            GET(SELF.TokenList,i)
            ASSERT(~ERRORCODE(),'Error reading token list, index [' & i & ']')
            DISPOSE(SELF.TokenList.Item)
        END
        DISPOSE(SELF.TokenList)

LexerClass.TakeLine PROCEDURE(*CSTRING TextLine)
InToks                  EQUATE(' ()=,{{};&<<>%^+-/*.')
i                       USHORT,AUTO
Indent                  USHORT,AUTO
InsideStr               BYTE(False)
Level                   BYTE(1)
LookaHead               LONG(0)
Strt                    LONG(0)
ThisLine                CSTRING(MaxLineLen),AUTO

    CODE
        LOOP i = 1 TO RECORDS(SELF.TokenList)
            GET(SELF.TokenList,i)
            ASSERT(~ERRORCODE(),'Error reading token list, index [' & i & ']')
            DISPOSE(SELF.TokenList.Item)
        END
        FREE(SELF.TokenList)
        IF ~TextLine THEN RETURN.
        i = INSTRING('!',TextLine,1,1)
        IF i = 1
            RETURN
        ELSIF ~i
            i = LEN(Textline) + 1
        END
        ThisLine = TextLine[1 : i - 1]
        LOOP i = 1 TO LEN(ThisLine);WHILE ThisLine[i] = ' '
        Indent = i - 1
        ThisLine = LEFT(ThisLine)
        LOOP
            LookaHead += 1
            IF LookaHead > LEN(ThisLine) THEN BREAK.
            IF ThisLine[LookaHead] = '.' AND (LookaHead = LEN(ThisLine) OR ThisLine[LookaHead + 1] = ' ' OR ThisLine[LookaHead + 1] = '.')  !Traps trailing periods on line
                IF Strt + 1 < LookaHead - 1
                    SELF.TokenList.Item &= NEW CSTRING(LEN(ThisLine[Strt + 1 : LookaHead - 1]) + 1)
                    SELF.TokenList.Item = ThisLine[Strt + 1 : LookaHead - 1]
                    SELF.TokenList.ChrPos = Strt + 1
                    DO AddToken
                END
                SELF.TokenList.Item &= NEW CSTRING(2)
                SELF.TokenList.Item = '.'
                SELF.TokenList.ChrPos = LookaHead
                DO AddToken
                LookaHead += 1
                Strt = LookaHead
                CYCLE
            END
            IF ThisLine[LookaHead]='''' AND ~InsideStr
                Strt = LookaHead
                InsideStr = True
            ELSIF ThisLine[LookaHead] = '''' AND InsideStr
                SELF.TokenList.Item &= NEW CSTRING(LEN(CLIP(ThisLine[Strt : LookaHead])) + 1)
                SELF.TokenList.Item = CLIP(ThisLine[Strt : LookaHead])
                SELF.TokenList.Chrpos = Strt
                DO AddToken
                InsideStr = False
                Strt = LookaHead
            END
            IF ~InsideStr
                IF INSTRING(ThisLine[LookaHead],InToks,1,1)
                    SELF.TokenList.Item &= NEW CSTRING(LEN(CLIP(ThisLine[Strt + 1 : LookAHead - 1])) + 1)
                    SELF.TokenList.Item = CLIP(ThisLine[Strt + 1 : LookAHead - 1])
                    SELF.TokenList.ChrPos = Strt + 1
                    DO AddToken
                    SELF.TokenList.Item &= NEW CSTRING(LEN(ThisLine[LookaHead]) + 1)
                    SELF.TokenList.Item = ThisLine[LookaHead]
                    SELF.TokenList.ChrPos = LookaHead
                    DO AddToken
                    Strt = LookaHead
                ELSIF LookaHead = LEN(ThisLine)
                    SELF.TokenList.Item &= NEW CSTRING(LEN(CLIP(ThisLine[Strt + 1 : LEN(ThisLine)])) + 1)
                    SELF.TokenList.Item = CLIP(ThisLine[Strt + 1 : LEN(ThisLine)])
                    SELF.TokenList.ChrPos=Strt + 1
                    DO AddToken
                END
            END
        END

AddToken            ROUTINE
    IF SELF.TokenList.Item
        SELF.TokenList.ChrPos += Indent
        IF ~InsideStr AND LEN(SELF.TokenList.Item) = 1 AND INSTRING(SELF.TokenList.Item,')}]',1,1)
            Level -= 1
        END
        SELF.TokenList.Level = Level
        !db.Message('Adding to token List: ' & self.TokenList.Item)
        ADD(SELF.TokenList)
        ASSERT(~ERRORCODE())
        IF ~InsideStr AND LEN(SELF.TokenList.Item) = 1 AND INSTRING(SELF.TokenList.Item,'({{[',1,1)
            Level += 1
        END
    !DB.DumpQue('TokenList',SELF.TokenList)
    END

  !COMPILE('_END_',_DEBUG_)
LexerClass.ViewTokens       PROCEDURE()

window                          WINDOW('Lexer Token List'),AT(,,336,177),FONT('Microsoft Sans Serif',8,,FONT:regular),SYSTEM,GRAY,DOUBLE
                                    LIST,AT(4,4,328,168),USE(?TokenList),VSCROLL,FORMAT('259L(1)|~Token~L(2)S(80)@s255@33L(1)|~Nest Lvl~L(2)@n_3@20L(1)|~ChrPos~L(2)@n_5@'), |
                                        FROM(SELF.TokenList)
                                END

    CODE
        OPEN(Window)
        ACCEPT
        END
        CLOSE(Window)
  !_END_


!  OMIT('_END_',_DEBUG_)
!LexerClass.ViewTokens       PROCEDURE()
!
!  CODE
!  _END_

LexerClass.FindToken        PROCEDURE(STRING Token,USHORT StartPos=1)
i                               USHORT,AUTO

    CODE
        !db.Message(Token)
        LOOP i = StartPos TO SELF.GetTokenCount()
            GET(SELF.TokenList,i)
            ASSERT(~ERRORCODE())
            IF UPPER(SELF.TokenList.Item) = UPPER(Token) THEN RETURN i.
        END
        RETURN 0

LexerClass.GetTokenCount    PROCEDURE()
    CODE
        RETURN RECORDS(SELF.TokenList)

LexerClass.GetToken PROCEDURE(USHORT ItemNo)
    CODE
        IF INRANGE(ItemNo,0,RECORDS(SELF.TokenList))
            GET(SELF.TokenList,ItemNo)
            ASSERT(~ERRORCODE())
            RETURN SELF.TokenList.Item
        ELSE
            RETURN ''
        END

LexerClass.GetStartChrPos   PROCEDURE(USHORT ItemNo)
    CODE
        ASSERT(INRANGE(ItemNo,1,RECORDS(SELF.TokenList)))
        GET(SELF.TokenList,ItemNo)
        RETURN SELF.TokenList.ChrPos

LexerClass.GetEndChrPos     PROCEDURE(USHORT ItemNo)
    CODE
        ASSERT(INRANGE(ItemNo,1,RECORDS(SELF.TokenList)))
        GET(SELF.TokenList,ItemNo)
        RETURN SELF.TokenList.ChrPos + LEN(SELF.TokenList.Item) - 1

LexerClass.TokenReplace     PROCEDURE(*CSTRING cLine,*CSTRING[] TokenSequence,*CSTRING ReplacementSequence,FileQueue FileList,<USHORT NumberTokens>)
Found                           BYTE,AUTO
i                               USHORT,AUTO
j                               USHORT,AUTO
k                               USHORT,AUTO
RVal                            BYTE(Level:Benign)
Slice                           CSTRING(MaxLineLen),AUTO
TokenPnt                        USHORT(1)
TokenStart                      USHORT,AUTO
TokenEnd                        USHORT,AUTO

    CODE
        IF OMITTED(NumberTokens) THEN NumberTokens = MAXIMUM(TokenSequence,1).
        ASSERT(INRANGE(NumberTokens,1,MAXIMUM(TokenSequence,1)))
        LOOP
            Found = True
            TokenPnt = SELF.FindToken(TokenSequence[1],TokenPnt)
            IF TokenPnt
                TokenStart = SELF.GetStartChrPos(TokenPnt) - 1
                LOOP i = 1 TO (NumberTokens - 1)
                    IF TokenSequence[i + 1,1] = '!'
                        IF LEN(TokenSequence[i + 1]) > 1
                            CASE UPPER(TokenSequence[i + 1,2 : LEN(TokenSequence[i + 1])])
                            OF 'ANYTHING'     !Can be anything but must be something
                                IF ~SELF.GetToken(TokenPnt + i) THEN Found = False.
                            OF 'KNOWNFILELABEL'
                                Found = False
                                LOOP j = 1 TO RECORDS(FileList)
                                    GET(FileList,j)
                                    ASSERT(~ERRORCODE())
                                    IF SELF.GetToken(TokenPnt + i) = FileList.Name THEN Found = True.
                                WHILE ~Found
                            OF 'KNOWNKEYLABEL'
                                Found = False
                                LOOP j = 1 TO RECORDS(FileList)
                                    GET(FileList,j)
                                    ASSERT(~ERRORCODE())
                                    IF ~FileList.Keys &= NULL
                                        LOOP k = 1 TO RECORDS(FileList.Keys)
                                            GET(FileList.Keys,k)
                                            ASSERT(~ERRORCODE())
                                            IF SELF.GetToken(TokenPnt + i) = FileList.Keys.Name THEN Found = True.
                                        WHILE ~Found
                                    END
                                WHILE ~Found
                            ELSE
                                ASSERT(False)
                            END
                        END
                    ELSE
                        IF SELF.GetToken(TokenPnt + i) <> TokenSequence[i + 1] THEN Found = False.
                    END
                WHILE Found
                IF Found
                    RVal = LEVEL:Notify
                    IF ReplacementSequence
                        TokenEnd = SELF.GetEndChrPos(TokenPnt+NumberTokens - 1) + 1
                        CLEAR(Slice)
                        LOOP
                            i = INSTRING('!',ReplacementSequence,1,1)
                            IF i
                                ASSERT(NUMERIC(ReplacementSequence[i + 1]) AND NUMERIC(ReplacementSequence[i + 2]))
                                ReplacementSequence = ReplacementSequence[1 : i - 1] & SELF.GetToken(TokenPnt + ReplacementSequence[i + 1 : i + 2] - 1) & ReplacementSequence[i + 3 : LEN(ReplacementSequence)]
                            END
                        WHILE i
                        Slice = CLIP(Slice) & ReplacementSequence
                        cLine = SUB(cline,1,TokenStart) & Slice & cLine[TokenEnd : LEN(cLine)]
                    END
                END
            ELSE
                BREAK
            END
            TokenPnt += 1
        END
        RETURN RVal

