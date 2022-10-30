!Clarion Application Converter - Engine Definition Module
!Copyright TopSpeed Corporation - All rights reserved

  INCLUDE('ABERROR.INC'),ONCE                                !ErrorManager from TopSpeed's ABC's
  INCLUDE('ABFILE.INC'),ONCE                                 !FileManager from TopSpeed's ABC's
  INCLUDE('ABUTIL.INC'),ONCE

!--- Constants ------------------------------------------------------------------------------

MaxLineLen          EQUATE(2048)                        !Maximum line length
YieldGranularity    EQUATE(128)                         !Sets the yield frequence for long loops

OwnerTabFeq         EQUATE(100)
OwnerTabTitle       EQUATE(150)
OwnerButtonFeq      EQUATE(200)
PromptFeq           EQUATE(300)
ComboFeq            EQUATE(400)

              ITEMIZE(1000),PRE(Msg)
DDEConnectErr       EQUATE        !0
SourceNameReq       EQUATE        !1
DestNameReq         EQUATE        !2
SourceNotFound      EQUATE        !3
DDEError            EQUATE        !4
NothingToDo         EQUATE        !5
FileNotFound        EQUATE        !6
SrcAndDestSame      EQUATE        !7
BadTXAOpen          EQUATE        !8
BadCreate           EQUATE        !9
BadTXA              EQUATE        !10
OutTXAWriteErr      EQUATE        !11
BadTXAExport        EQUATE        !12
BadTXAImport        EQUATE        !13
QueueAccessError    EQUATE        !14
OutputAPPorTXA      EQUATE        !15
DLLNotLoaded        EQUATE        !16
              END

Access:Exists       EQUATE(0)                         !Parameters of the C libray _access function
Access:WriteEnable  EQUATE(2)
Access:ReadEnable   EQUATE(4)

Action:Cancel       EQUATE(0)                         !make no changes
Action:Apply        EQUATE(2)                         !make all changes
Action:OmitCode     EQUATE(4)                         !Omit EMBED section
Action:UnCompile    EQUATE(8)                         !Make EMBED section uncompilable
Action:AssertFail   EQUATE(16)                        !Insert an ASSERTion failure
Action:Abort        EQUATE(32)                        !Abort conversion process



!--- Defined Types ------------------------------------------------------------------------

FNInfoType          GROUP,TYPE
Path                  CSTRING(File:MaxFilePath)
Drive                 CSTRING(3)
Directory             CSTRING(File:MaxFilePath)
Name                  CSTRING(9)
Extension             CSTRING(5)
                    END

AddinRecordType     GROUP,TYPE
Owner                 CSTRING(65)
Priority              BYTE                            !1->Lowest 255->Highest, Highest priority first
Name                  CSTRING(33)
PromptText            CSTRING(33)
Sections              CSTRING(65)
EmptySections         BYTE
AllowAuto             BYTE
                    END

ReplaceBlockItems   GROUP,TYPE
Number                USHORT
                    END

AdditionList        QUEUE,TYPE
Instance              BYTE                                !Instance number of additions
ParentInstance        BYTE
Name                  CSTRING(64)                         !Name of addition
                    END

CallsList           QUEUE,TYPE
Name                  CSTRING(64)
                    END

ProcedureList       QUEUE,TYPE
Name                  CSTRING(64)                         !procedure name
Prototype             CSTRING(64)                         !procedure prototype
Calls                 &CallsList                          !ref to list of procedures declared as called by this procedure
Additions             &AdditionList                       !ref to list of additions
                    END

KeyQueue            QUEUE,TYPE
Name                  CSTRING(64)
                    END

FileQueue           QUEUE,TYPE
Name                  CSTRING(64)
Keys                  &KeyQueue
                    END

TextLines           QUEUE,TYPE
Positional            LONG
InLine                &CSTRING
OutLine               &CSTRING
                    END

SectionLines        QUEUE,TYPE
LineNumber            STRING(5)
Line                  CSTRING(MaxLineLen)
FG_Normal             LONG
BG_Normal             LONG
FG_Selected           LONG
BG_Selected           LONG
UserChanged           BYTE
                    END

TerminatorList      QUEUE,TYPE                          !referenced in TXAQManagerClass
Sections              CSTRING(MaxLineLen)
Terminators           CSTRING(MaxLineLen)
                    END

InfoTextQueueType   QUEUE,TYPE
TextLine              STRING(80)
LineNo                LONG
                    END

TokenQueue          QUEUE,TYPE
Item                  &CSTRING
Level                 BYTE
ChrPos                LONG
                    END


OwnerMapType        QUEUE,TYPE                      !Maps owners to tabs
Owner                 CSTRING(65)
ButtonID              USHORT                        !Feq of button for this owner
TabID                 USHORT                        !Feq of tab for this owner
Combos                BYTE                          !No of Combos for this owner
                    END



!--- Class Definitions -------------------------------------------------------------------
!--- LexerClass publics provide part of the Rule addin API via RuleClass reference -------

LexerClass          CLASS,TYPE,MODULE('CNVENG')
TokenList             &TokenQueue,PRIVATE
Init                  PROCEDURE()
Kill                  PROCEDURE()
FindToken             PROCEDURE(STRING Token,USHORT StartPos=1),USHORT
GetEndChrPos          PROCEDURE(USHORT TokenNumber),LONG
GetStartChrPos        PROCEDURE(USHORT TokenNumber),LONG
GetToken              PROCEDURE(USHORT TokenNumber),STRING
GetTokenCount         PROCEDURE(),USHORT
TakeLine              PROCEDURE(*CSTRING TextLine)
TokenReplace          PROCEDURE(*CSTRING cLine,*CSTRING[] TokenSequence,*CSTRING Replacement,FileQueue FileList,<USHORT NumberTokens>),BYTE
ViewTokens            PROCEDURE()                       !Debug only
                    END

!--- RuleClass publics provide part of the Rule addin API --------------------------------

RuleClass           CLASS,TYPE,MODULE('CNVENG.CLW')
Buttons               USHORT
FileList              &FileQueue,PROTECTED
LastComment           CSTRING(255),PRIVATE
LastCommentPos        USHORT,PRIVATE
Lexer                 &LexerClass,PROTECTED           !Lexer methods form part of the Rule addin API
ProcedureQueue        &ProcedureList,PROTECTED
WindowSize            GROUP                           !Prefered window size for interface comsistency
XPos                    SHORT
YPos                    SHORT
Width                   SHORT
Height                  SHORT
                      END
Construct             PROCEDURE(),PRIVATE
Destruct              PROCEDURE(),PRIVATE
AfterConversion       PROCEDURE(),VIRTUAL               !Called in Rule priority sequence after full conversion
BeforeConversion      PROCEDURE(),VIRTUAL               !Called in Rule priority sequence before any conversion
CurrentAddition       PROCEDURE(),STRING
CurrentEMBED          PROCEDURE(),STRING
CurrentProcedure      PROCEDURE(),STRING
IsKnownFileLabel      PROCEDURE(STRING FileLabel),BYTE
IsKnownKeyLabel       PROCEDURE(STRING KeyLabel),BYTE
PreFilterLine         PROCEDURE(*CSTRING LinesText,STRING Contains),BYTE
Register              PROCEDURE(BYTE Priority,STRING Owner,STRING Descrip,STRING Prompt,STRING Sections,BYTE EmptySections=False,BYTE AllowAutomatic=True)
Replace               PROCEDURE(*CSTRING TargetStr,STRING FindStr,STRING ReplaceStr,BYTE ReplaceAll=True),BYTE,PROC
RestoreComment        PROCEDURE(*CSTRING)
SaveComment           PROCEDURE(*CSTRING Txt),USHORT
TakeSection           PROCEDURE(SectionClass SecQMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
WildInstring          PROCEDURE(STRING LookFor,STRING Str,*? StartPos,*? Length),BYTE
WildReplace           PROCEDURE(*CSTRING TargetStr,STRING FindStr,STRING ReplaceStr,BYTE ReplaceAll=True),BYTE,PROC
WildReplace           PROCEDURE(*CSTRING TargetStr,ReplaceBlockItems,SectionClass SectionMgr,InfoTextClass Info,LONG LineNo)
                    END

hInstancesQueue     QUEUE,TYPE
Item                  UNSIGNED
                    END

ConvertorINIClass   CLASS(INIClass),TYPE,MODULE('CNVENG.CLW')
ErrMgr                &ErrorClass,PRIVATE
hInstances            &hInstancesQueue,PRIVATE        !Hold the instance handles of each rule dll
Init                  PROCEDURE(ErrorClass ErrMgr)
Kill                  PROCEDURE()
LoadRuleDlls          PROCEDURE()
TryFetchAppNames      PROCEDURE(*CSTRING NameIn,*CSTRING NameOut)
UpdateAppNames        PROCEDURE(*CSTRING NameIn,*CSTRING NameOut)
                    END

AddinQueueType      QUEUE,TYPE
Info                  LIKE(AddinRecordType)
ComboUseIdx           BYTE                            !Pointer to combo's use variable
TabFeq                USHORT                          !Feq of tab combo is populated on
Mgr                   &RuleClass
                    END

DDEMgrClass         CLASS,TYPE,MODULE('CNVENG.CLW')
CWServer              LONG,PRIVATE
ErrorManager          &ErrorClass,PRIVATE
LogFile               &FILE !,PRIVATE
LogFileLine           &STRING,PRIVATE
Logging               BYTE,PRIVATE
AppendLogLine         PROCEDURE(STRING Txt),PRIVATE
Connect               PROCEDURE(),BYTE,PRIVATE
Disconnect            PROCEDURE,PRIVATE
ExportTXA             PROCEDURE(*CSTRING AppName),BYTE,PRIVATE
Init                  PROCEDURE(ErrorClass ErrMgr,BYTE LogConversation=False),PRIVATE
ImportTXA             PROCEDURE(*CSTRING AppName),PRIVATE
Kill                  PROCEDURE()
ShowErrorInfo         PROCEDURE(USHORT ErrNo,USHORT ErrId),PRIVATE
                    END

InTXAClass          CLASS(FileManager),TYPE,MODULE('CNVENG.CLW')
FileSize              LONG,PRIVATE
Line                  &STRING,PRIVATE
Init                  PROCEDURE(ErrorClass ErrMgr),PRIVATE
Kill                  PROCEDURE(),VIRTUAL,PRIVATE
Next                  PROCEDURE(),BYTE,PROC,DERIVED
ReadFile              PROCEDURE(ApplicationClass TXAQMgr,ProgressManagerClass Progress),BYTE,PRIVATE
                    END

OutTXAClass         CLASS(FileManager),TYPE,MODULE('CNVENG.CLW')
Line                  &STRING,PRIVATE
Init                  PROCEDURE(ErrorClass ErrMgr),PRIVATE
Kill                  PROCEDURE(),VIRTUAL,PRIVATE
Update                PROCEDURE(),BYTE,PROC,DERIVED
WriteFile             PROCEDURE(ApplicationClass TXAQMgr,ProgressManagerClass Progress),BYTE,PRIVATE
                    END

ProgressManagerClass CLASS,TYPE,MODULE('CNVENG.CLW')
BaseTrigger           BYTE,PRIVATE
Calls                 LONG,PRIVATE
Direction             BYTE,PRIVATE
Granularity           LONG,PRIVATE
ProgressControl       SHORT,PRIVATE
Trigger               LONG,DIM(100),PRIVATE
ClearTriggers         PROCEDURE(),PRIVATE
Complete              PROCEDURE(),PRIVATE
Init                  PROCEDURE(SHORT ControlID),PRIVATE
Reset                 PROCEDURE(BYTE UpdateDisplay=True),PRIVATE
SetPercentile         PROCEDURE(BYTE Percentile),PRIVATE
SetTriggers           PROCEDURE,PRIVATE
SetTriggers           PROCEDURE(*LONG[] TriggerValues,BYTE BaseTrigger=1,LONG Granularity),PRIVATE
SetTriggers           PROCEDURE(LONG LoRange,LONG HiRange,BYTE NoTriggers=100,BYTE BaseTrigger=1,<LONG Granularity>),PRIVATE
Update                PROCEDURE(LONG CheckValue),PRIVATE
                    END

BlockClass          CLASS,TYPE,MODULE('CNVENG.CLW')   !Abstract class -- do not instantiate
ErrMgr                &ErrorClass,PRIVATE
ErrorManager          PROCEDURE(),PRIVATE               !Generic error handler
Init                  PROCEDURE(ErrorClass E),PRIVATE
                    END

ApplicationClass    CLASS(BlockClass),TYPE,MODULE('CNVENG.CLW')
CurrentLine           &CSTRING,PRIVATE
NextID                LONG,PRIVATE
TermQ                 &TerminatorList,PRIVATE
TXAQ                  &TextLines,PRIVATE
AppendLine            PROCEDURE(*CSTRING L),BYTE,PRIVATE
AppendToSectionQ      PROCEDURE(LONG Idx,SectionClass SecQMgr),PRIVATE
BuildPositionals      PROCEDURE(<ProgressManagerClass ProgressMgr>),BYTE,PRIVATE
GetLine               PROCEDURE(LONG Idx),STRING,PRIVATE
GetLine               PROCEDURE(LONG Idx,*CSTRING Dest),PRIVATE
GetMaxLineNo          PROCEDURE(),LONG,PRIVATE
Init                  PROCEDURE(ErrorClass E),PRIVATE
IsEND                 PROCEDURE(LONG Idx),BYTE,PRIVATE
IsBraced              PROCEDURE(LONG Idx),BYTE,PRIVATE                                   !returns true if line is a section header
IsHeader              PROCEDURE(LONG Idx,*CSTRING SectionHeader,<*CSTRING MatchedHeader>),BYTE,PRIVATE
IsPrompt              PROCEDURE(LONG Idx),BYTE,PRIVATE
IsTerminator          PROCEDURE(*CSTRING SectionHeader,LONG Idx),BYTE,PRIVATE            !returns true if the line pointer to by idx contains a vaild terminator for the the section specified by SectionHeader
Kill                  PROCEDURE(),PRIVATE
PreProcess            PROCEDURE(ProcedureList ProcedureQueue,FileQueue FileList,ProgressManagerClass ProgressMgr,SHORT ProgressPromptFeq),BYTE,PRIVATE
ReplaceBlock          PROCEDURE(LONG StartIdx,LONG EndIdx,SectionClass SecQMgr),LONG,PROC,PRIVATE !Replaces the section of the TXQ StartIdx to EndIdx with the contents of the section manaager out going Q, returns length of TXAQ after replacement
SetLine               PROCEDURE(LONG Idx,*CSTRING Txt)
                    END

!--- SectionClass & InfoTextClass publics provide part of the Rule addin API -----------------------------------

InfoTextClass       CLASS(BlockClass),TYPE,MODULE('CNVENG.CLW')
ListControl           SHORT,PRIVATE
Q                     &InfoTextQueueType,PRIVATE
Init                  PROCEDURE(ErrorClass ErrMgr),PRIVATE
Kill                  PROCEDURE(),PRIVATE
GetLineNumber         PROCEDURE(),LONG,PRIVATE
SetNoteLineNumber     PROCEDURE(LONG LineNo),PRIVATE
AddLine               PROCEDURE(STRING Txt)
AddLine               PROCEDURE(STRING Txt,LONG LineNo)
AddTitle              PROCEDURE(STRING Txt)
AppendLastline        PROCEDURE(STRING Txt)
ClearAll              PROCEDURE
                    END

SectionClass        CLASS(BlockClass),TYPE,MODULE('CNVENG.CLW')
InQ                   &SectionLines,PRIVATE
OutQ                  &SectionLines,PRIVATE
CanRestore            PROCEDURE(LONG Pos),BYTE,PRIVATE
ClearAll              PROCEDURE(),PRIVATE
EqualLineCount        PROCEDURE(),BYTE,PRIVATE
GetLineNumber         PROCEDURE(LONG Pos),LONG,PRIVATE                     !Returns a text line number equivilent to a list box line
GetQueueLine          PROCEDURE(LONG LineNo),LONG,PRIVATE                  !Returns the queue line equivilent to code LineNo
Init                  PROCEDURE(ErrorClass E),PRIVATE
Kill                  PROCEDURE(),PRIVATE
RestoreLine           PROCEDURE(LONG Pos),PRIVATE
AppendLine            PROCEDURE(STRING Txt,BYTE Indent=2)                 !Appends Txt line to end of Output stream
ClearLine             PROCEDURE(LONG Pos)
GetLine               PROCEDURE(LONG Pos,*CSTRING Dest)
GetLineCount          PROCEDURE(),LONG
InsertLine            PROCEDURE(STRING Txt,LONG Pos,BYTE Indent=2)        !Inserts a line into output stream at position Pos
LineChanged           PROCEDURE(LONG Pos,*CSTRING Txt),BYTE
PrependLine           PROCEDURE(STRING Txt,BYTE Indent=2)                 !Adds Txt Line to start of section
RemoveLine            PROCEDURE(LONG Pos)
SetLine               PROCEDURE(LONG Pos,*CSTRING Src)
Touched               PROCEDURE(),BYTE
ViewOutStream         PROCEDURE()                                           !Debug Only
                    END


  