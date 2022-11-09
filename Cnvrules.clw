!TopSpeed Application Conversion Rules
!Copyright TopSpeed Corporation - All rights reserved
!Modified by RADFusion International, LLC for use in Clarion 7.x and beyond
!Open sourced by kind permission of Robert Zaunere
!In Memory of Russell Eggen


                    MEMBER()

  INCLUDE('CNVENG.INC'),ONCE

                    MAP
InitializeDLL         PROCEDURE,NAME('InitializeDLL')
                    END

OwnerName           EQUATE('Clarion 7.3 Application')

                    ITEMIZE(1),PRE()
LocateOnValue         EQUATE
LocateOnPosition      EQUATE
                    END

InsideVirtuals      STRING('%BrowseBoxDoubleClickHandler %BrowseBoxNotEmpty %BrowseBoxEmpty %AlertCaseKEYCODE '&|
                      '%RecordOutOfRange %RecordFilter %AfterRangeFilterCheck %FormatBrowse %EndOfFormatBrowse '&|
                      '%StartFillForwardRoutine %StartFetchRoutineForward %EndOfFillForwardRoutine '&|
                      '%EndFillRecordRoutineForward %StartBackwardRoutine %StartFillRecordRoutineBackward '&|
                      '%EndBackwardRoutine %EndFillRoutineBackward')


UnLinkedQ           QUEUE,TYPE
Name                  CSTRING(65)
ProperName            CSTRING(65)
Referenced            BYTE
                    END

AscViewer           CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

BadLocals           CLASS(RuleClass)
State                 BYTE,PRIVATE
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

BrowseFormula       CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

BrwRoutine          CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

BrwQueue            CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

BrwFeq              CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
VirtualFeqCheck       PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING FeqLabel,STRING Replacement,*CSTRING cLine,LONG LineNo)
                    END

Hints               CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

UnTerminatedOmits   CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
TerminatorFound       PROCEDURE(SectionClass SectionMgr,STRING LookFor,LONG StartLine),BYTE
                    END

StdFunc             CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

TBarEqu             CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

WinRoutine          CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

RepRoutine          CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

ProcRoutine         CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

FileAccess          CLASS(RuleClass)
lDev                  SHORT

Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
ReplaceFcall          PROCEDURE(*CSTRING cLine,LONG i,STRING Func,STRING NewFunc,SectionClass SectionMgr, InfoTextClass Info)
                    END

ChangeTplChain      CLASS(RuleCLass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

UnLinkedProc        CLASS(RuleClass)
ThisProcedure         CSTRING(64),PRIVATE
UnLinked              &UnLinkedQ,PRIVATE

BeforeConversion      PROCEDURE,VIRTUAL
Construct             PROCEDURE()
Destruct              PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

RptDetailUse        CLASS(RuleClass)
Construct             PROCEDURE()
TakeSection           PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,VIRTUAL
                    END

InitializeDLL       PROCEDURE()

  CODE

!--Ascii Viewer rules ----------------

AscViewer.Construct PROCEDURE()

  CODE
  SELF.Register(10,OwnerName,'Ascii Viewer Template Checker','&Change ASCII Box:','[ADDITION]')

AscViewer.TakeSection   PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                     CSTRING(MaxLineLen),AUTO

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply
  Info.AddTitle('ASCII Viewer Changes')
  SectionMgr.GetLine(1,cLine)
  IF cLine[1:4]='NAME'
    IF SELF.Replace(cLine,'ASCIIBox','AsciiViewControl')
      Info.AddLine('ASCIIBox control template is now AsciiViewControl',1)
    END
    IF SELF.Replace(cLine,'ASCIISearchButton','AsciiViewSearchButton')
      Info.AddLine('ASCIISearchButton control template is now AsciiViewSearchButton',1)
    END
    IF SELF.Replace(cLine,'ASCIIPrintButton','AsciiViewPrintButton')
      Info.AddLine('ASCIIPrintButton control template is now AsciiViewPrintButton',1)
    END
    IF SectionMgr.LineChanged(1,cLine)
      SectionMgr.SetLine(1,cLine)
    END
  END
  RETURN Level:Benign

!--Browse Formula rules

BrowseFormula.Construct PROCEDURE()

  CODE
  SELF.Register(25,OwnerName,'Browse Formula Checker','Browse &Formula:','[FORUMLA]')

BrowseFormula.TakeSection   PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                         CSTRING(MaxLineLen),AUTO
i                             LONG,AUTO

  CODE


  RETURN Level:Benign

!--Browse update rules-----

BrwRoutine.Construct    PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'Browse Routine Checker','&Browse Routines:','[SOURCE]')

BrwRoutine.TakeSection  PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

ForVirtual                GROUP
Number                      USHORT(6)
                            PSTRING('DO BRW!::InitializeBrowse')
                            PSTRING('SELF.ResetFromView')
                            PSTRING('InitializeBrowse routine now ResetFromView method')
                            PSTRING('DO BRW!::NewSelection')
                            PSTRING('SELF.TakeNewSelection()')
                            PSTRING('NewSelection routine now TakeNewSelection method.')
                            PSTRING('DO BRW!::AssignButtons')
                            PSTRING('Toolbar.SetTarget(SELF.ListControl)')
                            PSTRING('AssignButtons routine now Toolbar.SetTarget method : HINT Probably redundant')
                            PSTRING('DO BRW!::RefreshPage')
                            PSTRING('SELF.ResetSort(1)')
                            PSTRING('RefreshPage routine now ResetSort method.')
                            PSTRING('DO BRW!::GetRecord')
                            PSTRING('SELF.UpdateBuffer')
                            PSTRING('GetRecord routine call now UpdateBuffer method.')
                            PSTRING('DO BRW!::PostNewSelection')
                            PSTRING('SELF.PostNewSelection')
                            PSTRING('PostNewSelection routine now PostNewSelection method.')
                          END

ForInLine                 GROUP
Number                      USHORT(6)
                            PSTRING('DO BRW!::InitializeBrowse')
                            PSTRING('BRW!.ResetFromView')
                            PSTRING('InitializeBrowse routine now ResetFromView method')
                            PSTRING('DO BRW!::NewSelection')
                            PSTRING('BRW!.TakeNewSelection()')
                            PSTRING('NewSelection routine now TakeNewSelection method.')
                            PSTRING('DO BRW!::AssignButtons')
                            PSTRING('Toolbar.SetTarget(BRW!.ListControl)')
                            PSTRING('AssignButtons routine now Toolbar.SetTarget method : HINT Probably redundant')
                            PSTRING('DO BRW!::RefreshPage')
                            PSTRING('BRW!.ResetSort(1)')
                            PSTRING('RefreshPage routine now ResetSort method.')
                            PSTRING('DO BRW!::GetRecord')
                            PSTRING('BRW!.UpdateBuffer')
                            PSTRING('GetRecord routine now UpdateBuffer method.')
                            PSTRING('DO BRW!::PostNewSelection')
                            PSTRING('BRW!.PostNewSelection')
                            PSTRING('PostNewSelection routine now PostNewSelection method.')
                          END

cLine                     CSTRING(MaxLineLen),AUTO
CurrentLocateMode         BYTE(0)
i                         LONG(1)
j                         USHORT,AUTO
Prefix                    CSTRING(5),AUTO
Replacement               STRING(32),AUTO
SPos                      USHORT,AUTO
SLen                      USHORT,AUTO

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: '&SELF.CurrentEMBED()&CHOOSE(INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1)>0,' Inside VIRTUAL',''))
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine) AND SELF.PreFilterLine(cLine,'BRW|DO FORM::AssignButtons')
      IF SELF.WildInstring('BRW!::LocateMode',UPPER(cLine),SPos,SLen)
        SELF.Lexer.TakeLine(cLine)
        j=SELF.Lexer.FindToken(SUB(cLine,SPos,SLen))
        ASSERT(j)
        IF SELF.Lexer.GetToken(j+1)='='
          IF UPPER(SELF.Lexer.GetToken(j+2))='LOCATEONVALUE'
            CurrentLocateMode=LocateOnValue
          ELSIF UPPER(SELF.Lexer.GetToken(j+2))='LOCATEONVALUE'
            CurrentLocateMode=LocateOnPosition
          END
          cLine='!'&cline
          Info.AddLine('LocateMode variable no available',i)
        END
      END
      IF SELF.WildInstring('DO BRW!::LocateRecord',UPPER(cLine),SPos,SLen)
        IF CurrentLocateMode=LocateOnValue
          Replacement=CHOOSE(INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1)>0,'SELF.ResetQueue(Reset:Queue)','BRW!.ResetQueue(Reset:Queue)')
          Info.AddLine('LocateRecord routine call replaced by ResetQueue method call',i)
        ELSIF CurrentLocateMode=LocateOnposition
          Replacement=CHOOSE(INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1)>0,'SELF.ResetFromFile','BRW!.ResetFromFile')
          Info.AddLine('LocateRecord routine call replaced by ResetFromFile method call',i)
        ELSE
          Replacement=CHOOSE(INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1)>0,'SELF.ResetQueue(Reset:Queue)','BRW!.ResetQueue(Reset:Queue)')
          Info.AddLine('LocateRecord routine call replaced by ResetQueue method call',i)
        END
        SELF.WildReplace(cLine,'DO BRW!:LocateRecord',Replacement,True)
      END
      IF INSTRING(SELF.CurrentEMBED(),Insidevirtuals,1,1)
        SELF.WildReplace(cLine,ForVirtual,SectionMgr,Info,i)
      ELSE
        SELF.WildReplace(cLine,ForInline,SectionMgr,Info,i)
      END
      IF SELF.WildInstring('BRW!::RefreshMode',UPPER(cLine),SPos,SLen)
        SectionMgr.ClearLine(i)
        Info.AddLine('RefreshMode variable no longer exists.',i)
      END
      j=INSTRING('DO FORM::ASSIGNBUTTONS',UPPER(cLine),1,1)
      IF j
        cLine=SUB(cLine,1,j-1)&'Toolbar.SetTarget(-1)'&cLine[j+22 : LEN(cline)]
        Info.Addline('DO FORM:AssignButtons redirected to Toolbar.SetTarget method',i)
      END
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

BrwQueue.Construct  PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'Browse Queue Checker','Browse &Queue:','[SOURCE]')

BrwQueue.TakeSection    PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                     CSTRING(MaxLineLen),AUTO
i                         LONG(1)
Length                    USHORT,AUTO
Method                    CSTRING(64),AUTO
p                         BYTE,AUTO
Repl                      CSTRING(256),AUTO
StartPos                  USHORT,AUTO

  CODE
  SELF.Buttons = Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: ' & SELF.CurrentEMBED() & CHOOSE(INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1) > 0,' Inside VIRTUAL',''))
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine) AND SELF.PreFilterLine(cLine,'BRW|:Browse')
      IF SELF.WildInstring('BRW!::',cLine,StartPos,Length) AND ~INSTRING('VIEW:BROWSE',UPPER(cLine),1,1)                   !Trap usage of queue variables
        IF StartPos >= 3 AND SUB(cLine,INSTRING('DO ',UPPER(cLine),1,3),3)
          IF INSTRING(SELF.CurrentEmbed(),InsideVirtuals,1,1)
            IF SELF.WildReplace(cLine,'BRW!::','SELF.Q.') THEN Info.AddLine('Queue variables inside VIRTUALs now accessed as SELF.Q.',i).
          ELSE
            IF SELF.WildReplace(cLine,'BRW!::','BRW!.Q.') THEN Info.AddLine('Queue variables now accessed as BRWx.Q.',i).
          END
        END
      END
      IF INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1)
        LOOP
          p = INSTRING('QUEUE:BROWSE:',UPPER(cLine),1,1)
          IF p
            IF NUMERIC(cline[p + 13])
              SELF.Replace(cLine,SUB(cLine,p,14),'SELF.Q')
              Info.AddLine('Queue:Browse:x available in VIRTUALS as SELF.Q.',i)
            END
          ELSE
            BREAK
          END
        END
        IF SELF.Replace(cLine,'Queue:Browse','SELF.Q')
          Info.AddLine('Queue:Browse available in VIRTUALS as SELF.Q.')
        END
      END
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i += 1
  WHILE i <= SectionMgr.GetLineCount()
  RETURN LEVEL:Benign

BrwFeq.Construct    PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'Browse Feq Checker','Browse &Feq''s:','[SOURCE]')

BrwFeq.TakeSection  PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                 CSTRING(MaxLineLen),AUTO
i                     LONG(1)

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: '&SELF.CurrentEMBED()&CHOOSE(INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1)>0,' Inside VIRTUAL',''))
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine) AND SELF.PreFilterLine(cLine,'?')
      IF INSTRING(SELF.CurrentEMBED(),InsideVirtuals,1,1)
        SELF.VirtualFeqCheck(SectionMgr,Info,'?List','SELF.ListControl',cLine,i)
        SELF.VirtualFeqCheck(SectionMgr,Info,'?Change','SELF.ChangeControl',cLine,i)
        SELF.VirtualFeqCheck(SectionMgr,Info,'?Insert','SELF.InsertControl',cLine,i)
        SELF.VirtualFeqCheck(SectionMgr,Info,'?Delete','SELF.DeleteControl',cLine,i)
        SELF.VirtualFeqCheck(SectionMgr,Info,'?Select','SELF.SelectControl',cLine,i)
      END
      IF SectionMgr.LineChanged(i,cLine) THEN
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

BrwFeq.VirtualFeqCheck  PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING FeqLabel,STRING Replacement,*CSTRING cline,LONG LineNo)

p                         USHORT,AUTO

  CODE
  LOOP
    p=INSTRING(UPPER(FeqLabel)&':',UPPER(cLine),1,1)
    IF p
      IF NUMERIC(cline[p+LEN(FeqLabel)+1])
        SELF.Replace(cLine,SUB(cLine,p,LEN(FeqLabel)+2),Replacement)
        Info.AddLine(FeqLabel&':x available in VIRTUALS as '&Replacement&'.',LineNo)
      END
    ELSE
      BREAK
    END
  END
  IF SELF.Replace(cLine,FeqLabel,Replacement)
    Info.AddLine(FeqLabel&' available in VIRTUALS as '&Replacement&'.',LineNo)
  END

!---File Access Checker--------------

FileAccess.Construct    PROCEDURE()

  CODE
  SELF.Register(250,OwnerName,'Direct File Access Checker','&File Accesses:','[SOURCE]')

FileAccess.TakeSection  PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                     CSTRING(MaxLineLen),AUTO
i                         LONG(1)
InsLine                   CSTRING(MaxLineLen)
Replace                   CSTRING(MaxLineLen),AUTO
TokenPnt                  USHORT,AUTO

  CODE
  SELF.LDev = 0
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: '&SELF.CurrentEMBED())
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine) AND SELF.PreFilterLine(cLine,'Open|Close|Next|Previous|Put|Get|Add')
      SELF.Lexer.TakeLine(cLine)
      SELF.ReplaceFCall(cLine,i,'CLOSE','Close',SectionMgr,Info)
      SELF.ReplaceFCall(cLine,i,'NEXT','Next',SectionMgr,Info)
      SELF.ReplaceFCall(cLine,i,'PREVIOUS','Previous',SectionMgr,Info)
      SELF.ReplaceFCall(cLine,i,'PUT','Update',SectionMgr,Info)
      SELF.ReplaceFCall(cLine,i,'ADD','Insert',SectionMgr,Info)
      IF SELF.PreFilterLine(cLine,'Get') THEN DO CheckForGET.
      IF SELF.PreFilterLine(cLine,'Get') THEN DO CheckForOPEN.
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
      IF InsLine                                      !new line required to accomodate access mode setup
        Info.AddLine('Access mode copied to FileManager property.')
        SectionMgr.InsertLine(InsLine,i-1,0)
        CLEAR(InsLine)
        SELF.lDev-=1
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

CheckForOPEN        ROUTINE
  DATA
j   USHORT,AUTO
TokenStart  USHORT,AUTO
TokenEnd    USHORT,AUTO
Tokens  CSTRING(32),DIM(6),AUTO
  CODE
  Tokens[1]='OPEN'
  Tokens[2]='('
  Tokens[3]='!KnownFileLabel'
  Tokens[4]=')'
  Replace='Access:!03.Open'
  IF SELF.Lexer.TokenReplace(cLine,Tokens,Replace,SELF.FileList,4)=Level:Notify
    Info.AddLine('OPEN redirected to FileManager''s Open method.',i)
  ELSE
    Tokens[4]=','
    Tokens[5]='!Anything'
    Tokens[6]=')'
    Replace=''                                                                        !Check but don't replace
    IF SELF.Lexer.TokenReplace(cLine,Tokens,Replace,SELF.FileList,6)=Level:Notify
      TokenPnt=1
      LOOP
        TokenPnt=SELF.Lexer.FindToken('OPEN',TokenPnt)
        IF TokenPnt
          TokenStart=SELF.Lexer.GetStartChrPos(TokenPnt)-1
          IF SELF.Lexer.GetToken(TokenPnt+1)='(' AND SELF.IsKnownFileLabel(SELF.Lexer.GetToken(TokenPnt+2)) AND (SELF.Lexer.GetToken(TokenPnt+3)=',' OR SELF.Lexer.GetToken(TokenPnt+3)=')')
            j=SELF.Lexer.FindToken(')',TokenPnt)
            ASSERT(j>0)
            TokenEnd=SELF.Lexer.GetEndChrPos(j)+1
            IF TokenEnd<LEN(cLine)
              cLine=SUB(cLine,1,TokenStart)&'Access:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Open'&cLine[TokenEnd : LEN(cLine)]
            ELSE
              cLine=SUB(cLine,1,TokenStart)&'Access:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Open'
            END
            Info.AddLine('OPEN redirected to FileManager''s Open method.',i+SELF.lDev)
            IF SELF.Lexer.GetToken(TokenPnt+3)=',' THEN InsLine='Access:'&SELF.Lexer.GetToken(TokenPnt+2)&'.OpenMode='&SELF.Lexer.GetToken(TokenPnt+4).
          END
        ELSE
          BREAK
        END
        TokenPnt+=1
      END
    END
  END

CheckForGET         ROUTINE
  DATA
Tokens  CSTRING(32),DIM(6),AUTO
  CODE
  Tokens[1]='GET'
  Tokens[2]='('
  Tokens[3]='!KnownFileLabel'
  Tokens[4]=','
  Tokens[5]='!KnownKeyLabel'
  Tokens[6]=')'
  Replace='Access:!03.Fetch(!05)'
  IF SELF.Lexer.TokenReplace(cLine,Tokens,Replace,SELF.FileList)=Level:Notify
    Info.AddLine('GET(File,Key) redirected to FileManager'' Fetch method.',i+SELF.lDev)
    Info.AddLine('Warning : ERRORCODE() may no longer be valid')
  END

FileAccess.ReplaceFcall PROCEDURE(*CSTRING cLine,LONG i,STRING Func,STRING NewFunc,SectionClass SectionMgr, InfoTextClass Info)

f                         BYTE,AUTO
tLine                     CSTRING(MaxLineLen),AUTO
Tokens                    CSTRING(32),DIM(4),AUTO
Tokens1                   CSTRING(32),DIM(5),AUTO
Replace                   CSTRING(MaxLineLen),AUTO
p                         USHORT,AUTO

  CODE
  IF ~SELF.PreFilterLine(cLine,Func) THEN RETURN .
  Tokens[1]=Func
  Tokens[2]='('
  Tokens[3]='!KnownFileLabel'
  Tokens[4]=')'
  Replace='Access:!03.' & NewFunc & '()'
  IF SELF.Lexer.TokenReplace(cLine,Tokens,Replace,SELF.FileList)=Level:Notify
    Info.AddLine(Func&' redirected to FileManager''s ' & NewFunc &' method.',i+SELF.lDev)
    IF i<SectionMgr.GetLineCount()
      SectionMgr.GetLine(i+1,tLine)
      IF SELF.PreFilterLine(tLine,'Error')
        p=INSTRING('!',tLine,1,1)
        IF p THEN tLine=tLine[1 : p-1].
        Tokens1[1]='ERRORCODE'
        Tokens1[2]='('
        Tokens1[3]=')'
        Tokens1[4]='='
        Tokens1[5]='33'
        f=False
        DO ErrorChk
        IF ~f
          Tokens1[1]='ERROR'
          DO ErrorChk
        END
      END
    END
  END

ErrorChk            ROUTINE
  IF SELF.PreFilterLine(tLine,Tokens1[1]&'()') THEN
    SELF.Lexer.TakeLine(tLine)
    cLine = CLIP(LEFT(cLine))
    IF SELF.Lexer.TokenReplace(tLine,Tokens1,cLine,SELF.FileList) = Level:Notify
      Info.AddLine(Tokens1[1]&'() = 33 replaced by ' & NewFunc& ' return value.')
      cLine = tLine
      SectionMgr.ClearLine(i+1)
      f=True
    ELSIF SELF.Lexer.TokenReplace(tLine,Tokens1,cLine,SELF.FileList,3) = Level:Notify
      Info.AddLine(Tokens1[1]&'() call replaced by ' & NewFunc& ' return value.')
      cLine = tLine
      SectionMgr.ClearLine(i+1)
      f=True
    ELSE
      Info.AddLine('Warning : '&Tokens1[1]&'() may no longer be valid')
    END
  END

!--- Hints & Tips -------------------

Hints.Construct     PROCEDURE()

  CODE
  SELF.Register(25,OwnerName,'Conversion Hints','&Hints:','[SOURCE]',0,0)

Hints.TakeSection   PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                 CSTRING(MaxLineLen),AUTO
i                     LONG,AUTO

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Hints and Tips, Embed Point: '&SELF.CurrentEMBED())
  CASE SectionHeader
  OF '[SOURCE]'
    DO SourceProcessing
  END
  RETURN Level:Benign

SourceProcessing    ROUTINE
  DATA
j   USHORT,AUTO
  CODE
  i=1
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine) AND SELF.PrefilterLine(cLine,'RETURN|EXIT|RecordChanged')
      SELF.Lexer.TakeLine(cLine)
      j=SELF.Lexer.FindToken('RETURN')
      IF j
        IF ~(SELF.Lexer.GetToken(j+1) AND SELF.Lexer.GetToken(j+1) NOT='.')
          cLine=cLine&'  !** Check for return value required ** '
          Info.AddLine('RETURN may require a value now.',i)
        END
      END
      IF SELF.Lexer.FindToken('EXIT')
        cLine=cLine&'  !** Check EXIT allowed here ** '
        Info.AddLine('May not be able to EXIT from here.',i)
      END
      IF SELF.Lexer.FindToken('RecordChanged')
        Info.AddLine('RecordChanged variable no longer available',i)
        cLine=cLine&' !** RecordChanged variable is no longer available **'
      END
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()

!---Unterminated Omits Checker
UnTerminatedOmits.Construct PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'UnTerminated Omit Checker','Un&Term OMITS:','[SOURCE]')

UnTerminatedOmits.TakeSection   PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                             CSTRING(MaxLineLen),AUTO
i                                 LONG(1)
p1                                USHORT,AUTO
p2                                USHORT,AUTO
SText                             EQUATE('OMIT(''')
  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: '&SELF.CurrentEMBED())
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine)
      p1=INSTRING(SText,cLine,1,1)+LEN(SText)
      IF p1>LEN(SText)
        p2=INSTRING('''',cLine,1,p1)-1
        ASSERT(p2)
        IF ~SELF.TerminatorFound(SectionMgr,cLine[p1 : p2],i+1)
          Info.AddLine('Unterminated OMIT() statement detected - cannot omit over multiple embed points',i)
          cLine='!'&cLine
          SELF.RestoreComment(cLine)
          SectionMgr.SetLine(i,cLine)
        END
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

UnTerminatedOmits.TerminatorFound   PROCEDURE(SectionClass SectionMgr,STRING LookFor,LONG StartLine)

cLine                                 CSTRING(MaxLineLen),AUTO
i                                     LONG,AUTO

  CODE
  i=StartLine
  IF i<=SectionMgr.GetLineCount()
    LOOP
      SectionMgr.GetLine(i,cLine)
      IF INSTRING(UPPER(LookFor),UPPER(cLine),1,1) THEN RETURN True.
      i+=1
    WHILE i<=SectionMgr.GetLineCount()
  END
  RETURN False

!-- Standard PROCEDURE call checker---------

StdFunc.Construct   PROCEDURE()

  CODE
  SELF.Register(230,OwnerName,'Standard PROCEDURE Checker','2.0 &Std PROCEDUREs:','[SOURCE]')

StdFunc.TakeSection PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                 CSTRING(MaxLineLen),AUTO
i                     LONG(1)
j                     USHORT,AUTO
lDev                  SHORT(0)
Replace               CSTRING(MaxLineLen),AUTO
NewLines              QUEUE
Item                    CSTRING(MaxLineLen)
                      END
TokenPnt              USHORT,AUTO
TokenStart            USHORT,AUTO
TokenEnd              USHORT,AUTO

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: '&SELF.CurrentEMBED())
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine)
      IF SELF.PreFilterLine(cLine,'CheckOpen|INIRestoreWindow|INISaveWindow|StandardWarning|RISaveError|RIUpdate|WinResize')
        SELF.Lexer.TakeLine(cLine)
        IF SELF.PreFilterLine(cLine,'CheckOpen')
          DO CheckForCHECKOPEN
          IF SectionMgr.LineChanged(i,cLine) THEN SELF.Lexer.TakeLine(cLine).
        END
        IF SELF.PreFilterLine(cLine,'INIRestoreWindow')
          DO CheckForINIRESTOREWINDOW
          IF SectionMgr.LineChanged(i,cLine) THEN SELF.Lexer.TakeLine(cLine).
        END
        IF SELF.PreFilterLine(cLine,'INISaveWindow')
          DO CheckForINISAVEWINDOW
          IF SectionMgr.LineChanged(i,cLine) THEN SELF.Lexer.TakeLine(cLine).
        END
        IF SELF.PreFilterLine(cLine,'StandardWarning')
          DO CheckForSTANDARDWARNING
          IF SectionMgr.LineChanged(i,cLine) THEN SELF.Lexer.TakeLine(cLine).
        END
        IF SELF.PreFilterLine(cLine,'RISaveError')
          DO CheckForRISAVEERROR
          IF SectionMgr.LineChanged(i,cLine) THEN SELF.Lexer.TakeLine(cLine).
        END
        IF SELF.PreFilterLine(cLine,'RIUpdate')
          DO CheckForRIUPDATE
          IF SectionMgr.LineChanged(i,cLine) THEN SELF.Lexer.TakeLine(cLine).
        END
        IF SELF.PreFilterLine(cLine,'WinResize')
          DO CheckForResizer
        END
      END
      IF SELF.PreFilterLine(cLine,'ReportPreview|Preview:JumpToPage|Preview:SelectDisplay|SetupRealStops|NextRealStop|SetupStringStops|NextStringStop')
        SELF.Lexer.TakeLine(cLine)
        DO CheckOthers                                      !Procedures no longer required with no equivilent
      END
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
      IF Records(NewLines)
        LOOP j=1 TO RECORDS(NewLines)
          GET(NewLines,j)
          ASSERT(~ERRORCODE())
          SectionMgr.InsertLine(NewLines.Item,i-1,0)
          lDev-=1
        END
        FREE(NewLines)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

CheckForCHECKOPEN   ROUTINE
  DATA
Tokens  CSTRING(32),DIM(4),AUTO
  CODE
  Tokens[1]='CHECKOPEN'
  Tokens[2]='('
  Tokens[3]='!KnownFileLabel'
  Tokens[4]=')'
  Replace='Relate:!03.Open'
  IF SELF.Lexer.TokenReplace(cLine,Tokens,Replace,SELF.FileList,4)=Level:Notify
    Info.AddLine('CHECKOPEN call redirected to RelationManager''s Open method.',i+lDev)
  ELSE
    TokenPnt=1
    LOOP
      TokenPnt=SELF.Lexer.FindToken('CHECKOPEN',TokenPnt)
      IF TokenPnt
        TokenStart=SELF.Lexer.GetStartChrPos(TokenPnt)-1
        IF SELF.Lexer.GetToken(TokenPnt+1)='(' AND SELF.IsKnownFileLabel(SELF.Lexer.GetToken(TokenPnt+2))
          IF SELF.Lexer.GetToken(TokenPnt+3)=',' AND SELF.Lexer.GetToken(TokenPnt+5)=')'
            TokenEnd=SELF.Lexer.GetEndChrPos(TokenPnt+5)+1
            DO MakeOpenCall
            NewLines.Item='Relate:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Me.Create='&SELF.Lexer.GetToken(TokenPnt+4)
            ADD(NewLines)
            ASSERT(~ERRORCODE())
          ELSIF SELF.Lexer.GetToken(TokenPnt+3)=',' AND SELF.Lexer.GetToken(TokenPnt+4)=',' AND SELF.Lexer.GetToken(TokenPnt+6)=')'
            TokenEnd=SELF.Lexer.GetEndChrPos(TokenPnt+6)+1
            DO MakeOpenCall
            NewLines.Item='Relate:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Me.OpenMode='&SELF.Lexer.GetToken(TokenPnt+5)
            ADD(NewLines)
            ASSERT(~ERRORCODE())
          ELSIF SELF.Lexer.GetToken(TokenPnt+3)=',' AND SELF.Lexer.GetToken(TokenPnt+5)=',' AND SELF.Lexer.GetToken(TokenPnt+7)=')'
            TokenEnd=SELF.Lexer.GetEndChrPos(TokenPnt+7)+1
            DO MakeOpenCall
            NewLines.Item='Relate:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Me.Create='&SELF.Lexer.GetToken(TokenPnt+4)
            ADD(NewLines)
            ASSERT(~ERRORCODE())
            NewLines.Item='Relate:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Me.OpenMode='&SELF.Lexer.GetToken(TokenPnt+6)
            ADD(NewLines)
            ASSERT(~ERRORCODE())
          END
        END
      ELSE
        BREAK
      END
      TokenPnt+=1
    END
  END

MakeOpenCall        ROUTINE
  IF TokenEnd<LEN(cline)
    cLine=SUB(cLine,1,TokenStart)&'Relate:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Open'&cLine[TokenEnd : LEN(cLine)]
  ELSE
    cLine=SUB(cLine,1,TokenStart)&'Relate:'&SELF.Lexer.GetToken(TokenPnt+2)&'.Open'
  END
  Info.AddLine('CHECKOPEN call redirected to RelationManager''s Open method.',i+lDev)

CheckForINIRESTOREWINDOW    ROUTINE
  DATA
Tokens  CSTRING(32),DIM(6),AUTO
  CODE
  Tokens[1]='INIRESTOREWINDOW'
  Tokens[2]='('
  Tokens[3]='!Anything'
  Tokens[4]=','
  Tokens[5]='!Anything'
  Tokens[6]=')'
  Replace='INIMgr.Fetch(!03,0)'
  IF SELF.Lexer.TokenReplace(cLine,Tokens,replace,SELF.FileList)=Level:Notify
    Info.AddLine('INIRestoreWindow call redirected to INIManager''s Fetch method.',i+lDev)
  END

CheckForINISAVEWINDOW   ROUTINE
  DATA
Tokens  CSTRING(32),DIM(6),AUTO
  CODE
  Tokens[1]='INISAVEWINDOW'
  Tokens[2]='('
  Tokens[3]='!Anything'
  Tokens[4]=','
  Tokens[5]='!Anything'
  Tokens[6]=')'
  Replace='INIMgr.Update(!03,0)'
  IF SELF.Lexer.TokenReplace(cLine,Tokens,replace,SELF.FileList)=Level:Notify
    Info.AddLine('INISaveWindow call redirected to INIManager''s Update method.',i+lDev)
  END

CheckForSTANDARDWARNING ROUTINE
  DATA
Tokens  CSTRING(32),DIM(4),AUTO
  CODE
  Tokens[1]='STANDARDWARNING'
  Tokens[2]='('
  Tokens[3]='!Anything'
  Tokens[4]=')'
  CASE UPPER(SELF.Lexer.GetToken(3))
  OF 'WARN:NOERROR'
    Replace='Msg:None'
  OF 'WARN:AUTOINCERROR'
    Replace='Msg:RetryAutoInc'
  OF 'WARN:STARTOFASCIIQUEUE '
    Replace='Msg:SearchReachedBeginning'
  OF 'WARN:CONFIRMCANCEL'
    Replace='Msg:ConfirmCancel'
  OF 'WARN:NOERROR'
    Replace='Msg:None'
  OF 'WARN:AUTOINCERROR'
    Replace='Msg:RetryAutoInc'
  OF 'WARN:STARTOFASCIIQUEUE'
    Replace='Msg:SearchReachedBeginning'
  OF 'WARN:CONFIRMCANCEL'
    Replace='Msg:ConfirmCancel'
  OF 'WARN:CREATEERROR'
    Replace='Msg:CreateFailed'
  OF 'WARN:DELETEDISABLED'
    Replace='Msg:DeleteIllegal'
  OF 'WARN:DELETEERROR'
    Replace='Msg:RetryDelete'
  OF 'WARN:DISKERROR'
    Replace='Msg:OpenFailed'
  OF 'WARN:DUPLICATEKEY'
    Replace='Msg:DuplicateKey'
  OF 'WARN:ENDOFASCIIQUEUE'
    Replace='Msg:SearchReachedEnd'
  OF 'WARN:FILELOADERROR'
    Replace='Msg:FileLoadFailed'
  OF 'WARN:FILEZEROLENGTH'
    Replace='Msg:None'
  OF 'WARN:FORMRECORDCHANGEDERROR'
    Replace='Msg:ConcurrencyFailedFromForm'
  OF 'WARN:INSERTDISABLED'
    Replace='Msg:InsertIllegal'
  OF 'WARN:INSERTERROR'
    Replace='Msg:AddFailed'
  OF 'WARN:INVALIDFILE'
    Replace='Msg:OpenFailed'
  OF 'WARN:INVALIDKEY'
    Replace='Msg:RebuildKey'
  OF 'WARN:LOGOUTDELETEERROR'
    Replace='Msg:LogoutFailed'
  OF 'WARN:LOGOUTUPDATEERROR'
    Replace='Msg:LogoutFailed'
  OF 'WARN:NEWRECORDADDED'
    Replace='Msg:AddAnother'
  OF 'WARN:NOTINFILE'
    Replace='Msg:FieldNotInFile'
  OF 'WARN:OUTOFRANGE'
    Replace='Msg:FieldOutOfRange'
  OF 'WARN:OUTOFRANGEHIGH'
    Replace='Msg:FieldOutOfRangeHigh'
  OF 'WARN:OUTOFRANGELOW'
    Replace='Msg:FieldOutOfRangeLow'
  OF 'WARN:PROCEDURETODO'
    Replace='Msg:ProcedureToDo'
  OF 'WARN:PROCESSDELETEERROR'
    Replace='Msg:DeleteFailed'
  OF 'WARN:PROCESSPUTERROR'
    Replace='Msg:PutFailed'
  OF 'WARN:REBUILDERROR'
    Replace='Msg:RebuildFailed'
  OF 'WARN:RECORDCHANGEDERROR'
    Replace='Msg:ConcurrencyFailed'
  OF 'WARN:RECORDFETCHERROR'
    Replace='Msg:AbortReading'
  OF 'WARN:RESTRICTDELETE'
    Replace='Msg:RestrictDelete'
  OF 'WARN:RESTRICTUPDATE'
    Replace='Msg:RestrictUpdate'
  OF 'WARN:RIDELETEERROR'
    Replace='Msg:DeleteFailed'
  OF 'WARN:RIUPDATEERROR'
    Replace='Msg:PutFailed'
  OF 'WARN:SAVEONCANCEL'
    Replace='Msg:SaveRecord'
  OF 'WARN:STANDARDDELETE'
    Replace='Msg:ConfirmDelete'
  OF 'WARN:UPDATEDISABLED'
    Replace='Msg:UpdateIllegal'
  OF 'WARN:UPDATEERROR'
    Replace='Msg:RetrySave'
  OF 'WARN:VIEWOPENERROR'
    Replace='Msg:ViewOpenFailed'
  OF 'WARN:INVALIDRECORD'
    Replace='Msg:None'
  OF 'WARN:RECORDHELDERROR'
    Replace='Msg:RecordHeld'
  END
  Replace='GlobalErrors.Throw('&Replace&')'
  IF SELF.Lexer.TokenReplace(cLine,Tokens,Replace,SELF.FileList)
    Info.AddLine('StandardWarning becomes call to Error Manager''s Throw method',i+lDev)
  END

CheckForRISAVEERROR ROUTINE

CheckForResizer     ROUTINE
  DATA
Changed BYTE(False)
  CODE
  LOOP
    j=SELF.Lexer.FindToken('WinResize')
    IF j
      TokenStart=SELF.Lexer.GetStartChrPos(j)
      TokenEnd=SELF.Lexer.GetEndChrPos(j)
      cLine=SUB(cLine,1,TokenStart-1)&'Resizer'&cLine[TokenEnd+1 : LEN(cLine)]
      Changed=True
      SELF.Lexer.TakeLine(cLine)
    END
  WHILE j
  IF Changed THEN Info.AddLine('WinResize object not called Resizer',i).

CheckForRIUPDATE    ROUTINE
  DATA
FileLabel   CSTRING(64),AUTO
HasBracket  BYTE(False)
k1  USHORT,AUTO
k2  USHORT,AUTO
  CODE
  LOOP j=1 TO SELF.Lexer.GetTokenCount()
    IF INSTRING('RIUPDATE:',UPPER(SELF.Lexer.GetToken(j)),1,1)
      TokenStart=SELF.Lexer.GetStartChrPos(j)
      TokenEnd=0
      IF SELF.Lexer.GetToken(j+1)='('
        HasBracket=True
        LOOP k1=j+1 TO SELF.Lexer.GetTokenCount()
          IF SELF.Lexer.GetToken(k1)=')'
            TokenEnd=SELF.Lexer.GetEndChrPos(k1)
            BREAK
          END
        END
      ELSE
        TokenEnd=SELF.Lexer.GetEndChrPos(j)
      END
      ASSERT(TokenEnd>TokenStart)
      FileLabel=SELF.Lexer.GetToken(j)
      k1=INSTRING(':',FileLabel,1,1)
      ASSERT(k1)
      k2=INSTRING('(',FileLabel,1,1)
      IF ~k2 THEN k2=LEN(FileLabel)+1.
      FileLabel=FileLabel[k1+1 : K2-1]
      cLine=SUB(cLine,1,TokenStart-1)&'Relate:'&FileLabel&'.Update'&CHOOSE(HasBracket=False,'','()')&cLine[TokenEnd+1 : LEN(cLine)]
      Info.AddLine('RIUpdate: call redirected to relation manager Update method',i+lDev)
    END
  END

CheckOthers         ROUTINE
  TokenPnt=SELF.Lexer.FindToken('REPORTPREVIEW')
  IF TokenPnt
    DO HandlePrintPrevItem
    cLine=cLine&'  !ReportPreview call removed'
  END
  TokenPnt=SELF.Lexer.FindToken('PREVIEW:JUMPPAGE')
  IF TokenPnt
    DO HandlePrintPrevItem
    cLine=cLine&'  !Preview:JumpPage call removed'
  END
  TokenPnt=SELF.Lexer.FindToken('PREVIEW:SELECTDISPLAY')
  IF TokenPnt
    DO HandlePrintPrevItem
    cLine=cLine&'  !Preview:SelectDisplay call removed'
  END

HandlePrintPrevItem ROUTINE
  IF SELF.Lexer.GetEndChrPos(TokenPnt)<=LEN(cLine)
    cLine=SUB(cLine,1,SELF.Lexer.GetStartChrPos(TokenPnt)-1)&' '&cLine[SELF.Lexer.GetEndChrPos(TokenPnt)+1 : LEN(cLine)]
  ELSE
    cLine=SUB(cLine,1,SELF.Lexer.GetStartChrPos(TokenPnt)-1)
  END
  Info.AddLine('You must replace this call with calls to the new Print Previewer.',i+lDev)

!-- Toolbar Equate value checker ------

TbarEqu.Construct   PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'Toolbar Equate Value Checker','&Toolbar Equates','[EMBED][SOURCE][ADDITION][PROMPTS][DATA][WINDOW]',0)

TBarEqu.TakeSection PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

i                     LONG(1)
p1                    USHORT,AUTO
p2                    USHORT,AUTO
cLine                 CSTRING(MaxLineLen),AUTO

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply
  Info.AddTitle('2.00x toolbar equate names have changed.')
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine) AND SELF.PreFilterLine(cLine,'TBarBrw|@S0')
      IF SELF.Replace(cLine,'TBarBrw','Toolbar:')
        Info.AddLine('Constants prefixed TBarBrw now prefixed Toolbar:.',i)
      END
      IF SELF.Replace(cLine,'@S0','@S1')
        Info.AddLine('Fix to administrator bug',i)
      END
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

!-- Change Template Chain-------
ChangeTplChain.Construct    PROCEDURE()

  CODE
  !DB.Init('ChangeTplChain',True,0)
  !DB.SetFilter('`')
  SELF.Register(25,OwnerName,'Change Template Chain','&Change Tpl Chain:','[COMMON][ADDITION][PROMPTS]')

ChangeTplChain.TakeSection  PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

AddCategory                   CSTRING(32),AUTO
cLine                         CSTRING(MaxLineLen),AUTO
i                             LONG(1)

  CODE
  SELF.Buttons = Action:Apply
  Info.AddTitle('Template Name Changed: '&SectionHeader&' section')
  LOOP
    SectionMgr.GetLine(i,cLine)
    CLEAR(AddCategory)
    SELF.Lexer.TakeLine(cLine)
    IF SectionHeader = '[COMMON]'
      IF SELF.Lexer.GetToken(1) = 'FROM' 
        IF SELF.WildReplace(cLine,'Clarion','ABC',True)
          Info.AddLine('Clarion template changed to ABC Template',i)
        END
        IF SELF.WildReplace(cLine,'Browse','Window',True)
          AddCategory = 'Browse'
          Info.AddLine('Browse now has procedure type Window',i)
        END
        IF SELF.WildReplace(cLine,'Form','Window',True)
          AddCategory = 'Form'
          Info.AddLine('Form now has procedure type Window',i)
        END
        IF SELF.WildReplace(cLine,'Viewer','Window',True)
          AddCategory = 'Viewer'
          Info.AddLine('Ascii now has procedure type window',i)
        END
        IF SELF.WildReplace(cLine,'Menu','Window',True)
          AddCategory = 'Menu'
          Info.AddLine('Menu now has procedure type window',i)
        END
      END
    ELSIF SectionHeader = '[ADDITION]'
      IF SELF.Lexer.GetToken(1) = 'NAME'
        IF SELF.WildReplace(cLine,'Clarion','ABC',True)
          Info.AddLine('Clarion template changed to ABC Template',i)
        END
      END
    ELSE
      IF SELF.WildReplace(cLine,'(Clarion)','(ABC)',True)
        Info.AddLine('Clarion template changed to ABC Template',i)
      END
    END
    IF SectionMgr.LineChanged(i,cLine)
      SectionMgr.SetLine(i,cLine)
      IF AddCategory
        cLine = 'CATEGORY ''' & AddCategory & ''''
        SectionMgr.AppendLine(cLine,0)
        Info.AddLine('New CATEGORY statement required',i + 1)
      END
    END
    i += 1
  WHILE i <= SectionMgr.GetLineCount()
  RETURN LEVEL:Benign

!-- Unlinked procedure checker ----------

UnLinkedProc.Construct  PROCEDURE()

  CODE
  SELF.Register(25,OwnerName,'UnLinked Procedure Checker','&UnLinked Procs:','[SOURCE][CALLS][PROCEDURE]',True)
  SELF.UnLinked&=NEW UnLinkedQ

UnLinkedProc.Destruct   PROCEDURE()

  CODE
  DISPOSE(SELF.UnLinked)

UnLinkedProc.BeforeConversion   PROCEDURE()

  CODE
  ASSERT(~SELF.UnLinked&=NULL)

UnLinkedProc.TakeSection    PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                         CSTRING(MaxLineLen),AUTO
i                             LONG,AUTO
j                             USHORT,AUTO

  CODE
  IF SELF.CurrentProcedure()
    ASSERT(~SELF.UnLinked&=NULL)
    IF SectionHeader='[PROCEDURE]'
      DO NewProcedure
    ELSIF SectionHeader='[SOURCE]' AND SectionMgr.GetLineCount()
      DO ProcessSource
    ELSIF SectionHeader='[CALLS]'
      DO ProcessCalls
    ELSE
      Message('UnLinkedProc.TakeSection did not handle SectionHeader=' & SectionHeader &'||Expected: [PROCEDURE] [SOURCE] [CALLS]','UnLinkedProc Rule')  !2022-11-09 Carl Barnes: Seeing ASSERT not shopwing any test so added message just in case
      ASSERT(False,'UnLinkedProc.TakeSection did not handle SectionHeader=' & SectionHeader)
    END
  END
  RETURN Level:Benign

NewProcedure        ROUTINE
  SELF.ThisProcedure=SELF.CurrentProcedure()
  FREE(SELF.UnLinked)
  LOOP i=1 TO RECORDS(SELF.ProcedureQueue)
    GET(SELF.ProcedureQueue,i)
    ASSERT(~ERRORCODE())
    IF UPPER(SELF.ProcedureQueue.Name) NOT=UPPER(SELF.ThisProcedure)
      SELF.UnLinked.Name=UPPER(SELF.ProcedureQueue.Name)
      SELF.UnLinked.ProperName=SELF.ProcedureQueue.Name
      SELF.UnLinked.Referenced=False
      ADD(SELF.UnLinked,SELF.UnLinked.Name)
      ASSERT(~ERRORCODE())
    END
  END
  SELF.ProcedureQueue.Name=SELF.CurrentProcedure()
  GET(SELF.ProcedureQueue,SELF.ProcedureQueue.Name)
  ASSERT(~ERRORCODE())
  LOOP i=1 TO RECORDS(SELF.ProcedureQueue.Calls)
    GET(SELF.ProcedureQueue.Calls,i)
    ASSERT(~ERRORCODE())
    SELF.UnLinked.Name=UPPER(SELF.ProcedureQueue.Calls.Name)
    GET(SELF.UnLinked,SELF.UnLinked.Name)
    IF ~ERRORCODE()
      DELETE(SELF.UnLinked)
      ASSERT(~ERRORCODE())
    END
  END

ProcessSource       ROUTINE
  i=1
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine)
      LOOP j=1 TO RECORDS(SELF.UnLinked)
        GET(SELF.UnLinked,j)
        ASSERT(~ERRORCODE())
        IF ~SELF.UnLinked.Referenced
          IF SELF.PreFilterLine(cLine,SELF.UnLinked.ProperName)
            SELF.Lexer.TakeLine(cLine)
            IF SELF.Lexer.FindToken(SELF.UnLinked.ProperName)
              SELF.UnLinked.Referenced=True
              PUT(SELF.UnLinked)
              ASSERT(~ERRORCODE())
            END
          END
        END
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()

ProcessCalls        ROUTINE
  IF SectionMgr.GetLineCount()
    i=1
    LOOP
      SectionMgr.GetLine(i,cLine)
      SELF.UnLinked.Name=UPPER(cLine)
      GET(SELF.UnLinked,SELF.UnLinked.Name)
      IF ~ERRORCODE()
        DELETE(SELF.UnLinked)
        ASSERT(~ERRORCODE())
      END
      i+=1
    WHILE i<=SectionMgr.GetlineCount()
  END
  IF RECORDS(SELF.UnLinked)
    SELF.Buttons=Action:Cancel+Action:Apply
    Info.AddTitle(SELF.ThisProcedure&': Un-Linked Procedures Found in EMBED Code')
    LOOP j=1 TO RECORDS(SELF.UnLinked)
      GET(SELF.UnLinked,j)
      ASSERT(~ERRORCODE())
      IF SELF.UnLinked.Referenced
        SectionMgr.AppendLine(SELF.UnLinked.ProperName,0)
      END
    END
  END

!--Window Routine Checker-------------

WinRoutine.Construct    PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'Window Routine Checker','&Window Routines:','[SOURCE]')

WinRoutine.TakeSection  PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

ForInLine                 GROUP
Number                      USHORT(11)
                            PSTRING('ThisWindow.Kill')
                            PSTRING('RETURN Level:Fatal')
                            PSTRING('Window managers kill method replaced by RETURN Level:Fatal')
                            PSTRING('DO RefreshWindow')
                            PSTRING('ThisWindow.Reset')
                            PSTRING('RefreshWindow routine now Reset method.')
                            PSTRING('DO SyncWindow')
                            PSTRING('ThisWindow.Update')
                            PSTRING('SyncWindow routine now Update method.')
                            PSTRING('DO ProcedureReturn')
                            PSTRING('RETURN Level:Fatal')
                            PSTRING('Procedure return now RETURN Level:Fatal')
                            PSTRING('DO PrepareProcedure')
                            PSTRING('ThisWindow.Init')
                            PSTRING('PrepareProcedure now Init method')
                            PSTRING('ForceRefresh')
                            PSTRING('ThisWindow.ForcedReset')
                            PSTRING('ForceRefresh now inside window manager')
                            PSTRING('LocalResponse')
                            PSTRING('ThisWindow.Response')
                            PSTRING('LocalResponse now inside window manager')
                            PSTRING('RecordsToProcess')
                            PSTRING('ThisWindow.Process.RecordsToProcess')
                            PSTRING('RecordsToProcess now inside report manager')
                            PSTRING('LocalRequest')
                            PSTRING('ThisWindow.Request')
                            PSTRING('LocalRequest now inside window manager')
                            PSTRING('OriginalRequest')
                            PSTRING('ThisWindow.OriginalRequest')
                            PSTRING('OriginalRequest now inside window manager')
                            PSTRING('DO FORM::AssignButtons')
                            PSTRING('Toolbar.SetTarget(-1)')
                            PSTRING('FORM::AssignButtons now call to toolbar class : HINT Probably redundant')
                          END

cLine                     CSTRING(MaxLineLen),AUTO
i                         LONG(1)
Prefix                    CSTRING(5),AUTO

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: '&SELF.CurrentEMBED())
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.SaveComment(cLine)
      SELF.WildReplace(cLine,ForInline,SectionMgr,Info,i)
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

!---Report Routine Checker----

RepRoutine.Construct    PROCEDURE

  CODE
  SELF.Register(100,OwnerName,'Report Procedure Checker','&Report Procedures:','[PROCEDURE]',0)

RepRoutine.TakeSection  PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                     CSTRING(MaxLineLen),AUTO
IsReport                  BYTE(False)
i                         LONG,AUTO
InsertLine                LONG(0)

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply
  !2022-11-09 Carl Barnes: Change like ProcRoutine.TakeSection to deal with no [CALLS].
  !                        Change to FIRST check if its a REPORT with no [Window], if not we are done and Return Benign
  !                        Then find insert point and add [Window] 
!----------------------------------------------------------  
!  Info.AddTitle('Report found without a Window Structure')
!  LOOP i=1 TO SectionMgr.GetLineCount()                     !2022-11-09 Carl Barnes: Moved this code down after we're sure its needed
!    SectionMgr.GetLine(i,cLine)
!    IF cLine='[CALLS]'
!      InsertLine=i-1
!      BREAK
!    END
!  END
!  ASSERT(InsertLine)          !All procedures will contain a [CALLS] section, this is enforced by the convertor engine ... Carl FYI but not Selective Export to TXA

  LOOP i = 1 TO SectionMgr.GetLineCount()
    SectionMgr.GetLine(i,cLine)
    IF SUB(cLine,1,8) = '[WINDOW]'
      RETURN Level:Benign
    ELSIF UPPER(cLine)='FROM CLARION REPORT' OR UPPER(cline)='FROM ABC REPORT' OR UPPER(cline)='FROM CPCSRPTS UNIVREPORT'
      IsReport=True
    END
  END
  IF ~IsReport THEN RETURN Level:Benign.   !2022-11-09 Carl Barnes: Not a REPORT template so can stop rule processing. 

  Info.AddTitle('Report found without a Window Structure')
  LOOP i=1 TO SectionMgr.GetLineCount()
    SectionMgr.GetLine(i,cLine)
    IF cLine='[CALLS]'  |
    OR cLine='[REPORT]' THEN              !2022-11-09 Carl Barnes: the [Window] comes before [Report] so this is a good spot to insert     
      InsertLine=i-1
      BREAK
    END
  END

  !Message('Found [CALLS] InsertLine = '& InsertLine &'|SectionMgr.GetLineCount()='& SectionMgr.GetLineCount() , 'ProcRoutine.TakeSection')
  IF InsertLine=0 THEN                      !ASSERT(InsertLine) 
     SectionMgr.AppendLine(' ')             !Append blank line to end to insert Window before 
     InsertLine=SectionMgr.GetLineCount()   !This is my new blank line as insert point
  END 

  !Add Report [Window] in reverse order because inserting before [CALLS]
  SectionMgr.InsertLine('END',InsertLine)
  SectionMgr.InsertLine('BUTTON(''Cancel''),AT(45,42,50,15),USE(?Progress:Cancel),#ORIG(?Progress:Cancel)',InsertLine)
  SectionMgr.InsertLine('STRING(''''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#ORIG(?Progress:PctText)',InsertLine)
  SectionMgr.InsertLine('STRING(''''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#ORIG(?Progress:UserString)',InsertLine)
  SectionMgr.InsertLine('PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#ORIG(Progress:Thermometer)',InsertLine)
  SectionMgr.InsertLine('ProgressWindow WINDOW(''Report Progress...''),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE',InsertLine,0)
  SectionMgr.InsertLine('[WINDOW]',InsertLine,0)
  Info.AddLine('Window structure added',InsertLine)

  RETURN Level:Benign

!--- Process Routine Checker -----

ProcRoutine.Construct   PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'Process Procedure Checker','&Process Procedures:','[PROCEDURE]',0)

ProcRoutine.TakeSection PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                     CSTRING(MaxLineLen),AUTO
i                         LONG,AUTO
InsertLine                LONG(0)
IsProcess                 BYTE(False)

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
!2022-11-09 Carl Barnes:
!           In short: Improve to 1st check if need this rule, then handle missing [CALLS] so not ASSERT and crash.
!           This checks a Process template for missing a [Window] section. An old problem that I think not present since Cw20.
!           Original code FIRST looked for insert point i.e. [CALLS] section which comes just before [WINDOW]. 
!           The [CALLS] section is NOT present in a selective export [PROCEDURE] TXA so fails with ASSERT.
!           Carl changed to FIRST check if it's a PROCESS Template, or has a [Window], so no problem can Return Benign
!           Only it a PROCESS w/o [Window] look for [CALLS]. if not found add to end.
!----------------------------------------------------------
!  Info.AddTitle('Process found without a Window Structure')
!  LOOP i=1 TO SectionMgr.GetLineCount()                    !2022-11-09 Carl Barnes: Moved this code down after we're sure its needed
!    SectionMgr.GetLine(i,cLine)
!    IF cLine='[CALLS]'
!      InsertLine=i-1
!      BREAK
!    END
!  END
!  ASSERT(InsertLine) !All procedures will contain a [CALLS] section, this is enforced by the convertor engine ... Carl FYI but not Selective Export to TXA

  LOOP i = 1 TO SectionMgr.GetLineCount()
    SectionMgr.GetLine(i,cLine)
    IF SUB(cLine,1,8) = '[WINDOW]'
      RETURN Level:Benign
    ELSIF UPPER(cLine)='FROM CLARION PROCESS' OR UPPER(cLine)='FROM ABC PROCESS'
      IsProcess=True
    END
  END
  IF ~IsProcess THEN RETURN Level:Benign.   !2022-11-09 Carl Barnes: Not a PROCESS template so can stop rule processing. 

  Info.AddTitle('Process found without a Window Structure')
  LOOP i=1 TO SectionMgr.GetLineCount()
    SectionMgr.GetLine(i,cLine)
    IF cLine='[CALLS]'
      InsertLine=i-1
      BREAK
    END
  END
  !Message('Found [CALLS] InsertLine = '& InsertLine &'|SectionMgr.GetLineCount()='& SectionMgr.GetLineCount() , 'ProcRoutine.TakeSection')
  IF InsertLine=0 THEN                      !ASSERT(InsertLine) 
     SectionMgr.AppendLine(' ')             !Append blank line to end to insert Window before 
     InsertLine=SectionMgr.GetLineCount()   !This is my new blank line
  END 
  !Add Progress [Window] in reverse order because inserting before [CALLS] 
    SectionMgr.InsertLine('END',InsertLine)
    SectionMgr.InsertLine('BUTTON(''Cancel''),AT(45,42,50,15),USE(?Progress:Cancel),#ORIG(?Progress:Cancel)',InsertLine)
    SectionMgr.InsertLine('STRING(''''),AT(0,30,141,10),USE(?Progress:PctText),CENTER,#ORIG(?Progress:PctText)',Insertline)
    SectionMgr.InsertLine('STRING(''''),AT(0,3,141,10),USE(?Progress:UserString),CENTER,#ORIG(?Progress:UserString)',InsertLine)
    SectionMgr.InsertLine('PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100),#ORIG(Progress:Thermometer)',Insertline)
    SectionMgr.InsertLine('ProgressWindow WINDOW(''Progress...''),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE',Insertline,0)
    SectionMgr.InsertLine('[WINDOW]',Insertline,0)
    Info.AddLine('Window structure added',InsertLine)
  RETURN Level:Benign

!-- Place use variables on report details -----------------

RptDetailUse.Construct  PROCEDURE()

  CODE
  SELF.Register(100,OwnerName,'Report Detail Use Variables','&Report Use Details:','[REPORT]',0)

RptDetailUse.TakeSection    PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                         CSTRING(MaxLineLen),AUTO
i                             LONG(1)

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply
  Info.AddTitle('Use Variables on Report Details')
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.PreFilterLine(cLine,'Detail')
      SELF.Lexer.TakeLine(cLine)
      IF SELF.Lexer.FindToken('Detail',2) AND ~SELF.Lexer.FindToken('Use')
        cLine=cLine&',USE(?'&SELF.Lexer.GetToken(1)&')'
        SectionMgr.SetLine(i,cLine)
        Info.AddLine('Use value added to detail section',i)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

!-- Bad Locals ----------------------

BadLocals.Construct PROCEDURE()

  CODE
  SELF.Register(252,OwnerName,'Remove redundant locals','&Redundant Locals:','[DATA]',0)

BadLocals.TakeSection   PROCEDURE(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine                     CSTRING(MaxLineLen),AUTO
RPos                      LONG
I                         LONG,AUTO
J                         LONG,AUTO

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Redundant local variables found')
  LOOP I = 1 TO SectionMgr.GetLineCount()
    SectionMgr.GetLine(I,cLine)
    IF cLine[1] <> '[' AND cLine[1] <> '!' AND SELF.PreFilterLine(cLine,'LocalResponse |ForceRefresh |WindowOpened |WindowInitialized |RejectRecord |RecordsToProcess |RecordsProcessed |RecordsPerCycle |RecordsThisCycle |RecordStatus |RecordFiltered |PercentProgress |OriginalRequest |Update::Reloop ')
      Info.AddLine(cLine[1:INSTRING(' ',cLine)]&': No longer used by templates',i)
      SectionMgr.ClearLine(I)
      LOOP J = I-1 TO 1 BY -1
        SectionMgr.GetLine(J,cLine)
        IF cLine[1] = '[' OR cLine[1] = '!' AND cLine[2]<>'!'
          SectionMgr.ClearLine(J)
        ELSE
          BREAK
        END
      END
      LOOP WHILE I<SectionMgr.GetLineCount()
        SectionMgr.GetLine(I+1,cLine)
        IF cLine[1:3]='!!>'
          SectionMgr.ClearLine(I+1)
          I += 1
        ELSE
          BREAK
        END
      END
    END
  END
  RETURN Level:Benign

