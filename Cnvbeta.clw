!TopSpeed Application Conversion Rules for Applications passed through any C4 Beta

  MEMBER()

  INCLUDE('CNVENG.INC')

  MAP
InitializeDLL   PROCEDURE,NAME('InitializeDLL')
  END

OwnerName       EQUATE('Clarion 4 Betas')


ConvertC4B1     CLASS(RuleClass)
Construct         PROCEDURE
TakeSection       FUNCTION(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,DERIVED
                END

RemoveSymbols   CLASS(RuleClass)
Construct         PROCEDURE
TakeSection       FUNCTION(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,DERIVED
                END

UpdateSymbols   CLASS(RuleClass)
Construct         PROCEDURE
TakeSection       FUNCTION(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader),BYTE,DERIVED
                END



InitializeDLL PROCEDURE

  CODE





!--C4 beta1 name conversion-----------------------------------

ConvertC4B1.Construct PROCEDURE

  CODE
  SELF.Register(100,OwnerName,'Convert C4B1 Base Class names','C4B&1 Names:','[PROMPTS][SOURCE]')


ConvertC4B1.TakeSection FUNCTION(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

ForInLine   GROUP
Number        USHORT(11)
              PSTRING('(''StepLocator'')')
              PSTRING('(''StepLocatorClass'')')
              PSTRING('Default step locator class name is now StepLocatorClass.')
              PSTRING('(''EntryLocator'')')
              PSTRING('(''EntryLocatorClass'')')
              PSTRING('Default entry locator class name is now EntryLocatorClass.')
              PSTRING('(''IncrementalLocator'')')
              PSTRING('(''IncrementalLocatorClass'')')
              PSTRING('Default incremental locator class name is now IncrementalLocatorClass.')
              PSTRING('(''ContractingLocator'')')
              PSTRING('(''FilteredLocatorClass'')')
              PSTRING('Default filtered locator class name is now FilteredLocatorClass.')
              PSTRING('(''DOSFileLookupClass'')')
              PSTRING('(''SelectFileClass'')')
              PSTRING('Default file selector class is now SelectFileClass.')
              PSTRING('(''ThumbLongClass'')')
              PSTRING('(''StepLongClass'')')
              PSTRING('Default step class for long values is now StepLongClass.')
              PSTRING('(''ThumbRealClass'')')
              PSTRING('(''StepRealClass'')')
              PSTRING('Default step class for real values is now StepRealClass.')
              PSTRING('(''ThumbStringClass'')')
              PSTRING('(''StepStringClass'')')
              PSTRING('Default step class for string values is now StepStringClass.')
              PSTRING('(''ThumbCustomClass'')')
              PSTRING('(''StepCustomClass'')')
              PSTRING('Default step class for custom values is now StepCustomClass.')
              PSTRING('(''ErrorManager'')')
              PSTRING('(''ErrorClass'')')
              PSTRING('Default error manager now called ErrorClass')
              PSTRING('(''WindowResizeType'')')
              PSTRING('(''WindowResizeClass'')')
              PSTRING('Window resizer now called WindowResizeClass')
            END

cLine       CSTRING(MaxLineLen),AUTO
i           LONG(1)

  CODE
  IF SectionHeader='[PROMPTS]'
    DO PromptsSection
  ELSIF SectionHeader='[SOURCE]'
    DO SourceSection
  END

SourceSection ROUTINE
  DATA
j USHORT,AUTO
  CODE
  SELF.Buttons=Action:Cancel+Action:Apply+Action:OmitCode+Action:UnCompile+Action:AssertFail
  Info.AddTitle('Embed Point: '&SELF.CurrentEMBED())
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.PreFilterLine(cLine,'.Q.') AND SELF.SaveComment(cLine)
      LOOP j=1 TO LEN(cLine)
        IF cLine[j]='_' THEN cLine[j]=':'.    !Carl FYI: It appears C4 Beta used "_" in browse fields, now uses ":"
      END
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()

PromptsSection ROUTINE
  SELF.Buttons=Action:Cancel+Action:Apply
  Info.AddTitle('Browse classes')
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF INSTRING('(',cLine) AND SELF.SaveComment(cLine)
      SELF.WildReplace(cLine,ForInline,SectionMgr,Info,i)
      IF SectionMgr.LineChanged(i,cLine)
        SELF.RestoreComment(cLine)
        SectionMgr.SetLine(i,cLine)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign



!--- Remove Unwanted symbols ----

RemoveSymbols.Construct PROCEDURE

  CODE
  SELF.Register(252,OwnerName,'Remove Template Symbols','&Remove Symbols:','[PROMPTS][PERSIST]')


RemoveSymbols.TakeSection FUNCTION(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

cLine           CSTRING(MaxLineLen),AUTO
RemoveCnt       LONG(0)
i               LONG(1)
j               LONG,AUTO
InsideSection   BYTE(False)
PromptNames     CSTRING('%pClassName|%ClassName|%pClassMethod|%ClassMethod|%pClassMethodprototype|%ClassMethodPrototype|%ClassProperty|%pClassProperty|%ClassMethodList|%ClassPropertyList|%ClassImplementationfile|%ClassIncludeFile|%ClassMethods|%LocalReferences')

  CODE
  SELF.Buttons=Action:Apply
  IF SELF.CurrentProcedure()
    Info.AddTitle('Symbols Removed from TXA [PROMPTS] Section inside '&SELF.CurrentProcedure())
  ELSE
    Info.AddTitle('Symbols Removed from TXA [PROMPTS] Section')
  END
  LOOP
    SectionMgr.Getline(i,cline)
    IF cLine AND (cLine[1]='%' OR SUB(cLine,1,4)='WHEN')
      SELF.Lexer.TakeLine(cLine)
      IF SELF.Lexer.GetToken(1)='WHEN' AND InsideSection
        DO RemoveLine
        Info.AddLine('Line removed, instance of redundant symbol',i)
      ELSIF SELF.Lexer.GetToken(1)='%'
        IF INSTRING('%'&SELF.Lexer.GetToken(2),PromptNames,1,1)
          DO RemoveLine
          InsideSection=True
          Info.AddLine('Line removed, symbol redundant',i)
        ELSIF SELF.Lexer.GetToken(3)='DEPEND' AND SELF.Lexer.GetToken(4)='%' AND INSTRING('%'&SELF.Lexer.GetToken(5),PromptNames,1,1)
          DO RemoveLine
          InsideSection=True
          Info.AddLine('Line removed, symbol dependant upon redundant symbol %'&SELF.Lexer.GetToken(5),i)
        ELSE
          InsideSection=False
        END
      ELSE
        InsideSection=False
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

RemoveLine ROUTINE
  SectionMgr.ClearLine(i)



!--- Update Symbol Contents-----

UpdateSymbols.Construct PROCEDURE

  CODE
  SELF.Register(252,OwnerName,'Update Template Symbols','&Update Symbols:','[PROMPTS]')


UpdateSymbols.TakeSection FUNCTION(SectionClass SectionMgr,InfoTextClass Info,STRING SectionHeader)

Chgd  BYTE(False)
cLine CSTRING(MaxLineLen),AUTO
i     LONG(1)

  CODE
  SELF.Buttons=Action:Cancel+Action:Apply
  Info.AddTitle('Update Template Symbol Contents')
  LOOP
    SectionMgr.GetLine(i,cLine)
    IF SELF.PreFilterLine(cLine,'%HorizPositional|%VertPositional')
      IF SELF.Replace(cLine,'Fix Centre','Fix Center')
        SectionMgr.SetLine(i,cLine)
        Info.AddLine('Text of prompt value internationalized',i)
      END
    END
    i+=1
  WHILE i<=SectionMgr.GetLineCount()
  RETURN Level:Benign

