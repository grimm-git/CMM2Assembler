' ============================================================
' error.inc  -  Error and warning reporting
' ============================================================

DIM err.count% = 0
DIM err.line%  = 0   ' source line number of the instruction being processed

SUB setColorError()
    IF MM.DEVICE$="MMB4L" THEN
	CONSOLE FOREGROUND 4
    ELSE
	COLOR &HFF0000
    ENDIF
END SUB

SUB setColorWarning()
    IF MM.DEVICE$="MMB4L" THEN
	CONSOLE FOREGROUND 6
    ELSE
	COLOR &HFFFF00
    ENDIF
END SUB

SUB resetColor()
    IF MM.DEVICE$="MMB4L" THEN
	CONSOLE FOREGROUND 15
    ELSE
	COLOR &HFFFFFF
    ENDIF
END SUB

' Report an assembly error at the current source line.
' Increments err.count%; assembly continues to collect further errors.
' @param   msg$   error message discplayed in Red
SUB showError(msg$)
    err.count% = err.count% + 1
    setColorError
    PRINT "   Error line " + STR$(err.line%) + ": " + msg$
    resetColor
END SUB

' Report an assembly arning at the current source line (does not abort assembly).
' @param   msg$   warning message displayed in Yellow
SUB showWarn(msg$)
    setColorWarning
    PRINT "   Warning line " + STR$(err.line%) + ": " + msg$
    resetColor
END SUB

' Print an generic error message to the console
' @param   msg$   error message to be displayed in Red
' @Param   indent%   optional indention of the error message
SUB printError(msg$, indent%=0)
    setColorError
    PRINT STRING$(indent%," ") + "ERROR: " + msg$
    resetColor
END SUB

' Print an generic warning message to the console
' @param   msg$   warning message to be displayed in Yellow
' @Param   indent%   optional indention of the error message
SUB printWarning(msg$, indent%=0)
    setColorWarning
    PRINT STRING$(indent%," ") + "WARNING: " + msg$
    resetColor
END SUB

