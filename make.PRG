CLOSE ALL
CLEAR ALL
CLEAR

#DEFINE CRLF		CHR(13)+CHR(10)

SET SAFETY OFF
SET TALK OFF
ON ERROR

?"FEWS MAKE PROGRAM"
?


?"* Cleaning dist folder..."
ERASE dist\fews.*
??"DONE!"


?"* Building dist\fews.prg..."
LOCAL ARRAY aSources[10]
aSources[1] = "winsock.prg"
aSources[2] = "fewsrequest.prg"
aSources[3] = "fewsresponse.prg"
aSources[4] = "fewshandlerabstract.prg"
aSources[5] = "fewsstaticcontenthandler.prg"
aSources[6] = "fewscorshandler.prg"
aSources[7] = "support.prg"
aSources[8] = "base64helper.prg"
aSources[9] = "fewshistorymanager.prg"
aSources[10] = "fewsserver.prg"

LOCAL cCode
cCode = FILETOSTR("src\header.prg")
FOR i = 1 TO ALEN(aSources,1)
	cSource = FILETOSTR(".\src\" + aSources[i])	
	cCode = cCode + REPL("*",100) + CRLF + ;
	                "** " + UPPER(PADC(aSources[i],94)) + " **" + CRLF + ;
	                REPL("*",100) + CRLF + ;
	                cSource + CRLF + ;
	                PADC(" END OF " + UPPER(aSources[i]) + " ",100,"*") + CRLF + ;
	                CRLF
ENDFOR
STRTOFILE(cCode, "dist\fews.prg")
??"DONE!"

?"* Compiling..."
COMP dist\fews
??"DONE!"

?"* Checking for errors..."
IF FILE("dist\fews.err")
    ??"FOUND!"
	MODI FILE dist\fews.err
ELSE
 	??"DONE!"
    ?
    ?"PROCESS COMPLETED"
ENDIF
        
        