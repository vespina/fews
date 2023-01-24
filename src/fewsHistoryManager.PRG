DEFINE CLASS fewsHistoryManager AS Custom
	cDataFile = ""
	oEntry = NULL
	
	PROCEDURE beginRequest
		LPARAMETERS poRequest
		THIS.oEntry = CREATE("fewsHistoryEntry")
		WITH THIS.oEntry
		    .nConnId = poRequest.nId
			.tReceived = DATETIME()
			.nSecs = SECONDS()
			.cUrl = poRequest.cUrl
			.cRemoteHost = poRequest.socket.remoteHost
			.cRemoteIP = poRequest.socket.remoteHostIP
			.nRemotePort = poRequest.socket.remotePort
			.cQS = poRequest.cQueryString
		ENDWITH
		RETURN
		
		
	PROCEDURE completeRequest
		LPARAMETERS poResponse
		IF ISNULL(THIS.oEntry)
			RETURN
		ENDIF
		WITH THIS.oEntry
			.tSent = DATETIME()
			.nSecs = SECONDS() - .nSecs
			.cResponse = poResponse.cStatusCode
			.cHandler = poResponse.cHandler
		ENDWITH
		LOCAL nFH
		IF FILE(THIS.cDataFile)
			nFH = FOPEN(THIS.cDataFile,11)
			IF nFH > 0
				FSEEK(nFH,0,2)
			ENDIF
		ELSE
			nFH = FCREATE(THIS.cDataFile)
		ENDIF
		IF nFH < 0
			RETURN
		ENDIF
		FWRITE(nFH, THIS.oEntry.ToString() + CRLF)
		FCLOSE(nFH)
		RETURN
		
		

ENDDEFINE

DEFINE CLASS fewsHistoryEntry AS Custom
    nConnID = 0
	tReceived = {//::}
	cRemoteHost = ""
	nRemotePort = ""
	cRemoteIP = ""
	cUrl = ""
	cQS = ""
	cResponse = ""
	cHandler = ""
	tSent = {//::}
	nSecs = 0
	
	PROCEDURE ToString
	    LPARAMETER pnFormat
	    pnFormat = EVL(pnFormat, 1)
	    LOCAL cResp
	    cResp = ""
	    DO CASE
	       CASE pnFormat = 1
	            cResp = TTOC(THIS.tReceived) + "|" + ;
				        ALLT(STR(THIS.nConnID)) + "|" + ;
				        THIS.cRemoteIP + ":" + ALLT(STR(THIS.nRemotePort)) + IIF(EMPTY(THIS.cRemoteHost),"|"," (" + THIS.cRemoteHost + ")|") + ;
				        LOWER(THIS.cUrl) + "|" + ;
				        THIS.cQS + "|" +;
				        THIS.cHandler + "|" + ;
				        ALLT(TRANS(THIS.nSecs,"999.999"))+"s" + "|" + ;
				        THIS.cResponse
				        
		   CASE pnFormat = 2
		        cResp = "[" + TTOC(THIS.tReceived) + "] " + ;
		                LOWER(THIS.cUrl) + " " + ;
		                THIS.cResponse + " " + ;
		                "(" + ALLT(TRANS(THIS.nSecs,"999.999"))+"s)"
	    ENDCASE
		RETURN cREsp 
ENDDEFINE	