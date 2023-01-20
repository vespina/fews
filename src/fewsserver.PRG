
DEFINE CLASS fewsServer AS winsock

    cName = "FEWS"
	cVersion = "1.0"
	nPort = 8006
	cLocalIP = ""
	nMaxConnections = 5
	DIMEN aConnPool[1]
	DIMEN aHandlers[1]
	nHandlersCount = 0
	cDocumentsRoot = ""
	cLastRequest = ""
	cLastResponse = ""
	cLastStatusCode = ""
	oHistory = NULL
	cLogFolder = ""
	nLogLevel = 9
	
	PROCEDURE Init
		DIMENSION THIS.aConnPool[THIS.nMaxConnections]
		LOCAL nConn,oConn
		FOR nConn = 1 TO THIS.nMaxConnections
		    oConn = CREATE("fwwsRequest")
		    WITH oConn
		    	.nId = nConn
		    	.Bind(THIS)
		    ENDWITH
			THIS.aConnPool[nConn] = oConn
		ENDFOR
		THIS.addHandler("*","fewsStaticContentHandler")
		THIS.cDocumentsRoot = SET("DEFAULT") + CURDIR()
		THIS.oHistory = CREATE("fewsHistoryManager")
		THIS.cLogFolder = THIS.cDocumentsRoot
		RETURN

	PROCEDURE cLocalIP_Access
		RETURN THIS.socket.localIP
		
	PROCEDURE Error
		LPARAMETERS nError, cMethod, nLine
	     THIS.addToLog("[SRV][ERR][" + ALLT(STR(nError)) + "][" + cMethod + ":" + ALLT(Str(nLine)) +"] " + MESSAGE(),1)
	     RETURN	
		
	PROCEDURE Destroy
		LOCAL nConn,cOnError
		cOnError = ON("ERROR")
		ON ERROR RETURN
		FOR nConn = 1 TO THIS.nMaxConnections
			THIS.aConnPool[nConn].Unbind()
			THIS.aConnPool[nConn] = NULL
		ENDFOR
		IF !EMPTY(cOnError)
			ON ERROR &cOnError
		ELSE
			ON ERROR	
		ENDIF
		RETURN
			
	PROCEDURE Listen
	    THIS.oHistory.cDataFile = ADDBS(THIS.cLogFolder) + "fews_history.log" 
	    WITH THIS.socket
	    	.Protocol = 0  && TCP
	    	.LocalPort = THIS.nPort
	    	.Listen()
	    ENDWITH
	    THIS.addToLog("[SRV] Listening on http://" + THIS.cLocalIP + ":" + ALLT(STR(THIS.nPort)),5)
		RETURN
		

	PROCEDURE addHandler
		LPARAMETERS pcFilter, pcHandlerClass
		LOCAL nIndex
		nIndex = THIS.getHandlerIndex(pcFilter)
		IF nIndex = 0
			THIS.nHandlersCount = THIS.nHandlersCount + 1
			DIMEN THIS.aHandlers[THIS.nHandlersCount]
			nIndex = THIS.nHandlersCount
		ENDIF
		THIS.aHandlers[nIndex] = CREATE("wsKVP", LOWER(pcFilter), pcHandlerClass)
		RETURN
		

	PROCEDURE getHandlerIndex
		LPARAMETERS pcFilter
		LOCAL nIndex,nH
		nIndex = 0
		pcFilter = LOWER(pcFilter)
		FOR nH = 1 TO THIS.nHandlersCount
			IF THIS.aHandlers[nH].cName == pcFilter
				nIndex = nH
				EXIT
			ENDIF
		ENDFOR
		RETURN nIndex
				
		
	PROCEDURE getHandler
		LPARAMETERS pcFilter
		LOCAL nIndex,oHandler
		oHandler = NULL		
		nIndex = THIS.getHandlerIndex(pcFilter)
		IF nIndex = 0
			nIndex = THIS.getHandlerIndex("*")
		ENDIF
		IF nIndex > 0
			oHandler = CREATE( THIS.aHandlers[nIndex].cValue )	
		ENDIF
		RETURN oHandler

				
	PROCEDURE socket.ConnectionRequest
		LPARAMETERS requestid
		THIS.Parent.addToLog("[SVR] Request received from " + THIS.remoteHostHost + " (" + THIS.remoteHostIP + "), ID:" + TRANS(requestid,""),5)
		LOCAL oConn
		oConn = THISFORM.getAvailConn()
		IF !ISNULL(oConn)
			oConn.socket.Accept(requestid)
			THIS.Parent.addToLog("[SVR] Assigned to connection #" + ALLT(STR(oConn.nID)), 5)
		ELSE
			THIS.Parent.addToLog("[SVR] No available connections at this time",1)
			LOCAL ARRAY aCPS[1]
			FOR oConn = 1 TO ALINES(aCPS, THIS.Parent.getConnPoolState())
				THIS.PArent.addToLog("[SVR] " + aCPS[oConn],1)
			ENDFOR
		ENDIF
		RETURN
		


	PROCEDURE getAvailConn
		LOCAL oConn, nConn, oAvailConn
		oAvailConn = NULL
		FOR nConn = 1 TO THIS.nMaxConnections
		    oConn = THIS.aConnPool[nConn]
		    IF oConn.lHandled
		    	IF INLIST(oConn.nState,SOCKET_STATE_CONNECTED,SOCKET_STATE_CLOSING)
		    		oConn.Object.Close()
		    	ENDIF
		    	oConn.Unbind()
		    	RELEASE oConn		        
		    	oConn = CREATE("fewsRequest")
		    	oConn.nId = nConn
		    	oConn.Bind(THIS)
		    	THIS.aConnPool[nConn] = oConn
		    ENDIF
			IF oConn.nState = SOCKET_STATE_CLOSED
				oAvailConn = THIS.aConnPool[nConn]
				EXIT
			ENDIF
		ENDFOR
		RETURN oAvailConn



	PROCEDURE addToLog
		LPARAMETERS pcText,pnLevel
		LOCAL cLogFile
		cLogFile = ADDBS(THIS.cLogFolder) + "fews.log"
		pnLevel = EVL(pnLevel, 9)
		IF pnLevel > THIS.nLogLevel
			RETURN
		ENDIF
		IF !FILE(cLogFile)
			FCLOSE(FCREATE(cLogFile))
		ENDIF
		LOCAL nFH
		nFH = FOPEN(cLogFile,11)
		FSEEK(nFH,0,2)
		FWRITE(nFH,"[" + TTOC(DATETIME()) + "][" + ALLT(STR(pnLevel)) + "] " + pcText + CRLF)
		FCLOSE(nFH)
		IF _VFP.startMode = 0
			?DATETIME(),pnLevel,pcText
		ENDIF
		RETURN	
		
				
		
	PROCEDURE getConnPoolState
		LOCAL cState,nConn,oConn
		cState = ""
		FOR nConn = 1 TO THIS.nMaxConnections
		    oConn = THIS.aConnPool[nConn]
			cState = cState + ;
			         ALLT(STR(oConn.nId)) + "," + ;
			         oConn.cState + "," + ;
			         oConn.cUrl + "," + ;
			         oConn.cStatusCode + CRLF
		ENDFOR
		RETURN cState
ENDDEFINE
