DEFINE CLASS fewsRequest AS winsock
    nId = 0
	cRawHTTP = ""
	cVerb = ""
	cUrl = ""
	cQueryString = ""
	cContentType = ""
	cBoundary = ""
	cAccept = ""
	cUserAgent = ""
	DIMEN aHeaders[1]
	DIMEN aQS[1]
	DIMEN aFD[1]
	nHeadersCount = 0
	nQSCount = 0
	nFDCount = 0 
	lCORSPreflight = .F.
	cOrigin = ""
	cRawData = ""
	cError = ""
	oServer = NULL
	cFilter = ""
	lHandled = .F.
	cStatusCode = ""


	PROCEDURE Bind
		LPARAMETERS poServer
		THIS.oServer = poServer
		RETURN

		
	PROCEDURE Unbind
		THIS.oServer = NULL
		RETURN
		
		
	PROCEDURE cRawData_Access
		RETURN THIS.cContent	
			
			
	PROCEDURE socket.DataReceived
		LPARAMETERS pcHeaders,pcContent
		LOCAL oDelegate
		oDelegate = THIS.PArent.oServer.oDelegate
		WITH THIS.Parent
			.oServer.cLastRequest = .cHeaders + CRLF + .cContent
			.oServer.cLastResponse = ""
			.oServer.cLastStatusCode = ""
			.parseHeaders()
			.splitQS() 
			IF INLIST(LOWER(.cContentType), "multipart/form-data", "application/x-www-form-urlencoded")
				.parseFormData()
			ENDIF
			.oServer.oHistory.beginRequest(THIS.PArent)
			.oServer.addToLog("[REQ][" + ALLT(STR(.nId)) + "] " + .cUrl,5)
			.oServer.addToLog(.cHeaders, 9)
			IF !EMPTY(.cContent)
				.oServer.addToLog(LEFT(.cContent, 8192), 9)
			ENDIF
			
			IF _VFP.StartMode = 0 AND .oServer.lDebugMode
	            ?REPL("*",100)			
				?"[CONN #" + ALLT(STR(.nId)) + "]",.nState
			    ?.cHeaders
		    ENDIF
		    
		    LOCAL oResponse
		    oResponse = CREATE("fewsResponse")
		    lHandled = .F.
		    IF oDelegate.onBeforeHandleRequest(THIS.Parent)
		    	oResponse = .Handle()
		    	IF NOT ISNULL(oResponse)
		    		.lHandled = .T.
		    		oDelegate.onBeforeResponse(oResponse)
		    	ELSE
					IF EMPTY(oResponse.cHandler)
						.oServer.addToLog("[REQ][ERR] An appropiate handler could not be found (" + THIS.PArent.cUrl + ")",1)
						oResponse.initWithData("422 Unprocessable Entity")
					ELSE
						.oServer.addToLog("[REQ][ERR][" + oREsponse.cHandler + "] an appropiate response could not be generated",1)
			            oResponse.initWithData("400 Bad Request")		
					ENDIF			    	
		    	ENDIF
		    ELSE
		    	oResponse.initWithData("403 Forbidden")
		    ENDIF
		    .oServer.oHistory.completeRequest(oResponse)
		    .oServer.cLastResponse = oResponse.ToString()
		    .oServer.cLastStatusCode = oResponse.cStatusCode
		    oDelegate.onConsole(.oServer.oHistory.oEntry.ToString(2))
		    oDelegate.onRequestHandled(THIS.PArent, oResponse)
		    .cStatusCode = oResponse.cStatusCode 
		    IF _VFP.StartMode = 0 AND .oServer.lDebugMode
			    ? 
			    ?"-----------"
			    ?"[RESPONSE]"
			    ?LEFT(.oServer.cLastResponse,300)
		    ENDIF
		    .oServer.addToLog("[REQ] " + oResponse.cStatusCode + " (" + oResponse.cHandler + ")",5)
		    .oServer.addToLog(LEFT(.oServer.cLastResponse,8192),9)
			.SendData( CREATEBINARY(.oServer.cLastResponse) )	
		ENDWITH
		RETURN
		


	PROCEDURE Handle
	    LOCAL oResponse,oHandler
	    oResponse = CREATE("fewsResponse")
		IF ISNULL(THIS.oServer)
		    oResponse.cStatusCode = "503 Service Unavailable"
			RETURN oResponse
		ENDIF
		oResponse.cServerName = THIS.oServer.cName + " " + THIS.oServer.cVersion
		DO CASE
		   CASE THIS.lCORSPreflight
		        oHandler = CREATE("fewsCORSHandler")
		
		   OTHERWISE		
		   		oHandler = THIS.oServer.getHandler(THIS.cFilter)
		ENDCASE
		IF ISNULL(oHandler)
		    oResponse.cStatusCode = "501 Not Implemented"
			RETURN oResponse		
		ENDIF
		oResponse.cHandler = oHandler.Name
		oHandler.oRequest = THIS
		oHandler.oResponse = oResponse
		oHandler.Process()		
		RETURN oResponse


	PROCEDURE Header
		LPARAMETERS pcName,pcDefault
		LOCAL cValue,nHdr
		cValue = IIF(PCOUNT() = 1,"",pcDefault)
		pcName = ALLT(LOWER(pcName))
	    FOR nHdr = 1 TO THIS.nHeadersCount
	    	IF LOWER(THIS.aHeaders[nHdr].cName) == pcName
	    		cValue = THIS.aHeaders[nHdr].cValue
	    		EXIT
	    	ENDIF
	    ENDFOR
	    RETURN cValue
	    
	    
	PROCEDURE QS
		LPARAMETERS pcName,pcDefault
		LOCAL cValue,nHdr
		cValue = IIF(PCOUNT() = 1,"",pcDefault)
		pcName = ALLT(LOWER(pcName))		
	    FOR nHdr = 1 TO THIS.nQSCount
	    	IF THIS.aQS[nHdr].cName == pcName
	    		cValue = THIS.aQS[nHdr].cValue
	    		EXIT
	   	ENDIF
	    ENDFOR
	    RETURN cValue	    


	PROCEDURE Form
		LPARAMETERS pcName,pcDefault
		LOCAL cValue,nFD
		cValue = IIF(PCOUNT() = 1,"",pcDefault)
		pcName = ALLT(LOWER(pcName))	
	    FOR nFD = 1 TO THIS.nFDCount
	    	IF THIS.aFDD[nFD].cName == pcName
	    		cValue = THIS.aFD[nFD].cValue
	    		EXIT
	   	ENDIF
	    ENDFOR
	    RETURN cValue	    
		
		
	PROCEDURE addToLog
		LPARAMETERS pcText,pnLevel
		RETURN THIS.oServer.addToLog(pcText, pnLevel)

				
*!*	   PROCEDURE socket.DataArrival
*!*	      LPARAMETERS tnByteCount
*!*	      LOCAL lcBuffer
*!*	      lcBuffer = SPACE(tnByteCount)
*!*	      LOCAL cData
*!*	      This.GetData( @lcBuffer, , tnByteCount )
*!*	      *lcBuffer = STRT(lcBuffer,CRLF+CRLF,"")
*!*	      THIS.Parent.cRawData = THIS.PArent.cRawData + lcBuffer
*!*	      THIS.Parent.NQSCount=THIS.Parent.nQSCount + 1
*!*	      ?THIS.Parent.nQSCount,tnByteCount, LEN(lcBuffer), LEN(THIS.PArent.cRawData)
*!*	      STRTOFILE(THIS.PArent.cRawData, "request.txt")
*!*	      RETURN
				

	PROCEDURE parseHeaders
		LOCAL lnContentSize, lnLine, lcLine, llcHdrName, lcHdrValue, nQSPos
		LOCAL ARRAY aContent[1]
		lnContentSize = ALINES(aContent, THIS.cHeaders)  	
	    STORE "" TO lcData,lcContentType,lnContentLength,lcMethod,lcOrigin
	    THIS.nHeadersCount = 0
	    FOR lnLine = 1 TO lnContentSize
	    	lcLine = aContent[lnLine]
	    	DO CASE
	    		CASE lnLine = 1
	    			THIS.cVerb = UPPER(ALLTRIM(GETWORDNUM(lcLine, 1, " ")))
	    			THIS.cUrl = UPPER(ALLTRIM(GETWORDNUM(lcLine, 2, " ")))
	    			nQSPos = AT("?",THIS.cUrl)
	    			IF nQSPos > 0
	    				THIS.cQueryString = SUBS(THIS.cUrl, nQSPOS + 1)
	    				THIS.cUrl = LEFT(THIS.cUrl, nQSPOS - 1)
	    			ENDIF
	    			THIS.cFilter = "." + LOWER(JUSTEXT(THIS.cUrl))
	    				    		
	    	    CASE ": " $ lcLine
	    	         lcHdrName = GETWORDNUM(lcLine, 1, ": ")
	    	         lcHdrValue = GETWORDNUM(lcLine, 2, ": ")
	    	         THIS.nHeadersCount = THIS.nHeadersCount + 1
	    	         THIS.cHeaders = THIS.cHeaders + IIF(THIS.nHeadersCount > 1, CRLF, "") + lcLine
	    	         DIMEN THIS.aHeaders[THIS.nHeadersCount]
	    	         THIS.aHeaders[THIS.nHeadersCount] = CREATE("wsKVP",lcHdrName,lcHdrValue)
	    	         DO CASE	    	         
			    		CASE lcHdrName == "Content-Type"
			    			THIS.cContentType = ALLTRIM(LOWER(lcHdrValue))
			    			IF ATC("boundary",THIS.cContentType) > 0
			    				THIS.cBoundary = GETWORDNUM(GETWORDNUM(THIS.cContentType, 2,";"),2,"=")
			    				THIS.cContentType = GETWORDNUM(THIS.cContentType, 1, ";")
			    			ENDIF
			    			
			    		CASE lcHdrName == "Content-Length"
			    			THIS.nContentLength = INT(VAL(lcHdrValue))

			    	   CASE lcHdrName == "Origin"
		                    THIS.cOrigin = ALLTRIM(SUBSTR(lcLine, 8)) 
		                    
		               CASE lcHdrName == "User-Agent"
		               	    THIS.cUserAgent = lcHdrValue
                    ENDCASE
	    	ENDCASE 	    	
	    ENDFOR	    
	    IF THIS.cVerb == "OPTIONS" AND !EMPTY(THIS.cOrigin)
	    	THIS.lCORSPreFlight = .T.
	    ENDIF	    	        	        	    	    
		RETURN


	PROCEDURE parseFormData
	    LOCAL nFD,cFDName,cFDValue
	    DO CASE
	       CASE THIS.cContentType == "multipart/form-data"
			    LOCAL ARRAY aContent[1]
			    LOCAL nLines,nLine,cLine
			    nLines = ALINES(aContent, THIS.cContent)
			    THIS.nFDCount = OCCURS(UPPER(THIS.cBoundary),UPPER(THIS.cContent)) - 1
			    IF THIS.nFDCount > 0
			        DIMENSION THIS.aFD[THIS.nFDCount]
			        nFD = 0
				    FOR nLine = 1 TO nLines - 1
				    	cLine = aContent[nLine]
				    	IF ATC(THIS.cBoundary,cLine) > 0
				    		cFDName = LOWER(CHRT(GETWORDNUM(aContent[nLine + 1],2,"="),["],""))
				    		cFDValue = aContent[nLine + 3]
				    		nFD = nFD + 1
				    		THIS.aFD[nFD] = CREATE("wsKVP",cFDName,cFDValue)
				    	ENDIF
				    ENDFOR
			    ENDIF
			    
			    
			CASE THIS.cContentType == "application/x-www-form-urlencoded"
				 THIS.nFDCount = GETWORDCOUNT(THIS.cContent, "&")
				 IF THIS.nFDCount > 0
					DIMEN THIS.aFD[THIS.nFDCount]					
					FOR nFD = 1 TO THIS.nFDCount
						cFDValue = GETWORDNUM(THIS.cContent, nFD, "&")
						cFDName = LOWER(GETWORDNUM(cFDValue, 1, "="))
						cFDValue = GETWORDNUM(cFDValue, 2, "=")
						THIS.aFD[nFD] = CREATE("wsKVP", cFDName, cFDValue)
					ENDFOR
				 ENDIF			
		ENDCASE
		RETURN
					
						
	PROCEDURE splitQS
		THIS.nQSCount = GETWORDCOUNT(THIS.cQueryString, "&")
		IF THIS.nQSCount > 0
			DIMEN THIS.aQS[THIS.nQSCount]
			LOCAL nQS,cQSName,cQSValue
			FOR nQS = 1 TO THIS.nQSCount
				cQSValue = GETWORDNUM(THIS.cQueryString, nQS, "&")
				cQSName = LOWER(GETWORDNUM(cQSValue, 1, "="))
				cQSValue = GETWORDNUM(cQSValue, 2, "=")
				THIS.aQS[nQS] = CREATE("wsKVP", cQSName, cQSValue)
			ENDFOR
		ENDIF
		RETURN
						
						
							
ENDDEFINE
