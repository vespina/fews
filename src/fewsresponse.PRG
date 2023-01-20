DEFINE CLASS fewsResponse AS Custom
	
	cStatusCode = "200 OK"
	cContentType = "text/html"
	nContentLength = 0
	cContent = ""
	cServerName = ""
	cHandler = ""
	lCORSPreflight = .F.
	cCORSOrigin = ""

	PROCEDURE Init
	  LPARAMETERS pcStatusCode, pcContentType, pcContent
	  THIS.initWithData(pcStatusCode, pcContentType, pcContent)
	  RETURN 
	  	
	  	
	PROCEDURE cContent_Assign(cValue)
		THIS.cContent = cValue
		THIS.nContentLength = LEN(cValue)
		RETURN
		  	
		  	
	PROCEDURE initWithData
	  LPARAMETERS pcStatusCode, pcContentType, pcContent
	  LOCAL cSignature
	  cSignature = VARTYPE(pcStatusCode) + VARTYPE(pcContentType) + VARTYPE(pcContent)
	  DO CASE 
	     CASE cSignature = "CLL"
	          THIS.cStatusCode = pcStatusCode
	     
	     CASE cSignature = "CCL"
	          THIS.cStatusCode = "200 OK"
	          THIS.cContentType = pcStatusCode
	          THIS.cContent = pcContentType
	          
	     CASE cSignature = "CCC"
	          THIS.cStatusCode = pcStatusCode
	          THIS.cContentType = pcContentType
	          THIS.cContent = pcContent

  	     OTHERWISE
	          THIS.cStatusCode = "200 OK"
	          THIS.cContentType = "text/html"
	          THIS.cContent = ""     	 
	  ENDCASE
	  RETURN	
	
	
		
	PROCEDURE ToString
	  LPARAMETERS pcStatusCode, pcContentType, pcContent
	  IF PCOUNT() > 0
	  	THIS.initWithData(pcStatusCode, pcContentType, pcContent)
	  ENDIF
      LOCAL cREsponse
      cResponse = ""
      DO CASE
         CASE THIS.lCORSPreFlight
              cResponse = [HTTP/1.1 200 OK] + ;
						  [Content-Length: 0] + ;
						  [Connection: keep-alive] + ;
						  [Access-Control-Allow-Origin: ] + THIS.cCORSOrigin + ;
						  [Access-Control-Allow-Methods: POST, OPTIONS] + ;
						  [Access-Control-Allow-Headers: Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With] + ;
						  [Access-Control-Max-Age: 86400] 
						  
         OTHERWISE
			  cResponse = [HTTP/1.1 ] + THIS.cStatusCode + CRLF + ;
						  [Cache-Control: private] + CRLF + ;
						  [Content-Type: ] + THIS.cContentType + CRLF + ;
						  [Content-length: ] + ALLT(STR(THIS.nContentLength)) + CRLF + ;
						  [Server: ] + THIS.cServerName + CRLF + ;
						  [Access-Control-Allow-Origin: *] + CRLF + ;
						  [Access-Control-Allow-Headers: Content-Type] + CRLF + ;
						  [Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS] + CRLF + ;
						  [Date: ]+ T2GMT(DATETIME()) + CRLF + ;
						  CRLF + ;
						  THIS.cContent
	  ENDCASE					  
	  RETURN cResponse    
ENDDEFINE
