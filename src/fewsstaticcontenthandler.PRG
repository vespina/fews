DEFINE CLASS fewsStaticContentHandler AS fewsHandlerAbstract
    Name = "staticContentHandler"
    oB64 = NULL
    
    PROCEDURE Init
    	THIS.oB64 = CREATE("base64Helper")
    	RETURN
    	
	PROCEDURE Process
	    LOCAL oReq,oResp
	    oReq = THIS.oRequest
	    oResp = THIS.oResponse
	    LOCAL cLocalFile,cUrl
	    cUrl = oReq.cUrl
	    IF cUrl == "/"
	    	cUrl = "/INDEX.HTML"
	    ENDIF
	    cUrl = SUBS(cUrl, 2)
	    cLocalFile = ADDBS(oReq.oServer.cDocumentsRoot) + CHRT(cUrl,"/","\")
	    IF FILE(cLocalFile)
	    	LOCAL cExt,cContent
	    	cExt = LOWER(JUSTEXT(cLocalFile))
	    	cContent = FILETOSTR(cLocalFile)
	    	DO CASE
	    	   CASE INLIST(cExt,"html","htm")
	    	        oResp.initWithData("200 OK","text/html", cContent)

	    	   CASE INLIST(cExt,"html","csv")
	    	        oResp.initWithData("200 OK","text/csv", cContent)

	    	   CASE INLIST(cExt,"json")
	    	        oResp.initWithData("200 OK","application/json", cContent)	 

	    	   CASE INLIST(cExt,"pdf")
	    	        oResp.initWithData("200 OK","application/pdf", cContent)	 
	    	        	    	        
	    	   CASE INLIST(cExt,"js","txt","css")
	    	        oResp.initWithData("200 OK","text/plain", cContent)	   
	    	        
	    	   CASE INLIST(cExt,"bmp","gif","jpg","jpeg")
	    	        cExt = STRT(cExt,"jpg","jpeg")
	    	        oResp.initWithData("200 OK","image/" + cExt, cContent)	   
	    	        	    	        
	    	   OTHERWISE
	    	        cContent = THIS.oB64.encodeString(cContent)
	    	   	    oResp.initWithData("200 OK","application/octet-stream", cContent)	
	    	ENDCASE
	    ELSE
	    	oResp.cStatusCode = "404 NOT FOUND"
	    ENDIF
		RETURN
ENDDEFINE
