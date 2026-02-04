 Elm code for maintaining a LispLog-like weblog.

 Reads and writes RSS, as xml or json.

 See [lisplog.org](https://lisplog.org) for lisp implementation of this. The new one adds Markdown as the default content type, but supports HTML, as now. and writes HTML, to make browsing FAST.
 
 To debug, execute once:
 
     cd .../elmlog/site
     elm reactor
     
Then developmente becomes:

     1. Change source .elm files
     2. bin/build
     3. Refresh browser pointing at http://localhost:8000/index.html

