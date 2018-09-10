(define-easy-handler (blog :uri (fmt "~A/blog" *ctxt*)) (cont)
  (page "Blog"
    (when cont
      (dodaj-comment *logyear* *logmonth* *logday*  cont))
    (dolist (e (query "select * from comments"))
      (out "<div><h3>" (nth 3 e) "/"  (nth 2 e) "/"  (nth 1 e) "</h3>"  (nth 4 e) "</div>" ))

    ))


(define-easy-handler (blog2 :uri (fmt "~A/bloginsert" *ctxt*)) ()
  (with-output-to-string(s)
     (let ((*standard-output* s))
       (format t "<html>~&")
       (format t "<head>~&")
       (format t "<title>Insert</title>~&")
       (out "<script language='JavaScript' type='text/javascript'" (fmt "src='/files~A/rte/richtext.js'>" *ctxt*))
       (out "</script>")
       (format t "</head>~&")
       (format t "<body>~&")
       (main-menu)
       (terpri)
       (out "<hr>")
       (out *logday* "/" *logmonth* "/" *logyear* "<br>" )

    (out (fmt "<form action='~A/blog' name='myform' method='post' onsubmit='return submitForm();'>" *ctxt*))
    (terpri)
    (out "<script language='JavaScript' type='text/javascript'>")(terpri)

    (out "
function submitForm() {
//make sure hidden and iframe values are in sync before submitting form
updateRTE('cont'); //use this when syncing only 1 rich text editor ('rtel' is name of editor)
//updateRTEs(); //uncomment and call this line instead if there are multiple rich text editors inside the form
//alert(\"Submitted value: \"+document.myform.cont.value) //alert submitted value
return true; //Set to false to disable form submission, for easy debugging.
}

//Usage: initRTE(imagesPath, includesPath, cssFile)
initRTE(\"/files/primalogs/images/\", \"/files/primalogs/rte\", \"/files/primalogs/rte\");

")
    (out "writeRichText('cont','',400,200,true,false);")
    (out "</script>")

    ;(out "<textarea name='cont' rows='5' cols='30'></textarea>")
    (out "<input type='submit' value='Dodaj'>")
    (out "</form>")

        (format t "~&</body>~&")
       (format t "<html>~&"))))