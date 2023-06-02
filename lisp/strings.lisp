(write-line "Hello World")
(write-line "Welcome to Tutorials Point")

;escaping the double quote character
(write-line "Welcome to \"Tutorials Point\"")

(write-line (string-upcase "a big hello from tutorials point"))
(write-line (string-capitalize "a big hello from tutorials point"))

(write-line (string-trim " " "   a big hello from tutorials point   "))
(write-line (string-left-trim " " "   a big hello from tutorials point   "))
(write-line (string-right-trim " " "   a big hello from tutorials point   "))
(write-line (string-trim " a" "   a big hello from tutorials point   "))

(write (length "Hello World"))
(terpri)
(write-line (subseq "Hello World" 6))
(write (char "Hello World" 8))
(terpri)

;sorting the strings
(write (sort (vector "Amal" "Akbar" "Anthony") #'string<))
(terpri)

;merging the strings
(write (merge 'vector (vector "Rishi" "Zara" "Priyanka") 
	      (vector "Anju" "Anuj" "Avni") #'string<))
(terpri)

(write-line (reverse "Are we not drawn onward, we few, drawn onward to new era"))
