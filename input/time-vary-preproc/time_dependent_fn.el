;;; -*- Mode: Emacs-Lisp -*- 
;This database auxiliary file is for use with time dependent database files.

(defconst comment_symbol "#"
"The symbol inserted before a comment.")

(defconst time_types '(("15MIN") ("1HOUR") ("1DAY") ("1WEEK") ("1MONTH") ("1YEAR") ("IR-DAY") ("IR-MONTH") ("IR-YEAR") ("IR-DECADE"))
	"The six basic time steps for time series data.")

(defconst area-const "DELTA"
  "The area constant used as the default for the area field.")

(defconst data-types '(("FLOW") ("STAGE") ("EC"))
  "A list of all the hydro data types.")
(defconst type-units '(("FLOW" . "CFS") ("STAGE" . "FEET") ("EC" . "MMOHMS"))
  "An alist of all the allowable data types and the associated data units.")

(defvar old-field-value ""
  "Stores the original value of the current field being modified.")

;; Set some parameters for the database
(database-set-no-of-fields database 12)
(database-set-print-name database "Time-Dependent Data Entry Form")
(setq dbf-every-change-function 'field-modified)
(setq dbf-enter-field-function 'field-entered)

;; Set the characters that seperate fields and records
(sepinfo-set-sep-string (database-record-sepinfo database) "\n")
(sepinfo-set-sep-string (database-field-sepinfo database) "\t")
(sepinfo-set-sep-regexp (database-field-sepinfo database) "\t+")
(sepinfo-set-sep-regexp-submatch (database-field-sepinfo database) 0)

;; declare some new types needed for the time dependent database

;; the "area-type" type
(let ((ds (copy-displayspec (displaytype->displayspec 'one-line-string-or-nil))))
  (displayspec-set-indent ds nil)
  (define-displaytype-from-displayspec 'area-type ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'one-line-string-or-nil))))
  (recordfieldspec-set-default-value rs "DELTA")
  (define-recordfieldtype-from-recordfieldspec 'area-type rs))

;; the "dtype" type
(let ((ds (copy-displayspec (displaytype->displayspec 'one-line-string-or-nil))))
  (displayspec-set-indent ds nil)
  (define-displaytype-from-displayspec 'dtype ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'one-line-string-or-nil))))
  (recordfieldspec-set-default-value rs "FLOW")
  (define-recordfieldtype-from-recordfieldspec 'dtype rs))

;; the "dunits" type
(let ((ds (copy-displayspec (displaytype->displayspec 'one-line-string-or-nil))))
  (displayspec-set-indent ds nil)
  (define-displaytype-from-displayspec 'dunits ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'one-line-string-or-nil))))
  (recordfieldspec-set-default-value rs "CFS")
  (define-recordfieldtype-from-recordfieldspec 'dunits rs))

;; the "timestr" type
(let ((ds (copy-displayspec (displaytype->displayspec 'one-line-string))))
  (displayspec-set-max-height ds 1)
  (displayspec-set-indent ds nil)
  (displayspec-set-match-actual->display ds 'string-match-actual->display)
  (displayspec-set-match-display->actual ds 'string-match-display->actual)
  (define-displaytype-from-displayspec 'timestr ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'one-line-string))))
  (recordfieldspec-set-type rs 'timestr)
  (recordfieldspec-set-default-value rs "0100")
  (recordfieldspec-set-order-fn rs (function string-order-ci))
  (recordfieldspec-set-sort-fn rs (function string-lessp-ci))
  (recordfieldspec-set-match-function rs (function string-match-function))
  (recordfieldspec-set-help-info rs "Time: 0000 to 2400")
  (recordfieldspec-set-constraint-function rs (function timestr))
  (define-recordfieldtype-from-recordfieldspec 'timestr rs))

;; the "time-step" type
(let ((ds (copy-displayspec (displaytype->displayspec 'one-line-string))))
  (displayspec-set-max-height ds 1)
  (displayspec-set-indent ds nil)
  (define-displaytype-from-displayspec 'time-step ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'one-line-string))))
  (recordfieldspec-set-type rs 'time-step)
  (recordfieldspec-set-default-value rs "1DAY")
  (recordfieldspec-set-help-info rs "Time sizes: 15MIN, 1DAY, 1WEEK, 1MONTH, 1YEAR, IR-DAY, IR-MONTH, IR-YEAR, IR-DECADE")  
  (recordfieldspec-set-constraint-function rs (function time-step))
  (define-recordfieldtype-from-recordfieldspec 'time-step rs))

;; the "one-line-string-or-nil" type
(let ((ds (copy-displayspec (displaytype->displayspec 'one-line-string))))
  (displayspec-set-max-height ds 1)
  (displayspec-set-indent ds nil)
  (define-displaytype-from-displayspec 'one-line-string-or-nil ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'one-line-string))))
  (recordfieldspec-set-default-value rs "*")
  (recordfieldspec-set-constraint-function rs (function no-whitespace))
  (define-recordfieldtype-from-recordfieldspec 'one-line-string-or-nil rs))

;; the "comment-type" type
(let ((ds (copy-displayspec (displaytype->displayspec 'string-or-nil))))
  (displayspec-set-indent ds nil)
  (displayspec-set-actual->display ds (function removecommentsymbol))
  (displayspec-set-display->actual ds (function insertcommentsymbol))
  (define-displaytype-from-displayspec 'comment-type ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'string-or-nil))))
  (recordfieldspec-set-default-value rs comment_symbol)
  (define-recordfieldtype-from-recordfieldspec 'comment-type rs))



;; Functions to handle the database

(defun field-modified (fieldname old new)
  "This function is called each time a field is modified in the time dependent
database. Mainly used to insert proper values for the fields."
  (cond

   ;; To match  data type wit data units
   ( (string= "data-type" fieldname)
     (let (( completion ""))
       (setq completion (try-completion (upcase new) data-types))
       (dbf-displayed-record-set-field fieldname completion)
       (if (setq units4type (cdr (assoc completion type-units)))
	   (dbf-displayed-record-set-field 'data-units units4type))))

   ;; For the time step
   ( (string= "step" fieldname)
     (if (string-match old new)
	 (setq new (substring new 0 (match-beginning 0))))
     
     (let (( completion ""))
       (setq completion (try-completion (upcase new) time_types))
       (dbf-displayed-record-set-field fieldname completion)

       (cond
	
	((string= "1DAY" completion)
	 (dbf-displayed-record-set-field 'phr "*"))
	
	((string= "1WEEK" completion)
	 (dbf-displayed-record-set-field 'phr "*"))
	
	((string= "1MONTH" completion)
	 (dbf-displayed-record-set-field 'phr "*")
	 (dbf-displayed-record-set-field 'pdy "*"))
	
	((string= "1YEAR" completion)
	 (dbf-displayed-record-set-field 'phr "*")
	 (dbf-displayed-record-set-field 'pdy "*")
	 (dbf-displayed-record-set-field 'pmh "*")))))

   
   ;; For padding starting hour or padding ending hour
   ( (or (string= "stime" fieldname) (string= "etime" fieldname))
     (if (string-match old new)
	 (setq new (substring new 0 (match-beginning 0))))
     (let ( (new-padded new) )
       (while (< (length new-padded) 4)
	 (setq new-padded (concat "0" new-padded)))
       (dbf-displayed-record-set-field fieldname new-padded)))
   
   ( (string= "comment" fieldname) 
     (if (not (string= new ""))
	 (if (not (string= comment_symbol
			   (substring new 0 1)))
	     (progn
	       (setq new (concat comment_symbol new))
	       (dbf-displayed-record-set-field fieldname new)))))
   (t
    (message "Field Modified"))) t)

;; A function to insure that time values fall with-in valid time ranges.
(defun timestr (field-value record field_no database)
  "Type timestr must be between 0 and 2400 inclusive"
  (if (string-match old-field-value field-value)
      (setq field-value (substring field-value 0 (match-beginning 0))))
  (if (and (<= 0 (string-to-int field-value)) (>= 2400 (string-to-int field-value)))
      t
    (error "%s invalid. Must be between 0000 and 2400" field-value)))


;; A function to insure that only valid time steps are selected
(defun time-step (field-value record field_no database)
  "Pre-defined type to used to indicate the available time steps for the time dependent database."
  (let ((completion ""))
    (setq completion (try-completion (upcase field-value) time_types))
    (setq dbf-redisplay-entire-record-p t)
    (cond 

     ( ( and (stringp completion) (or (string= completion "IR-D") 
				      (string= completion "IR-") ))
       (record-set-field record (fieldnumber->fieldname field_no database) database completion t)
       (dbf-redisplay-entire-record-maybe)
       (goto-char (dbf-this-field-end-pos))
       (error "Completions are IR-DAY, IR-MONTH, IR-YEAR, or IR-DECADE.")
       nil)

     ( ( and (stringp completion) (assoc completion time_types) )
       (record-set-field record (fieldnumber->fieldname field_no database) database completion t)
       (dbf-redisplay-entire-record-maybe)
       (goto-char (dbf-this-field-end-pos))       
       t)

     ( (eq completion t)
       t)
     
     ( (eq completion nil)
      (error "%s is not 15MIN, 1HOUR, 1DAY, 1WEEK, 1MONTH, 1YEAR, IR-DAY, IR-MONTH, IR-YEAR, or IR-DECADE." field-value)
      nil)

     ( t
      (error "Completions are 15MIN, 1HOUR, 1DAY, 1WEEK, 1MONTH, 1YEAR, IR-DAY, IR-MONTH, IR-YEAR, or IR-DECADE.")
      nil))))
     
(defun no-whitespace (field-value record field_no database)
  "Insures that there is no whitespace within fields"
  (if (not (string-match "[ \t\n]+" field-value))
      t
    (error "No spaces, tabs, or newlines allowed in this field.")))

(defun field-entered (field_no)
  (message "Just entered field %d with value %s" field_no
	  (setq old-field-value
		(dbf-displayed-record-field 
		 (if (listp (setq old_field (nth field_no fieldnames-list)))
		     (car-safe old_field)
		   old_field)))))
	      
(defun removecommentsymbol (commentstr)
  (if (not (string= "" commentstr))
      (if (string= comment_symbol
		   (substring commentstr 0 1))
	  (substring commentstr 1))))

(defun insertcommentsymbol (commentstr)
  (if (not (string= "" commentstr))
      (if (not (string= comment_symbol
			(substring commentstr 0 1)))
	  (concat comment_symbol commentstr))))
