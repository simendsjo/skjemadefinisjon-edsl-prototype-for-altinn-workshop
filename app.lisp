(declaim (optimize (speed 0) (space 0) (debug 3)))

(cl:in-package :app)

(deftype severity ()
  '(member trace info warn error fatal))

(deftype party ()
  '(member organization sub-unit))

(deftype visibility ()
  '(member owner user))

(defstruct (computation (:copier nil))
  (depends-on '() :read-only t)
  (original nil :read-only t)
  (reduced nil :read-only t))

(defstruct (validation-result (:copier nil))
  (severity 'info :type severity :read-only t)
  (visibility 'user :type 'visibility :read-only t)
  (message "" :type string :read-only t)
  (data nil :read-only t)
  (exception nil :read-only t))

(defstruct (appdata (:copier nil))
  (instance-id (uuid:make-v4-uuid) :read-only t :type uuid:uuid)
  (app-id nil :read-only t :type symbol)
  (version 0 :read-only t :type fixnum)
  (version-symbol (gensym) :read-only t :type symbol)
  (field-data (f:empty-map) :read-only t :type f:map)
  (audit (f:empty-seq) :read-only t :type f:seq)
  (validation-results (f:empty-seq) :read-only t :type f:seq))

(defstruct (text (:copier nil))
  (id nil :type symbol :read-only t)
  (lang nil :type symbol :read-only t)
  (value nil :read-only t))

(defstruct ui
  (id)
  (input nil)
  (label)
  (value)
  (required)
  (read-only)
  (data '()))

(defstruct (field (:copier nil))
  (id nil :read-only t :type symbol)
  (voldemort nil :read-only t :type boolean)
  (type 'string :read-only t :type symbol)
  (data-id nil :read-only t :type symbol)
  (value nil :read-only t)
  (read-only nil :read-only t)
  (if t :read-only t)
  (ui nil :read-only t)
  (visible t :read-only t)
  (texts '() :read-only t)
  (caption nil :read-only t)
  (options nil :read-only t)
  (visibility 'user :read-only t)
  (required nil :read-only t)
  (validations '() :read-only t)
  (types '() :read-only t)
  (children '() :read-only t))

(defstruct (app (:copier nil))
  (org nil :read-only t :type symbol)
  (id nil :read-only t :type symbol)
  (title "" :read-only t :type string)
  (party-types-allowed '() :read-only t)
  (texts '() :read-only t)
  (types '() :read-only t)
  (fields '() :read-only t)
  (field-lookup '() :read-only t)
  (field-parent-lookup '() :read-only t))

(defparameter *top-level-types* '())
(defparameter *apps* '())
(defvar *appdata-lock* (bt:make-lock "appdata"))
(defvar *appdata* (f:empty-map)
  "APPDATA by instance id.")

(defun list-types (&optional (types *top-level-types*))
  (mapcar #'field-id types))

(defun list-apps (&optional (apps *apps*))
  (mapcar #'app-id apps))

(defmacro define-computation (expr)
  (with-gensyms (e)
    `(let ((,e ',expr))
       (if (or (consp ,e)
               (and (symbolp ,e) (not (member ,e '(t nil)))))
           (make-computation :depends-on '()
                             :original ,e
                             :reduced ,e)
           ,e))))

(defmacro define-text (id lang value)
  `(make-text :id ,id
              :lang ,lang
              :value (define-computation ,value)))


(defun field-spec->define-field (spec)
  "Converts a field specification to a keyword property list suitable for DEFINE-FIELD.
The specification is roughly (TYPE ID? KEYWORD-ARGUMENT* CHILD*). The function
is recursively called for each child. If TYPE is a MACRO-FUNCTION, the macro is
expanded, and the result is further processed. If TYPE is a function, the
function is evaluated, and the result is further processed. If TYPE is
DEFINE-FIELD or MAKE-FIELD, we assume everything is processed correctly and
returns the input unmodified.

Examples:
(define-field what ever) -> (define-field what ever)
(make-field what ever) -> (make-field what ever)

(some-macro what ever) -> (field-spec->define-field (macroexpand (some-macro what ever)))
(some-function what ever) -> (field-spec->define-field (eval (some-function what ever)))

(:type what :ever here) -> (define-field :type what :ever here)

(type) -> (define-field :type type)
(type id) -> (define-field :type type :id id)
(type id :a 1) -> (define-field :type type :id id :a 1)
(type id :a 1 (c1)) -> (define-field :type type :id id :a 1 :children ((define-field :type c1)))
(type id (c1)) -> (define-field :type type :id id :children ((define-field :type c1)))
(type :a 1) -> (define-field :type type :a 1)
(type :a 1 (c1)) -> (define-field :type type :a 1 :children ((define-field :type c1)))
(type (c1)) -> (define-field :type type :children ((define-field :type c1)))
(type (c1) (c2)) -> (define-field :type type :children ((define-field :type c1) (define-field :type c2)))"
  (cond
    ((member (car spec) '(make-field define-field))
     spec)
    ((and (kwplist? spec) (eq :type (car spec)))
     (cons 'define-field spec))
    ((macro-function (car spec))
     (field-spec->define-field (macroexpand spec)))
    ((and (boundp (car spec)) (symbol-function (car spec)))
     (field-spec->define-field (apply (car spec) (cdr spec))))
    (t (let* ((type (car spec))
              (args (cdr spec))
              (groups (group-kwplists args)))
         (case (length groups)
           ;; Just type
           (0 (list 'define-field :type type))
           ;; symbols (id, children or both) or kwplist
           (1 (let ((arg (car args))
                    (group (car groups)))
                (cond
                  ;; kwplist
                  ((kwplist? group) (append (list 'define-field :type type) args))
                  ;; id
                  ((symbolp arg)
                   (if-let ((children (cdr group)))
                     (list 'define-field :type type :id arg :children (mapcar #'field-spec->define-field children))
                     (list 'define-field :type type :id arg)))
                  ;; children
                  ((consp group) (list 'define-field :type type :children (mapcar #'field-spec->define-field group)))
                  ;; error
                  (t (error "Expected id, keyword property list or children")))))
           ;; both symbols and kwplist
           (2 (let* ((g1 (first groups))
                     (g2 (second groups))
                     (kwplist (or (kwplist? g1) (kwplist? g2)))
                     (symbols (if (eq kwplist g1) g2 g1)))
                (cond
                  ;; kwplist children
                  ((eq kwplist g1)
                   (append (list 'define-field :type type :children (mapcar #'field-spec->define-field symbols))
                           kwplist))
                  ;; id? kwplist
                  (t (append (list 'define-field :type type :id (car symbols))
                             kwplist)))))
           ;; id kwlist children
           (3 (let ((id (first groups))
                    (kwplist (second groups))
                    (children (third groups)))
                (append (list 'define-field :type type :id (car id) :children (mapcar #'field-spec->define-field children))
                        kwplist)))
           ;; error
           (t (error "n element not implemented")))))))

(defun expand-field (spec)
  "Expands a field specification down to MAKE-FIELD."
  (cond
    ((eq 'make-field (car spec)) spec)
    ((eq 'define-field (car spec)) (expand-field (macroexpand spec)))
    (t (expand-field (field-spec->define-field spec)))))

(defun eval-field (spec)
  "Expands a field specification down to MAKE-FIELD and evaluates the result."
  (eval (expand-field spec)))

(defmacro define-field (&key (type nil) (id nil) (data-id nil) (caption nil) (value nil) (read-only nil) (if t) (visible t) (required nil) (visibility 'user) (validations '()) (options '()) (texts '()) (ui nil) (types '()) (children '()))
  ;; TODO: Check that type exists
  `(make-field :id (or ',id (gensym (format nil "~(~A~)#" ',type)))
               :voldemort (eq nil ',id)
               :type ',type
               :data-id ',data-id
               :caption (define-computation ,caption)
               :value (define-computation ,value)
               :if (define-computation ,if)
               :read-only (define-computation ,read-only)
               :visible (define-computation ,visible)
               :required (define-computation ,required)
               :visibility ',visibility
               :validations (define-computation ,validations)
               :texts ',texts
               :ui ,ui
               :options (list ,@(mapcar #'expand-field options))
               :types (list ,@(mapcar #'expand-field types))
               :children (list ,@(mapcar #'expand-field children))
               ))

;;; App data types
(defmacro define-type (type id &rest args)
  "Defines a top-level field type."
  ;; TODO: Check for type in local types and top-level types
  (with-gensyms (fty fid field)
    `(let* ((,fty ',type)
            (,fid ',id)
            (,field (eval-field (append (list :type ,fty :id ,fid) '(,@args)))))
       (setf *top-level-types* (delete-if (lambda (old) (eq (field-id old) ,fid)) *top-level-types*))
       (push ,field *top-level-types*)
       (format nil "Created type (~A ~A)" ,fty ,fid))))

(define-type nil +builtin+
  :data-id nil)

(define-type +builtin+ +prose+
  :data-id nil)

(defmacro define-prose (id &rest args)
  "Defines prose where the first argument is :CAPTION rather than :ID.  The
underlying field will be named %ID, while the macro taking caption first is ID."
  (let* ((macro-id id)
         (type-id (symbolicate "%" macro-id)))
    `(progn
       (define-type +prose+ ,type-id
         :data-id nil
         ,@args)
       (defmacro ,macro-id (caption &rest args)
         (append (list ',type-id :caption caption) args)))))

(define-prose paragraph
  :ui (lambda (app field) (spinneret:with-html (:p (field-caption field)))))

(define-prose header
  :ui (lambda (app field) (spinneret:with-html (:h* (field-caption field)))))

(define-type +builtin+ +grouping+)

(define-type +grouping+ group
  :data-id nil)

(define-type +grouping+ page
  :data-id nil
  :ui (lambda (app field) (spinneret:with-html
                   (:section
                    (when-let ((caption (field-caption field)))
                      (:h* caption))))))


(define-type +builtin+ +field+
  :data-id nil)

(defun field-label (app field)
  (spinneret:with-html
    (when-let (caption (field-caption* app field))
      (:label :for (field-id field) caption))))

(defun field-input (app field input-type)
  (spinneret:with-html
    ;; TODO: Compute readonly and visible based on field and data
    (:input :name (field-id field)
            :id (field-id field)
            :type input-type
            ;; :readonly (or (field-read-only field) (null (field-data-id field)))
            ;; FIXME: strings get double quoted here. Should use some spinneret :raw somehow
            :onchange (ps:ps
                        (let* ((js-field (ps:lisp (symbol-name (field-id field))))
                               (element (ps:chain document (get-element-by-id js-field)))
                               (new-value (if (equal "CHECKBOX" (ps:lisp (symbol-name input-type))) (ps:@ element checked) (ps:@ element value))))
                          (set-value js-field new-value))))))

(define-type +field+ string
  :data-id nil
  :ui (lambda (app field) (spinneret:with-html
                            (field-label app field)
                            (field-input app field :string))))

(define-type +field+ integer
  :data-id nil
  :ui (lambda (app field) (spinneret:with-html
                            (field-label app field)
                            (field-input app field :number))))

(define-type +field+ bool
  :data-id nil
  :ui (lambda (app field) (spinneret:with-html
                            (field-input app field :checkbox)
                            (field-label app field))))

(defmacro iff (expr &rest args)
  `(group ,(gensym "iff#") :if ,expr ,@args))

(defmacro if-else (expr true-children false-children &rest args)
;; FIXME: We need to parse the arguments using field-spec (?) to allow args before the child/children
  `(group ,(gensym "if-else#")
          (iff ,expr ,@args ,@true-children)
          (iff (not ,expr) ,@args ,@false-children)))

(define-type bool %yes/no
  :texts ((yes :nb "Ja"
               :en "No")
          (no :nb "Nei"
              :en "No"))
  :options ((bool yes :value t) (bool no :value nil)))

(defmacro yes/no (id caption &rest args)
  `(%yes/no ,id :caption ,caption ,@args))

(defmacro yes/no-group (id caption &rest args)
  (let* ((spec (cdr (field-spec->define-field (append (list '_ id :caption caption) args))))
         (id (getf spec :id))
         (caption (getf spec :caption))
         (children (getf spec :children)))
    ;; We want to supply args to the yes/no, but we don't want to supply
    ;; children, id etc
    (remf spec :id)
    (remf spec :type)
    (remf spec :caption)
    (remf spec :children)
    `(group ,(gensym "yes/no-group#")
            ;; TODO: må legge til args minus children
            (yes/no ,id ,caption ,@spec)
            (iff ,id
                 ,@children))))

(defmacro define-app (id &rest args)
  ;; We reuse the same specification as for DEFINE-FIELD to easily get the field calculation
  (with-gensyms (spec types fields all-fields field-lookup field-parent-lookup app)
    `(let* ((,spec (cdr (field-spec->define-field '(,(gensym "define-app#") ,id ,@args))))
            (,types (mapcar #'eval-field (getf ,spec :types)))
            (,fields (mapcar #'eval-field (getf ,spec :children)))
            (,all-fields (append *top-level-types* ,types ,fields))
            (,field-lookup (make-field-lookup ,all-fields))
            (,field-parent-lookup (make-field-parent-lookup ,all-fields ,field-lookup))
            (,app (make-app :org (getf ,spec :org)
                            :id (getf ,spec :id)
                            :title (or (getf ,spec :title) "")
                            :types ,types
                            :fields ,fields
                            :field-lookup ,field-lookup
                            :field-parent-lookup ,field-parent-lookup)))
       (setf *apps* (delete-if (lambda (old) (equal (app-id old) (app-id ,app))) *apps*))
       (push ,app *apps*)
       ,app)))

(defun make-field-lookup (fields &optional (table (make-hash-table)))
  "Construct a lookup from FIELD-ID to the FIELD structure."
  (dolist (field fields)
    (setf (gethash (field-id field) table) field)
    (make-field-lookup (field-children field) table))
  table)

(defun make-field-parent-lookup (fields field-lookup &optional (table (make-hash-table)))
  "Construct a lookup from the FIELD-ID to the FIELD structure of FIELD-PARENT."
  (dolist (field fields)
    (setf (gethash (field-id field) table)
          (gethash (field-type field) field-lookup))
    (make-field-parent-lookup (field-children field) field-lookup table))
  table)

(defun find-field-slot (app field getter)
  "Traverse up the hierarcy, returning the first non-nil value."
  (if (null field)
      nil
      (or (funcall getter field)
          (find-field-slot app (gethash (field-id field) (app-field-parent-lookup app)) getter))))

(defun field-caption* (app field)
  "Finds the closest non-nil :CAPTION."
  (find-field-slot app field #'field-caption))

(defun field-ui* (app field)
  "Find the closest non-nil :UI."
  (find-field-slot app field #'field-ui))

(defun field->html (app field)
  (spinneret:with-html
    (:div
     (:comment (format nil "~A ~A" (field-type field) (field-id field)))
     (when-let ((ui (field-ui* app field)))
       (funcall ui app field))
     (dolist (child (field-children field))
       (field->html app child)))))

(defun app->html (app instance &optional (file nil))
  (flet ((run ()
           (spinneret:with-html
             (:doctype)
             (:html
              (:comment (app-id app))
              (:head (:title (app-title app))
                     (:script
                      (:raw
                       (ps:ps
                         (setf (ps:@ document app) (ps:create)
                               (ps:@ document app app-id) (ps:lisp (symbol-name (app-id app)))
                               (ps:@ document app instance-id) (ps:lisp (format nil "~A" (appdata-instance-id instance)))
                               (ps:@ document app data-version) (ps:lisp (appdata-version instance))
                               (ps:@ document app data-version-symbol) (ps:lisp (symbol-name (appdata-version-symbol instance))))

                         (defun set-value (field-id new-value)
                           (let ((form-data (ps:new -form-data)))
                             (ps:chain form-data (append "instance-id" (ps:@ document app instance-id)))
                             (ps:chain form-data (append "data-version" (ps:@ document app data-version)))
                             (ps:chain form-data (append "data-version-symbol" (ps:@ document app data-version-symbol)))
                             (ps:chain form-data (append "field-id" field-id))
                             (ps:chain form-data (append "new-value" new-value))
                             (ps:chain (fetch "http://localhost:5055/set-value"
                                              (ps:create :method "POST"
                                                         :mode "cors"
                                                         :cache "no-cache"
                                                         :credentials "same-origin"
                                                         :redirect "error"
                                                         :referrer-policy "no-referrer"
                                                         :body form-data))
                                       (then (lambda (result)
                                               ;; TODO: Handle result
                                               (setf (ps:@ document app previous-result) result)
                                               (ps:chain result (json)
                                                         (then (lambda (data)
                                                                 (setf (ps:@ document app data-version) (ps:@ data data-version)
                                                                       (ps:@ document app data-version-symbol) (ps:@ data data-version-symbol))
                                                                 (ps:@ document app)))))))))))))
              (:body
               (:comment "Begin fields")
               (dolist (field (app-fields app))
                 (field->html app field))
               (:comment "End fields"))
              (:footer)))))
    (if file
        (with-open-file (spinneret:*html* file :direction :output :if-exists :supersede :if-does-not-exist :create)
          (run))
        (run))))

(defun list-instances (&optional (instances *appdata*))
  (let ((result '()))
    (f:do-map (_ instance instances result)
      (push (list :instance-id (appdata-instance-id instance)
                  :app-id (appdata-app-id instance)
                  :version (appdata-version instance)
                  :version-symbol (appdata-version-symbol instance)) result))))
