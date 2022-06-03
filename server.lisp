(in-package :app)

(defvar *server* nil)

(hunchentoot:define-easy-handler (root-url :uri "/") ()
  (hunchentoot:redirect "/create-instance"))

(hunchentoot:define-easy-handler (launch-instance :uri "/launch-instance") (instance-id)
  (in-package :app)
  (let* ((instance-id (uuid:make-uuid-from-string instance-id))
         (instance (f:@ *appdata* instance-id))
         (app (find-if (lambda (app) (equal (appdata-app-id instance) (app-id app))) *apps*)))
    (spinneret:with-html-string
      (app->html app instance))))

(hunchentoot:define-easy-handler (create-instance :uri "/create-instance") (app-id)
  (in-package :app)
  (if (null app-id)
      (spinneret:with-html-string
        (:ul (dolist (app (list-apps))
               (:li (:a :href (format nil  "/create-instance?app-id=~A" app) app))))
        (:ul (dolist (instance (list-instances))
               (:li (:a :href (format nil "/launch-instance?instance-id=~A" (getf instance :instance-id)) (format nil "~A" (getf instance :instance-id)))))))
      (let* ((app-id (read-from-string app-id))
             (appdata (make-appdata :app-id app-id))
             (instance-id (appdata-instance-id appdata)))
        (bt:with-lock-held (*appdata-lock*)
          (setq *appdata* (f:with *appdata* instance-id appdata)))
        (hunchentoot:redirect (format nil "/launch-instance?instance-id=~A" instance-id)))))

(hunchentoot:define-easy-handler (set-value :uri "/set-value") (instance-id data-version data-version-symbol field-id new-value)
  (in-package :app)
  (let* ((instance-id (uuid:make-uuid-from-string instance-id))
         (data-version (parse-integer data-version))
         (data-version-symbol (read-from-string data-version-symbol))
         (field-id (read-from-string field-id))
         (old-appdata (f:@ *appdata* instance-id))
         (old-version (appdata-version old-appdata))
         (new-version (+ 1 old-version))
         (old-version-symbol (appdata-version-symbol old-appdata))
         (new-version-symbol (gensym "version"))
         (old-field-data (appdata-field-data old-appdata))
         (old-value (f:@ old-field-data field-id))
         (audit (list :field-id field-id
                      :old-value old-value
                      :old-version old-version
                      :old-version-symbol old-version-symbol
                      :new-value new-value
                      :new-version new-version
                      :new-version-symbol new-version-symbol))
         (new-instance (with old-appdata
                             :version new-version
                             :version-symbol new-version-symbol
                             :field-data (f:with old-field-data field-id new-value)
                             :audit (f:with-first (appdata-audit old-appdata) audit)))
         (new-appdata (f:with *appdata*
                              instance-id
                              new-instance)))
    ;; FIXME: Seems like checkboxes is reported as "on" even when turned off
    ;; TODO: Get app, get data
    ;; data-version+data-version-symbol different? Stale data, throw
    ;; Find all dependencies.
    ;; Set data, recalculate all dependencies, bumb data-version, set data-vesion-symbol
    ;; Commit data -- what about race conditions? Lock and check versions before commit.
    ;; Report changes (diff) back with data-version+data-version-symbol
    ;; (when (not (equal old-version data-version))
    ;;   (error "version differ"))
    ;; (when (not (equal old-version-symbol data-version-symbol))
    ;;   (error "version-symbol differ"))
    ;; Same version and old = new? Do nothing? Why was the endpoint triggered? Signal error?
    ;; (when (equal old-value new-value)
    ;;   (error "same data"))
    ;;   TODO: Lock only the instance. Should the appdata instance contain a lock? Is this possible?
    (bt:with-lock-held (*appdata-lock*)
      ;; TODO: See if someone has updated data before we took the lock. In that case, restart until it's not the case
      (setq *appdata* new-appdata))
    (let ((shasht:*write-alist-as-object* t))
      (shasht:write-json (list (cons "dataVersion" (appdata-version new-instance))
                               (cons "dataVersionSymbol" (appdata-version-symbol new-instance)))
                         nil))))

(defun start-server ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil))
  (unless *server*
    (setf *server* (make-instance 'hunchentoot:easy-acceptor :port 5055)))
  (hunchentoot:start *server*))
