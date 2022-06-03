(cl:in-package :app)


(defmethod fset:compare ((u1 uuid:uuid) (u2 uuid:uuid))
  (if (uuid:uuid= u1 u2)
      :equal
      ;; TODO: Support ordering to get determinism
      :unequal))

(defun group-kwplists (args)
  "Groups by keyword property lists.
Examples
() -> ()
(a) -> ((a))
(:a 1) -> ((:a 1))
(a b :q 1 :r 2 x y) -> ((a b) (:q 1 :r 2) (x y))"
  (labels ((run (rest group prev-keyword? result)
             (let ((cur-keyword? (keywordp (car rest))))
               (cond
                 ((and group (or (null rest) (not (eq cur-keyword? prev-keyword?))))
                  (run rest
                       '()
                       cur-keyword?
                       (cons (reverse group) result)))
                 ((null rest)
                  (reverse result))
                 (cur-keyword?
                  (let ((p (list (cadr rest) (car rest)))
                        (rest (cddr rest)))
                    (run rest
                         (append p group)
                         cur-keyword?
                         result)))
                 (t
                  (let ((a (car rest))
                        (rest (cdr rest)))
                    (run rest
                         (cons a group)
                         cur-keyword?
                         result)))))))
    (run args
         '()
         (keywordp (car args))
         '())))

(defun kwplist? (list)
  "Returns LIST if LIST is a property list and all the keys are KEYWORDS, T if LIST is NIL, NIL otherwise."
  (or (null list)
      (and (keywordp (car list))
           (not (null (cdr list)))
           (kwplist? (cddr list))
           list)))

(defun symbol->keyword (symbol)
  (make-keyword (symbol-name symbol)))

(defun slots->plist (class)
  "Returns all class/struct slots as a property list."
  (mapcan (lambda (slot)
            (let ((name (sb-mop:slot-definition-name slot)))
              (cons (symbol->keyword name)
                    (list (slot-value class name)))))
          (sb-mop:class-slots (class-of class))))

(defun struct-ctor (struct)
  "Returns the default constructor name for struct objects, e.g. the struct S
returns MAKE-S. BUG: Only works for default ctor."
  (read-from-string (format nil "make-~(~A~)" (sb-mop:class-name (class-of struct)))))

(defun map-plist (f plist)
  "Maps F over (LIST KEY VALUE) values from PLIST."
  (labels ((run (result rest)
             (trivia:match rest
               (nil (reverse result))
               ((list* k v rest)
                (run (cons (funcall f k v) result)
                     rest)))))
    (run '() plist)))

(defun override-plist (old-plist new-plist)
  "Returns OLD-PLIST with values from matching keys in NEW-PLIST.
Example:
(override-plist '(:old old :both 1) '(:both new :new 1)) -> '(:old a :both new)"
  (mapcan 'identity
          (map-plist (lambda (key old-val)
                       (if-let ((new-val (member key new-plist)))
                         `(,key ,(cadr new-val))
                         `(,key ,old-val)))
                     old-plist)))

(defun with (struct &rest new)
  "Constructs a new STRUCT of the same type, but replace property values with those which exists in NEW.
E.g. (with s :id 'new) -> (make-s :id 'new ...)"
  (apply (struct-ctor struct)
         (override-plist (slots->plist struct) new)))
