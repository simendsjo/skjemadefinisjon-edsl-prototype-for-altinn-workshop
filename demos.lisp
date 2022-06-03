(in-package :app)

(defun app-demo-empty ()
  (define-app app-demo-empty))

(defun app-demo-basic-prose ()
  (define-app app-demo-basic-prose
      (paragraph "paragraph")
    (header "header")))

(defun app-demo-basic-fields ()
  (define-app app-demo-basic-fields
      (bool bool1)
    (integer integer1)
    (string string1)))

(defun app-demo-basic-groups ()
  (define-app app-demo-basic-groups
      (page
            (paragraph "in page 1")
            (group (paragraph "in group 1"))
            (group (paragraph "in group 2")))))

(defun app-demo-basic-conditionals ()
  (define-app app-demo-basic-conditionals
      (group :if t (paragraph "group :if"))
    (iff t (paragraph "iff"))
    (if-else t
             ((header "if-else, t branch"))
             ((header "if-else, nil branch")))))

(defun app-demo-yes/no ()
  (define-app app-demo-yes/no
      (bool foo?)
    (yes/no foo? "caption"
            (string child))))

(defun app-demo-yes/no-group ()
  (define-app app-demo-yes/no-group
    (yes/no-group the-bool "caption"
                  (string child))))


(defun app-demo-complex ()
  (define-app krt-1127a-1
    :types ((integer sum
                     :data-id nil
                     :caption "a+b+c="
                     :value (+ (or a 0) (or b 0) (or c 0))
                     :texts ((sum-zero :nb "Summen er null"
                                       :en "Sum is zero"))
                     :validations ((and (zerop it) (warn sum-zero))))
            (integer positive-integer
                     :texts ((non-positive :nb "Verdien må være positiv"
                                           :en "Value has to be positive"))
                     :validations ((and (negative-integer-p it) (error non-positive)))))
    (page (string some-string :caption "en tekst her takk")
          (integer some-integer :caption "og en int")
          (bool some-bool :caption "checkbox her"))
    (page (yes/no-group foo "Foo?"
                        (integer some-i :caption "Et tall over 10 takk")
                        (iff (> some-i 10)
                             (positive-integer a :caption "a: pos int")
                             (positive-integer b :caption "b: pos int")
                             (positive-integer c :caption "c: pos int")
                             (sum)))
          (yes/no-group bar "Bar?"
                        (a)
                        (b)
                        (c)
                        (sum)))))
