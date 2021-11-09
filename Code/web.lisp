(cl:in-package :direct-manipulation-hack)

(setf (cl-who:html-mode) :html5)
(defmacro page ((&key title) &body body)
  `(cl-who:with-html-output-to-string (*standard-output*)
     (:html
      (:title (cl-who:str ,title))
      (:body
       ,@body))))

(defun url-for-plot (plot)
  (format nil "/plot?x=~d&z=~d"
          (x plot) (z plot)))

(hunchentoot:define-easy-handler (home-page :uri "/") ()
  (page (:title "Plots")
    (:ul
     (with-plot-lock ()
       (maphash (lambda (position plot)
                  (cl-who:htm
                    (:li
                     (:a :href (url-for-plot plot)
                         (cl-who:esc (name plot)))
                     (format t " at (~d, ~d)"
                             (car position)
                             (cdr position)))))
                *plots*)))))

(defmacro with-parsed ((&rest variables) &body body)
  `(let ,(loop for var in variables
               collect `(,var (parse-integer ,var)))
     ,@body))

(hunchentoot:define-easy-handler (plot-page :uri "/plot") (x z program)
  (with-plot-lock ()
    (with-parsed (x z)
      (let ((plot (gethash (cons x z) *plots*)))
        (unless (null program)
          (setf (program plot) program))
        (page (:title (name plot))
          (:form :action "/!update" :method :post
           (:input :name "name" :style "font-size: 200%; font-weight: bold"
                   :value (cl-who:str (name plot)))
           (:p
            "Cloned from "
            (:a :href (url-for-plot (parent plot))
                (cl-who:str (name (parent plot)))))
           (unless (null (children plot))
             (cl-who:htm
               (:h2 "Children")
               (:ul
                (loop for child in (children plot)
                      do (cl-who:htm (:li (:a :href (url-for-plot child)
                                              (cl-who:str (name child)))))))))
           (:br)
           (:textarea :name "program" :rows 20 :cols 80
                      :spellcheck "false"
            (cl-who:str (program plot)))
           (:input :type "hidden" :name "x" :value x)
           (:input :type "hidden" :name "z" :value z)
           (:br)
           (:input :type "submit" :value "Update")
           (:input :type "submit" :formaction "/!reset" :value "Reset program"))
          (:form :action "/clone" 
           (:input :type "hidden" :name "x" :value x)
           (:input :type "hidden" :name "z" :value z)
           (:input :type "submit" :value "Clone")))))))

(hunchentoot:define-easy-handler (update-plot-page :uri "/!update") (x z name program)
  (with-plot-lock ()
    (with-parsed (x z)
      (let ((plot (gethash (cons x z) *plots*)))
        (setf (name plot) name
              (program plot) program)
        (hunchentoot:redirect (url-for-plot plot))
        ""))))

(hunchentoot:define-easy-handler (clone-page :uri "/clone") (x z)
  (with-plot-lock ()
    (page (:title "Pick a plot")
      (:h1 "Pick a plot")
      (:style
       "td { border: 1px black solid; width: 1em; text-align: center;}
td > a {color: black !important; text-decoration: none;}")
      (:table
       (loop for child-x below +plot-area-size+
             do (cl-who:htm
                  (:tr
                   (loop for child-z below +plot-area-size+
                         for used = (gethash (cons child-x child-z) *plots*)
                         do (cl-who:htm
                              (:td :style (if used
                                              "background: black"
                                              "background: white")
                                   (unless used
                                     (cl-who:htm
                                       (:a :href (format nil "/!clone?parent-x=~d&parent-z=~d&x=~d&z=~d"
                                                         x z
                                                         child-x child-z)
                                           "X")))))))))))))

(hunchentoot:define-easy-handler (actually-clone-page :uri "/!clone")
    (parent-x parent-z x z)
  (with-plot-lock ()
    (with-parsed (parent-x parent-z x z)
      (let ((parent (or (gethash (cons parent-x parent-z) *plots*)
                        (error "No parent"))))
        (hunchentoot:redirect (url-for-plot
                               (clone-plot parent x z)))
        ""))))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))

(hunchentoot:define-easy-handler (reset-plot-page :uri "/!reset") (x z)
  (with-plot-lock ()
    (with-parsed (x z)
      (let ((plot (gethash (cons x z) *plots*)))
        ;; Reset package
        (delete-package (plot-package plot))
        (setf (slot-value plot '%package)
              (fresh-package x z))
        ;; Reset program
        (setf (program plot) (program plot))
        (hunchentoot:redirect (url-for-plot plot))
        ""))))
