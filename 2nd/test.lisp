(ql:quickload '(:optima :lisp-unit))
(use-package '(:optima :lisp-unit))

(defun string->list (str)
  (mapcar 'string (coerce str 'list)))

(defun list->string (lst)
  (coerce (mapcar 'character lst) 'string))

(defvar *stars*
  (list '(:star "A" :color "W")
        '(:star "B" :color "W")
        '(:star "C" :color "W")
        '(:star "D" :color "W")
        '(:star "E" :color "W")
        '(:star "F" :color "R")
        '(:star "G" :color "R")
        '(:star "H" :color "R")
        '(:star "I" :color "R")
        '(:star "J" :color "R")))

(defparameter *routes*
  (list '("A" "J" "I" "C")
        '("A" "F" "G" "D")
        '("B" "J" "F" "E")
        '("B" "I" "H" "D")
        '("C" "H" "G" "E")))

(defun value-of (field)
  `(getf star ,field))

(defun values-of (stars field)
  (mapcar #'(lambda (star) (getf star field)) stars))

(defun select (selector-fn)
  (remove-if-not selector-fn *stars*))

(defun select-first (selector-fn)
  (first (select selector-fn)))

(defun where (&key name names)
  (lambda (star)
    (or (if name (equal (getf star :star) name))
        (if names (member (getf star :star) names :test #'equal)))))

(defun update (selector-fn &key color)
  (setf *stars*
        (mapcar
          #'(lambda (star)
              (when (funcall selector-fn star)
                (if color (setf (getf star :color) color)))
              star)
          *stars*)))

(defun route-of (route pos)
  (match pos
         (0 route)
         (1 (subseq route pos))
         (otherwise (route-of (reverse route) (- 3 pos)))))

(defun find-route (star)
  (remove nil
    (mapcar
      #'(lambda (route)
          (let ((pos (position star route :test #'equal)))
            (if pos (route-of route pos))))
      *routes*)))

(defun transit (routes)
  (dolist (route routes)
    (let* ((colors (mapcar #'(lambda (name) (getf (select-first (where :name name)) :color)) route)))
      (match colors
             ((or (list "W" "R" "W")
                  (list "W" "R" _ "W"))
              (update (where :names route) :color "W"))
             ((or (list "R" "W" "R")
                  (list "R" "W" _ "R"))
              (update (where :names route) :color "R"))))))

(defun inverse-of (color)
  (if (equal color "R") "W" "R"))

(defun flip-color (star-name &aux (star (select-first (where :name star-name))))
  (let* ((color (getf star :color)))
    (update (where :name star-name) :color (inverse-of color))
    (transit (find-route star-name))))

(defun colors ()
  (list->string
    (mapcar #'(lambda (star) (getf star :color))
            *stars*)))

(defun flip (routes)
  (flip-color (car routes))

  (if (cdr routes)
    (flip (cdr routes))
    (colors)))

(defun reset-stars ()
  (update (where :names '("A" "B" "C" "D" "E")) :color "W")
  (update (where :names '("F" "G" "H" "I" "J")) :color "R"))

(defun solve (inputs)
  (reset-stars)
  (flip (string->list inputs)))

(define-test flip-stars
  (assert-equal (solve "A") "RWWWWRRRRR")
  (assert-equal (solve "F") "WWWWWWWRRW")
  (assert-equal (solve "J") "WWWWWWRRWW")
  (assert-equal (solve "AA") "WWWWWWWRWW")
  (assert-equal (solve "IC") "WWRWWRRRWW")
  (assert-equal (solve "FC") "WWRWWWWRRW")
  (assert-equal (solve "AE") "RWWWRRRRRR")
  (assert-equal (solve "GJ") "WWWWWWWWWW")
  (assert-equal (solve "CCB") "WRWWWRWWWR")
  (assert-equal (solve "BEF") "WRWWRWWRRR")
  (assert-equal (solve "JGD") "WWWRWWWWWW")
  (assert-equal (solve "IHCC") "WWWWWRWWWW")
  (assert-equal (solve "AIDD") "RWWWWRRWWR")
  (assert-equal (solve "IJFA") "RWWWWWWWWW")
  (assert-equal (solve "ABCDE") "RRRRRRRRRR")
  (assert-equal (solve "ICEBA") "RRRWRRRRRR")
  (assert-equal (solve "DAHHD") "RWWWWRWWWR")
  (assert-equal (solve "GJIJC") "WWRWWWWWRR")
  (assert-equal (solve "FGHIJ") "WWWWWWWWRR")
  (assert-equal (solve "HJICGA") "RWRWWRRRRR")
  (assert-equal (solve "IBCIGC") "WRWWWWWWWW")
  (assert-equal (solve "BIJJJB") "WWWWWWRWWW")
  (assert-equal (solve "DCBCHGD") "WRWWWWWRRW")
  (assert-equal (solve "JEABDHD") "RRWWRRRWRR")
  (assert-equal (solve "JHFADHE") "RWWRRRRRWW")
  (assert-equal (solve "HDGGDBIB") "WWWWWWWWWW")
  (assert-equal (solve "IIDIHCCG") "WWWRWRRWWW")
  (assert-equal (solve "BBFBICIE") "WRRWRRRWWW")
  (assert-equal (solve "HJHCFBJGG") "WRRWWWWRRW")
  (assert-equal (solve "AJJIEAAII") "RWWWRWWWWR")
  (assert-equal (solve "AIDHJFGAE") "WWWRRWWWWW")
  (assert-equal (solve "FGBGHCBHJJ") "WWRWWWWRRW")
  (assert-equal (solve "EFIGIGGHHJ") "WWWWRRRWWR")
  (assert-equal (solve "HGAFDIFFFF") "RWWRWRRRRW")
  (assert-equal (solve "AABBCCDDEE") "WWWWWWWWWW")
  (assert-equal (solve "ABCDEFGHIJ") "RRRRRWWWWW")
  (assert-equal (solve "FGHIJABCDE") "RRRRRRRRRR"))

(run-tests :all)
