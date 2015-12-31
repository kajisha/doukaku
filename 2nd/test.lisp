(ql:quickload :optima)
(use-package :opTima)

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

(defun test (inputs expected)
  (assert (equal (flip (string->list inputs)) expected))
  (reset-stars))

(test "A" "RWWWWRRRRR")
(test "F" "WWWWWWWRRW")
(test "J" "WWWWWWRRWW")
(test "AA" "WWWWWWWRWW")
(test "IC" "WWRWWRRRWW")
(test "FC" "WWRWWWWRRW")
(test "AE" "RWWWRRRRRR")
(test "GJ" "WWWWWWWWWW")
(test "CCB" "WRWWWRWWWR")
(test "BEF" "WRWWRWWRRR")
(test "JGD" "WWWRWWWWWW")
(test "IHCC" "WWWWWRWWWW")
(test "AIDD" "RWWWWRRWWR")
(test "IJFA" "RWWWWWWWWW")
(test "ABCDE" "RRRRRRRRRR")
(test "ICEBA" "RRRWRRRRRR")
(test "DAHHD" "RWWWWRWWWR")
(test "GJIJC" "WWRWWWWWRR")
(test "FGHIJ" "WWWWWWWWRR")
(test "HJICGA" "RWRWWRRRRR")
(test "IBCIGC" "WRWWWWWWWW")
(test "BIJJJB" "WWWWWWRWWW")
(test "DCBCHGD" "WRWWWWWRRW")
(test "JEABDHD" "RRWWRRRWRR")
(test "JHFADHE" "RWWRRRRRWW")
(test "HDGGDBIB" "WWWWWWWWWW")
(test "IIDIHCCG" "WWWRWRRWWW")
(test "BBFBICIE" "WRRWRRRWWW")
(test "HJHCFBJGG" "WRRWWWWRRW")
(test "AJJIEAAII" "RWWWRWWWWR")
(test "AIDHJFGAE" "WWWRRWWWWW")
(test "FGBGHCBHJJ" "WWRWWWWRRW")
(test "EFIGIGGHHJ" "WWWWRRRWWR")
(test "HGAFDIFFFF" "RWWRWRRRRW")
(test "AABBCCDDEE" "WWWWWWWWWW")
(test "ABCDEFGHIJ" "RRRRRWWWWW")
(test "FGHIJABCDE" "RRRRRRRRRR")
