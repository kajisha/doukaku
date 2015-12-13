(defparameter *stars*
  (list '(:star "A" :w "I" :r "H")
        '(:star "B" :w "G" :r "D")
        '(:star "C" :w "A" :r "J")
        '(:star "D" :w "I" :r "F")
        '(:star "E" :w "C" :r "B")
        '(:star "F" :w "A" :r "H")
        '(:star "G" :w "E" :r "D")
        '(:star "H" :w "C" :r "J")
        '(:star "I" :w "G" :r "F")
        '(:star "J" :w "E" :r "B")))

(defun string->list (str)
  (mapcar 'string (coerce str 'list)))

(defun list->string (lst)
  (coerce (mapcar 'character lst) 'string))

(defun name-of (star)
  (getf star :star))

(defun find-star (star)
  (car
    (remove-if-not
      #'(lambda (s) (equal (name-of s) star))
      *stars*)))

(defun destination (&key from route)
  (if route
    (find-star (getf from (intern route :keyword)))))

(defun stars-cruise (&key star course)
  (if star
    (cons (name-of star)
          (stars-cruise
            :star (destination :from star :route (car course))
            :course (cdr course)))))

(defun run-cruise (course)
  (list->string
    (stars-cruise
      :star (find-star (car (string->list course)))
      :course (cdr (string->list course)))))

(defun test (course visited)
  (equal (run-cruise course) visited))

; unit test
(ql:quickload :lisp-unit)
(use-package 'lisp-unit)

(define-test test-stars-cruise
  (assert-true (test "AW" "AI"))
  (assert-true (test "GR" "GD"))
  (assert-true (test "GW" "GE"))
  (assert-true (test "IR" "IF"))
  (assert-true (test "HR" "HJ"))
  (assert-true (test "BWW" "BGE"))
  (assert-true (test "ARW" "AHC"))
  (assert-true (test "GRR" "GDF"))
  (assert-true (test "BWR" "BGD"))
  (assert-true (test "JWWW" "JECA"))
  (assert-true (test "DRRR" "DFHJ"))
  (assert-true (test "CWWR" "CAIF"))
  (assert-true (test "HWWW" "HCAI"))
  (assert-true (test "GWRWR" "GEBGD"))
  (assert-true (test "FRRRW" "FHJBG"))
  (assert-true (test "JRRWW" "JBDIG"))
  (assert-true (test "JWWRRW" "JECJBG"))
  (assert-true (test "GRRRWW" "GDFHCA"))
  (assert-true (test "BRWRWR" "BDIFAH"))
  (assert-true (test "IRWRRWR" "IFAHJEB"))
  (assert-true (test "IWWWRRW" "IGECJBG"))
  (assert-true (test "GWWRWWR" "GECJECJ"))
  (assert-true (test "HRRWRWRW" "HJBGDIFA"))
  (assert-true (test "FRWWWRRW" "FHCAIFHC"))
  (assert-true (test "HRWWWRWRW" "HJECAHCJE"))
  (assert-true (test "CWWWRRWWW" "CAIGDFAIG"))
  (assert-true (test "BRRRWRRRRW" "BDFHCJBDFA"))
  (assert-true (test "FRWRRWRRWW" "FHCJBGDFAI"))
  (assert-true (test "GWRRRRWRWRW" "GEBDFHCJEBG"))
  (assert-true (test "DRWWWWWWRRW" "DFAIGECAHJE"))
  (assert-true (test "ARRRRWRRRRWW" "AHJBDIFHJBGE"))
  (assert-true (test "AWWWWWWRRWRR" "AIGECAIFHCJB"))
  (assert-true (test "JWWWRRWRWRWWR" "JECAHJEBGDIGD"))
  (assert-true (test "CRWRWRRWWWRWW" "CJEBGDFAIGDIG"))
  (assert-true (test "DWRWRWRWRWWRWW" "DIFAHCJEBGEBGE"))
  (assert-true (test "GRWWWRRRRWRWRR" "GDIGEBDFHCJEBD"))
  (assert-true (test "ARWWWRWWRWWWWWW" "AHCAIFAIFAIGECA"))
  (assert-true (test "DWRWRRWRWWRWWRW" "DIFAHJEBGEBGEBG"))
  (assert-true (test "JRWRRRRRWRRRRRWR" "JBGDFHJBGDFHJBGD"))
  (assert-true (test "IRWWRRWWWWWRRWWR" "IFAIFHCAIGEBDIGD")))

(run-tests '(test-stars-cruise))
