(defparameter *stars*
  (list '(:star "A" :white "I" :red "H")
        '(:star "B" :white "G" :red "D")
        '(:star "C" :white "A" :red "J")
        '(:star "D" :white "I" :red "F")
        '(:star "E" :white "C" :red "B")
        '(:star "F" :white "A" :red "H")
        '(:star "G" :white "E" :red "D")
        '(:star "H" :white "C" :red "J")
        '(:star "I" :white "G" :red "F")
        '(:star "J" :white "E" :red "B")))

(defun string->list (str)
  (mapcar 'string (coerce str 'list)))

(defun list->string (lst)
  (coerce (mapcar 'character lst) 'string))

(defun route (name)
  (if (equal name "W") :white :red))

(defun name-of (star)
  (getf star :star))

(defun find-star (star)
  (car
    (remove-if-not
      #'(lambda (s) (equal (name-of s) star))
      *stars*)))

(defun dest (&key from route)
  (find-star (getf from (route route))))

(defun stars-cruise (course visited)
  (let* ((current-star (find-star (car course)))
         (destination (name-of (dest :from current-star :route (cadr course))))
         (course-remaining (cddr course)))

        (push (name-of current-star) visited)

        (if course-remaining
          (stars-cruise (cons destination course-remaining) visited)
          (progn
            (push destination visited)
            (list->string (reverse visited))))))

(defun test (course visited)
  (equal (stars-cruise (string->list course) nil) visited))

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
