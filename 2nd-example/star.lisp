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

(defparameter *cruise* nil)

(defun star (s)
  (getf s :star))

(defun find-star (star)
  (car
    (remove-if-not
      #'(lambda (s) (equal (star s) star))
      *stars*)))

(defun route (name)
  (if (equal name "W") :white :red))

(defun next-star (current route-name)
  (find-star (getf current (route route-name))))

(defun stars-cruise (tours)
  (let* ((current-star (find-star (pop tours)))
         (next-route (pop tours))
         (next-star (star (next-star current-star next-route))))

        (push (star current-star) *cruise*)

        (if tours
          (stars-cruise (cons next-star tours))
          (push next-star *cruise*))))

(defun run-cruise (tours)
  (setq *cruise* nil)
  (stars-cruise (mapcar 'string (coerce tours 'list)))
  (coerce (mapcar 'character (reverse *cruise*)) 'string))

; unit test
(ql:quickload :lisp-unit)
(use-package 'lisp-unit)

(define-test test-stars-cruise
  (assert-equal (run-cruise "AW") "AI")
  (assert-equal (run-cruise "GR") "GD")
  (assert-equal (run-cruise "GW") "GE")
  (assert-equal (run-cruise "IR") "IF")
  (assert-equal (run-cruise "HR") "HJ")
  (assert-equal (run-cruise "BWW") "BGE")
  (assert-equal (run-cruise "ARW") "AHC")
  (assert-equal (run-cruise "GRR") "GDF")
  (assert-equal (run-cruise "BWR") "BGD")
  (assert-equal (run-cruise "JWWW") "JECA")
  (assert-equal (run-cruise "DRRR") "DFHJ")
  (assert-equal (run-cruise "CWWR") "CAIF")
  (assert-equal (run-cruise "HWWW") "HCAI")
  (assert-equal (run-cruise "GWRWR") "GEBGD")
  (assert-equal (run-cruise "FRRRW") "FHJBG")
  (assert-equal (run-cruise "JRRWW") "JBDIG")
  (assert-equal (run-cruise "JWWRRW") "JECJBG")
  (assert-equal (run-cruise "GRRRWW") "GDFHCA")
  (assert-equal (run-cruise "BRWRWR") "BDIFAH")
  (assert-equal (run-cruise "IRWRRWR") "IFAHJEB")
  (assert-equal (run-cruise "IWWWRRW") "IGECJBG")
  (assert-equal (run-cruise "GWWRWWR") "GECJECJ")
  (assert-equal (run-cruise "HRRWRWRW") "HJBGDIFA")
  (assert-equal (run-cruise "FRWWWRRW") "FHCAIFHC")
  (assert-equal (run-cruise "HRWWWRWRW") "HJECAHCJE")
  (assert-equal (run-cruise "CWWWRRWWW") "CAIGDFAIG")
  (assert-equal (run-cruise "BRRRWRRRRW") "BDFHCJBDFA")
  (assert-equal (run-cruise "FRWRRWRRWW") "FHCJBGDFAI")
  (assert-equal (run-cruise "GWRRRRWRWRW") "GEBDFHCJEBG")
  (assert-equal (run-cruise "DRWWWWWWRRW") "DFAIGECAHJE")
  (assert-equal (run-cruise "ARRRRWRRRRWW") "AHJBDIFHJBGE")
  (assert-equal (run-cruise "AWWWWWWRRWRR") "AIGECAIFHCJB")
  (assert-equal (run-cruise "JWWWRRWRWRWWR") "JECAHJEBGDIGD")
  (assert-equal (run-cruise "CRWRWRRWWWRWW") "CJEBGDFAIGDIG")
  (assert-equal (run-cruise "DWRWRWRWRWWRWW") "DIFAHCJEBGEBGE")
  (assert-equal (run-cruise "GRWWWRRRRWRWRR") "GDIGEBDFHCJEBD")
  (assert-equal (run-cruise "ARWWWRWWRWWWWWW") "AHCAIFAIFAIGECA")
  (assert-equal (run-cruise "DWRWRRWRWWRWWRW") "DIFAHJEBGEBGEBG")
  (assert-equal (run-cruise "JRWRRRRRWRRRRRWR") "JBGDFHJBGDFHJBGD")
  (assert-equal (run-cruise "IRWWRRWWWWWRRWWR") "IFAIFHCAIGEBDIGD"))

(run-tests '(test-stars-cruise))
