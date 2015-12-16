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

(defun run-cruise (course &aux (course-list (string->list course)))
  (list->string
    (stars-cruise
      :star (find-star (car course-list))
      :course (cdr course-list))))

(defun test (course visited)
  (assert (equal (run-cruise course) visited)))

(test "AW" "AI")
(test "GR" "GD")
(test "GW" "GE")
(test "IR" "IF")
(test "HR" "HJ")
(test "BWW" "BGE")
(test "ARW" "AHC")
(test "GRR" "GDF")
(test "BWR" "BGD")
(test "JWWW" "JECA")
(test "DRRR" "DFHJ")
(test "CWWR" "CAIF")
(test "HWWW" "HCAI")
(test "GWRWR" "GEBGD")
(test "FRRRW" "FHJBG")
(test "JRRWW" "JBDIG")
(test "JWWRRW" "JECJBG")
(test "GRRRWW" "GDFHCA")
(test "BRWRWR" "BDIFAH")
(test "IRWRRWR" "IFAHJEB")
(test "IWWWRRW" "IGECJBG")
(test "GWWRWWR" "GECJECJ")
(test "HRRWRWRW" "HJBGDIFA")
(test "FRWWWRRW" "FHCAIFHC")
(test "HRWWWRWRW" "HJECAHCJE")
(test "CWWWRRWWW" "CAIGDFAIG")
(test "BRRRWRRRRW" "BDFHCJBDFA")
(test "FRWRRWRRWW" "FHCJBGDFAI")
(test "GWRRRRWRWRW" "GEBDFHCJEBG")
(test "DRWWWWWWRRW" "DFAIGECAHJE")
(test "ARRRRWRRRRWW" "AHJBDIFHJBGE")
(test "AWWWWWWRRWRR" "AIGECAIFHCJB")
(test "JWWWRRWRWRWWR" "JECAHJEBGDIGD")
(test "CRWRWRRWWWRWW" "CJEBGDFAIGDIG")
(test "DWRWRWRWRWWRWW" "DIFAHCJEBGEBGE")
(test "GRWWWRRRRWRWRR" "GDIGEBDFHCJEBD")
(test "ARWWWRWWRWWWWWW" "AHCAIFAIFAIGECA")
(test "DWRWRRWRWWRWWRW" "DIFAHJEBGEBGEBG")
(test "JRWRRRRRWRRRRRWR" "JBGDFHJBGDFHJBGD")
(test "IRWWRRWWWWWRRWWR" "IFAIFHCAIGEBDIGD")
