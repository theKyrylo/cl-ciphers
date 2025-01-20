(defvar *alphabet* "AĄBCĆDEĘFGHIJKLŁMNŃOÓPQRSŚTUVWXYZŻŹ")

(defun caesar-encode (string offset &optional (alphabet *alphabet*))
  (with-output-to-string (stream)
    (loop for char across string
          for position = (position char alphabet :test #'char-equal)
          if (null position)
            do (write-char char stream)
          else do
            (let* ((new-position (mod (- position offset) (length alphabet)))
                   (new-char (char alphabet new-position))
                   (new-char (if (upper-case-p char) new-char (char-downcase new-char))))
              (write-char new-char stream)))))

(defun caesar-decode (string offset &optional (alphabet *alphabet*))
  (caesar-encode string (- offset) alphabet))
