(defvar *alphabet* "AĄBCĆDEĘFGHIJKLŁMNŃOÓPQRSŚTUVWXYZŻŹ")

(defun caesar-encode (string offset &key (alphabet *alphabet*) ignore-other-symbols ignore-case)
  (with-output-to-string (stream)
    (loop for char across string
          for position = (position char alphabet :test #'char-equal)
          if (null position)
            do (if (null ignore-other-symbols)
                   (write-char char stream))
          else do
            (let* ((new-position (mod (- position offset) (length alphabet)))
                   (new-char (char alphabet new-position))
                   (new-char (if (and (upper-case-p char) (null ignore-case))
                                 new-char (char-downcase new-char))))
              (write-char new-char stream)))))

(defun caesar-decode (string offset &key (alphabet *alphabet*))
  (caesar-encode string (- offset) :alphabet alphabet
                                   :ignore-other-symbols nil
                                   :ignore-case nil))
