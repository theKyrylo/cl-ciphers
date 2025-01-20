(defparameter *alphabet* "AĄBCĆDEĘFGHIJKLŁMNŃOÓPQRSŚTUVWXYZŹŻ")

(defun %shift-alphabet (alphabet offset)
  (concatenate 'string
                   (subseq alphabet (- (length alphabet) offset)) ; should I put this length to let??
                   (subseq alphabet 0 (- (length alphabet) offset))))

(defun %caesar-helper (phrase alphabet new-alphabet collector)
  (loop
    for char across (string-upcase phrase)
    do (when (find char alphabet)
         (setf collector
               (concatenate
                'string
                collector (subseq new-alphabet
                               (position char alphabet)
                               (+ 1 (position char alphabet))))))
    finally (return-from %caesar-helper collector)))

(defun caesar-encrypt (phrase offset)
  (let ((new-alphabet (%shift-alphabet *alphabet* offset)))
    (%caesar-helper phrase *alphabet* new-alphabet "")))

(defun caesar-decrypt (phrase offset)
  (let ((old-alphabet (%shift-alphabet *alphabet* offset)))
    (%caesar-helper phrase old-alphabet *alphabet* "")))
