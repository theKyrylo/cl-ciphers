(defparameter *alphabet* "AĄBCĆDEĘFGHIJKLŁMNŃOÓPQRSŚTUVWXYZŹŻ")

(defun caesar-encrypt (phrase offset)
  (loop
    with new-alphabet = (concatenate 'string
                                     (subseq *alphabet* offset)
                                     (subseq *alphabet* 0 offset))
    with result = ""
    for char across (string-upcase phrase)
    do (when (find char *alphabet*)
         (setf result
               (concatenate
                'string
                result (subseq *alphabet*
                               (position char new-alphabet)
                               (+ 1 (position char new-alphabet))))))
    finally (return-from caesar-encrypt result)))
