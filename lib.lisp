(defun num-digits (n pow num)
  (if (>= n (expt 10 pow))
      (num-digits n (1+ pow) (1+ num))
      num))

(defun split-string-by-space (string)
  (loop for i = 0 then (1+ j) as j = (position #\Space string :start i)
	collect (subseq string i j)
	while j))