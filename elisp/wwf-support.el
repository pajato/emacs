(defvar wwf-values '((?a 1) (?b 4) (?c 4) (?d 2) (?e 1) (?f 4) (?g 3) (?h 3) (?i 1) (?j 10) (?k 5) (?l 2) (?m 4) (?n 2) (?o 1) (?p 4) (?q 10) (?r 1) (?s 1) (?t 1) (?u 2) (?v 5) (?w 4) (?x 8) (?y 3) (?z 10)))

(defun wwf-search (rack re)
  "Using letters from the given RACK and the EXTRA letters, run the given RE over the WWF dictionary."
  (save-excursion
    (let ((tiles (concat rack (wwf-get-extra re)))
          (rack-letter-counts (wwf-count-letters-in-word rack))
          (word-buf (get-buffer-create "wwf-occur"))
          (dict-buf (get-buffer "wwf-dict.txt"))
          xre blank-count result)
      (set-buffer "wwf-dict.txt")
      (goto-char (point-min))
      (setq blank-count (or (cadr (assoc ?. rack-letter-counts)) 0)
            xre (wwf-expand-re re rack (> blank-count 0))
            result (wwf-process-words word-buf tiles))
      (concat "Searching for \"" xre "\" and found:\n" result))))

(defun wwf-get-extra (re)
  "Extract the extra tiles used in the regular expression RE.
The extra tiles are those that are upcased."
  (let ((letter-list (string-to-list re))
        (result "")
        letter)
    (while letter-list
      (setq letter (car letter-list)
            letter-list (cdr letter-list))
      (if (null (equal letter (downcase letter)))
          (setq result (concat result (char-to-string (downcase letter))))))
    result))

(defun wwf-occur (re buf)
  "Run 'occur' on the regular expression RE placing the results
in buffer BUF. Return true iff some matches are found."
  (occur-1 (concat "^" xre "$") 0 (list (current-buffer)) buf)
  (buffer-name buf))

(defun wwf-validate (word)
  (wwf-search word "" word))

(defun wwf-expand-re (re rack blankp)
  "Expand the given RE substituting the letters in the rack for each '!' encountered."
  (let ((rack-re (concat "[" rack "]"))
        (letter-list (string-to-list re))
        letter result)
    (while letter-list
      (setq letter (car letter-list)
            letter-list (cdr letter-list)
            result (cond
                    ((and (null result) (eq ?! letter)) (wwf-substitute blankp rack-re))
                    ((null result)  (char-to-string (downcase letter)))
                    ((eq ?! letter) (concat result (wwf-substitute blankp rack-re)))
                    (t (concat result (char-to-string (downcase letter)))))))
    result))

(defun wwf-substitute (blankp re)
  (if blankp
      "."
    re))

(defun wwf-process-words (buf tiles)
  "Process all occurrences of WWF dictionary words which match
the regular expression RE placing the results in buffer BUF to
see if they match the letters in TILES."
  (let ((re "[ ]+[0-9]+:\\(.+\\)")
        (no-matches "No WWF matches")
        (tiles-letter-counts (wwf-count-letters-in-word tiles))
        wwf-word wwf-words wwf-count result)
    ;; Determine if there are any potential matches:
    (wwf-occur re buf)
    (if (null (buffer-name buf))
        (setq result no-matches)
      ;; Process the potential matches...
      (set-buffer buf)
      (goto-char (point-min))
      (forward-line 1)
      (while (looking-at re)
        (if (wwf-is-valid-word (match-string 1) tiles-letter-counts)
            (cond
             ((null wwf-words) (setq wwf-words (list (match-string 1))))
             (t (setq wwf-words (append (list (match-string 1)) wwf-words)))))
        (forward-line 1))
      (setq wwf-words (sort wwf-words 'wwf-sort))
      (setq wwf-count (length wwf-words))
      (setq result (format "%s\n" (cond
                                   ((= 0 wwf-count) no-matches)
                                   ((= 1 wwf-count) "One WWF match")
                                   (t (format "%d WWF matches" wwf-count)))))
      ;; Determine if there are any words to format for the console.
      (if (> wwf-count 0)
          (while wwf-words
            (setq wwf-word (car wwf-words)
                  wwf-words (cdr wwf-words)
                  result (concat result (format "%s - %d\n" wwf-word (wwf-word-value wwf-word)))))))
    result))

(defun wwf-word-value (word)
  (let ((letter-counts (wwf-count-letters-in-word word))
        (result 0)
        values)
    (setq values (mapcar 'wwf-letter-values letter-counts))
    (while values
      (setq value (car values)
            values (cdr values)
            result (+ result value)))
    result))

(defun wwf-letter-values (pair)
  (let ((base (cadr (assoc (car pair) wwf-values)))
        (multiplier (cadr pair)))
    (* base multiplier)))

(defun wwf-sort (first second)
  "Return true if the length of FIRST is less than the length of SECOND."
  (< (length first) (length second)))

(defun wwf-count-letters-in-word (word)
  "Build and return an alist of component letters and the number of timee each is located."
  (let ((letter-list (string-to-list word))
        result letter pair)
    (while letter-list
      (setq letter (car letter-list)
            letter-list (cdr letter-list)
            pair (assoc letter result)
            result (cond
                    ((null result) (list (list letter 1)))
                    ((null pair) (append (list (list letter 1)) result))
                    ((numberp (cadr pair))
                     (setcdr pair (list (1+ (cadr pair))))
                     result))))
    result))

(defun wwf-is-valid-word (word tiles-letter-counts)
  "Return t iff the count of letters in wwf-word-letter-counts for each letter is less than or equal to the count of each letter in TILES-LETTER-COUNTS."
  ;; A word is valid in two ways: 1) in the absence of blank tiles
  ;; ('.' character) when the number of occurrences of each letter is
  ;; less than or equal to number of occurrences of that letter in the
  ;; TILES_LETTER_COUNTS argument, and 2) when the total number of
  ;; excess letters is less than or equal to the number of blanks in
  ;; the TILES_LETTER_COUNTS argument.
  (let ((blanks (or (cadr (assoc ?. tiles-letter-counts)) 0))
        (pairs (wwf-count-letters-in-word word))
        count letter pair)
    (while (and pairs (>= blanks 0))
      (setq pair (car pairs)
            pairs (cdr pairs)
            count (cadr pair))
      (if (not (wwf-is-valid-letter pair tiles-letter-counts))
          (setq blanks (- blanks count))))
    (and (null pairs) (>= blanks 0))))

(defun wwf-is-valid-letter (pair tiles-letter-counts)
  "Return t iff the number of occurences of the letter that is the car of arg is less than or equal to the number of occurrences of that letter in TILES-LETTER-COUNTS."
  (let ((letter (car pair))
        (count (cadr pair))
        base-pair)
    (setq base-pair (assoc letter tiles-letter-counts))
    (and base-pair (<= count (cadr base-pair)))))

(find-file "~/lib/emacs/elisp/wwf-dict.txt")
