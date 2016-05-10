(ert-deftest spaceline-literal-segments-l ()
  "Tests literal segments (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '("a" "b" 1.23 nil "longer" -2) nil)
    (should (string= " a > b > 1.23 > longer > -2 > "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-list-segments-l ()
  "Tests list segments (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '(("a" "b" "c") ("def" "ghj")) nil)
    (should (string= " a b c > def ghj > "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-nested-list-segments-l ()
  "Tests nested list segments (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '(("a" "b" "c") ("def" ("rofl" "mao" "sup") "ghj")) nil)
    (should (string= " a b c > def rofl mao sup ghj > "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-symbol-l ()
  "Tests symbols (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-define-segment a "" "a")
    (spaceline-define-segment b "" nil)
    (spaceline-define-segment c "" var)
    (spaceline-install '(a b) nil)
    (should (string= " a > " (spaceline-ml-main)))
    (spaceline-install '(a b c) nil)
    (let ((var "alpha"))
      (should (string= " a > alpha > " (spaceline-ml-main))))
    (let ((var "beta"))
      (should (string= " a > beta > " (spaceline-ml-main))))))

(ert-deftest spaceline-list-separators-l ()
  "Tests the :separator property of lists (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '((("a" "b" "c") :separator "~~")) nil)
    (should (string= " a~~b~~c > " (spaceline-ml-main)))))

(ert-deftest spaceline-nested-list-separators-l ()
  "Tests the :separator property of nested lists (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '((("a" (("b" "c" "d") :separator "$$") "e") :separator "~~")) nil)
    (should (string= " a~~b$$c$$d~~e > " (spaceline-ml-main)))))

(ert-deftest spaceline-symbol-list-l ()
  "Tests symbols returning lists, with the :separator property (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-define-segment a "" '("a" "b" "c"))
    (spaceline-define-segment b "" '("a" "b" "c") :separator "^")
    (spaceline-install '(a) nil)
    (should (string= " a b c > " (spaceline-ml-main)))
    (spaceline-install '((a :separator ".")) nil)
    (should (string= " a.b.c > " (spaceline-ml-main)))
    (spaceline-install '(b) nil)
    (should (string= " a^b^c > " (spaceline-ml-main)))
    (spaceline-install '((b :separator "!")) nil)
    (should (string= " a!b!c > " (spaceline-ml-main)))))

(ert-deftest spaceline-fallback-literal-l ()
  "Tests the :fallback property for literals (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '((nil :fallback "b")) nil)
    (should (string= " b > " (spaceline-ml-main)))
    (spaceline-install '(("a" :fallback "b")) nil)
    (should (string= " a > " (spaceline-ml-main)))))

(ert-deftest spaceline-fallback-symbol-l ()
  "Tests the :fallback property for symbols (left side)."
  (spaceline-define-segment success "" "success")
  (spaceline-define-segment failure "" nil)
  (spaceline-define-segment fallback "" nil :fallback success)
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '((failure :fallback "b")) nil)
    (should (string= " b > " (spaceline-ml-main)))
    (spaceline-install '((success :fallback "b")) nil)
    (should (string= " success > " (spaceline-ml-main)))
    (spaceline-install '(fallback) nil)
    (should (string= " success > " (spaceline-ml-main)))))

(ert-deftest spaceline-tight-l ()
  "Tests the :tight properties (left side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '("a" ("b" :tight t) "c") nil)
    (should (string= " a b c > " (spaceline-ml-main)))
    (spaceline-install '("a" ("b" :tight-left t) "c") nil)
    (should (string= " a b > c > " (spaceline-ml-main)))
    (spaceline-install '("a" ("b" :tight-right t) "c") nil)
    (should (string= " a > b c > " (spaceline-ml-main)))
    (spaceline-define-segment tight "" "tight" :tight t)
    (spaceline-install '("a" tight "c") nil)
    (should (string= " a tight c > " (spaceline-ml-main)))))

(ert-deftest spaceline-literal-segments-r ()
  "Tests literal segments (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '("a" "b" 1.23 nil "longer" -2))
    (should (string= " < a < b < 1.23 < longer < -2 "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-list-segments-r ()
  "Tests list segments (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '(("a" "b" "c") ("def" "ghj")))
    (should (string= " < a b c < def ghj "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-nested-list-segments-r ()
  "Tests nested list segments (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '(("a" "b" "c") ("def" ("rofl" "mao" "sup") "ghj")))
    (should (string= " < a b c < def rofl mao sup ghj "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-symbol-r ()
  "Tests symbols (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-define-segment a "" "a")
    (spaceline-define-segment b "" nil)
    (spaceline-define-segment c "" var)
    (spaceline-install nil '(a b))
    (should (string= " < a " (spaceline-ml-main)))
    (spaceline-install nil '(a b c))
    (let ((var "alpha"))
      (should (string= " < a < alpha " (spaceline-ml-main))))
    (let ((var "beta"))
      (should (string= " < a < beta " (spaceline-ml-main))))))

(ert-deftest spaceline-list-separators-r ()
  "Tests the :separator property of lists (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '((("a" "b" "c") :separator "~~")))
    (should (string= " < a~~b~~c " (spaceline-ml-main)))))

(ert-deftest spaceline-nested-list-separators-r ()
  "Tests the :separator property of nested lists (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '((("a" (("b" "c" "d") :separator "$$") "e") :separator "~~")))
    (should (string= " < a~~b$$c$$d~~e " (spaceline-ml-main)))))

(ert-deftest spaceline-symbol-list-r ()
  "Tests symbols returning lists, with the :separator property (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-define-segment a "" '("a" "b" "c"))
    (spaceline-define-segment b "" '("a" "b" "c") :separator "^")
    (spaceline-install nil '(a))
    (should (string= " < a b c " (spaceline-ml-main)))
    (spaceline-install nil '((a :separator ".")))
    (should (string= " < a.b.c " (spaceline-ml-main)))
    (spaceline-install nil '(b))
    (should (string= " < a^b^c " (spaceline-ml-main)))
    (spaceline-install nil '((b :separator "!")))
    (should (string= " < a!b!c " (spaceline-ml-main)))))

(ert-deftest spaceline-fallback-literal-r ()
  "Tests the :fallback property for literals (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '((nil :fallback "b")))
    (should (string= " < b " (spaceline-ml-main)))
    (spaceline-install nil '(("a" :fallback "b")))
    (should (string= " < a " (spaceline-ml-main)))))

(ert-deftest spaceline-fallback-symbol-r ()
  "Tests the :fallback property for symbols (right side)."
  (spaceline-define-segment success "" "success")
  (spaceline-define-segment failure "" nil)
  (spaceline-define-segment fallback "" nil :fallback success)
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '((failure :fallback "b")))
    (should (string= " < b " (spaceline-ml-main)))
    (spaceline-install nil '((success :fallback "b")))
    (should (string= " < success " (spaceline-ml-main)))
    (spaceline-install nil '(fallback))
    (should (string= " < success " (spaceline-ml-main)))))

(ert-deftest spaceline-tight-r ()
  "Tests the :tight properties (right side)."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-right ?<))
    (spaceline-install nil '("a" ("b" :tight t) "c"))
    (should (string= " < a b c " (spaceline-ml-main)))
    (spaceline-install nil '("a" ("b" :tight-left t) "c"))
    (should (string= " < a b < c " (spaceline-ml-main)))
    (spaceline-install nil '("a" ("b" :tight-right t) "c"))
    (should (string= " < a < b c " (spaceline-ml-main)))
    (spaceline-define-segment tight "" "tight" :tight t)
    (spaceline-install nil '("a" tight "c"))
    (should (string= " < a tight c " (spaceline-ml-main)))))

(ert-deftest spaceline-recompile ()
  "Tests recompilation."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-define-segment a "" "a")
    (spaceline-install '(a) nil)
    (should (string= " a > " (spaceline-ml-main)))
    (spaceline-define-segment a "" "b")
    (should (string= " a > " (spaceline-ml-main)))
    (spaceline-install)
    (should (string= " b > " (spaceline-ml-main)))
    (spaceline-define-segment a "" "a" :tight t)
    (should (string= " b > " (spaceline-ml-main)))
    (spaceline-install)
    (should (string= "a " (spaceline-ml-main)))
    (spaceline-define-segment a "" "c")
    (should (string= "a " (spaceline-ml-main)))
    (spaceline-install)
    (should (string= " c > " (spaceline-ml-main)))))
