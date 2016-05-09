(ert-deftest spaceline-literal-segments ()
  "Tests literal segments."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '("a" "b" 1.23 nil "longer" -2) nil)
    (should (string= " a > b > 1.23 > longer > -2 > "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-list-segments ()
  "Tests list segments."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '(("a" "b" "c") ("def" "ghj")) nil)
    (should (string= " a b c > def ghj > "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-nested-list-segments ()
  "Tests nested list segments."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '(("a" "b" "c") ("def" ("rofl" "mao" "sup") "ghj")) nil)
    (should (string= " a b c > def rofl mao sup ghj > "
                     (spaceline-ml-main)))))

(ert-deftest spaceline-list-separators ()
  "Tests the :separator property of lists."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '((("a" "b" "c") :separator "~~")) nil)
    (should (string= " a~~b~~c > " (spaceline-ml-main)))))

(ert-deftest spaceline-nested-list-separators ()
  "Tests the :separator property of nested lists."
  (let ((powerline-default-separator 'utf-8)
        (powerline-utf-8-separator-left ?>))
    (spaceline-install '((("a" (("b" "c" "d") :separator "$$") "e") :separator "~~")) nil)
    (should (string= " a~~b$$c$$d~~e > " (spaceline-ml-main)))))
