(ert-deftest orgability--drawer-block-test ()
  "Test that `orgability--drawer-block' works as expected."
  ;; no drawer
  (should
   (equal
    nil
    (with-one-header-and-no-drawer
     (orgability--drawer-block "topics"))))
  ;; empty drawer
  (should
   (equal
    (cons 77 86)
    (with-one-header-and-empty-drawer
     (orgability--drawer-block "topics"))))
  ;; empty drawer + inside
  (should
   (equal
    (cons 86 86)
    (with-one-header-and-empty-drawer
     (orgability--drawer-block "topics" nil t))))
  ;; drawer
  (should
   (equal
    (cons 77 104)
    (with-one-header-and-drawer
     (orgability--drawer-block "topics"))))
  ;; drawer + inside
  (should
   (equal
    (cons 86 104)
    (with-one-header-and-drawer
     (orgability--drawer-block "topics" nil t))))
  ;; no drawer + force == empty drawer
  (should
   (equal
    (with-one-header-and-empty-drawer
     (orgability--drawer-block "topics"))
    (with-one-header-and-no-drawer
     (orgability--drawer-block "topics" t))))
  ;; no drawer + force + inside == empty drawer + inside
  (should
   (equal
    (with-one-header-and-empty-drawer
     (orgability--drawer-block "topics" nil t))
    (with-one-header-and-no-drawer
     (orgability--drawer-block "topics" t t)))))
