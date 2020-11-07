(progn
  (princ "\nSelectionFilter v1.0 by Masoud Nasiri, 6 November, 2020 @ Copyleft. All wrongs resered.")
  (princ)
)

(defun c:selectionFilter ()
  (defun-q *error* (messageError)(princ))
  (firstMenu))

(defun-q firstMenu (/ filterType)
  (initget "a A b B c C e E h H l L m M o O p P r R s S t T 3d 3D")
  (setq filterType (getkword "\nEnter entity type <Other>:[Line/Polyline/Arc/Circle/Spline/Ellipse/Hatch/Text/Mtext/Block/3D Solid/Region/Other]"))
  (if (or (= filterType nil) (= filterType "O"))
    (secondMenu)
    (filterFunction T filterType)
  ))

(defun-q secondMenu (/ filterType)
  (initget "b B h H m M p P r R x X 3d 3D")
  (setq filterType (getkword "\nEnter entity type <Back>:[Point/3D Polyline/Helix/Mline/Xline/Ray/Back]"))
  (if (or (= filterType nil) (= filterType "B"))
    (firstMenu)
    (filterFunction nil filterType)
  ))

(defun-q filterFunction (mainMenu filterType / selectionSet)
  (if mainMenu
    (cond
      ((= (strcase filterType) "A")  (setq filterType "ARC"))
      ((= (strcase filterType) "B")  (setq filterType "INSERT"))
      ((= (strcase filterType) "C")  (setq filterType "CIRCLE"))
      ((= (strcase filterType) "E")  (setq filterType "ELLIPSE"))
      ((= (strcase filterType) "H")  (setq filterType "HATCH"))
      ((= (strcase filterType) "L")  (setq filterType "LINE"))
      ((= (strcase filterType) "M")  (setq filterType "MTEXT"))
      ((= (strcase filterType) "P")  (setq filterType "LWPOLYLINE"))
      ((= (strcase filterType) "R")  (setq filterType "REGION"))
      ((= (strcase filterType) "S")  (setq filterType "SPLINE"))
      ((= (strcase filterType) "T")  (setq filterType "TEXT"))
      ((= (strcase filterType) "3D") (setq filterType "3DSOLID")))
    (cond
      ((= (strcase filterType) "H")  (setq filterType "HELIX"))
      ((= (strcase filterType) "M")  (setq filterType "MLINE"))
      ((= (strcase filterType) "P")  (setq filterType "POINT"))
      ((= (strcase filterType) "R")  (setq filterType "RAY"))
      ((= (strcase filterType) "X")  (setq filterType "XLINE"))
      ((= (strcase filterType) "3D") (setq filterType "POLYLINE"))))
  (setq selectionSet (cadr (ssgetfirst)))
  (if selectionSet
    (progn
      (command "_.select" selectionSet "")
      (sssetfirst nil (ssget "P" (list (cons 0 filterType)))))
    (sssetfirst nil (ssget (list (cons 0 filterType)))))
  (princ))
