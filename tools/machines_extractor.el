
(defun elmame--serialize-devices (machine)
  (let (devices devices-str)
    (setq devices (seq-filter (lambda (p) (equal (car p) 'device))
                              (cddr machine)))
    (dolist (device devices)
      (let (device-str device-props inst inst-props inst-name inst-briefname)
        (when (string= "1" (alist-get 'mandatory (nth 1 device)))
          (setq device-str "mandatory t"))
        (setq device-props (cddr device))
        (setq inst (assoc 'instance device-props))
        (setq inst-props (nth 1 inst))
        (setq inst-name (alist-get 'name inst-props))
        (setq inst-briefname (alist-get 'briefname inst-props))
        (when inst-briefname
          (setq device-str
                (concat "briefname " (json-serialize inst-briefname)
                        " " device-str)))
        (when inst-name
          (setq device-str
                (concat "name " (json-serialize inst-name)
                        " " device-str)))
        (setq devices-str (concat devices-str
                                  (if (> (length (or devices-str "")) 0)
                                      " " "")
                                  "(" device-str ")" ))))
    (concat "(" devices-str ")")))

(defun elmame-machines-extractor ()
  (let (info-data)
    (save-window-excursion
      ;;(switch-to-buffer "**mame info xml**")
      (with-temp-buffer
	(erase-buffer)
	(insert-file "../documents/local/info/mame.xml")
	(setq info-data (libxml-parse-xml-region (point-min) (point-max))) )
      
      (switch-to-buffer "**machine lines**")
      (erase-buffer)
      (insert "(defun mame-machine-info-loader-load () '(") ;; write function def
      (dolist (machine (seq-drop info-data 2))
	(let (manufacturer name year desc isdevice runnable isbios)
	  
	  (setq isdevice (alist-get 'isdevice (nth 1 machine)))
	  (setq runnable (alist-get 'runnable (nth 1 machine)))
	  (setq isbios (alist-get 'isbios (nth 1 machine)))

	  (when (and (not (string= isdevice "yes"))
		     ;;(not (string= runnable "yes"))
		     (not (string= runnable "no")) ;; TODO: need to test
		     (not (string= isbios "yes")))
	    
	    (setq name (alist-get 'name (nth 1 machine)))
	    
	    (setq year (assoc 'year (seq-drop machine 2)))
	    (when year
	      (setq year (nth 2 year)) )
	    
	    (setq manufacturer (assoc 'manufacturer (seq-drop machine 2)))
	    (when manufacturer
	      (setq manufacturer (nth 2 manufacturer)) )
	    
	    (setq desc (assoc 'description (seq-drop machine 2)))
	    (when desc
	      (setq desc (nth 2 desc)) )
	    (insert (format "(name %s year %s manufacturer %s desc %s devices %s)"
			    (json-encode-string name)
			    (json-encode-string year)
			    (json-encode-string manufacturer)
			    (json-encode-string desc)
                            (elmame--serialize-devices machine))
                    "\n")
	    )
	  )
	)
      (insert ") )") ;; write function end
      )
    )
  )
