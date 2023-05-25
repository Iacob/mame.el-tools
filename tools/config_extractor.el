
(require 'xml)

(defun elmame-config-extractor ()
  (let (info-data)
    (save-window-excursion
      (switch-to-buffer "**mame info xml**")
      (erase-buffer)
      (insert-file "../documents/local/info/mame.xml")
      (setq info-data (xml-parse-region (point-min) (point-max)))

      (switch-to-buffer "**mame info el**")
      (insert (format "%s" info-data))
      ;;(write-file "mame_info.el")
      )
    )
  )
