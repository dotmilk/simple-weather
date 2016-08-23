(defvar sw/location "4468261")
(defvar sw/units :metric)
(defvar sw/delimiter "=>")
(defvar sw/dashes "-")

(defvar sw/texts '(:current "Current weather in"
                            :wind "Wind"
                            :humidity "Humidity"
                            :pressure "Pressure"
                            :sunrise "Sunrise"
                            :sunset "Sunset"
                            :forecast "forecast"))

(defvar sw/icons `(:clear "☀"
                          :sun "☀"
                          :moon "☽"
                          :clouds "☁"
                          :rain "☔"
                          :fog "▓"
                          :mist "░"
                          :haze "▒"
                          :snow "❄"
                          :thunderstorm "⚡"))

(defvar sw/open-weather-api-key "28269b2a8af5c37613ccf316df28faa7")

(defun sw/current ()
  (interactive)
  (message "%s" (sw/fetch :current sw/location)))

(defun sw/forecast ()
  (interactive)
  (message "%s" (sw/fetch :forecast sw/location)))

(defun sw/fetch (cmd location)
  (let* ((url (sw/build-url cmd location))
         (raw-data (url-retrieve-synchronously url)))
    (cond
     ((eq :forecast cmd) (sw/extract-forecast raw-data))
     ((eq :current cmd) (sw/extract-current raw-data)))))

(defun sw/build-url (cmd location)
  (let* ((url-cmd (cond
                   ((eq :current cmd) "weather?")
                   ((eq :forecast cmd) "forecast/daily?")))
         (url-location (format "id=%s" location))
         (url (format "http://api.openweathermap.org/data/2.5/%s%s&units=%s&appid=%s"
                      url-cmd
                      url-location
                      (substring (symbol-name sw/units) 1)
                      sw/open-weather-api-key)))
    url))

(defun sw/extract-forecast (raw)
  (with-current-buffer raw
    (goto-char url-http-end-of-headers)
    (delete-region (point-min) (1+ (point)))
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (result (sw/modify-for-units (json-read-from-string (buffer-string))))
           (city (plist-get result :city))
           (out `(,(plist-get city :name)
                  ,(plist-get sw/texts :forecast)
                  ,sw/delimiter))
           (days (plist-get result :list))
           (collected (subseq (sw/collect-forecast-days days
                                                        (plist-get result :scale))
                              0 5)))
      (concat (mapconcat 'identity out " ")
              " "
              (mapconcat 'identity collected " - ")))))

(defun sw/collect-forecast-days (days scale)
  (cl-loop for day
           in days
           collect (mapconcat
                    'identity
                    `(,(concat
                        (sw/epoch-to-date (plist-get day :dt))
                        ":")
                      ,(concat
                        (format "%.0f"
                                (plist-get (plist-get day :temp) :min))
                        "/"
                        (format "%.0f"
                                (plist-get (plist-get day :temp) :max)))
                      ,scale
                      ,(sw/get-icon
                        (plist-get (car (plist-get day :weather)) :main))) " ")))

(defun sw/extract-current (raw)
  (with-current-buffer
      raw
    (goto-char url-http-end-of-headers)
    (delete-region (point-min) (1+ (point)))
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (result (json-read-from-string (buffer-string)))
           (main (sw/modify-for-units (plist-get result :main)))
           (sys (plist-get result :sys))
           (wind (plist-get result :wind))
           (sky (plist-get (car (plist-get result :weather)) :main))
           (name (plist-get result :name))
           (temperature (plist-get main :temp))
           (humidity (plist-get main :humidity))
           (pressure (plist-get main :pressure))
           (sunrise (plist-get sys :sunrise))
           (sunset (plist-get sys :sunset))
           (wind-speed (plist-get wind :speed))
           (azimuth (plist-get wind :deg))
           (direction (sw/azimuth-to-cardinal azimuth))
           (period (sw/find-period sunrise sunset)))
      (mapconcat 'identity `(,(plist-get sw/texts :current)
                             ,name
                             ,(format "%.0f" temperature)
                             ,(plist-get main :scale)
                             ,(if (equal sky "Clear")
                                  (plist-get sw/icons period)
                                (sw/get-icon sky))
                             ,sw/dashes
                             ,(plist-get sw/texts :wind)
                             ,sw/delimiter
                             ,(format "%0.f" wind-speed)
                             ,(plist-get main :speed-unit)
                             ,(format "%s" direction)
                             ,sw/dashes
                             ,(plist-get sw/texts :humidity)
                             ,sw/delimiter
                             ,(format "%0.f" humidity)
                             ,sw/dashes
                             ,(plist-get sw/texts :pressure)
                             ,sw/delimiter
                             ,pressure
                             ,(plist-get main :pressure-unit))
                 " "))))

(defun sw/epoch-to-date (epoch)
  (format-time-string "%a %b %d" (seconds-to-time epoch)))

(defun sw/azimuth-to-cardinal (azimuth)
  (let ((directions '(N NNE NE ENE E ESE SE SSE S SSW SW WSW W WNW NW NNW)))
    (nth (round (/ (mod (+ azimuth 11.25) 360) 22.5)) directions)))

(defun sw/find-period (sunrise sunset)
  (milk-trace `(,sunrise ,sunset))
  (let ((now (current-time))
        (period :sun))
    (when (or (time-less-p (seconds-to-time sunset) now)
              (time-less-p now (seconds-to-time sunrise)))
      (setf period :moon))
    period))
;;(:scale "°C" :speed_unit "m/s" :pressure_unit "hPa")
(defun sw/modify-for-units (plist)
  (let* ((out plist))
    (cond
     ((eq :metric sw/units)
      (setq out (plist-put out :scale "°C")
            out (plist-put out :speed-unit "m/s")
            out (plist-put out :pressure-unit "hPa"))
      (when (plist-get plist :pressure)
        (plist-put out :pressure
                   (format "%0.0f" (plist-get out :pressure)))))
     ((eq :imperial sw/units)
      (setq out (append out
                        '(:scale "°F" :speed-unit "mph" :pressure-unit "inHg"))
            out (plist-put out
                           :pressure
                           (format "%0.2f" (* 0.0295
                                              (or (plist-get out :pressure) 0.1)))))))
    out))

(defun sw/get-icon (status)
  (plist-get sw/icons (intern (concat ":" (downcase status)))))

(provide 'simple-weather)
