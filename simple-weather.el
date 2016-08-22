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

(defvar sw/open-weather-api-key "85a4e3c55b73909f42c6a23ec35b7147")

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
     ((eq :forecast cmd) (sw/format-forecast raw-data))
     ((eq :current cmd) (sw/format-current raw-data)))))

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

(defun sw/format-forecast (raw)
  (let ((forecast (sw/extract-forecast raw)))
    ;; (mapconcat 'identity forecast " ")
    forecast
    ))

(defun sw/format-current (raw)
  (let* ((weather (sw/modify-for-units (sw/extract-current raw))))
    (format "%s %s %s %s %s %s %s %s %s%s %s %s %s %s %s%% %s %s %s %s%s"
            (plist-get sw/texts :current)
            (plist-get weather :name)
            (plist-get weather :temperature)
            (plist-get weather :scale)
            (plist-get weather :sky-icon)
            sw/dashes
            (plist-get sw/texts :wind)
            sw/delimiter
            (plist-get weather :wind)
            (plist-get weather :speed_unit)
            (plist-get weather :direction)
            sw/dashes
            (plist-get sw/texts :humidity)
            sw/delimiter
            (plist-get weather :humidity)
            sw/dashes
            (plist-get sw/texts :pressure)
            sw/delimiter
            (plist-get weather :pressure)
            (plist-get weather :pressure-unit))))

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
           (main (plist-get result :main))
           (sys (plist-get result :sys))
           (wind (plist-get result :wind))
           (sky (plist-get (car (plist-get result :weather))
                           :main))
           (name (plist-get result :name))
           (sky-icon (sw/get-icon sky))
           (temperature (plist-get main :temp))
           (humidity (plist-get main :humidity))
           (pressure (plist-get main :pressure))
           (sunrise (plist-get sys :sunrise))
           (sunset (plist-get sys :sunset))
           (wind-speed (plist-get wind :speed))
           (azimuth (plist-get wind :deg))
           (direction (sw/azimuth-to-cardinal azimuth))
           (period (sw/find-period sunrise sunset)))

      `(:name ,name :sky ,sky :sky-icon ,sky-icon :temperature ,temperature :humidity ,humidity
              :pressure ,pressure :sunrise ,sunrise :sunset ,sunset
              :wind ,wind-speed :azimuth ,azimuth :direction ,direction
              :period ,period))))

(defun sw/epoch-to-date (epoch)
  (format-time-string "%a %b %d" (seconds-to-time epoch)))

(defun sw/azimuth-to-cardinal (azimuth)
  (let ((directions '(N NNE NE ENE E ESE SE SSE S SSW SW WSW W WNW NW NNW)))
    (nth (round (/ (mod (+ azimuth 11.25) 360) 22.5)) directions)))

(defun sw/find-period (sunrise sunset)
  (let ((now (current-time))
        (period 'day))
    (when (or (time-less-p (seconds-to-time sunset) now)
              (time-less-p now (seconds-to-time sunrise)))
      (setf period 'night))
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
                        '(:scale "°F" :speed_unit "mph" :pressure_unit "inHg"))
            out (plist-put out
                           :pressure
                           (format "%0.2f" (* 0.0295
                                              (or (plist-get out :pressure) 0.1)))))))
    out))

(defun sw/get-icon (status)
  (plist-get sw/icons (intern (concat ":" (downcase status)))))

(provide 'simple-weather)
