;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define (gnc:expand-path new-list current-list default-generator)
  (define (expand-path-item item)
    (cond ((string? item) (list item))
          ((symbol? item)
           (case item
             ((default) (default-generator))
             ((current) current-list)
             (else
              (gnc:warn "bad symbol " item " in gnc path. Ignoring.")
              '())))
          (else 
           (gnc:warn "bad item " item " in gnc path. Ignoring.")
           '())))
  (apply append (map expand-path-item new-list)))

(define (gnc:make-dir dir)
  (if (access? dir X_OK)
      #t
      (false-if-exception (mkdir dir #o700))))

(define (gnc:make-home-dir)
  (let ((home-dir (build-path (getenv "HOME") ".gnucash")))
    (gnc:make-dir home-dir)))

(define gnc:current-config-auto
  (build-path (getenv "HOME") ".gnucash" "config-1.6.auto"))

(define gnc:load-user-config-if-needed
  (let ((user-config-loaded? #f))

    (define (try-load file-suffix)
      (let ((file (build-path (getenv "HOME") ".gnucash" file-suffix)))
        (if (access? file F_OK)
            (if (false-if-exception (primitive-load file))
                (begin
                  (set! user-config-loaded? #t)
                  #t)
                (begin
                  (gnc:warn "failure loading " file)
                  #f))
            #f)))

    (lambda ()
      (if (not user-config-loaded?)
          (begin
            (gnc:debug "loading user configuration")
            (or-map try-load
                    '("config-1.6.user" "config.user"
                      "config-1.6.auto" "config.auto")))))))

;; the system config should probably be loaded from some directory
;; that wouldn't be a site wide mounted directory, like /usr/share
;; However, the code below seems to zero in on /usr/share/gnucash/config
;; ... ahh but that's OK, right ??
(define gnc:load-system-config-if-needed
  (let ((system-config-loaded? #f))
    (lambda ()
      (if (not system-config-loaded?)
          (begin
            (gnc:debug "loading system configuration")
            
            (let ((system-config (gnc:find-file
                                  "config"
                                  (gnc:config-var-value-get gnc:*config-path*))))
              (if (false-if-exception (primitive-load system-config))
                  (set! system-config-loaded? #t)        
                  (begin
                    (gnc:warn "failure loading " system-config)
                    #f))))))))
