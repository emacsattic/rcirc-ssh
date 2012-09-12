;;; rcirc-ssh.el --- do irc over ssh sessions

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is makes ssh sessions to boxes which have irc daemons on
;; them. You ssh in with port forwarding and then establish the irc
;; connection over the tunnel.

;;; Code:

(require 'cl)
(require 'kv)

(defvar rcirc--server-ssh-connections nil
  "List of ssh connection buffer/processes")
(defvar rcirc--ssh-session-history nil
  "Completing read history variable.")
(defun rcirc--do-ssh (host port)
  (let* ((url-form (format "%s:%d" host port))
         (connection-str (format " *ssh-%s-%d*" host port))
         (ssh-buffer (get-buffer-create connection-str))
         (proc
          (start-process
           ;; We should check for an existing process with this name before
           ;; starting the process
           connection-str
           ssh-buffer
           ;; We could actually randomize the 2nd port
           "ssh" "-N" "-L" (format "%d:localhost:6667" port) host)))
    (add-to-list
     'rcirc--server-ssh-connections
     (cons url-form (list :process proc :localport 6667)))))

(defun rcirc--kill-ssh (host-port)
  "Kill the ssh sesion for HOST-PORT a string.

The string is like: host:port, eg: localhost:22"
  (interactive
   (list
    (completing-read
     "host:port: "
     rcirc--server-ssh-connections
     nil
     t ; require-match
     nil ; initial
     'rcirc--ssh-session-history)))
  (delete-process
   (plist-get
    (assoc-default host-port rcirc--server-ssh-connections)
    :process)))

(provide 'rcirc-ssh)
;;; rcirc-ssh.el ends here
