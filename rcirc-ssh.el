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
(setq lexical-binding t)

(defvar rcirc--server-ssh-connections nil
  "List of ssh connection buffer/processes.

This is state for rcirc-ssh.  It keeps the list of servers to
which we have a connection.")

(defvar rcirc--ssh-session-history nil
  "Completing read history variable.")

(defvar rcirc--ssh-server-history nil
  "Completing read history variable.")

(defvar rcirc--ssh-port-history nil
  "Completing read history variable.")

(defcustom rcirc-ssh-servers ()
  "List of rcirc servers that require an SSH connection first.

A server in this list will have the connection to it made through
an SSH session proxy.  The ssh connection is made with a port
forward to the relevant port (using a randomized local port) and
then the rcirc connection is made to that."
  :group 'rcirc
  :type '(repeat string))

(defun rcirc--do-ssh (host port)
  "Make an rcirc ssh session."
  (interactive
   (list
    (read-from-minibuffer
     "host: "
     nil
     nil
     nil
     'rcirc--ssh-server-history)
    (string-to-number
     (read-from-minibuffer
      "port: "
      nil
      nil
      nil
      'rcirc--ssh-port-history))))
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
           "ssh" "-N" "-L" (format "%d:localhost:6667" port) host))
         (pair (cons url-form (list :process proc :localport 6667))))
    (add-to-list 'rcirc--server-ssh-connections pair)
    pair))

(defun rcirc-ssh-kill (host-port)
  "Kill the ssh sesion for HOST-PORT a string.

The string is like: host:port, eg: localhost:22"
  (interactive
   (list
    (completing-read
     "host:port: "
     rcirc--server-ssh-connections
     nil ; predicate
     t   ; require-match
     nil ; initial
     'rcirc--ssh-session-history)))
  (let ((pair (assoc host-port rcirc--server-ssh-connections)))
    (when pair
      (delete-process
       (plist-get
        (assoc-default host-port rcirc--server-ssh-connections)
        :process))
      (setq rcirc--server-ssh-connections
            (delq pair rcirc--server-ssh-connections)))))

(defun rcirc-ssh-list ()
  "List the current rcirc ssh sessions."
  (interactive)
  (with-current-buffer (get-buffer-create "*rcirc ssh sessions*")
    (setq buffer-read-only t)
    (unwind-protect
         (let ((inhibit-read-only t))
           (erase-buffer)
           (loop for session in rcirc--server-ssh-connections
              do
                (destructuring-bind (host-port &key process localport) session
                  (princ
                   (format "%s on %s    [%s]\n"
                           host-port
                           localport
                           (process-status process))
                   (current-buffer))))
           (switch-to-buffer (current-buffer))))))

(defadvice rcirc-connect (before
                          rcirc-ssh-server-connect
                          first
                          (server
                           &optional port nick user-name
                           full-name startup-channels password encryption)
                          activate)
  "Connecct to the ssh proxy for certain rcirc servers."
  (when (member server rcirc-ssh-servers)
    (let ((ssh-proxy (rcirc--do-ssh server (or port 6667))))
      (message
       "rcirc-ssh making connection to %s on port %d"
       server (or port 6667))
      (sit-for 2)
      (setq server "localhost")
      (setq port (plist-get (cdr ssh-proxy) :localport)))))

(provide 'rcirc-ssh)

;;; rcirc-ssh.el ends here
