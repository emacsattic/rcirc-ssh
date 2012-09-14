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

(defun rcirc--do-ssh (host port &optional callback)
  "Make an rcirc ssh session to HOST on PORT.

Optionally call CALLBACK when the processes state changes.
Callback is passed the PROC, the STATUS and the LOCAL-PORT."
  (let* ((url-form (format "%s:%d" host port))
         (connection-str (format " *ssh-%s-%d*" host port))
         (ssh-buffer (get-buffer-create connection-str))
         (local-port 6667) ; could randomize this
         (proc
          (start-process
           ;; We should check for an existing process with this name before
           ;; starting the process
           connection-str
           ssh-buffer
           "ssh" "-N" "-L" (format "%d:localhost:%d" port local-port) host)))
    ;; Set the sentinel
    (set-process-sentinel
     proc
     (lambda (proc status)
       (message
        "rcirc-ssh connection to %s has status: %s"
        url-form
        status)
       (when (functionp callback)
         (funcall callback proc status local-port))))
    ;; Make sure the state of what proceses we have gets updated
    (let ((pair (cons url-form (list :process proc :localport 6667))))
      (add-to-list 'rcirc--server-ssh-connections pair)
      pair)))

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


;;;###autoload
(defun rcirc-ssh-connect (server
                          &optional port nick user-name
                            full-name startup-channels password encryption)
  "Connecct to the rcirc with possible ssh proxying."
  (if (member server rcirc-ssh-servers)
      (rcirc--do-ssh
       server
       (or port 6667)
       ;; Supply the callback to start irc after the ssh
       (lambda (proc status local-port)
         (when (equal status "started\n") ; WHAT STATE SHOULD IT BE??
           ;; Do the proxy connection over the ssh tunnel
           (rcirc-ssh--rcirc-connect
            "localhost"
            local-port
            nick user-name full-name
            startup-channels password
            encryption))))
      ;; Else do a straight connection to the server
      (rcirc-ssh--rcirc-connect
       server
       port
       nick user-name full-name
       startup-channels password
       encryption)))

;; Bootstrapping

(defvar rcirc-ssh--rcirc-connect 'x)

(defun rcirc-ssh--bootstrap ()
  "Bootstrap rcirc-ssh by taking over `rcirc-connect'.

The `rcirc-connect' function is saved and changed to the
`rcirc-ssh-connect' function which does ssh connection to the irc
server (if required by the `rcirc-ssh-servers' variable) before
setting up the irc connection.

The original function is saved on the `rcirc-connect' symbol with
the property `rcirc-original'."
  (unless (get 'rcirc-connect 'rcirc-original)
    (let ((original (symbol-function 'rcirc-connect)))
      (put 'rcirc-connect 'rcirc-original original)
      (fset rcirc-ssh--rcirc-connect original)
      (fset rcirc-connect (symbol-function 'rcirc-ssh-connect)))))


(provide 'rcirc-ssh)

;;; rcirc-ssh.el ends here
