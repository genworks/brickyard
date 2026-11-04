;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(gwl:define-package :brickyard
  (:export #:assembly #:*sample-build*
           #:world-connectors #:world-extent #:connections #:collisions
           #:snap-spec #:to-ldraw #:gate #:private-beta))

(defpackage :brickyard-api
  (:use :common-lisp)
  (:export #:use-build #:new-build #:parts #:report #:joints #:describe-build
           #:add-part #:move-part #:turn-part #:swing-part #:color-part #:copy-part
           #:remove-part #:clear-build))
