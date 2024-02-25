;;; early-init.el --- Emacs config before GUI load -*- coding: utf-8; lexical-binding: t; -*-


;; Copyroght (C) 2024 pachicobue

;; Author: pachicobue <tigerssho@gmail.com>

;; This program is free software: you can redistribute it and/or modify
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

;; Emacsの設定ファイル

;;; Code:

(setq debug-on-error t)
(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(setq user-emacs-directory (expand-file-name "local/" user-emacs-directory))
(custom-set-variables '(custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; early-init.el ends here
