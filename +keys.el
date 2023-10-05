;;; +keys.el -*- lexical-binding: t; -*-

(map! :map 'evil-normal-state-map
      :prefix "z"
      "<right>" #'evil-scroll-right
      "<left>" #'evil-scroll-left)
